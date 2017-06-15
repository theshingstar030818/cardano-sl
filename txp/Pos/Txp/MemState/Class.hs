{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TemplateHaskell     #-}

-- | Type class necessary for Transaction processing (Txp)
-- and some useful getters and setters.

module Pos.Txp.MemState.Class
       ( JLMemPool (..)
       , MonadTxpMem
       , askTxpMem
       , TxpHolderTag
       , getUtxoModifier
       , getLocalTxsNUndo
       , getMemPool
       , getLocalTxs
       , getLocalTxsMap
       , getTxpExtra
       , modifyTxpLocalData
       , setTxpLocalData
       , clearTxpMemPool
       ) where

import           Universum

import qualified Control.Concurrent.STM as STM
import           Data.Aeson.TH          (deriveJSON, defaultOptions)
import           Data.Default           (Default (def))
import qualified Data.HashMap.Strict    as HM
import qualified Ether
import qualified GHC.Conc               as Conc
import           System.IO.Unsafe       (unsafePerformIO)

import           Pos.Txp.Core.Types     (TxAux, TxId, TxOutAux)
import           Pos.Txp.MemState.Types (GenericTxpLocalData (..),
                                         GenericTxpLocalDataPure,
                                         MemPoolModifyReason)
import           Pos.Txp.Toil.Types     (MemPool (..), UtxoModifier)
import           Pos.Util.TimeWarp      (currentTime, CanJsonLog (..))

-- | Json log of one mempool modification.
data JLMemPool = JLMemPool
    { -- | Reason for modifying the mempool
      jlmReason      :: MemPoolModifyReason
      -- | Queue length when trying to modify the mempool (not including this
      --   modifier, so it could be 0).
    , jlmQueueLength :: Int
      -- | Time spent waiting for the lock (microseconds)
    , jlmWait        :: Integer
      -- | Time spent doing the modification (microseconds, while holding the lock).
    , jlmModify      :: Integer
      -- | Size of the mempool before the modification.
    , jlmSizeBefore  :: Int
      -- | Size of the mempool after the modification.
    , jlmSizeAfter   :: Int
      -- | How much memory was allocated during the modification.
    , jlmAllocated   :: Int
    } deriving Show

$(deriveJSON defaultOptions ''JLMemPool)

data TxpHolderTag

-- | Reduced equivalent of @MonadReader (GenericTxpLocalData mw) m@.
type MonadTxpMem ext = Ether.MonadReader TxpHolderTag (GenericTxpLocalData ext)

askTxpMem :: MonadTxpMem ext m => m (GenericTxpLocalData ext)
askTxpMem = Ether.ask @TxpHolderTag

getTxpLocalData
    :: (MonadIO m, MonadTxpMem e m)
    => (GenericTxpLocalData e -> STM.STM a) -> m a
getTxpLocalData getter = askTxpMem >>= \ld -> atomically (getter ld)

getUtxoModifier
    :: (MonadTxpMem e m, MonadIO m)
    => m UtxoModifier
getUtxoModifier = getTxpLocalData (STM.readTVar . txpUtxoModifier)

getLocalTxsMap
    :: (MonadIO m, MonadTxpMem e m)
    => m (HashMap TxId TxAux)
getLocalTxsMap = _mpLocalTxs <$> getMemPool

getLocalTxs
    :: (MonadIO m, MonadTxpMem e m)
    => m [(TxId, TxAux)]
getLocalTxs = HM.toList <$> getLocalTxsMap

getLocalTxsNUndo
    :: (MonadIO m, MonadTxpMem e m)
    => m ([(TxId, TxAux)], HashMap TxId (NonEmpty TxOutAux))
getLocalTxsNUndo =
    getTxpLocalData $ \TxpLocalData {..} ->
        (,) <$> (HM.toList . _mpLocalTxs <$> STM.readTVar txpMemPool) <*>
        STM.readTVar txpUndos

getMemPool :: (MonadIO m, MonadTxpMem e m) => m MemPool
getMemPool = getTxpLocalData (STM.readTVar . txpMemPool)

getTxpExtra :: (MonadIO m, MonadTxpMem e m) => m e
getTxpExtra = getTxpLocalData (STM.readTVar . txpExtra)

txpLocalDataLock :: MVar ()
txpLocalDataLock = unsafePerformIO $ newMVar ()
{-# NOINLINE txpLocalDataLock #-}

-- | An IORef to hold the current number of threads waiting on the
--   txpLocalDataLock. It's incremented before taking it, and decremented
--   after it's taken, so it's not necessarily exact.
txpLocalDataLockQueueLength :: IORef Int
txpLocalDataLockQueueLength = unsafePerformIO $ newIORef 0
{-# NOINLINE txpLocalDataLockQueueLength #-}

modifyTxpLocalData
    :: (MonadIO m, MonadTxpMem ext m, CanJsonLog m)
    => MemPoolModifyReason
    -> (GenericTxpLocalDataPure ext -> (a, GenericTxpLocalDataPure ext)) 
    -> m a
modifyTxpLocalData reason f =
    askTxpMem >>= \TxpLocalData{..} -> do
        qlength <- atomicModifyIORef' txpLocalDataLockQueueLength $ \i -> (i + 1, i)
        timeBeginWait <- currentTime
        _ <- takeMVar txpLocalDataLock
        timeEndWait <- currentTime
        _ <- atomicModifyIORef' txpLocalDataLockQueueLength $ \i -> (i - 1, ())
        let timeWait = timeEndWait - timeBeginWait
        allocBeginModify <- liftIO Conc.getAllocationCounter
        timeBeginModify <- currentTime
        (res, oldSize, newSize) <- atomically $ do
            curUM  <- STM.readTVar txpUtxoModifier
            curMP  <- STM.readTVar txpMemPool
            curUndos <- STM.readTVar txpUndos
            curTip <- STM.readTVar txpTip
            curExtra <- STM.readTVar txpExtra
            let (res, (newUM, newMP, newUndos, newTip, newExtra))
                  = f (curUM, curMP, curUndos, curTip, curExtra)
            STM.writeTVar txpUtxoModifier newUM
            STM.writeTVar txpMemPool newMP
            STM.writeTVar txpUndos newUndos
            STM.writeTVar txpTip newTip
            STM.writeTVar txpExtra newExtra
            pure (res, _mpSize curMP, _mpSize newMP)
        timeEndModify <- currentTime
        allocEndModify <- liftIO Conc.getAllocationCounter
        putMVar txpLocalDataLock ()
        let timeModify = timeEndModify - timeBeginModify
            -- Allocation counter counts down, so
            -- allocBeginModify >= allocEndModify
            allocModify = allocBeginModify - allocEndModify
        let jsonEvent = JLMemPool
                { jlmReason      = reason
                , jlmQueueLength = qlength
                  -- Wait and modify times in the JLMemPool datatype are
                  -- not Microseconds, because Microseconds has no aeson
                  -- instances.
                , jlmWait        = fromIntegral timeWait
                , jlmModify      = fromIntegral timeModify
                , jlmSizeBefore  = fromIntegral oldSize
                , jlmSizeAfter   = fromIntegral newSize
                  -- fromIntegral :: Int64 -> Int
                  -- It's probably fine.
                , jlmAllocated   = fromIntegral allocModify
                }
        jsonLog jsonEvent
        pure res

setTxpLocalData
    :: (MonadIO m, MonadTxpMem ext m, CanJsonLog m)
    => MemPoolModifyReason
    -> GenericTxpLocalDataPure ext -> m ()
setTxpLocalData reason x = modifyTxpLocalData reason (const ((), x))

clearTxpMemPool 
    :: (MonadIO m, MonadTxpMem ext m, Default ext, CanJsonLog m) 
    => MemPoolModifyReason
    -> m ()
clearTxpMemPool reason = modifyTxpLocalData reason clearF
  where
    clearF (_, _, _, tip, _) = ((), (mempty, def, mempty, tip, def))
