{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Monadic represantion of something that has @json@ journaled log
-- of operations.

module Pos.Util.JsonLog
       ( JLEvent(..)
       , JLBlock (..)
       , JLTxS (..)
       , JLTxR (..)
       , JLTimedEvent (..)
       , jlCreatedBlock
       , jlAdoptedBlock
       , MonadJL
       , jlLog
       , appendJL
       , fromJLSlotId
       , JLFile(..)
       , usingJsonLogFilePath
       ) where

import           Control.Concurrent.MVar (withMVar)
import           Data.Aeson              (encode)
import           Data.Aeson.TH           (deriveJSON)
import qualified Data.ByteString.Lazy    as LBS
import qualified Ether
import           Formatting              (sformat)
import           Mockable                (Catch, Mockable)
import           Serokell.Aeson.Options  (defaultOptions)
import           System.IO               (hClose)
import           System.Wlog             (CanLog, HasLoggerName)
import           Universum               hiding (catchAll)

import           Pos.Binary.Block        ()
import           Pos.Binary.Core         ()
import           Pos.Communication.Relay.Logic (InvReqDataFlowLog)
import           Pos.Crypto              (Hash, hash, hashHexF)
import           Pos.Ssc.Class.Types     (Ssc)
import           Pos.Types               (BiSsc, Block, SlotId (..), blockHeader,
                                          blockTxs, epochIndexL, gbHeader, gbhPrevBlock,
                                          headerHash, headerSlot)
import           Pos.Util.TimeWarp       (currentTime)

type BlockId = Text
type TxId = Text
type JLSlotId = (Word64, Word16)

-- | Json log of one block with corresponding 'BlockId'.
data JLBlock = JLBlock
    { jlHash      :: BlockId
    , jlPrevBlock :: BlockId
    , jlTxs       :: [TxId]
    , jlSlot      :: JLSlotId
    } deriving Show

-- | Json log of one transaction sent from the (light) wallet.
data JLTxS = JLTxS
    { jlsNodeId :: Text
    , jlsTxId   :: Text
    , jlsInvReq :: InvReqDataFlowLog
    } deriving Show

-- | Json log of one transaction being received by a node.
data JLTxR = JLTxR
    { jlrTxId     :: Text
    , jlrReceived :: Integer
    , jlrError    :: Maybe Text
    } deriving Show

-- | Get 'SlotId' from 'JLSlotId'.
fromJLSlotId :: JLSlotId -> SlotId
fromJLSlotId (ep, sl) = SlotId (fromIntegral ep) (fromIntegral sl)

-- | Json log event.
data JLEvent = JLCreatedBlock JLBlock
             | JLAdoptedBlock BlockId
             | JLTpsStat Int
             | JLMemPoolSize Int
             | JLTxSent JLTxS
             | JLTxReceived JLTxR 
  deriving Show

-- | 'JLEvent' with 'Timestamp' -- corresponding time of this event.
data JLTimedEvent = JLTimedEvent
    { jlTimestamp :: Integer
    , jlEvent     :: JLEvent
    } deriving (Show)

$(deriveJSON defaultOptions ''JLBlock)
$(deriveJSON defaultOptions ''JLTxS)
$(deriveJSON defaultOptions ''JLTxR)
$(deriveJSON defaultOptions ''JLEvent)
$(deriveJSON defaultOptions ''JLTimedEvent)

-- | Return event of created block.
jlCreatedBlock :: BiSsc ssc => Block ssc -> JLEvent
jlCreatedBlock block = JLCreatedBlock $ JLBlock {..}
  where
    jlHash = showHash $ headerHash block
    jlPrevBlock = showHash $ either (view gbhPrevBlock) (view gbhPrevBlock) (block ^. blockHeader)
    jlSlot = (fromIntegral $ siEpoch slot, fromIntegral $ siSlot slot)
    jlTxs = case block of
              Left _   -> []
              Right mB -> map fromTx . toList $ mB ^. blockTxs
    slot :: SlotId
    slot = either (\h -> SlotId (h ^. epochIndexL) 0) (view $ gbHeader . headerSlot) $ block
    fromTx = showHash . hash

showHash :: Hash a -> Text
showHash = sformat hashHexF

-- | Returns event of created 'Block'.
jlAdoptedBlock :: Ssc ssc => Block ssc -> JLEvent
jlAdoptedBlock = JLAdoptedBlock . showHash . headerHash

-- | Append event into log by given 'FilePath'.
appendJL :: (MonadIO m) => FilePath -> JLEvent -> m ()
appendJL path ev = liftIO $ do
    tev <- mkTimedEvent ev
    LBS.appendFile path tev 

-- | Turn a Json log event into a ByteString with timestamp.
mkTimedEvent :: MonadIO m => JLEvent -> m LBS.ByteString
mkTimedEvent ev = do
    time <- currentTime
    return $ encode $ JLTimedEvent (fromIntegral time) ev

-- | Note: not an ideal representation. One branch used
--     Maybe (MVar FilePath)
--   and another used
--     Maybe (MVar Handle)
--   so to make merging simpler they were unified.
--
--   If a FilePath is given, the file will be opened, written, and closed
--   at each 'jlLog', whereas giving a handle allows the caller to control
--   acquision and release, possibly holding the file open for the duration
--   of the program.
newtype JLFile = JLFile (Maybe (MVar (Either FilePath Handle)))

-- | Monad for things that can log Json log events.
type MonadJL m =
    ( Ether.MonadReader' JLFile m
    , MonadIO m
    , Mockable Catch m
    , HasLoggerName m
    , CanLog m )

jlLog :: MonadJL m => JLEvent -> m ()
jlLog ev = do
    JLFile jlFileM <- Ether.ask'
    whenJust jlFileM $ \logFileMV -> do
        timed <- mkTimedEvent ev
        liftIO $ withMVar logFileMV $ \choice -> case choice of
            Left fp -> bracket (openFile fp WriteMode) hClose (flip LBS.hPut timed)
            Right h -> LBS.hPut h timed

usingJsonLogFilePath
    :: ( MonadIO m, MonadMask m )
    => Maybe FilePath
    -> Ether.ReaderT' JLFile m a
    -> m a
usingJsonLogFilePath mPath act = case mPath of
    Nothing   -> Ether.runReaderT' act (JLFile Nothing)
    Just path -> bracket (openFile path WriteMode) (liftIO . hClose) $ \h -> do
        hMV <- newMVar (Right h)
        Ether.runReaderT' act (JLFile (Just hMV))
