{-# LANGUAGE CPP #-}

-- | Tx sending functionality in Rubbish.

module Tx
       ( sendToAllGenesis
       ) where

import           Universum

import           Control.Concurrent.STM.TQueue    (newTQueue, tryReadTQueue, writeTQueue)
import           Control.Monad.Catch              (Exception (..), try)
import           Control.Monad.Except             (runExceptT, throwError)
import qualified Data.ByteString                  as BS
import           Data.ByteString.Base58           (bitcoinAlphabet, encodeBase58)
import qualified Data.HashMap.Strict              as HM
import           Data.List                        ((!!))
import qualified Data.Set                         as S (fromList)
import qualified Data.Text                        as T
import qualified Data.Text.IO                     as T
import           Data.Time.Units                  (convertUnit, toMicroseconds)
import           Formatting                       (build, int, sformat, shown, stext,
                                                   string, (%))
import           Mockable                         (Mockable, Production, SharedAtomic,
                                                   SharedAtomicT, bracket, concurrently,
                                                   currentTime, delay, forConcurrently,
                                                   modifySharedAtomic, newSharedAtomic,
                                                   runProduction)
import           NeatInterpolation                (text)
import           Network.Transport.Abstract       (Transport, hoistTransport)
import qualified Network.Transport.TCP            as TCP (TCPAddr (..))
import           Node.Conversation                (ConversationActions (..))
import           Node.Message.Class               (Message (..))
import           Serokell.Util                    (ms, sec)
import           System.IO                        (BufferMode (LineBuffering), hClose,
                                                   hFlush, hSetBuffering, stdout)
import           System.Random                    (randomRIO)
import           System.Wlog                      (logDebug, logError, logInfo)
import           System.Wlog.CanLog               (WithLogger)

import           Pos.Binary                       (Raw, serialize')
import qualified Pos.Client.CLI                   as CLI
import           Pos.Client.Txp.Balances          (getOwnUtxoForPk)
import           Pos.Client.Txp.Util              (createTx)
import           Pos.Communication                (NodeId, OutSpecs, SendActions, Worker,
                                                   WorkerSpec, dataFlow, delegationRelays,
                                                   immediateConcurrentConversations,
                                                   relayPropagateOut, submitTx,
                                                   submitTxRaw, submitUpdateProposal,
                                                   submitVote, txRelays, usRelays, worker)
import           Pos.Communication.Types.Protocol (Conversation (..), SendActions (..),
                                                   enqueueMsg, withConnectionTo)
import           Pos.Constants                    (genesisBlockVersionData,
                                                   genesisSlotDuration, isDevelopment)
import           Pos.Core                         (addressHash, coinF)
import           Pos.Core.Address                 (makeAddress)
import           Pos.Core.Context                 (HasCoreConstants, giveStaticConsts)
import           Pos.Core.Types                   (AddrAttributes (..),
                                                   AddrSpendingData (..), Timestamp (..),
                                                   mkCoin)
import           Pos.Crypto                       (Hash, SecretKey, SignTag (SignUSVote),
                                                   emptyPassphrase, encToPublic,
                                                   fakeSigner, fullPublicKeyHexF, hash,
                                                   hashHexF, noPassEncrypt, safeCreatePsk,
                                                   safeSign, safeToPublic, toPublic,
                                                   unsafeHash, withSafeSigner)
import           Pos.Data.Attributes              (mkAttributes)
import           Pos.Genesis                      (BalanceDistribution (..),
                                                   balanceDistribution, devBalancesDistr,
                                                   devGenesisContext,
                                                   genesisContextProduction,
                                                   genesisDevSecretKeys, gtcUtxo)
import           Pos.Launcher                     (BaseParams (..), LoggingParams (..),
                                                   bracketTransport, loggerBracket)
import           Pos.Network.Types                (MsgType (..), Origin (..))
import           Pos.Rubbish                      (LightWalletMode, WalletParams (..),
                                                   makePubKeyAddressRubbish,
                                                   runWalletStaticPeers)
import           Pos.Ssc.GodTossing               (SscGodTossing)
import           Pos.Txp                          (TxOut (..), TxOutAux (..), txaF,
                                                   unGenesisUtxo)
import           Pos.Update                       (BlockVersionData (..),
                                                   BlockVersionModifier (..),
                                                   SystemTag (..), UpdateData (..),
                                                   UpdateVote (..), mkUpdateProposalWSign)
import           Pos.Util.UserSecret              (readUserSecret, usKeys)
import           Pos.Util.Util                    (powerLift)
import           Pos.Wallet                       (addSecretKey, getBalance,
                                                   getSecretKeys)
import           Pos.WorkMode                     (RealMode, RealModeContext)

import           Command                          (CmdCtx (..), Command (..),
                                                   SendMode (..),
                                                   SendToAllGenesisParams (..),
                                                   parseCommand)
import           RubbishOptions                   (RubbishAction (..),
                                                   RubbishOptions (..), getRubbishOptions)

----------------------------------------------------------------------------
-- Send to all genesis
----------------------------------------------------------------------------

-- | Count submitted and failed transactions.
--
-- This is used in the benchmarks using send-to-all-genesis
data TxCount = TxCount
    { _txcSubmitted :: !Int
    , _txcFailed    :: !Int
      -- How many threads are still sending transactions.
    , _txcThreads   :: !Int }

addTxSubmit :: Mockable SharedAtomic m => SharedAtomicT m TxCount -> m ()
addTxSubmit mvar = modifySharedAtomic mvar (\(TxCount submitted failed sending) -> return (TxCount (submitted + 1) failed sending, ()))

addTxFailed :: Mockable SharedAtomic m => SharedAtomicT m TxCount -> m ()
addTxFailed mvar = modifySharedAtomic mvar (\(TxCount submitted failed sending) -> return (TxCount submitted (failed + 1) sending, ()))

sendToAllGenesis :: HasCoreConstants => SendActions LightWalletMode -> SendToAllGenesisParams -> CmdCtx -> LightWalletMode ()
sendToAllGenesis sendActions (SendToAllGenesisParams duration conc delay_ sendMode tpsSentFile) CmdCtx{..} = do
    let nNeighbours = length na
    let slotDuration = fromIntegral (toMicroseconds genesisSlotDuration) `div` 1000000 :: Int
        keysToSend = zip skeys (balanceDistribution genesisBalanceDistr)
    tpsMVar <- newSharedAtomic $ TxCount 0 0 conc
    startTime <- show . toInteger . getTimestamp . Timestamp <$> currentTime
    Mockable.bracket (openFile tpsSentFile WriteMode) (liftIO . hClose) $ \h -> do
        liftIO $ hSetBuffering h LineBuffering
        liftIO . T.hPutStrLn h $ T.intercalate "," [ "slotDuration=" <> show slotDuration
                                                   , "sendMode=" <> show sendMode
                                                   , "conc=" <> show conc
                                                   , "startTime=" <> startTime
                                                   , "delay=" <> show delay_ ]
        liftIO $ T.hPutStrLn h "time,txCount,txType"
        txQueue <- atomically $ newTQueue
        -- prepare a queue with all transactions
        logInfo $ sformat ("Found "%shown%" keys in the genesis block.") (length keysToSend)
        -- Light wallet doesn't know current slot, so let's assume
        -- it's 0-th epoch. It's enough for our current needs.
        forM_ (zip keysToSend [0..]) $ \((key, _balance), n) -> do
            outAddr <- makePubKeyAddressRubbish (toPublic key)
            let val1 = mkCoin 1
                txOut1 = TxOut {
                    txOutAddress = outAddr,
                    txOutValue = val1
                    }
                txOuts = TxOutAux txOut1 :| []
            neighbours <- case sendMode of
                SendNeighbours -> return na
                SendRoundRobin -> return [na !! (n `mod` nNeighbours)]
                SendRandom -> do
                    i <- liftIO $ randomRIO (0, nNeighbours - 1)
                    return [na !! i]
            atomically $ writeTQueue txQueue (key, txOuts, neighbours)

            -- every <slotDuration> seconds, write the number of sent and failed transactions to a CSV file.
        let writeTPS :: LightWalletMode ()
            writeTPS = do
                delay (sec slotDuration)
                curTime <- show . toInteger . getTimestamp . Timestamp <$> currentTime
                finished <- modifySharedAtomic tpsMVar $ \(TxCount submitted failed sending) -> do
                    -- CSV is formatted like this:
                    -- time,txCount,txType
                    liftIO $ T.hPutStrLn h $ T.intercalate "," [curTime, show $ submitted, "submitted"]
                    liftIO $ T.hPutStrLn h $ T.intercalate "," [curTime, show $ failed, "failed"]
                    return (TxCount 0 0 sending, sending <= 0)
                if finished
                then logInfo "Finished writing TPS samples."
                else writeTPS
            -- Repeatedly take transactions from the queue and send them.
            -- Do this n times.
            sendTxs :: Int -> LightWalletMode ()
            sendTxs n
                | n <= 0 = do
                      logInfo "All done sending transactions on this thread."
                      modifySharedAtomic tpsMVar $ \(TxCount submitted failed sending) ->
                          return (TxCount submitted failed (sending - 1), ())
                | otherwise = (atomically $ tryReadTQueue txQueue) >>= \case
                      Just (key, txOuts, neighbours) -> do
                          utxo <- getOwnUtxoForPk $ safeToPublic (fakeSigner key)
                          etx <- createTx utxo (fakeSigner key) txOuts (toPublic key)
                          case etx of
                              Left err -> do
                                  addTxFailed tpsMVar
                                  logError (sformat ("Error: "%build%" while trying to send to "%shown) err neighbours)
                              Right (tx, _) -> do
                                  res <- submitTxRaw (immediateConcurrentConversations sendActions neighbours) tx
                                  addTxSubmit tpsMVar
                                  logInfo $ if res
                                      then sformat ("Submitted transaction: "%txaF%" to "%shown) tx neighbours
                                      else sformat ("Applied transaction "%txaF%", however no neighbour applied it") tx
                          delay $ ms delay_
                          logInfo "Continuing to send transactions."
                          sendTxs (n - 1)
                      Nothing -> logInfo "No more transactions in the queue."
            sendTxsConcurrently n = void $ forConcurrently [1..conc] (const (sendTxs n))
        -- Send transactions while concurrently writing the TPS numbers every
        -- slot duration. The 'writeTPS' action takes care to *always* write
        -- after every slot duration, even if it is killed, so as to
        -- guarantee that we don't miss any numbers.
        void $ concurrently writeTPS (sendTxsConcurrently duration)
