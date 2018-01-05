{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Broadcast.PubSub
  ( Subscription(..)
  , PubSubHealth(..)
  , PubSubQ(..)
  , new
  , enqueue
  , dequeueThread
  , dequeueTChan
  ) where

import           Universum
import           Control.Concurrent.MVar (modifyMVar_)
import           Control.Concurrent.STM.TChan (TChan, isEmptyTChan, newTChan, tryReadTChan, unGetTChan, writeTChan)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Time (UTCTime, getCurrentTime)
import qualified Data.Set as Set
import           Formatting (sformat, shown, string, (%))
import           System.Wlog.CanLog (WithLogger, logDebug)
import qualified System.Wlog.CanLog as Log
import           System.Wlog.Severity (Severity(..))

import qualified Mockable as M
import           Network.Broadcast.OutboundQueue
                    ( CtrlMsg(..)
                    , Dequeue(..)
                    , Failures
                    , MaxInFlight(..)
                    , ReconsiderAfter
                    , Signal
                    , ThreadRegistry
                    , forkThread
                    , newSignal
                    , poke
                    , retryIfNothing
                    , runDequeuePolicy
                    , withThreadRegistry
                    , waitAllThreads )
import           Network.Broadcast.PubSub.Types

type PubSubInFlight nid = Map nid Int

updateInFlightFor :: forall m topic nid. (MonadIO m, Ord nid)
               => PubSubPacket topic nid
               -> (Int -> Int)
               -> MVar (PubSubInFlight nid)
               -> m ()
updateInFlightFor PubSubPacket{..} f var =
    liftIO $ modifyMVar_ var (return . Map.alter g pspDestId)
    where
        g :: Maybe Int -> Maybe Int
        g = Just . maybe (f 0) f

type PubSubFailurePolicy = SomeException -> ReconsiderAfter

-- | Subscription data
data Subscription = Subscription {
      subBlock :: !Bool
    , subTxp   :: !Bool
    , subMpc   :: !Bool
    }

data PubSubHealth nid = PubSubHealth {
      -- | Total number of failed "AddTopic"
      pshFailedAddTopic    :: MVar (Map nid Int)

      -- | Total number of failed "RemoveTopic"
    , pshFailedRemoveTopic :: MVar (Map nid Int)

      -- | Total number of failed "Request"
    , pshFailedRequest     :: MVar (Map nid Int)

      -- | Total number of failed "Ignore"
    , pshFailedIgnore      :: MVar (Map nid Int)

      -- | Total number of failed "Keepalive"
    , pshFailedKeepalive   :: MVar (Map nid Int)

      -- | Total number of failed "Announce"
    , pshFailedAnnounce    :: MVar (Map nid Int)

      -- | Total number of failed "Payload"
    , pshFailedPayload     :: MVar (Map nid Int)
    }

newPubSubHealth :: forall nid m. (MonadIO m)
                => m (PubSubHealth nid)
newPubSubHealth = liftIO $ PubSubHealth
    <$> newMVar Map.empty
    <*> newMVar Map.empty
    <*> newMVar Map.empty
    <*> newMVar Map.empty
    <*> newMVar Map.empty
    <*> newMVar Map.empty
    <*> newMVar Map.empty

data PubSubFailure topic nid = FailedSend (PubSubMsg topic) nid

failureCounter :: PubSubFailure topic nid -> PubSubHealth nid -> MVar (Map nid Int)
failureCounter (FailedSend msg _) PubSubHealth{..} =
    case msg of
        SubscriptionMsg (AddTopic _)    -> pshFailedAddTopic
        SubscriptionMsg (RemoveTopic _) -> pshFailedRemoveTopic
        SubscriptionMsg (Request _ _)   -> pshFailedRequest
        SubscriptionMsg (Ignore _ _)    -> pshFailedIgnore
        SubscriptionMsg (Keepalive _)   -> pshFailedKeepalive
        PublicationMsg (Announce _ _)   -> pshFailedAnnounce
        PublicationMsg (Payload _ _)    -> pshFailedPayload

failureSeverity :: PubSubFailure topic nid -> Severity
failureSeverity (FailedSend _ _) = Warning

failureFormat :: (Show nid) => String -> PubSubFailure topic nid -> Text
failureFormat self (FailedSend msg nid)
    = sformat ( string
              % ": dequeue instruction " % fmtPubSubMsg
              % " failed to send to " % shown
              ) self msg nid

logFailure :: (WithLogger m, MonadIO m)
           => PubSubQ topic nid
           -> nid
           -> PubSubFailure topic nid
           -> m ()
logFailure PubSubQ{..} nid failure = do
    Log.logMessage
        (failureSeverity failure)
        (failureFormat psqSelf failure)
    liftIO $ modifyMVar_ (failureCounter failure psqHealth) (return . Map.alter incCounter nid)
    where
        incCounter :: Maybe Int -> Maybe Int
        incCounter Nothing = Just 1
        incCounter (Just c) = Just (c + 1)


intFailure :: forall m topic nid. MonadIO m
           => PubSubQ topic nid
           -> PubSubPacket topic nid
           -> UTCTime
           -> SomeException
           -> m ()
intFailure PubSubQ{..} p sendStartTime err = do
    liftIO $ modifyMVar_ psqFailures
        $ return
        . Map.insert (pspDestId p)
            (sendStartTime, psqFailurePolicy err)

data PubSubPacket topic nid = PubSubPacket {
      -- | The message send throught the channel
      pspPayload :: !(PubSubMsg topic)

    -- | Type of the node the packets needs to be sent to
    , pspDestId  :: !nid

    -- | Error of sent action
    , pspSent    :: !(MVar (Maybe SomeException))
    }

data PubSubQ topic nid = ( Ord nid
                         , Show nid
                         ) => PubSubQ {
      -- | Node ID of the current node (primarily for debugging purposes)
      psqSelf            :: !String

      -- | keep as in OutboundQ (rate limiting and max in flight)
    , psqDequeue         :: !Dequeue

      -- | Failure policy
    , psqFailurePolicy   :: !PubSubFailurePolicy

      -- | Messages sent but not yet acknowledged
    , psqInFlight        :: !(MVar (PubSubInFlight nid))

      -- | Nodes that we should not send any messages to right now because
      -- of rate limiting
    , psqRateLimited     :: !(MVar (Set nid))

      -- | Messages scheduled but not yet sent
    , psqScheduled       :: !(TChan (PubSubPacket topic nid))

      -- | instead of buckets we need to track subscriptions
    , psqSubscriptions   :: !(MVar (Map nid Subscription))

      -- | Recent communication failures
    , psqFailures        :: !(MVar (Failures nid))

      -- | Used to send control messages to the main thread
    , psqCtrlMsg         :: !(MVar CtrlMsg)

      -- | Signal we use to wake up blocked threads
    , psqSignal          :: !(Signal CtrlMsg)

      -- | Some metrics about the queue's health
    , psqHealth          :: !(PubSubHealth nid)
    }

new :: forall m topic nid.
       ( MonadIO m
       , Ord nid
       , Show nid
       )
    => String
    -> Dequeue
    -> PubSubFailurePolicy
    -> m (PubSubQ topic nid)
new psqSelf psqDequeue psqFailurePolicy = do
    psqInFlight <- newMVar Map.empty
    psqRateLimited <- newMVar Set.empty
    psqScheduled <- atomically newTChan
    psqSubscriptions <- newMVar Map.empty
    psqFailures <- newMVar Map.empty
    psqCtrlMsg <- newEmptyMVar

    -- Only look for control messages when the queue is empty
    let checkCtrlMsg :: IO (Maybe CtrlMsg)
        checkCtrlMsg =
            ifM (atomically $ isEmptyTChan psqScheduled)
                (tryTakeMVar psqCtrlMsg)
                (return Nothing)

    psqSignal <- newSignal checkCtrlMsg
    psqHealth <- newPubSubHealth

    return PubSubQ{..}

enqueue :: forall m topic nid. MonadIO m
        => PubSubQ topic nid
        -> PubSubMsg topic
        -> [nid]
        -> m [PubSubPacket topic nid]
enqueue PubSubQ{..} msg nids = do
    packets <- traverse createPacket nids
    traverse_ (atomically . writeTChan psqScheduled) packets
    liftIO $ poke psqSignal
    return packets

    where
        createPacket :: nid -> m (PubSubPacket topic nid)
        createPacket nid = do
            psqPacketSend <- liftIO newEmptyMVar
            return (PubSubPacket msg nid psqPacketSend) 

type PubSubSendMsg m topic nid = PubSubMsg topic -> nid -> m ()

{-------------------------------------------------------------------------------
  Interpreter for the dequeueing policy
-------------------------------------------------------------------------------}

checkMaxInFlight :: Ord nid => Dequeue -> PubSubInFlight nid -> nid -> Bool
checkMaxInFlight (Dequeue _ (MaxInFlight maxInFlight)) inFlight nid =
    case nid `Map.lookup` inFlight of
        Just n  -> n < maxInFlight
        Nothing -> True

intDequeue :: forall m topic nid
            . ( MonadIO m
              , M.Mockable M.Catch m
              , M.Mockable M.Bracket m
              , WithLogger m)
           => PubSubQ topic nid
           -> ThreadRegistry m
           -> PubSubSendMsg m topic nid
           -> m (Maybe CtrlMsg)
intDequeue psq@PubSubQ{..} threadRegistry sendMsg = do
    mPacket <- getPacket
    case mPacket of
        Left ctrlMsg -> return $ Just ctrlMsg
        Right p      -> sendPacket p $> Nothing
    where
        getPacket :: m (Either CtrlMsg (PubSubPacket topic nid))
        getPacket = retryIfNothing psqSignal $ do
            inFlight <- readMVar psqInFlight
            rateLimited <- readMVar psqRateLimited
            dequeueTChan psqScheduled
                (\(PubSubPacket _ nid _) ->
                    not (nid `Set.member` rateLimited)
                    && checkMaxInFlight psqDequeue inFlight nid)

        sendPacket :: PubSubPacket topic nid -> m ()
        sendPacket p@PubSubPacket{..} = do
            sendStartTime <- liftIO $ getCurrentTime

            -- Mark the message as in-flight
            updateInFlightFor p (+1) psqInFlight

            -- Block until a node is not rate limitted according to `Dequeue`
            -- policy
            runDequeuePolicy threadRegistry psqSignal psqRateLimited pspDestId psqDequeue

            forkThread threadRegistry $ \unmask -> do
                logDebug $ debugMsg "sending" p
                ma <- M.try $ unmask $ sendMsg pspPayload pspDestId

                -- Reduce the in-flight count
                updateInFlightFor p (+(-1)) psqInFlight
                liftIO $ poke psqSignal

                case ma of
                    Left err -> do
                        liftIO $ putMVar pspSent (Just err)
                        logFailure psq pspDestId (FailedSend pspPayload pspDestId)
                        intFailure psq p sendStartTime err
                        return ()
                    Right _  -> do
                        liftIO $ putMVar pspSent Nothing
                        return ()

            logDebug $ debugMsg "sent" p

            return ()


        debugMsg :: String -> PubSubPacket topic nid -> Text
        debugMsg pref PubSubPacket{..} =
            sformat (string % ": " % string % " " % fmtPubSubMsg % " to " % shown)
                    psqSelf pref pspPayload pspDestId

-- | The dequeue thread
--
-- It is the responsibility of the next layer up to fork this thread; this
-- function does not return unless told to terminate using 'waitShutdown'.
dequeueThread :: forall m topic nid. (
                   MonadIO              m
                 , M.Mockable M.Bracket m
                 , M.Mockable M.Catch   m
                 , M.Mockable M.Async   m
                 , M.Mockable M.Fork    m
                 , Ord (M.ThreadId      m)
                 , WithLogger           m
                 )
              => PubSubQ topic nid
              -> PubSubSendMsg m topic nid
              -> m ()
dequeueThread psQ sendMsg = withThreadRegistry $ \threadRegistry ->
    let loop :: m ()
        loop = do
          mCtrlMsg <- intDequeue psQ threadRegistry sendMsg
          case mCtrlMsg of
            Nothing      -> loop
            Just ctrlMsg -> do
              waitAllThreads threadRegistry
              case ctrlMsg of
                Shutdown ack -> do liftIO $ putMVar ack ()
                Flush    ack -> do liftIO $ putMVar ack ()
                                   loop

    in loop

-- Auxiliary functions
dequeueTChan :: forall m a. MonadIO m => TChan a -> (a -> Bool) -> m (Maybe a)
dequeueTChan chan predicat = atomically (go [])
    where
    go as = do
        ma <- tryReadTChan chan
        case ma of
            Nothing
                -> traverse (unGetTChan chan) as $> Nothing
            Just a
                | predicat a
                -> traverse (unGetTChan chan) as $> Just a
                | otherwise
                -> go (a : as)
