{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Server listeners for delegation logic

module Pos.Delegation.Listeners
       ( delegationRelays
       , delegationRelayMkListeners
       ) where

import           Universum

import qualified Data.Text.Buildable
import qualified Ether
import           Formatting                    (build, sformat, shown, (%))
import           Serokell.Util.Text            (pairBuilder)
import           System.Wlog                   (logDebug, logInfo)
import           Network.Broadcast.Relay       (DataParams (..), PropagationMsg (..),
                                                Relay (..), handleDataL)

import           Pos.Binary                    ()
import           Pos.Communication.Limits      ()
import           Pos.Communication.Message     ()
import           Pos.Communication.Protocol    (PackingType, MkListeners, constantListeners)
import           Pos.Communication.Listener    (listenerConv)
import           Pos.Context                   (BlkSemaphore (..))
import           Pos.Core                      (getOurKeys, EpochIndex, ProxySKLight,
                                                ProxySigLight)
import           Pos.Crypto                    (SignTag (SignProxySK), proxySign,
                                                pskDelegatePk, ProxySecretKey)
import           Pos.Delegation.Logic          (ConfirmPskLightVerdict (..),
                                                PskHeavyVerdict (..),
                                                PskLightVerdict (..),
                                                processConfirmProxySk,
                                                processProxySKHeavy, processProxySKLight)
import           Pos.Delegation.Types          (ProxySKLightConfirmation)
import           Pos.Types                     (ProxySKHeavy)
import           Pos.WorkMode.Class            (WorkMode)

instance Buildable ProxySKLightConfirmation where
    build = pairBuilder

-- | Listeners for requests related to delegation processing.
delegationRelays
    :: forall ssc m. WorkMode ssc m
    => (PropagationMsg PackingType -> m ())
    -> [Relay PackingType m]
delegationRelays propagate =
        [ Data propagate (DataParams (pskLightRelay propagate))
        , Data propagate (DataParams pskHeavyRelay)
        , Data propagate (DataParams confirmPskRelay)
        ]

delegationRelayMkListeners
    :: forall ssc m . WorkMode ssc m
    => (PropagationMsg PackingType -> m ())
    -> MkListeners m
delegationRelayMkListeners propagate = constantListeners [
      listenerConv (handleDataL propagate (pskLightRelay propagate))
    , listenerConv (handleDataL propagate pskHeavyRelay)
    , listenerConv (handleDataL propagate confirmPskRelay)
    ]

pskLightRelay
    :: ( WorkMode ssc m )
    => (PropagationMsg PackingType -> m ())
    -> t
    -> ProxySecretKey (EpochIndex, EpochIndex)
    -> m Bool
pskLightRelay propagate = \_ pSk -> do
    logDebug $ sformat ("Got request to handle lightweight psk: "%build) pSk
    verdict <- processProxySKLight pSk
    logResult pSk verdict
    case verdict of
        PLUnrelated -> pure True
        PLAdded -> do
           (sk, pk) <- getOurKeys
           if pskDelegatePk pSk == pk then do
               -- if we're final delegate, don't propagate psk, propagate proof instead
               logDebug $
                   sformat ("Generating delivery proof and propagating it to neighbors: "%build) pSk
               let proof = proxySign SignProxySK sk pSk pSk
               -- FIXME this should not be necessary!!!
               -- Some other piece of the system should initiate the proof
               -- broadcast.
               propagate (DataOnlyPM Nothing (pSk, proof))
               pure False
           else pure True
        _ -> pure False
  where
    logResult pSk PLAdded =
        logInfo $ sformat ("Got valid related proxy secret key: "%build) pSk
    logResult pSk PLRemoved =
        logInfo $
        sformat ("Removing keys from issuer because got "%
                 "self-signed revocation: "%build) pSk
    logResult _ verdict =
        logDebug $
        sformat ("Got proxy signature that wasn't accepted. Reason: "%shown) verdict

pskHeavyRelay
    :: WorkMode ssc m
    => t
    -> ProxySKHeavy
    -> m Bool
pskHeavyRelay = \_ -> handlePsk
  where
    handlePsk :: forall ssc m. WorkMode ssc m => ProxySKHeavy -> m Bool
    handlePsk pSk = do
        logDebug $ sformat ("Got request to handle heavyweight psk: "%build) pSk
        verdict <- processProxySKHeavy @ssc pSk
        logDebug $ sformat ("The verdict for cert "%build%" is: "%shown) pSk verdict
        case verdict of
            PHIncoherent -> do
                -- We're probably updating state over epoch, so leaders
                -- can be calculated incorrectly.
                blkSemaphore <- Ether.asks' unBlkSemaphore
                void $ readMVar blkSemaphore
                handlePsk pSk
            PHAdded -> pure True
            _ -> pure False

confirmPskRelay
    :: WorkMode ssc m
    => t
    -> (ProxySKLight, ProxySigLight ProxySKLight)
    -> m Bool
confirmPskRelay = \_ (pSk, proof) -> do
    verdict <- processConfirmProxySk pSk proof
    pure $ case verdict of
        CPValid -> True
        _       -> False
