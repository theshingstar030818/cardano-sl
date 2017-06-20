{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Instance of SscListenersClass

module Pos.Ssc.GodTossing.Listeners
       ( -- * Instances
         -- ** instance SscListenersClass SscGodTossing
       ) where

import           Universum

import           Control.Lens                          (at, to)
import           Data.Tagged                           (Tagged (..), tagWith)
import           Formatting                            (build, sformat, (%))
import           Node.Message.Class                    (Message)
import           System.Wlog                           (logDebug)

import           Network.Broadcast.Relay               (DataMsg, InvOrData,
                                                        InvReqDataParams (..),
                                                        Relay (..), ReqMsg,
                                                        PropagationMsg,
                                                        handleReqL, handleInvL)

import           Pos.Communication.BiP                 (BiP)
import           Pos.Binary.Class                      (Bi)
import           Pos.Binary.Crypto                     ()
import           Pos.Binary.GodTossing                 ()
import           Pos.Binary.Infra                      ()
import           Pos.Communication.Limits.Types        (MessageLimited)
import           Pos.Communication.MessagePart         (MessagePart)
import           Pos.Communication.Protocol            (constantListeners, MkListeners)
import           Pos.Communication.Listener            (listenerConv)
import           Pos.Core                              (StakeholderId, addressHash)
import           Pos.Security.Util                     (shouldIgnorePkAddress)
import           Pos.Ssc.Class.Listeners               (SscListenersClass (..))
import           Pos.Ssc.Extra                         (sscRunLocalQuery)
import           Pos.Ssc.GodTossing.Core               (getCertId, getCommitmentsMap)
import           Pos.Ssc.GodTossing.LocalData          (ldModifier, sscIsDataUseful,
                                                        sscProcessCertificate,
                                                        sscProcessCommitment,
                                                        sscProcessOpening,
                                                        sscProcessShares)
import           Pos.Ssc.GodTossing.Network.Constraint (GtMessageConstraints)
import           Pos.Ssc.GodTossing.Toss               (GtTag (..), TossModifier,
                                                        tmCertificates, tmCommitments,
                                                        tmOpenings, tmShares)
import           Pos.Ssc.GodTossing.Type               (SscGodTossing)
import           Pos.Ssc.GodTossing.Types.Message      (MCCommitment (..), MCOpening (..),
                                                        MCShares (..),
                                                        MCVssCertificate (..))
import           Pos.Ssc.Mode                          (SscMode)

instance GtMessageConstraints => SscListenersClass SscGodTossing where
    sscRelays propagate = Tagged $ fmap fst
        [ commitmentRelay propagate
        , openingRelay propagate
        , sharesRelay propagate
        , vssCertRelay propagate
        ]
    sscRelayMkListeners propagate = Tagged $ mconcat $ fmap snd
        [ commitmentRelay propagate
        , openingRelay propagate
        , sharesRelay propagate
        , vssCertRelay propagate
        ]

commitmentRelay
    :: ( GtMessageConstraints, SscMode SscGodTossing m
       , MessageLimited (ReqMsg (Tagged MCCommitment StakeholderId))
       , MessageLimited (InvOrData (Tagged MCCommitment StakeholderId) MCCommitment)
       )
    => (PropagationMsg BiP -> m ())
    -> (Relay BiP m, MkListeners m)
commitmentRelay propagate =
    sscRelay propagate CommitmentMsg
             (\(MCCommitment (pk, _, _)) -> addressHash pk)
             (\id tm -> MCCommitment <$> tm ^. tmCommitments . to getCommitmentsMap . at id)
             (\(MCCommitment comm) -> sscProcessCommitment comm)

openingRelay
    :: ( GtMessageConstraints, SscMode SscGodTossing m
       , MessageLimited (ReqMsg (Tagged MCOpening StakeholderId))
       , MessageLimited (InvOrData (Tagged MCOpening StakeholderId) MCOpening)
       )
    => (PropagationMsg BiP -> m ())
    -> (Relay BiP m, MkListeners m)
openingRelay propagate =
    sscRelay propagate OpeningMsg
             (\(MCOpening k _) -> k)
             (\id tm -> MCOpening id <$> tm ^. tmOpenings . at id)
             (\(MCOpening key open) -> sscProcessOpening key open)

sharesRelay
    :: ( GtMessageConstraints, SscMode SscGodTossing m
       , MessageLimited (ReqMsg (Tagged MCShares StakeholderId))
       , MessageLimited (InvOrData (Tagged MCShares StakeholderId) MCShares)
       )
    => (PropagationMsg BiP -> m ())
    -> (Relay BiP m, MkListeners m)
sharesRelay propagate =
    sscRelay propagate SharesMsg
             (\(MCShares k _) -> k)
             (\id tm -> MCShares id <$> tm ^. tmShares . at id)
             (\(MCShares key shares) -> sscProcessShares key shares)

vssCertRelay
    :: ( GtMessageConstraints, SscMode SscGodTossing m
       , MessageLimited (ReqMsg (Tagged MCVssCertificate StakeholderId))
       , MessageLimited (InvOrData (Tagged MCVssCertificate StakeholderId) MCVssCertificate)
       )
    => (PropagationMsg BiP -> m ())
    -> (Relay BiP m, MkListeners m)
vssCertRelay propagate =
    sscRelay propagate VssCertificateMsg
             (\(MCVssCertificate vc) -> getCertId vc)
             (\id tm -> MCVssCertificate <$> tm ^. tmCertificates . at id)
             (\(MCVssCertificate cert) -> sscProcessCertificate cert)

sscRelay
    :: forall m err contents .
       ( SscMode SscGodTossing m
       , Buildable err
       , Buildable contents
       , Typeable contents
       , MessageLimited (DataMsg contents)
       , Bi (DataMsg contents)
       , MessagePart contents
       , Message (InvOrData (Tagged contents StakeholderId) contents)
       , Message (ReqMsg (Tagged contents StakeholderId))
       , MessageLimited (ReqMsg (Tagged contents StakeholderId))
       , MessageLimited (InvOrData (Tagged contents StakeholderId) contents)
       )
    => (PropagationMsg BiP -> m ())
    -> GtTag
    -> (contents -> StakeholderId)
    -> (StakeholderId -> TossModifier -> Maybe contents)
    -> (contents -> ExceptT err m ())
    -> (Relay BiP m, MkListeners m)
sscRelay propagate gtTag contentsToKey toContents processData =
    (InvReqData propagate irdp, mkListeners)
  where
    irdp :: InvReqDataParams (Tagged contents StakeholderId) contents m
    irdp = InvReqDataParams
          { contentsToKey = pure . tagWith contentsProxy . contentsToKey
          , handleInv = \_ -> sscIsDataUseful gtTag . unTagged
          , handleReq =
              \_ (Tagged addr) -> toContents addr . view ldModifier <$> sscRunLocalQuery ask
          , handleData = \_ dat -> do
                let addr = contentsToKey dat
                -- [CSL-685] TODO: Add here malicious emulation for network
                -- addresses when TW will support getting peer address
                -- properly
                handleDataDo dat addr =<< shouldIgnorePkAddress addr
          }
    mkListeners = constantListeners [
          listenerConv (handleReqL (handleReq irdp))
        , listenerConv (handleInvL propagate irdp)
        ]
    contentsProxy = (const Proxy :: (contents -> k) -> Proxy contents) contentsToKey
    ignoreFmt =
        "Malicious emulation: data " %build % " for id " %build %
        " is ignored"
    handleDataDo dat id shouldIgnore
        | shouldIgnore = False <$ logDebug (sformat ignoreFmt id dat)
        | otherwise = sscProcessMessage processData dat

sscProcessMessage
    :: (SscMode SscGodTossing m, Buildable err)
    => (a -> ExceptT err m ()) -> a -> m Bool
sscProcessMessage sscProcessMessageDo dat =
    runExceptT (sscProcessMessageDo dat) >>= \case
        Left err ->
            False <$ logDebug (sformat ("Data is rejected, reason: " %build) err)
        Right () -> return True
