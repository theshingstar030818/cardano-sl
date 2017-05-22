{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}

-- | Protocol/versioning related communication types.

module Pos.Communication.Types.Protocol
       ( HandlerSpec (..)
       , VerInfo (..)
       , HandlerSpecs
       , inSpecs
       , notInSpecs
       , ListenerSpec (..)
       , InSpecs (..)
       , OutSpecs (..)
       , Listener
       , Worker
       , Action
       , SendActions (..)
       , N.ConversationActions (..)
       , Conversation (..)
       , Action'
       , Worker'
       , NSendActions
       , PeerData
       , mergeLs
       , toOutSpecs
       , convH
       , ListenersWithOut
       , WorkerSpec
       , ActionSpec (..)
       , N.NodeId
       , JsonNodeId (..)
       ) where

import           Data.Aeson            (ToJSON (..), FromJSON (..))
import qualified Data.HashMap.Strict   as HM
import qualified Data.Text.Buildable   as B
import qualified Data.Text.Encoding    as Text (encodeUtf8, decodeUtf8)
import qualified Data.Binary           as Bin
import qualified Data.ByteString.Lazy  as BL
import qualified Data.ByteString.Base64 as B64 (encode, decode)
import           Formatting            (bprint, build, hex, int, sformat, stext, (%))
import qualified Node                  as N
import           Node.Message          (Message (..), MessageName (..))
import           Serokell.Util.Base16  (base16F)
import           Serokell.Util.Text    (listJson, mapJson)
import           Universum

import           Pos.Binary.Class      (Bi)
import           Pos.Communication.BiP (BiP)
import           Pos.Core.Types        (BlockVersion)
import           Pos.Util.TimeWarp     (nodeIdToAddress)

type PeerData = VerInfo

type Listener = N.Listener BiP PeerData
type Worker m = Action m ()
type Action m a = NSendActions m -> m a
type Action' m a = SendActions m -> m a
type Worker' m = Action' m ()
type NSendActions = N.SendActions BiP PeerData
newtype ActionSpec m a = ActionSpec (VerInfo -> Action m a)
type WorkerSpec m = ActionSpec m ()

-- TODO move to time-warp-nt
instance Buildable N.NodeId where
    build nNodeId =
        maybe "<unknown host:port>" (uncurry $ bprint (stext%":"%int)) $
                   first decodeUtf8 <$>
                   nodeIdToAddress nNodeId

data SendActions m = SendActions {
       -- | Establish a bi-direction conversation session with a node.
       withConnectionTo
           :: forall t .
              N.NodeId
           -> (PeerData -> NonEmpty (Conversation m t))
           -> m t
}

data Conversation m t where
    Conversation
        :: ( Bi snd, Message snd, Bi rcv, Message rcv )
        => (N.ConversationActions snd rcv m -> m t)
        -> Conversation m t

-- | A NodeId which has To/From JSON instances, using the N.NodeId Binary
--   instance and base64 encoding.
newtype JsonNodeId = JsonNodeId N.NodeId
  deriving (Eq, Ord, Show)

instance ToJSON JsonNodeId where
    toJSON (JsonNodeId nodeId) = toJSON . Text.decodeUtf8 . B64.encode . BL.toStrict . Bin.encode $ nodeId

instance FromJSON JsonNodeId where
    parseJSON v = do
        bs <- Text.encodeUtf8 <$> parseJSON v
        case B64.decode bs of
            Left err -> fail err
            Right b64decoded -> case Bin.decodeOrFail (BL.fromStrict b64decoded) of
                Left (_, _, err) -> fail err
                Right (_, _, nodeId) -> pure $ JsonNodeId nodeId

data HandlerSpec
    = ConvHandler { hsReplyType :: MessageName}
    | UnknownHandler Word8 ByteString
    deriving (Show, Generic, Eq)

convH :: (Message snd, Message rcv) => Proxy snd -> Proxy rcv -> (MessageName, HandlerSpec)
convH pSnd pReply = (messageName pSnd, ConvHandler $ messageName pReply)

instance Buildable HandlerSpec where
    build (ConvHandler (MessageName replyType)) =
        bprint ("Conv "%base16F) replyType
    build (UnknownHandler htype hcontent) =
        bprint ("UnknownHandler "%hex%" "%base16F) htype hcontent

instance Buildable (MessageName, HandlerSpec) where
    build (MessageName rcvType, h) = bprint (base16F % " -> " % build) rcvType h

type HandlerSpecs = HashMap MessageName HandlerSpec

instance Buildable HandlerSpecs where
    build x = bprint ("HandlerSpecs: "%listJson) (HM.toList x)

data VerInfo = VerInfo
    { vIMagic        :: Int32
    , vIBlockVersion :: BlockVersion
    , vIInHandlers   :: HandlerSpecs
    , vIOutHandlers  :: HandlerSpecs
    } deriving (Eq, Generic, Show)

instance Buildable VerInfo where
    build VerInfo {..} = bprint ("VerInfo { magic="%hex%", blockVersion="
                                %build%", inSpecs="%mapJson%", outSpecs="
                                %mapJson%"}")
                                vIMagic
                                vIBlockVersion
                                (HM.toList vIInHandlers)
                                (HM.toList vIOutHandlers)

inSpecs :: (MessageName, HandlerSpec) -> HandlerSpecs -> Bool
inSpecs (name, sp) specs = case name `HM.lookup` specs of
                              Just sp' -> sp == sp'
                              _        -> False

notInSpecs :: (MessageName, HandlerSpec) -> HandlerSpecs -> Bool
notInSpecs sp' = not . inSpecs sp'

data ListenerSpec m = ListenerSpec
    { lsHandler :: VerInfo -> Listener m -- ^ Handler accepts out verInfo and returns listener
    , lsInSpec  :: (MessageName, HandlerSpec)
    }

newtype InSpecs = InSpecs HandlerSpecs
  deriving (Eq, Show, Generic)

newtype OutSpecs = OutSpecs HandlerSpecs
  deriving (Eq, Show, Generic)

instance Monoid InSpecs where
    mempty = InSpecs mempty
    (InSpecs a) `mappend` (InSpecs b) =
          InSpecs $ HM.unionWithKey merger a b
      where
        merger name h1 h2 =
          error $ sformat
              ("Conflicting key in input spec: "%build%" "%build)
              (name, h1) (name, h2)

instance Monoid OutSpecs where
    mempty = OutSpecs mempty
    (OutSpecs a) `mappend` (OutSpecs b) =
          OutSpecs $ HM.unionWithKey merger a b
      where
        merger name h1 h2 =
          if h1 == h2
             then h1
             else error $ sformat
                    ("Conflicting key output spec: "%build%" "%build)
                    (name, h1) (name, h2)

mergeLs :: [(ListenerSpec m, OutSpecs)] -> ([ListenerSpec m], OutSpecs)
mergeLs = second mconcat . unzip

toOutSpecs :: [(MessageName, HandlerSpec)] -> OutSpecs
toOutSpecs = OutSpecs . HM.fromList

type ListenersWithOut m = ([ListenerSpec m], OutSpecs)
