{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}

module Pos.Network.Types
    ( -- * Types
      NetworkConfig(..)
    , Topology(..)
    , StaticPeers(..)
    , RelayDiscovery(..)
    , NodeName(..)
    , NodeRegion(..)
    , NodeRoutes(..)
    , NodeMetadata(..)
    , NetworkConfigException(..)
      -- ** Re-exports from time-warp-nt
    , MsgType(..)
    , NodeType(..)
    , Origin(..)
      -- ** Re-exports from elsewhere
    , NodeId(..)
      -- * Derived info
    , nodeInfoFor
    , staticToPeers
    ) where

import           Universum
import           Control.Exception (throw)
import           Data.Aeson (FromJSON(..), ToJSON(..), (.:), (.:?), (.=))
import           Network.Broadcast.OutboundQueue
import           Network.Broadcast.OutboundQueue.Classification
import           Node.Internal (NodeId (..))
import           Pos.Util.Config
import qualified Data.Aeson            as A
import qualified Data.Aeson.Types      as A
import qualified Data.ByteString.Char8 as BS.C8
import qualified Data.HashMap.Lazy     as HM
import qualified Data.Map.Strict       as M
import qualified Data.Text             as T
import qualified Data.Yaml             as Yaml
import qualified Network.DNS           as DNS

{-------------------------------------------------------------------------------
  Types describing the network topology
-------------------------------------------------------------------------------}

-- | Information about the network in which a node participates.
data NetworkConfig nid = NetworkConfig {
      ncNodeType :: !NodeType
      -- ^ The role that this node plays in the network.
    , ncTopology :: !(Topology nid)
      -- ^ Description of the network topology
    }
  deriving (Show)

-- | (Part of) the network topology
data Topology nid =
    -- | Statically configured topology
    --
    -- This is used for core and relay nodes
    TopologyStatic (StaticPeers nid)

    -- | Behind NAT nodes
    --
    -- Behind NAT nodes use DNS to discover relay nodes
  | TopologyBehindNAT RelayDiscovery
  deriving (Show)

-- | Static information about peers
data StaticPeers nid = StaticPeers {
      staticPeers :: !(Map nid (NodeMetadata nid))
    }
  deriving (Show)

-- | Relay discovery
data RelayDiscovery = RelayDiscovery {
      -- | DNS domains
      --
      -- The outer list corresponds to backup DNS servers; the inner lists
      -- provide multiple domain names which serve different purposes
      -- (e.g., the first might be configured to return geolocated hosts,
      -- with the second a load-balancing fall-back). The idea is that
      -- querying each of the elements of the inner lists provides a complete
      -- set of relay nodes to be tried, using the backup domain names only when
      -- one or more of the primary ones fail.
      relayDnsDomains :: Alts (AllOf DNS.Domain)
    }
  deriving (Show)

newtype NodeName = NodeName Text
    deriving (Show, Ord, Eq)

newtype NodeRegion = NodeRegion Text
    deriving (Show, Ord, Eq)

newtype NodeRoutes nid = NodeRoutes (AllOf (Alts nid))
    deriving (Show)

data NodeMetadata nid = NodeMetadata
    { nmType   :: !NodeType
    , nmRegion :: !NodeRegion
    , nmRoutes :: !(NodeRoutes nid)
    }
    deriving (Show)

-- | Something is wrong with the network configuration
data NetworkConfigException =
    -- | We cannot parse the .yaml file
    CannotParseNetworkConfig Yaml.ParseException

    -- | The .yaml file contains a node name which is undefined
  | forall nid. Show nid => UndefinedNodeName nid

    -- | The static routes for a node contains an empty list of alternatives
  | forall nid. Show nid => EmptyListOfAltsFor nid

deriving instance Show NetworkConfigException
instance Exception NetworkConfigException

{-------------------------------------------------------------------------------
  Deriving info
-------------------------------------------------------------------------------}

-- | Look up node info for a particular node
--
-- This assumes that this is a node that is actually defined in this config.
-- It is used primarily to look up information about nodes mentioned /within/
-- the config file itself; so if those names do not exist, the config file
-- is invalid.
nodeInfoFor :: (Show nid, Ord nid)
            => StaticPeers nid -> nid -> NodeMetadata nid
nodeInfoFor StaticPeers{..} node =
    case M.lookup node staticPeers of
      Nothing  -> throw $ UndefinedNodeName node
      Just nfo -> nfo

-- | Construct 'Peers' data structure from 'StaticPeers'
staticToPeers :: forall nid. (Show nid, Ord nid)
              => StaticPeers nid -> nid -> Peers nid
staticToPeers staticPeers node =
    peersFromList $ map aux routes
  where
    routes :: AllOf (Alts nid)
    NodeRoutes routes = nmRoutes $ nodeInfoFor staticPeers node

    -- We assume that
    --
    -- * The list of alternatives is not empty, and
    -- * All nodes in a single list of alternatives are of the same type
    aux :: Alts nid -> (NodeType, Alts nid)
    aux []           = throw $ EmptyListOfAltsFor node
    aux alts@(alt:_) = (nmType (nodeInfoFor staticPeers alt), alts)

{-------------------------------------------------------------------------------
  FromJSON instances
-------------------------------------------------------------------------------}

instance FromJSON NodeRegion where
  parseJSON = fmap NodeRegion . parseJSON

instance FromJSON NodeName where
  parseJSON = fmap NodeName . parseJSON

instance FromJSON nid => FromJSON (NodeRoutes nid) where
  parseJSON = fmap NodeRoutes . parseJSON

instance FromJSON NodeType where
  parseJSON = A.withText "NodeType" $ \typ -> do
      case T.unpack typ of
        "core"     -> return NodeCore
        "edge"     -> return NodeEdge
        "relay"    -> return NodeRelay
        _otherwise -> fail $ "Invalid NodeType " ++ show typ

instance FromJSON RelayDiscovery where
  parseJSON = fmap (RelayDiscovery . aux) . parseJSON
    where
      aux :: [[String]] -> [[DNS.Domain]]
      aux = map (map BS.C8.pack)

instance FromJSON nid => FromJSON (NodeMetadata nid) where
  parseJSON = A.withObject "NodeInfo" $ \obj -> do
      nmType   <- obj .: "type"
      nmRegion <- obj .: "region"
      nmRoutes <- obj .: "static-routes"
      return NodeMetadata{..}

instance FromJSON (StaticPeers NodeName) where
  parseJSON = A.withObject "StaticConfig" $ \obj ->
      StaticPeers . M.fromList <$> mapM aux (HM.toList obj)
    where
      aux :: (Text, A.Value) -> A.Parser (NodeName, NodeMetadata NodeName)
      aux (name, val) = (NodeName name, ) <$> parseJSON val

instance FromJSON (Topology NodeName) where
  parseJSON = A.withObject "Topology" $ \obj -> do
      mNodes  <- obj .:? "nodes"
      mRelays <- obj .:? "relays"
      case (mNodes, mRelays) of
        (Just nodes, Nothing)  -> TopologyStatic    <$> parseJSON nodes
        (Nothing, Just relays) -> TopologyBehindNAT <$> parseJSON relays
        (Just _, Just _) ->
          fail "Topology: expected either 'nodes' or 'relays', not both"
        (Nothing, Nothing) ->
          fail "Topology: expected 'nodes' or 'relays' field"

instance IsConfig (Topology NodeName) where
  configPrefix = return Nothing

{-------------------------------------------------------------------------------
  ToJSON instances
-------------------------------------------------------------------------------}

instance ToJSON NodeRegion where
  toJSON (NodeRegion region) = toJSON region

instance ToJSON NodeName where
  toJSON (NodeName name) = toJSON name

instance ToJSON nid => ToJSON (NodeRoutes nid) where
  toJSON (NodeRoutes routes) = toJSON routes

instance ToJSON NodeType where
  toJSON NodeCore  = toJSON ("core"  :: Text)
  toJSON NodeEdge  = toJSON ("edge"  :: Text)
  toJSON NodeRelay = toJSON ("relay" :: Text)

instance ToJSON RelayDiscovery where
  toJSON RelayDiscovery{..} = toJSON $ aux relayDnsDomains
    where
      aux :: [[DNS.Domain]] -> [[String]]
      aux = map (map BS.C8.unpack)

instance ToJSON nid => ToJSON (NodeMetadata nid) where
  toJSON NodeMetadata{..} = A.object [
        "type"          .= nmType
      , "region"        .= nmRegion
      , "static-routes" .= nmRoutes
      ]

instance ToJSON (StaticPeers NodeName) where
  toJSON StaticPeers{..} =
      A.object (map aux $ M.toList staticPeers)
    where
      aux :: (NodeName, NodeMetadata NodeName) -> A.Pair
      aux (NodeName name, info) = name .= info

instance ToJSON (Topology NodeName) where
  toJSON (TopologyStatic    nodes)  = A.object [ "nodes"  .= nodes ]
  toJSON (TopologyBehindNAT relays) = A.object [ "relays" .= relays ]
