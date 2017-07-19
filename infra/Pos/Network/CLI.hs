module Pos.Network.CLI (
    NetworkConfigOpts(..)
  , networkConfigOption
  , nodeIdOption
  , intNetworkConfigOpts
  ) where

import           Universum
import           Pos.Network.Types
import qualified Data.Yaml                  as Yaml
import qualified Options.Applicative.Simple as Opt

{-------------------------------------------------------------------------------
  CLI
-------------------------------------------------------------------------------}

-- | Command line options that will (after parsing) become 'NetworkConfig'
data NetworkConfigOpts = NetworkConfigOpts {
      ncoNodeType :: NodeType -- ^ Type of this node
    , ncoTopology :: FilePath -- ^ Path to the file describing network topology
    }

{-------------------------------------------------------------------------------
  Parsers
-------------------------------------------------------------------------------}

networkConfigOption :: Opt.Parser NetworkConfigOpts
networkConfigOption = NetworkConfigOpts
    <$> nodeTypeOption
    <*> topologyOption

nodeTypeOption :: Opt.Parser NodeType
nodeTypeOption = asum [
      Opt.flag' NodeEdge $ mconcat [
          Opt.long "edge"
        , Opt.help "Declare this node to be an edge node"
        ]
    , Opt.flag' NodeRelay $ mconcat [
          Opt.long "relay"
        , Opt.help "Declare this node to be a relay node"
        ]
    , Opt.flag' NodeCore $ mconcat [
          Opt.long "core"
        , Opt.help "Declare this node to be a core node"
        ]
    ]

topologyOption :: Opt.Parser FilePath
topologyOption =
    Opt.strOption $ mconcat [
        Opt.long "topology"
      , Opt.metavar "FILEPATH"
      , Opt.help "Path to a YAML file containing the network topology"
      ]

nodeIdOption :: Opt.Parser String
nodeIdOption =
    Opt.strOption $
      Opt.long "node-id" <>
      Opt.metavar "NODE_ID" <>
      Opt.value "node0" <>
      Opt.help "Identifier for this node within the network"

{-------------------------------------------------------------------------------
  Interpreter
-------------------------------------------------------------------------------}

intNetworkConfigOpts :: NetworkConfigOpts -> IO (NetworkConfig NodeName)
intNetworkConfigOpts NetworkConfigOpts{..} = do
    let ncNodeType = ncoNodeType
    mNcTopology <- Yaml.decodeFileEither ncoTopology
    case mNcTopology of
      Right ncTopology -> return NetworkConfig{..}
      Left  err        -> throwM $ CannotParseNetworkConfig err
