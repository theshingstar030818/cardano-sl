{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Main where

import           Prelude
import           CLI
import           Types
import           Control.Lens
import           Control.Monad.Reader
import           Data.Default                (def)
import           Data.Maybe                  (fromJust, fromMaybe, isJust)
import           Data.IORef                  (newIORef)
import           GHC.Conc
import           Lib
import           Mockable                    (Production, runProduction)
import           Options.Generic
import           Pos.Client.CLI              (CommonArgs (..), CommonNodeArgs (..),
                                              NodeArgs (..), getNodeParams, gtSscParams)
import           Pos.DB.Rocks.Functions
import           Pos.DB.Rocks.Types
import           Pos.Launcher
import           Pos.Network.CLI
import           Pos.Network.Types
import           Pos.Ssc.SscAlgo
import           Pos.Util.JsonLog
import           Pos.Util.UserSecret         (usVss)
import           Pos.Wallet.SscType          (WalletSscType)
import           Pos.Wallet.Web.Mode
import           Pos.Wallet.Web.State.Acidic
import           Pos.Wallet.Web.State.State  (WalletState)
import           Pos.WorkMode
import           Stats                       (showStatsAndExit)
import           System.IO
import           System.Wlog.LoggerName
import           System.Wlog.LoggerNameBox

newRealModeContext :: HasConfigurations => NodeDBs -> ConfigurationOptions -> Production (RealModeContext WalletSscType)
newRealModeContext dbs confOpts = do
    let nodeArgs = NodeArgs {
      sscAlgo            = GodTossingAlgo
    , behaviorConfigPath = Nothing
    }
    let networkOps = NetworkConfigOpts {
          networkConfigOptsTopology = Nothing
        , networkConfigOptsKademlia = Nothing
        , networkConfigOptsSelf     = Nothing
        , networkConfigOptsPort     = 3030
        , networkConfigOptsPolicies = Nothing
        }
    let cArgs@CommonNodeArgs {..} = CommonNodeArgs {
           dbPath                 = "node-db"
         , rebuildDB              = True
         , devSpendingGenesisI    = Nothing
         , devVssGenesisI         = Nothing
         , keyfilePath            = "test-secret.key"
         , backupPhrase           = Nothing
         , externalAddress        = Nothing
         , bindAddress            = Nothing
         , peers                  = mempty
         , networkConfigOpts      = networkOps
         , jlPath                 = Nothing
         , kademliaDumpPath       = "kademlia.dump"
         , commonArgs             = CommonArgs {
               logConfig            = Nothing
             , logPrefix            = Nothing
             , reportServers        = mempty
             , updateServers        = mempty
             , configurationOptions = confOpts
             }
         , updateLatestPath       = "update"
         , updateWithPackage      = False
         , noNTP                  = True
         , route53Params          = Nothing
         , enableMetrics          = False
         , ekgParams              = Nothing
         , statsdParams           = Nothing
         , cnaDumpGenesisDataPath = Nothing
         }
    nodeParams <- getNodeParams cArgs nodeArgs
    let vssSK = fromJust $ npUserSecret nodeParams ^. usVss
    let gtParams = gtSscParams cArgs vssSK (npBehaviorConfig nodeParams)
    bracketNodeResources @WalletSscType @IO nodeParams gtParams $ \NodeResources{..} ->
        RealModeContext <$> pure dbs
                        <*> pure nrSscState
                        <*> pure nrTxpState
                        <*> pure nrDlgState
                        <*> jsonLogConfigFromHandle stdout
                        <*> pure (LoggerName "dbgen")
                        <*> pure nrContext
                        <*> initQueue (defaultNetworkConfig (TopologyAuxx mempty)) Nothing


walletRunner :: HasConfigurations
             => ConfigurationOptions
             -> NodeDBs
             -> WalletState
             -> UberMonad a
             -> IO a
walletRunner confOpts dbs ws act = runProduction $ do
    let addrCIdHashes :: IO AddrCIdHashes
        addrCIdHashes = AddrCIdHashes <$> (newIORef mempty)

    wwmc <- WalletWebModeContext <$> pure ws
                                 <*> liftIO (newTVarIO def)
                                 <*> liftIO addrCIdHashes
                                 <*> newRealModeContext dbs confOpts
    runReaderT act wwmc

newWalletState :: (MonadIO m, HasConfigurations) => Bool -> m WalletState
newWalletState recreate =
  -- If the user passed the `--add-to` option, it means we don't have
  -- to rebuild the DB, but rather append stuff into it.
  liftIO $ openState (not recreate) "wallet-db"

defLoggerName :: LoggerName
defLoggerName = LoggerName "dbgen"

instance HasLoggerName IO where
  getLoggerName = pure defLoggerName
  modifyLoggerName _ x = x

-- TODO(ks): Fix THIS! We need more flexible configuration than this.
newConfig :: CLI -> ConfigurationOptions
newConfig CLI{..} = defaultConfigurationOptions {
    cfoSystemStart = if genProd then Nothing else Just 1515589769
  , cfoFilePath = "node/configuration.yaml"
  , cfoKey = if genProd then "mainnet_wallet_macos64" else "dev"
  }

main :: IO ()
main = do
  cli@CLI{..} <- getRecord "DBGen"
  let cfg = newConfig cli
  withConfigurations cfg $ do
    when showStats (showStatsAndExit cli)
    dbs <- openNodeDBs False (fromMaybe "fake-db" nodePath)
    spec <- loadGenSpec config
    ws <- newWalletState (isJust addTo)
    walletRunner cfg dbs ws (generate cli spec)
    closeState ws

