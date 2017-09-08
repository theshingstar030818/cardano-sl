{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS -Wno-unused-imports -Wno-unused-top-binds #-}

module Main
       ( main
       ) where

import           Universum

import           Data.Maybe          (fromJust, listToMaybe)
import qualified Control.Concurrent          as Conc
import           Control.Lens           (makeLensesWith)
import           Formatting          (sformat, shown, (%))
import qualified Mockable as Mockable
import           Mockable (Mockable, Production, Fork, Throw, Catch, runProduction)
import           System.Wlog
import           Control.Concurrent.Async (race_)
import qualified Brick as B
import qualified Brick.AttrMap as B
import qualified Brick.BChan as B
import qualified Brick.Widgets.Core as B
import qualified Brick.Widgets.Center as B
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as B
import qualified Graphics.Vty as V
import qualified Control.Exception           as Exception

import           Pos.Util.Util          (postfixLFields)
import           Pos.Binary          ()
import           Pos.Client.CLI      (CommonNodeArgs (..), NodeArgs (..),
                                      SimpleNodeArgs (..))
import qualified Pos.Client.CLI      as CLI
import           Pos.Communication   (OutSpecs, WorkerSpec)
import           Pos.Core            (HasCoreConstants, Timestamp (..), giveStaticConsts)
import           Pos.Launcher        (NodeParams (..), runNodeReal)
import           Pos.Ssc.Class       (SscConstraint, SscParams)
import           Pos.Ssc.GodTossing  (SscGodTossing)
import           Pos.Ssc.NistBeacon  (SscNistBeacon)
import           Pos.Ssc.SscAlgo     (SscAlgo (..))
import           Pos.Update          (updateTriggerWorker)
import           Pos.Util.UserSecret (usVss)
import           Pos.WorkMode        (RealMode, LogAction(..))

newtype Logless a = Logless { runLogless :: IO a }
    deriving (Functor, Applicative, Monad, MonadIO)

type instance Mockable.ThreadId Logless = Conc.ThreadId

instance Mockable Fork Logless where
    liftMockable (Mockable.Fork m)        = Logless $ Conc.forkIO (runLogless m)
    liftMockable (Mockable.MyThreadId)    = Logless $ Conc.myThreadId
    liftMockable (Mockable.ThrowTo tid e) = Logless $ Conc.throwTo tid e

instance Mockable Throw Logless where
    liftMockable (Mockable.Throw e) = Logless $ Exception.throwIO e

instance Mockable Catch Logless where
    liftMockable (Mockable.Catch act handler) = Logless $
        runLogless act `Exception.catch` (runLogless . handler)

instance HasLoggerName Logless where
    getLoggerName = pure "logless"
    modifyLoggerName _ = identity

instance CanLog Logless where
    dispatchMessage _ _ _ = return ()

actionWithoutWallet
    :: forall ssc.
       ( SscConstraint ssc
       , HasCoreConstants)
    => LogAction Production
    -> SscParams ssc
    -> NodeParams
    -> Production ()
actionWithoutWallet logAction sscParams nodeParams =
    runNodeReal @ssc logAction nodeParams sscParams plugins
  where
    plugins :: ([WorkerSpec (RealMode ssc)], OutSpecs)
    plugins = updateTriggerWorker

nodeAction :: LogAction Production -> SimpleNodeArgs -> IO ()
nodeAction logAction (SimpleNodeArgs (cArgs@CommonNodeArgs {..}) (nArgs@NodeArgs {..})) = giveStaticConsts $ do
    systemStart <- runLogless $ CLI.getNodeSystemStart $ CLI.sysStart commonArgs
    currentParams <- runLogless $ CLI.getNodeParams cArgs nArgs systemStart
    let vssSK = fromJust $ npUserSecret currentParams ^. usVss
    let gtParams = CLI.gtSscParams cArgs vssSK (npBehaviorConfig currentParams)
    runProduction $ case sscAlgo of
        NistBeaconAlgo ->
            actionWithoutWallet @SscNistBeacon logAction () currentParams
        GodTossingAlgo ->
            actionWithoutWallet @SscGodTossing logAction gtParams currentParams

data AppState = AppState
    { asLog :: ![LogEvent]
    }

makeLensesWith postfixLFields ''AppState

data AppEvent = AppEventLog LogEvent

type BApp = B.App AppState AppEvent ()

logAction :: B.BChan AppEvent -> LogAction Production
logAction eventChan = LogAction $ \lgname sev msg -> liftIO $
    B.writeBChan eventChan (AppEventLog (LogEvent lgname sev msg))

main :: IO ()
main = do
    args <- CLI.getSimpleNodeOptions
    eventChan <- B.newBChan 100
    race_ (nodeAction (logAction eventChan) args) (brickAction eventChan)

brickAction :: B.BChan AppEvent -> IO ()
brickAction eventChan = void $
    B.customMain (V.mkVty V.defaultConfig) (Just eventChan) app initialState
  where
      app :: BApp
      app = B.App
          { B.appDraw = \s ->
                  [logsWidget (s ^. asLog_L)]
          , B.appChooseCursor = \_ _ -> Nothing
          , B.appHandleEvent = onEvent
          , B.appStartEvent = return
          , B.appAttrMap = const (B.attrMap V.defAttr [])
          }

      initialState = AppState []

      onEvent s (B.VtyEvent e) = case e of
          V.EvKey (V.KChar 'q') [] -> B.halt s
          _ -> B.continue s
      onEvent s (B.AppEvent e) = case e of
          AppEventLog logEv -> B.continue $ s & asLog_L %~ (logEv:)
      onEvent s _ = B.continue s

logsWidget :: [LogEvent] -> B.Widget ()
logsWidget logEvents = B.vBox (renderLogEvent <$> take 100 logEvents)

renderLogEvent :: LogEvent -> B.Widget ()
renderLogEvent (LogEvent _lgname _sev msg) = B.txtWrap msg
