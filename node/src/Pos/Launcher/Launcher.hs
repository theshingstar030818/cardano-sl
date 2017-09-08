-- | Applications of runners to scenarios.

module Pos.Launcher.Launcher
       ( -- * Node launchers.
         runNodeReal
       ) where

import           Mockable                   (Production)

import           Pos.Communication.Protocol (OutSpecs, WorkerSpec)
import           Pos.Core                   (HasCoreConstants)
import           Pos.Launcher.Param         (NodeParams (..))
import           Pos.Launcher.Resource      (NodeResources (..), bracketNodeResources)
import           Pos.Launcher.Runner        (runRealMode)
import           Pos.Launcher.Scenario      (runNode)
import           Pos.Ssc.Class              (SscConstraint)
import           Pos.Ssc.Class.Types        (SscParams)
import           Pos.WorkMode               (RealMode, LogAction)
import           Pos.Util.Util              (powerLift)
import           Pos.Util.LogAction         (hoistLogAction)

-----------------------------------------------------------------------------
-- Main launchers
-----------------------------------------------------------------------------

-- | Run full node in real mode.
runNodeReal
    :: forall ssc.
       ( SscConstraint ssc
       , HasCoreConstants)
    => LogAction Production
    -> NodeParams
    -> SscParams ssc
    -> ([WorkerSpec (RealMode ssc)], OutSpecs)
    -> Production ()
runNodeReal logAction np sscnp plugins = bracketNodeResources logAction np sscnp action
  where
    action :: HasCoreConstants => NodeResources ssc (RealMode ssc) -> Production ()
    action nr@NodeResources {..} =
        runRealMode
            (hoistLogAction powerLift logAction)
            nr
            (runNode @ssc nr plugins)
