{-# LANGUAGE ScopedTypeVariables #-}

-- | Applications of runners to scenarios.

module Pos.Launcher.Launcher
       ( -- * Node launchers.
         runNodeReal
       ) where

import           Mockable                   (Production)

import           Pos.Communication.Protocol (OutSpecs, WorkerSpec)
import           Pos.Launcher.Param         (NodeParams (..))
import           Pos.Launcher.Resource      (NodeResources (..), bracketNodeResources,
                                             hoistNodeResources)
import           Pos.Launcher.Runner        (runRealMode, runToProd)
import           Pos.Launcher.Scenario      (runNode)
import           Pos.Security               (SecurityWorkersClass)
import           Pos.Ssc.Class              (SscConstraint)
import           Pos.Ssc.Class.Types        (SscParams)
import           Pos.Util.Util              (powerLift)
import           Pos.Util.JsonLog           (JsonLogConfig (..))
import           Pos.WorkMode               (RealMode)

-----------------------------------------------------------------------------
-- Main launchers
-----------------------------------------------------------------------------

-- | Run full node in real mode.
runNodeReal
    :: forall ssc.
       (SscConstraint ssc, SecurityWorkersClass ssc)
    => NodeParams
    -> SscParams ssc
    -> ([WorkerSpec (RealMode ssc)], OutSpecs)
    -> Production ()
runNodeReal np sscnp plugins = bracketNodeResources np sscnp action
  where
    action :: NodeResources ssc Production -> Production ()
    action nr =
        -- nr' is used lazily in runToProd to define nr'. Is this ok?
        -- Perhaps the issue is that we get the NodeResources in Production.
        -- Can't we get them in some more general monadic type?
        let nr' :: NodeResources ssc (RealMode ssc)
            nr'@NodeResources {..} = hoistNodeResources powerLift (runToProd nr' JsonLogDisabled) nr
        in  runRealMode nr' (runNode @ssc nrContext nr' plugins)
