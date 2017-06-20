-- | Listeners for network events in SSC algorithm implementation.

module Pos.Ssc.Class.Listeners
       ( SscListenersClass(..)
       ) where

import           Data.Tagged             (Tagged)

import           Network.Broadcast.Relay (Relay, PropagationMsg)
import           Pos.Communication.BiP   (BiP)
import           Pos.Communication.Protocol (MkListeners)
import           Pos.Ssc.Class.Types     (Ssc (..))
import           Pos.Ssc.Mode            (SscMode)


-- | Class for defining listeners in DHT @SSC@ implementation.
class Ssc ssc => SscListenersClass ssc where
    sscRelays
        :: SscMode ssc m
        -- TODO free up the packing type parameter from BiP / rethink whether
        -- this class needs to exist.
        => (PropagationMsg BiP -> m ())
        -> Tagged ssc [Relay BiP m]
    sscRelayMkListeners
        :: SscMode ssc m
        => (PropagationMsg BiP -> m ())
        -> Tagged ssc (MkListeners m)
