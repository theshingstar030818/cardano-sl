-- | Server part.

module Pos.Communication.Server
       ( allListeners
       , serverLoggerName
       , sscRelays
       , sscRelayMkListeners
       , txRelays
       , txRelayMkListeners
       , delegationRelays
       , delegationRelayMkListeners
       , usRelays
       ) where

import           Universum

import           Data.Tagged                 (untag)
import           System.Wlog                 (LoggerName)
import           Network.Broadcast.Relay     (PropagationMsg)

import           Pos.Binary.Communication    ()
import           Pos.Block.Network.Listeners (blockListeners)
import           Pos.Communication.Types.Protocol (PackingType)
import           Pos.Communication.Protocol  (MkListeners (..))
import           Pos.Communication.Util      (wrapListener)
import           Pos.Delegation.Listeners    (delegationRelays, delegationRelayMkListeners)
import           Pos.Ssc.Class               (SscListenersClass (..), SscWorkersClass)
import           Pos.Txp                     (txRelayMkListeners, txRelays)
import           Pos.Update                  (usRelays, usRelayMkListeners)
import           Pos.WorkMode.Class          (WorkMode)

-- | All listeners running on one node.
allListeners
    :: (SscListenersClass ssc, SscWorkersClass ssc, WorkMode ssc m)
    => (PropagationMsg PackingType -> m ())
    -> MkListeners m
allListeners propagate = mconcat
        [ modifier "block"       $ blockListeners
        , modifier "tx"          $ txRelayMkListeners propagate
        , modifier "delegation"  $ delegationRelayMkListeners propagate
        , modifier "update"      $ usRelayMkListeners propagate
        , modifier "ssc"         $ untag (sscRelayMkListeners propagate)
        ]
  where
    modifier lname mkL = mkL { mkListeners = mkListeners' }
      where
        mkListeners' v p =
            let ls = mkListeners mkL v p
                f = wrapListener (serverLoggerName <> lname)
            in  map f ls

-- | Logger name for server.
serverLoggerName :: LoggerName
serverLoggerName = "server"
