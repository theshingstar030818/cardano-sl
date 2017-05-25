{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE PolyKinds    #-}
{-# LANGUAGE TypeFamilies #-}

-- | Default implementation of WithNodeContext.
--
-- FIXME this was deleted in master, but used in the benchmarking branch.
-- It should be deleted, and this relay log callback mechanism moved elsewhere.

module Pos.Context.Holder
       ( defaultRelayLogCallback
       ) where

import           Universum
import qualified Ether.Reader                  as Ether.E
import           Pos.Communication.Types.Relay (RelayLogCallback, RelayLogEvent (..))
import           Pos.Context.Context           (NodeContextTag, NodeContext (..))
import           Pos.Util.JsonLog              (MonadJL, JLEvent (..), jlLog)

defaultRelayLogCallback
    :: ( Ether.E.MonadReader NodeContextTag (NodeContext ssc) m
       , MonadJL m
       , MonadIO m
       )
    => RelayLogCallback m
defaultRelayLogCallback e = do 
    jlLog $ JLRelayEvent e
    case e of
        RelayQueueFull        -> Ether.E.asks @NodeContextTag ncOnRelayQueueFull >>= liftIO
        EnqueueDequeueTime dt -> Ether.E.asks @NodeContextTag ncOnRelayDequeue >>= liftIO . ($ dt)
