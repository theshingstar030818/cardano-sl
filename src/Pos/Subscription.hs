module Pos.Subscription (
    MonadSubscription(..)
  ) where

import Pos.Communication.Protocol (NodeId)

class MonadSubscription m where
  subscribe   :: NodeId -> m ()
  unsubscribe :: NodeId -> m ()
