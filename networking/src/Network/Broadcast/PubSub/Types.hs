{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}

module Network.Broadcast.PubSub.Types
  ( PubSub
  , Subscription(..)
  , Publication(..)
  , PubSubMsg(..)
  ) where

data PubSub (topic :: * -> * -> *) (d :: * -> *)

-- |
-- Subscriber can send values of this type.
data Subscription topic where
  AddTopic :: topic k v -> Subscription topic
  RemoveTopic :: topic k v -> Subscription topic
  Request :: topic k v -> k -> Subscription topic
  Ignore :: topic k v -> k -> Subscription topic
  Keepalive :: Subscription topic

-- |
-- Publisher can send values of this type.
data Publication topic where
  Announce :: topic k v -> k -> Publication topic
  Payload :: topic k v -> v -> Publication topic

-- |
-- Messages put on `PubSubQueue`
-- Lets keep publication msg per node in the queue, this will simplify rate
-- limitting (which we can do per node as in `OutboundQ`) and in flight messages.
data PubSubMsg topic nid
  = Publication nid (Publication topic)
  | Subscription nid (Subscription topic)
