{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Broadcast.PubSub.Types
  ( PubSub
  , SubscriptionMsg(..)
  , PublicationMsg(..)
  , PubSubMsg(..)
  , FormatTopic(..)
  , fmtPubSubMsg
  ) where

import           Data.Monoid ((<>))
import           Data.Proxy (Proxy(..))
import qualified Data.Text.Lazy.Builder as B
import           Formatting (Format, bprint, later, (%))

data PubSub (topic :: * -> * -> *) (d :: * -> *)

-- |
-- Subscriber can send values of this type.
data SubscriptionMsg topic where
  AddTopic      :: FormatTopic topic k v => !(topic k v) -> SubscriptionMsg topic
  RemoveTopic   :: FormatTopic topic k v => !(topic k v) -> SubscriptionMsg topic
  Request       :: FormatTopic topic k v => !(topic k v) -> !k -> SubscriptionMsg topic
  Ignore        :: FormatTopic topic k v => !(topic k v) -> !k -> SubscriptionMsg topic
  Keepalive     :: FormatTopic topic k v => !(topic k v) -> SubscriptionMsg topic

fmtSubscriptionMsg :: forall r topic. Format r (SubscriptionMsg topic -> r)
fmtSubscriptionMsg = later fmt
    where
    fmt :: SubscriptionMsg topic -> B.Builder
    fmt (AddTopic t) =
        B.fromText "AddTopic " <> bprint formatTopic t
    fmt (RemoveTopic t) =
        B.fromText "RemoveTopic " <> bprint formatTopic t
    fmt (Request (t :: topic k v) k) =
        B.fromText "Request " <> bprint formatTopicWithKey t k
    fmt (Ignore t k) =
        B.fromText "Ignore " <> bprint formatTopicWithKey t k
    fmt (Keepalive t) =
        B.fromText "Keepalive " <> bprint formatTopic t

-- |
-- Publisher can send values of this type.
data PublicationMsg topic where
  Announce  :: FormatTopic topic k v => !(topic k v) -> !k -> PublicationMsg topic
  Payload   :: FormatTopic topic k v => !(topic k v) -> !v -> PublicationMsg topic

fmtPublicationMsg :: forall r topic. Format r (PublicationMsg topic -> r)
fmtPublicationMsg = later fmt
    where
    fmt :: PublicationMsg topic -> B.Builder
    fmt (Announce (t :: topic k v) k) =
        B.fromText "Announce "
        <> bprint formatTopicWithKey t k
    fmt (Payload t v) =
        B.fromText "Publish "
        <> bprint formatTopicWithValue t v

-- |
-- Messages put on `PubSubQueue`
-- Lets keep publication msg per node in the queue, this will simplify rate
-- limitting (which we can do per node as in `OutboundQ`) and in flight messages.
data PubSubMsg topic
  = PublicationMsg !(PublicationMsg topic)
  | SubscriptionMsg !(SubscriptionMsg topic)

fmtPubSubMsg :: forall topic r. Format r (PubSubMsg topic -> r)
fmtPubSubMsg = later fmt
    where
    fmt :: PubSubMsg topic -> B.Builder
    fmt (PublicationMsg msg) = bprint fmtPublicationMsg msg
    fmt (SubscriptionMsg msg) = bprint fmtSubscriptionMsg msg

class FormatTopic topic k v where
    formatTopic          :: forall r. Format r (topic k v -> r)
    formatTopicKey       :: forall r. Proxy (topic k v) -> Format r (k -> r)
    formatTopicValue       :: forall r. Proxy (topic k v) -> Format r (v -> r)

formatTopicWithKey :: forall topic k v r. FormatTopic topic k v
                   => Format r (topic k v -> k -> r)
formatTopicWithKey = formatTopic % " " % formatTopicKey (Proxy :: Proxy (topic k v))

formatTopicWithValue :: forall topic k v r. FormatTopic topic k v
                     => Format r (topic k v -> v -> r)
formatTopicWithValue = formatTopic % " " % formatTopicValue (Proxy :: Proxy (topic k v))
