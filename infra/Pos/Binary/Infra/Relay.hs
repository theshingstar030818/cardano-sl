module Pos.Binary.Infra.Relay
       (
       ) where

import           Universum

import           Pos.Binary.Class              (Bi, get, label, put)
import           Network.Broadcast.Relay       (InvMsg (..), ReqMsg (..))

instance (Bi key) => Bi (InvMsg key) where
    put InvMsg {..} = put imKey
    get = label "InvMsg" $ InvMsg <$> get

instance (Bi key) => Bi (ReqMsg key) where
    put ReqMsg {..} = put rmKey
    get = label "ReqMsg" $ ReqMsg <$> get
