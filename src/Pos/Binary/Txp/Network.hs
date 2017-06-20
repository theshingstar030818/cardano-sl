-- | Binary serialization of network Txp types.

module Pos.Binary.Txp.Network
       (
       ) where

import           Universum

import           Network.Broadcast.Relay.Types (DataMsg (..))

import           Pos.Binary.Class              (Bi (..), label)
import           Pos.Txp.Network.Types         (TxMsgContents (..))

----------------------------------------------------------------------------
-- Network
----------------------------------------------------------------------------

instance Bi (DataMsg TxMsgContents) where
    put (DataMsg (TxMsgContents txAux)) =
        put txAux
    get = label "DataMsg TxMsgContents" $
        DataMsg <$> (TxMsgContents <$> get)
