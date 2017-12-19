{-# LANGUAGE Rank2Types #-}

-- | Different utils for wallets

module Pos.Wallet.Web.Util
    ( getWalletAccountIds
    , getAccountAddrsOrThrow
    , getWalletAddrMetas
    , getWalletAddrs
    , getWalletAddrsSet
    , decodeCTypeOrFail
    , getWalletAssuredDepth
    ) where

import           Universum

import qualified Data.Set                   as S
import           Formatting                 (build, sformat, (%))

import           Pos.Core                   (BlockCount)
import           Pos.Util.Servant           (FromCType (..), OriginType)
import           Pos.Util.Util              (maybeThrow)
import           Pos.Wallet.Web.Assurance   (AssuranceLevel (HighAssurance),
                                             assuredBlockDepth)
import           Pos.Wallet.Web.ClientTypes (AccountId (..), Addr, CId,
                                             CWAddressMeta (..), Wal,
                                             cwAssurance)


import           Pos.Wallet.Web.Error       (WalletError (..))
import           Pos.Wallet.Web.State       (AddressLookupMode, WalletSnapshot,
                                             getAccountIds, getAccountWAddresses,
                                             getWalletMeta)

getWalletAccountIds :: WalletSnapshot -> CId Wal -> [AccountId]
getWalletAccountIds ws cWalId = filter ((== cWalId) . aiWId) (getAccountIds ws)

getAccountAddrsOrThrow
    :: MonadThrow m
    => WalletSnapshot -> AddressLookupMode -> AccountId -> m [CWAddressMeta]
getAccountAddrsOrThrow ws mode accId =
    maybeThrow noWallet (getAccountWAddresses ws mode accId)
  where
    noWallet =
        RequestError $
        sformat ("No account with id "%build%" found") accId

getWalletAddrMetas
    :: MonadThrow m
    => WalletSnapshot -> AddressLookupMode -> CId Wal -> m [CWAddressMeta]
getWalletAddrMetas ws lookupMode cWalId =
    concatMapM (getAccountAddrsOrThrow ws lookupMode)
               (getWalletAccountIds ws cWalId)

getWalletAddrs
    :: MonadThrow m
    => WalletSnapshot -> AddressLookupMode -> CId Wal -> m [CId Addr]
getWalletAddrs ws = (cwamId <<$>>) ... getWalletAddrMetas ws

getWalletAddrsSet
    :: MonadThrow m
    => WalletSnapshot -> AddressLookupMode -> CId Wal -> m (Set (CId Addr))
getWalletAddrsSet ws lookupMode cWalId =
    S.fromList . map cwamId <$> getWalletAddrMetas ws lookupMode cWalId

decodeCTypeOrFail :: (MonadThrow m, FromCType c) => c -> m (OriginType c)
decodeCTypeOrFail = either (throwM . DecodeError) pure . decodeCType

getWalletAssuredDepth
    :: WalletSnapshot -> CId Wal -> Maybe BlockCount
getWalletAssuredDepth ws wid =
    assuredBlockDepth HighAssurance . cwAssurance <$>
    getWalletMeta ws wid
