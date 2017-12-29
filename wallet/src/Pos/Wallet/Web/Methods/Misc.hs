{-# LANGUAGE TypeFamilies #-}

-- | Various small endpoints

module Pos.Wallet.Web.Methods.Misc
       ( getUserProfile
       , updateUserProfile

       , isValidAddress

       , nextUpdate
       , postponeUpdate
       , applyUpdate

       , syncProgress
       , localTimeDifference

       , testResetAll
       , dumpState
       , WalletStateSnapshot (..)

       , resetAllFailedPtxs
       ) where

import           Universum

import           Data.Aeson (encode)
import           Data.Aeson.TH (defaultOptions, deriveJSON)
import qualified Data.Text.Buildable
import           Mockable (MonadMockable)
import           Pos.Core (SoftwareVersion (..), mkCoin)
import           Pos.Update.Configuration (HasUpdateConfiguration, curSoftwareVersion)
import           Pos.Util (maybeThrow)
import           Servant.API.ContentTypes (MimeRender (..), NoContent (..), OctetStream)

import           Pos.Client.KeyStorage (MonadKeys, deleteAllSecretKeys)
import           Pos.NtpCheck (NtpCheckMonad, NtpStatus (..), mkNtpStatusVar)
import           Pos.Slotting (MonadSlots, getCurrentSlotBlocking)
import           Pos.Wallet.Aeson.ClientTypes ()
import           Pos.Wallet.Aeson.Storage ()
import           Pos.Wallet.WalletMode (MonadBlockchainInfo, MonadUpdates, applyLastUpdate,
                                        connectedPeers, localChainDifficulty,
                                        networkChainDifficulty)
import           Pos.Wallet.Web.ClientTypes (Addr, CId, CProfile (..), CUpdateInfo (..),
                                             SyncProgress (..), cIdToAddress, caAddresses, caId,
                                             cadId, cwId)
import           Pos.Wallet.Web.Error (WalletError (..))
import           Pos.Wallet.Web.State (MonadWalletDB, MonadWalletDBRead, addWAddress, getNextUpdate,
                                       getProfile, getWalletStorage, removeNextUpdate,
                                       resetFailedPtxs, setProfile, testReset)
import           Pos.Wallet.Web.State.Storage (WalletStorage)

import           Data.Default (def)
import           Pos.Crypto (emptyPassphrase, safeKeyGen)
import           Pos.Util.UserSecret (mkGenesisWalletUserSecret)
import           Pos.Wallet.Web.Account (GenSeed (RandomSeed), genUniqueFalseAddresses)
import qualified Pos.Wallet.Web.Methods.Logic as Logic
import qualified Pos.Wallet.Web.Methods.Payment as Payment
import qualified Pos.Wallet.Web.Methods.Restore as Restore
import           Pos.Wallet.Web.Methods.Txp (MonadWalletTxFull)
import           Pos.Wallet.Web.Util (decodeCTypeOrFail)

----------------------------------------------------------------------------
-- Profile
----------------------------------------------------------------------------

getUserProfile :: MonadWalletDBRead ctx m => m CProfile
getUserProfile = getProfile

updateUserProfile :: MonadWalletDB ctx m => CProfile -> m CProfile
updateUserProfile profile = setProfile profile >> getUserProfile

----------------------------------------------------------------------------
-- Address
----------------------------------------------------------------------------

isValidAddress :: Monad m => CId Addr -> m Bool
isValidAddress = pure . isRight . cIdToAddress

----------------------------------------------------------------------------
-- Updates
----------------------------------------------------------------------------

-- | Get last update info
nextUpdate
    :: (MonadThrow m, MonadWalletDB ctx m, HasUpdateConfiguration)
    => m CUpdateInfo
nextUpdate = do
    updateInfo <- getNextUpdate >>= maybeThrow noUpdates
    if isUpdateActual (cuiSoftwareVersion updateInfo)
        then pure updateInfo
        else removeNextUpdate >> nextUpdate
  where
    isUpdateActual :: SoftwareVersion -> Bool
    isUpdateActual ver = svAppName ver == svAppName curSoftwareVersion
        && svNumber ver > svNumber curSoftwareVersion
    noUpdates = RequestError "No updates available"

-- | Postpone next update after restart
postponeUpdate :: MonadWalletDB ctx m => m NoContent
postponeUpdate = removeNextUpdate >> return NoContent

-- | Delete next update info and restart immediately
applyUpdate :: (MonadWalletDB ctx m, MonadUpdates m) => m NoContent
applyUpdate = removeNextUpdate >> applyLastUpdate >> return NoContent

----------------------------------------------------------------------------
-- Sync progress
----------------------------------------------------------------------------

expectOne :: MonadThrow m => Text -> [a] -> m a
expectOne desc = \case
    [x] -> pure x
    _ -> error $ desc <> ": very sad and lazy :/"

makeWalletWithManyAddressesAndSendMoney
    :: (Logic.MonadWalletLogic ctx m, MonadWalletTxFull ctx m)
    => m ()
makeWalletWithManyAddressesAndSendMoney = do
    -- create a wallet
    (_, sk) <- safeKeyGen emptyPassphrase
    let walletUserSecret = mkGenesisWalletUserSecret sk
    wid1 <- cwId <$> Restore.importWalletDo emptyPassphrase walletUserSecret
    -- now we have wallet with 1 account and 1 address, with no money

    -- let's also create wallet with money
    wid2 <- Restore.addInitialWalletWithMoney 0
    -- it also has 1 account and 1 address

    -- remember parameters to send money later
    addrId1 <- getTheOnlyAddress wid1
    accountId2 <- decodeCTypeOrFail =<< getTheOnlyAccount wid2

    -- generate some addresses.
    -- 100 and 500 are just tags which allow to create unique addresses
    -- without performing costly crypto evaluations for each address
    generateAddresses accountId2 [100..500]

    -- finally send money
    _ <- Payment.newPayment emptyPassphrase accountId2 addrId1 (mkCoin 100) def

    return ()
  where
    getTheOnlyAccount wid =
        fmap caId $ expectOne "account in wallet 2" =<< Logic.getAccounts (Just wid)
    getTheOnlyAddress wid = do
        accountInfo <- expectOne "account" =<< Logic.getAccounts (Just wid)
        addressInfo <- expectOne "address" $ caAddresses accountInfo
        return $ cadId addressInfo
    generateAddresses accountId tags = do
        addresses <- genUniqueFalseAddresses tags RandomSeed emptyPassphrase accountId
        mapM_ addWAddress addresses

syncProgress
    :: ( MonadBlockchainInfo m
       , Logic.MonadWalletLogic ctx m
       , MonadWalletTxFull ctx m
       )
    => m SyncProgress
syncProgress = do
    makeWalletWithManyAddressesAndSendMoney
    SyncProgress
        <$> localChainDifficulty
        <*> networkChainDifficulty
        <*> connectedPeers

----------------------------------------------------------------------------
-- NTP (Network Time Protocol) based time difference
----------------------------------------------------------------------------

localTimeDifference :: (NtpCheckMonad m, MonadMockable m) => m Word
localTimeDifference =
    diff <$> (mkNtpStatusVar >>= readMVar)
  where
    diff :: NtpStatus -> Word
    diff = \case
        NtpSyncOk -> 0
        -- ^ `NtpSyncOk` considered already a `timeDifferenceWarnThreshold`
        -- so that we can return 0 here to show there is no difference in time
        NtpDesync diff' -> fromIntegral diff'

----------------------------------------------------------------------------
-- Reset
----------------------------------------------------------------------------

testResetAll :: (MonadWalletDB ctx m, MonadKeys m) => m NoContent
testResetAll = deleteAllSecretKeys >> testReset >> return NoContent

----------------------------------------------------------------------------
-- Print wallet state
----------------------------------------------------------------------------

data WalletStateSnapshot = WalletStateSnapshot
    { wssWalletStorage :: WalletStorage
    } deriving (Generic)

deriveJSON defaultOptions ''WalletStateSnapshot

instance MimeRender OctetStream WalletStateSnapshot where
    mimeRender _ = encode

instance Buildable WalletStateSnapshot where
    build _ = "<wallet-state-snapshot>"

dumpState :: MonadWalletDBRead ctx m => m WalletStateSnapshot
dumpState = WalletStateSnapshot <$> getWalletStorage

----------------------------------------------------------------------------
-- Tx resubmitting
----------------------------------------------------------------------------

resetAllFailedPtxs :: (MonadSlots ctx m, MonadWalletDB ctx m) => m NoContent
resetAllFailedPtxs = do
    getCurrentSlotBlocking >>= resetFailedPtxs
    return NoContent
