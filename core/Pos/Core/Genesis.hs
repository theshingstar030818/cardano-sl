{-# LANGUAGE RankNTypes #-}

module Pos.Core.Genesis
       (
         protocolMagic
       , genesisHash

       -- * Genesis constants
       , staticSysStart
       , staticBlkSecurityParam
       , genesisBlockVersionData
       , genesisScriptVersion
       , genesisSlotDuration
       , genesisMaxBlockSize
       , genesisMaxHeaderSize
       , genesisMaxTxSize
       , genesisMpcThd
       , genesisHeavyDelThd
       , genesisUpdateVoteThd
       , genesisMaxUpdateProposalSize
       , genesisUpdateProposalThd
       , genesisUpdateImplicit
       , genesisSoftforkRule
       , genesisTxFeePolicy
       , genesisUnlockStakeEpoch

       -- * Constants/devmode
       , genesisDevKeyPairs
       , genesisDevPublicKeys
       , genesisDevSecretKeys
       , genesisDevHdwSecretKeys
       , genesisDevFlatDistr

       -- * /genesis-core.bin/
       , module Pos.Core.Genesis.Types
       , module Pos.Core.Genesis.Parser

       -- ** Derived data
       , genesisProdAvvmBalances
       , genesisProdInitializer
       , genesisProdDelegation

       -- * Utils
       , generateGenesisKeyPair
       , generateHdwGenesisSecretKey

       , giveStaticConsts
       ) where

import           Universum

import qualified Data.Text                  as T
import           Data.Time.Units            (Millisecond)
import           Serokell.Data.Memory.Units (Byte)
import           Formatting                 (int, sformat, (%))

import           Pos.Binary.Crypto       ()
import           Pos.Core.Coin           (unsafeMulCoin)
import           Pos.Core.Constants      (genesisKeysN)
import           Pos.Core.Context.Const  (HasCoreConstants, CoreConstants (..), giveConsts)
import           Pos.Core.Fee            (TxFeePolicy)
import           Pos.Core.Types          (Address, mkCoin, BlockCount, BlockVersionData (..),
                                          CoinPortion, EpochIndex (..), ScriptVersion,
                                          SoftforkRule (..), Timestamp (..))
import           Pos.Crypto.SafeSigning  (EncryptedSecretKey, emptyPassphrase,
                                          safeDeterministicKeyGen)
import           Pos.Crypto.Signing      (PublicKey, SecretKey, deterministicKeyGen)
import           Pos.Crypto.Hashing      (Hash, unsafeHash)

-- reexports
import           Pos.Core.Genesis.Parser
import           Pos.Core.Genesis.Types



genesisHash :: Hash a
genesisHash = unsafeHash @Text "patak"
{-# INLINE genesisHash #-}

-- | Protocol magic constant. Is put to block serialized version to
-- distinguish testnet and realnet (for example, possible usages are
-- wider).
protocolMagic :: Int32
protocolMagic = fromIntegral . pcProtocolMagic . gcProtocolConsts $ genData

-- | System start time.
staticSysStart :: Timestamp
staticSysStart = gcStartTime genData

-- | Security parameter which is maximum number of blocks which can be
-- rolled back. This value is embedded into library and can be used
-- only for initialization. The actual value should be fetched from
-- runtime context (it can differ from this one).
staticBlkSecurityParam :: BlockCount
staticBlkSecurityParam = fromIntegral . pcK . gcProtocolConsts $ genData

-- | 'BlockVersionData' for genesis 'BlockVersion'.
genesisBlockVersionData :: BlockVersionData
genesisBlockVersionData = gcBlockVersionData genData

----------------------------------------------------------------------------
-- Genesis
----------------------------------------------------------------------------

-- | ScriptVersion used at the very beginning
genesisScriptVersion :: ScriptVersion
genesisScriptVersion = 0

-- | Initial length of slot.
genesisSlotDuration :: Millisecond
genesisSlotDuration = bvdSlotDuration genesisBlockVersionData

-- | Initial block size limit.
genesisMaxBlockSize :: Byte
genesisMaxBlockSize = bvdMaxBlockSize genesisBlockVersionData

-- | Maximum size of a block header (in bytes)
genesisMaxHeaderSize :: Byte
genesisMaxHeaderSize = bvdMaxHeaderSize genesisBlockVersionData

-- | See 'Pos.CompileConfig.ccGenesisMaxTxSize'.
genesisMaxTxSize :: Byte
genesisMaxTxSize = bvdMaxTxSize genesisBlockVersionData

-- | See 'ccGenesisMaxUpdateProposalSize'.
genesisMaxUpdateProposalSize :: Byte
genesisMaxUpdateProposalSize = bvdMaxProposalSize genesisBlockVersionData

-- | See 'Pos.CompileConfig.ccGenesisMpcThd'.
genesisMpcThd :: CoinPortion
genesisMpcThd = bvdMpcThd genesisBlockVersionData

-- | See 'Pos.CompileConfig.ccGenesisHeavyDelThd'.
genesisHeavyDelThd :: CoinPortion
genesisHeavyDelThd = bvdHeavyDelThd genesisBlockVersionData

-- | See 'ccGenesisUpdateVoteThd'.
genesisUpdateVoteThd :: CoinPortion
genesisUpdateVoteThd = bvdUpdateVoteThd genesisBlockVersionData

-- | See 'ccGenesisUpdateProposalThd'.
genesisUpdateProposalThd :: CoinPortion
genesisUpdateProposalThd = bvdUpdateProposalThd genesisBlockVersionData

-- | See 'ccGenesisUpdateImplicit'.
genesisUpdateImplicit :: Integral i => i
genesisUpdateImplicit = fromIntegral $
    bvdUpdateImplicit genesisBlockVersionData

-- | Genesis softfork resolution rule.
genesisSoftforkRule :: SoftforkRule
genesisSoftforkRule = bvdSoftforkRule genesisBlockVersionData

genesisTxFeePolicy :: TxFeePolicy
genesisTxFeePolicy = bvdTxFeePolicy genesisBlockVersionData

genesisUnlockStakeEpoch :: EpochIndex
genesisUnlockStakeEpoch = bvdUnlockStakeEpoch genesisBlockVersionData

----------------------------------------------------------------------------
-- Constants/development
----------------------------------------------------------------------------

-- | List of pairs from 'SecretKey' with corresponding 'PublicKey'.
genesisDevKeyPairs :: [(PublicKey, SecretKey)]
genesisDevKeyPairs = map generateGenesisKeyPair [0 .. genesisKeysN - 1]

-- | List of 'PublicKey's in genesis.
genesisDevPublicKeys :: [PublicKey]
genesisDevPublicKeys = map fst genesisDevKeyPairs

-- | List of 'SecretKey's in genesis.
genesisDevSecretKeys :: [SecretKey]
genesisDevSecretKeys = map snd genesisDevKeyPairs

-- | List of 'SecretKey's in genesis for HD wallets.
genesisDevHdwSecretKeys :: [EncryptedSecretKey]
genesisDevHdwSecretKeys =
    map generateHdwGenesisSecretKey [0 .. genesisKeysN - 1]

-- | Default flat stakes distributed among 'genesisKeysN' (from constants).
genesisDevFlatDistr :: BalanceDistribution
genesisDevFlatDistr =
    FlatBalances genesisKeysN $
    mkCoin 10000 `unsafeMulCoin` (genesisKeysN :: Int)

----------------------------------------------------------------------------
-- GenesisCore derived data, production
----------------------------------------------------------------------------

-- | Genesis avvm balances.
genesisProdAvvmBalances :: GenesisAvvmBalances
genesisProdAvvmBalances = gsAvvmDistr genesisSpec

-- | Genesis initializer determines way of initialization
-- utxo, bootstrap stakeholders, etc.
genesisProdInitializer :: GenesisInitializer
genesisProdInitializer = gsInitializer genesisSpec

-- | 'GenesisDelegation' for production mode.
genesisProdDelegation :: GenesisDelegation
genesisProdDelegation = gsHeavyDelegation genesisSpec

----------------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------------

generateGenesisKeyPair :: Int -> (PublicKey, SecretKey)
generateGenesisKeyPair =
    deterministicKeyGen .
    encodeUtf8 .
    T.take 32 . sformat ("My awesome 32-byte seed #" %int % "             ")

generateHdwGenesisSecretKey :: Int -> EncryptedSecretKey
generateHdwGenesisSecretKey =
    snd .
    flip safeDeterministicKeyGen emptyPassphrase .
    encodeUtf8 .
    T.take 32 . sformat ("My 32-byte hdw seed #" %int % "                  ")

giveStaticConsts :: (HasCoreConstants => r) -> r
giveStaticConsts = giveConsts $ CoreConstants staticBlkSecurityParam
