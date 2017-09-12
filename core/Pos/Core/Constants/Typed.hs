-- | This module contains core constants with more descriptive types
-- (comparing to 'Pos.Core.Constants.Raw').

module Pos.Core.Constants.Typed
       (

       -- * Genesis constants
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
       ) where

import           Universum

import           Data.Time.Units            (Millisecond)
import           Serokell.Data.Memory.Units (Byte)

import           Pos.Core.Constants.Raw     (coreConfig)
import           Pos.Core.Fee               (TxFeePolicy)
import           Pos.Core.Types             (BlockCount, BlockVersionData (..),
                                             CoinPortion, EpochIndex (..), ScriptVersion,
                                             SoftforkRule (..), Timestamp (..))

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
