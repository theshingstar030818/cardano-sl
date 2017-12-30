{-# LANGUAGE RankNTypes #-}

-- | Functions for restoring blocks from a block dump.

module Pos.Block.Dump
       ( parseBlockDump
       ) where

import           Universum

import qualified Codec.CBOR.Decoding as Cbor hiding (DecodeAction (..))
import qualified Codec.CBOR.Read as Cbor
import           Conduit (Conduit, Consumer, (.|))
import qualified Conduit as C
import           Control.Monad.Morph (hoist)
import           Control.Monad.ST (ST)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Conduit.Lzma as Lzma
import qualified Data.Text.Buildable
import           Formatting (bprint, int, stext, (%))
import           Serokell.Util (listJson)

import           Pos.Binary.Class (Bi)
import qualified Pos.Binary.Class as Bi
import           Pos.Core.Block (Block)

----------------------------------------------------------------------------
-- Block dump parsing
----------------------------------------------------------------------------

data ParseBlockDumpError
    = BlockParseError !Text
    | DumpFormatError !Text
    | UnexpectedBlockVersion !Word16 ![Word16]
    | EmptyInput
    | LeftoverInput
    | NotEnoughBlocks !Int
    deriving (Show, Eq)

instance Exception ParseBlockDumpError

instance Buildable ParseBlockDumpError where
    build (BlockParseError reason) =
        bprint ("Failed to parse a block: "%stext) reason
    build (DumpFormatError reason) =
        bprint ("Block dump has wrong format: "%stext) reason
    build (UnexpectedBlockVersion ver expected) =
        bprint ("Expected one of block versions "%listJson%", got "%int)
               expected ver
    build EmptyInput =
        bprint "Block dump is completely empty (no bytes)"
    build LeftoverInput =
        bprint "Block dump contains extra data after the blocks"
    build (NotEnoughBlocks n) =
        bprint ("Block dump ended but we expected "%int%" more blocks") n

-- conduits

-- | A conduit that parses blocks from a CBOR-encoded LZMA-compressed list.
-- Throws an error if there are less or more blocks than specified in the
-- list, or if there's extra input left.
parseBlockDump
    :: (Bi Block, C.MonadResource m, MonadThrow m)
    => Conduit ByteString m Block
parseBlockDump =
    Lzma.decompress memlimit .| C.chunksOfCE 65536 .| do
        (ver, len) <- parseHeader
        when (ver /= 0) $ throwM (UnexpectedBlockVersion ver [0])
        parseBlockStream len
  where
    memlimit = Just (200*1024*1024)  -- 200 MB

-- | Deserialize the header of a dump, assuming it's fully contained in the
-- first chunk of the input.
--
-- Dumps look like this: @[version, [block1, block2, ...]]@. In CBOR terms,
-- this is translated into this sequence of tokens:
--
-- @
-- listlen=2, version, listlen=N, [block1] [block2] ...
-- @
--
-- This conduit returns the version and the list length.
--
parseHeader
    :: MonadThrow m
    => Consumer ByteString m (Word16, Int)
parseHeader = C.await >>= \case
    Nothing    -> throwM EmptyInput
    Just chunk -> case runStateT parser (BSL.fromStrict chunk) of
        Left (Cbor.DeserialiseFailure _ err) ->
            throwM (DumpFormatError (toText err))
        Right (x, bs) -> do
            C.leftover (BSL.toStrict bs)
            pure x
  where
    parser = do
        peekCbor $ Cbor.decodeListLenCanonicalOf 2
        ver <- peekCbor Cbor.decodeWord16Canonical
        len <- peekCbor Cbor.decodeListLenCanonical
        pure (ver, len)

-- | Decode a stream of blocks. There must be exactly the number of blocks
-- we're passed.
parseBlockStream
    :: (MonadIO m, MonadThrow m, Bi Block)
    => Int                        -- ^ Number of blocks to expect
    -> Conduit ByteString m Block
parseBlockStream 0 =
    awaitNE >>= \case
        "" -> pure ()
        _  -> throwM LeftoverInput
parseBlockStream n =
    awaitCbor >>= \case
        Left err           -> throwM (BlockParseError err)
        Right Nothing      -> throwM (NotEnoughBlocks n)
        Right (Just block) -> C.yield block >> parseBlockStream (n-1)

----------------------------------------------------------------------------
-- Utilities
----------------------------------------------------------------------------

-- | Run a decoder on a bytestring, “peeking” a value out of it.
peekCbor
    :: (forall s. Cbor.Decoder s a)
    -> StateT LByteString (Either Cbor.DeserialiseFailure) a
peekCbor d = StateT (fmap swap . Cbor.deserialiseFromBytes d)

-- | Decode one CBOR value from the input.
--
-- Returns a wrapped 'Nothing' if there's no input left.
awaitCbor
    :: (MonadIO m, Bi a)
    => Consumer ByteString m (Either Text (Maybe a))
awaitCbor = hoist stToIO $ do
    chunk <- awaitNE
    case chunk of
        "" -> pure (Right Nothing)
        bs -> do C.leftover bs
                 go False =<< lift (Cbor.deserialiseIncremental Bi.decode)
  where
    go :: Bool              -- ^ End of input reached?
       -> Bi.IDecode s a    -- ^ Result of decoding so far
       -> Consumer ByteString (ST s) (Either Text a)
    go eof = \case
        Cbor.Fail rest _ (Cbor.DeserialiseFailure _ err) -> do
            C.leftover rest
            pure (Left (toText err))
        Cbor.Partial f | not eof -> do
            awaitNE >>= \case
                "" -> go True  =<< lift (f Nothing)
                bs -> go False =<< lift (f (Just bs))
        Cbor.Partial _ ->    -- impossible, I think
            pure (Left "CBOR decoder returned 'Partial' after end of input")
        Cbor.Done rest _ x -> do
            C.leftover rest
            pure (Right x)

-- | Get a chunk of data, only returning an empty 'ByteString' at the end of
-- the stream.
awaitNE :: Monad m => Consumer ByteString m ByteString
awaitNE = loop
  where
    loop = C.await >>= maybe (return mempty) check
    check bs
        | null bs   = loop
        | otherwise = return bs
