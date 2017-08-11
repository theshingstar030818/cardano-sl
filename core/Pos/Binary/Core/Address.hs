{-# LANGUAGE ScopedTypeVariables #-}

-- | Binary serialization of Pos.Types.Address
module Pos.Binary.Core.Address () where

import           Universum

import           Data.Default        (def)
import           Data.Digest.CRC32   (CRC32 (..))
import           Data.Word           (Word8)
import           Formatting          (format, shown, (%))
import           Pos.Binary.Class    (Bi (..), Decoder, decodeFull, deserialize',
                                      encodeListLen, enforceSize, serialize')
import           Pos.Binary.Crypto   ()
import           Pos.Core.Types      (AddrPkAttrs (..), Address (..))
import           Pos.Data.Attributes (Attributes, decodeAttributes, encodeAttributes)

{- NOTE: Address serialization
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

An address is serialized as follows:

    <crc32><one-byte tag><content>

This lets us have backwards compatibility. For instance, if a newer version
of CSL adds an address with tag 5:

    data Address
        = ...
        | SuperAddress A B C

then older versions would deserialize it as follows:

    UnknownAddressType 5 <some bytes>

The CRC helps detecting accidental (or hostile) changes in the raw data,
and is computed on the whole `<tag><address-as-a-blob>` binary format.
-}

----------------------------------------

instance Bi (Attributes AddrPkAttrs) where
    encode = encodeAttributes [(0, serialize' . addrPkDerivationPath)]
    decode = decodeAttributes def $ \n v acc -> case n of
        0 -> Just $ acc { addrPkDerivationPath = deserialize' v }
        _ -> Nothing

-- | Converts the `Address` (without the CRC32) into binary blob
-- comprehensive of the serialised address and the tag.
-- We prefix the serialised `Address` with the tag `24`, which is
-- used to signal what follows next is a binary blob encoded using
-- the CBOR format:
-- Cfr. https://tools.ietf.org/html/rfc7049#section-2.4.4.1
toTaggedAddress :: Address -> ByteString
toTaggedAddress addr = serialize' tagAndBlob
    where
      tagAndBlob :: (Word8, Word8, ByteString)
      tagAndBlob = case addr of
          PubKeyAddress keyHash attrs -> (0, 24, serialize' (keyHash, attrs))
          ScriptAddress scrHash       -> (1, 24, serialize' scrHash)
          RedeemAddress keyHash       -> (2, 24, serialize' keyHash)
          UnknownAddressType t bs     -> (t, 24, bs) -- Should an unknown Address blob take the 24 tag?

instance Bi Address where
    encode addr =
        let taggedAddress = toTaggedAddress addr
        in encodeListLen 2 <> encode (crc32 taggedAddress) <> encode taggedAddress
    decode = do
        enforceSize "Address" 2
        expectedCRC   <- decode @Word32
        taggedAddress <- decode @ByteString
        let actualCRC = crc32 taggedAddress
        if expectedCRC /= actualCRC
           then fail $ toString $ format ("Address " % shown % " has invalid checksum: " % shown) taggedAddress actualCRC
           else decodeAddress taggedAddress

-- | Conceptually the inverse of `toTaggedAddress`, as it will deserialise an `Address` given
-- its binary-blob representation.
decodeAddress :: ByteString -> Decoder s Address
decodeAddress taggedAddress = do
    case decodeFull taggedAddress of
        Left e ->
            let failMsg = "decodeAddress failed when decoding " <> show taggedAddress <> ": " <> show e
            in fail failMsg
        Right (t :: Word8, semTag :: Word8, bs :: ByteString) -> do
            if semTag /= 24
              then fail $ "Tag 24 not found before deserialising an Address: " <> show semTag
              else pure $ case t of
                  0 -> uncurry PubKeyAddress $ deserialize' bs
                  1 -> ScriptAddress         $ deserialize' bs
                  2 -> RedeemAddress         $ deserialize' bs
                  _ -> UnknownAddressType t bs
