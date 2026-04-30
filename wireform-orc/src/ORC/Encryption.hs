{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
-- | ORC column encryption support (ORC v1.6+).
--
-- ORC's column encryption design differs from Parquet's:
--
-- * Each /encrypted column subtree/ has a randomly generated
--   "local key". The local key is stored in the file footer
--   encrypted by a "master key" provided by an external KMS.
-- * Per-stripe rotation: 'StripeInformation' carries an
--   'encryptStripeId' counter (incremented per stripe) plus an
--   'encryptedLocalKeys' array (one per encrypted variant). The
--   actual stream-encryption key for a stripe is derived from
--   @AES-CTR(localKey, encryptStripeId)@ rather than reused
--   verbatim, which limits the impact of a key compromise to one
--   stripe.
-- * Stream encryption is AES-CTR (no authentication tag); the
--   per-stripe key + a derived IV from the stream offset are passed
--   to a CTR keystream.
--
-- This module supplies:
--
-- * The cryptographic primitives ('encryptStripeKey',
--   'deriveStreamIv', 'aesCtrXor') wired to the same @crypton@
--   library Parquet's encryption uses.
-- * The protobuf encoders for the four messages a writer needs to
--   emit in the footer's @Encryption@ field
--   ('encodeEncryption', 'encodeEncryptionKey',
--   'encodeEncryptionVariant', 'encodeDataMask').
-- * A @KeyProviderKind@ enum.
--
-- The actual integration into the whole-file writer (rotating keys
-- per stripe, encrypting each stream's bytes, populating the
-- @encrypted_local_keys@ on each 'StripeInformation') is composed
-- by the caller once they pick a KMS — this module deliberately
-- doesn't dictate how local keys are generated or where master
-- keys come from.
module ORC.Encryption
  ( -- * Algorithms
    EncryptionAlgorithm (..)
  , KeyProviderKind (..)
    -- * Stripe-key derivation
  , encryptStripeKey
  , deriveStreamIv
  , aesCtrXor
    -- * Protobuf encoders for the @Encryption@ footer field
  , Encryption (..)
  , EncryptionKey (..)
  , EncryptionVariant (..)
  , DataMask (..)
  , encodeEncryption
  , encodeEncryptionKey
  , encodeEncryptionVariant
  , encodeDataMask
  ) where

import qualified Crypto.Cipher.AES as Crypto
import qualified Crypto.Cipher.Types as Cipher
import Crypto.Error (CryptoFailable (..))
import Data.Bits (shiftL, shiftR, (.|.), (.&.))
import qualified Data.ByteArray as BA
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import Data.Word (Word32, Word64, Word8)

-- ============================================================
-- Algorithm and key provider enums
-- ============================================================

-- | The AES algorithm + key length used for stream encryption.
-- Matches @orc_proto.proto::EncryptionAlgorithm@.
data EncryptionAlgorithm
  = AES_CTR_128
  | AES_CTR_192
  | AES_CTR_256
  | UnknownEncryption
  deriving (Show, Eq)

algorithmTag :: EncryptionAlgorithm -> Int
algorithmTag = \case
  UnknownEncryption -> 0
  AES_CTR_128       -> 1
  AES_CTR_192       -> 2
  AES_CTR_256       -> 3

-- | Which KMS produced the master keys. Mirrors
-- @orc_proto.proto::KeyProviderKind@.
data KeyProviderKind
  = ProviderUnknown
  | ProviderHadoop
  | ProviderAwsKms
  | ProviderGcpKms
  | ProviderAzureKms
  deriving (Show, Eq)

providerTag :: KeyProviderKind -> Int
providerTag = \case
  ProviderUnknown  -> 0
  ProviderHadoop   -> 1
  ProviderAwsKms   -> 2
  ProviderGcpKms   -> 3
  ProviderAzureKms -> 4

-- ============================================================
-- Cryptographic primitives
-- ============================================================

-- | Derive a per-stripe encryption key from the column's local key
-- and the stripe ordinal, per the ORC encryption spec:
--
-- @stripeKey = AES-CTR(localKey, IV = (stripeId padded to 16 bytes))@
--
-- The result is used as the AES key for the stripe's stream
-- encryption. Rotating per stripe limits the blast radius of a
-- single key compromise.
encryptStripeKey
  :: ByteString  -- ^ local key (16 / 24 / 32 bytes)
  -> Word64      -- ^ stripe id
  -> Either String ByteString
encryptStripeKey localKey stripeId =
  let !iv = stripeIdToIV stripeId
      !payload = BS.replicate (BS.length localKey) 0
   in case aesCtrCombine localKey iv payload of
        Left e -> Left e
        Right ks -> Right ks

-- | The 16-byte AES-CTR IV the spec derives from a stream's byte
-- offset within the stripe, per the same @stripe-id-as-IV@
-- construction:
--
-- @iv[0..7]   = stripeId (BE)
-- iv[8..15]  = streamOffset (BE)@
deriveStreamIv :: Word64 -> Word64 -> ByteString
deriveStreamIv stripeId streamOffset =
  BL.toStrict $ B.toLazyByteString $
       B.word64BE stripeId
    <> B.word64BE streamOffset

-- | XOR a payload with the AES-CTR keystream produced by
-- (key, iv). For ORC encryption this is the only cipher operation
-- on stream bytes - both encrypt and decrypt use the same call.
aesCtrXor
  :: ByteString  -- ^ key (16 / 24 / 32 bytes)
  -> ByteString  -- ^ 16-byte IV
  -> ByteString  -- ^ payload
  -> Either String ByteString
aesCtrXor = aesCtrCombine

aesCtrCombine
  :: ByteString
  -> ByteString
  -> ByteString
  -> Either String ByteString
aesCtrCombine key iv payload = withAes key
  (\(c :: Crypto.AES128) -> ctrCombine c iv payload)
  (\(c :: Crypto.AES192) -> ctrCombine c iv payload)
  (\(c :: Crypto.AES256) -> ctrCombine c iv payload)

ctrCombine
  :: forall c. Cipher.BlockCipher c
  => c -> ByteString -> ByteString -> Either String ByteString
ctrCombine cipher iv buf = case Cipher.makeIV iv of
  Nothing -> Left "ORC.Encryption: bad IV length (expected 16 bytes)"
  Just (ivc :: Cipher.IV c) -> Right (Cipher.ctrCombine cipher ivc buf)

withAes
  :: ByteString
  -> (Crypto.AES128 -> Either String r)
  -> (Crypto.AES192 -> Either String r)
  -> (Crypto.AES256 -> Either String r)
  -> Either String r
withAes key k128 k192 k256 = case BS.length key of
  16 -> case Cipher.cipherInit key :: CryptoFailable Crypto.AES128 of
    CryptoPassed c -> k128 c
    CryptoFailed e -> Left ("ORC.Encryption: AES128 init: " ++ show e)
  24 -> case Cipher.cipherInit key :: CryptoFailable Crypto.AES192 of
    CryptoPassed c -> k192 c
    CryptoFailed e -> Left ("ORC.Encryption: AES192 init: " ++ show e)
  32 -> case Cipher.cipherInit key :: CryptoFailable Crypto.AES256 of
    CryptoPassed c -> k256 c
    CryptoFailed e -> Left ("ORC.Encryption: AES256 init: " ++ show e)
  n  -> Left ("ORC.Encryption: unsupported key length " ++ show n)

stripeIdToIV :: Word64 -> ByteString
stripeIdToIV sid =
  BL.toStrict $ B.toLazyByteString $
    B.byteString (BS.replicate 8 0)
    <> B.word64BE sid

-- ============================================================
-- Footer encryption messages (protobuf encoders)
-- ============================================================
--
-- Field numbers from orc_proto.proto:
--
--   message Encryption {
--     repeated DataMask           mask     = 1;
--     repeated EncryptionKey      key      = 2;
--     repeated EncryptionVariant  variants = 3;
--     optional KeyProviderKind    keyProvider = 4;
--   }
--
--   message EncryptionKey {
--     optional string keyName    = 1;
--     optional uint32 keyVersion = 2;
--     optional EncryptionAlgorithm algorithm = 3;
--   }
--
--   message EncryptionVariant {
--     optional uint32 root         = 1;
--     optional uint32 key          = 2;  // index into Encryption.key
--     optional bytes  encryptedKey = 3;
--     repeated Stream stripeStatistics = 4;
--     optional bytes  fileStatistics    = 5;
--   }
--
--   message DataMask {
--     optional string  name           = 1;
--     repeated string  maskParameters = 2;
--     repeated uint32  columns        = 3;
--   }

data Encryption = Encryption
  { encMasks       :: ![DataMask]
  , encKeys        :: ![EncryptionKey]
  , encVariants    :: ![EncryptionVariant]
  , encKeyProvider :: !KeyProviderKind
  } deriving (Show, Eq)

data EncryptionKey = EncryptionKey
  { ekName      :: !ByteString  -- empty = field omitted
  , ekVersion   :: !Word32
  , ekAlgorithm :: !EncryptionAlgorithm
  } deriving (Show, Eq)

data EncryptionVariant = EncryptionVariant
  { evRoot         :: !Word32
  , evKey          :: !Word32     -- index into Encryption.keys
  , evEncryptedKey :: !ByteString
  } deriving (Show, Eq)

data DataMask = DataMask
  { dmName       :: !ByteString
  , dmParameters :: ![ByteString]
  , dmColumns    :: ![Word32]
  } deriving (Show, Eq)

encodeEncryption :: Encryption -> ByteString
encodeEncryption e = BL.toStrict $ B.toLazyByteString $
       foldMap (protoLengthDelimited 1 . encodeDataMask)         (encMasks e)
    <> foldMap (protoLengthDelimited 2 . encodeEncryptionKey)    (encKeys e)
    <> foldMap (protoLengthDelimited 3 . encodeEncryptionVariant) (encVariants e)
    <> protoVarintField 4
         (fromIntegral (providerTag (encKeyProvider e)))

encodeEncryptionKey :: EncryptionKey -> ByteString
encodeEncryptionKey ek = BL.toStrict $ B.toLazyByteString $
  optName <> protoVarintField 2 (fromIntegral (ekVersion ek))
          <> protoVarintField 3 (fromIntegral (algorithmTag (ekAlgorithm ek)))
  where
    optName
      | BS.null (ekName ek) = mempty
      | otherwise           = protoLengthDelimited 1 (ekName ek)

encodeEncryptionVariant :: EncryptionVariant -> ByteString
encodeEncryptionVariant ev = BL.toStrict $ B.toLazyByteString $
       protoVarintField 1 (fromIntegral (evRoot ev))
    <> protoVarintField 2 (fromIntegral (evKey ev))
    <> protoLengthDelimited 3 (evEncryptedKey ev)

encodeDataMask :: DataMask -> ByteString
encodeDataMask dm = BL.toStrict $ B.toLazyByteString $
       optName
    <> foldMap (protoLengthDelimited 2) (dmParameters dm)
    <> foldMap (protoVarintField 3 . fromIntegral) (dmColumns dm)
  where
    optName
      | BS.null (dmName dm) = mempty
      | otherwise           = protoLengthDelimited 1 (dmName dm)

-- ============================================================
-- Protobuf helpers
-- ============================================================

protoVarintField :: Int -> Word64 -> B.Builder
protoVarintField fieldNum v =
  protoTag fieldNum 0 <> protoVarint v

protoLengthDelimited :: Int -> ByteString -> B.Builder
protoLengthDelimited fieldNum payload =
     protoTag fieldNum 2
  <> protoVarint (fromIntegral (BS.length payload))
  <> B.byteString payload

protoTag :: Int -> Int -> B.Builder
protoTag fieldNum wireType =
  protoVarint (fromIntegral ((fieldNum `shiftL` 3) .|. wireType))

protoVarint :: Word64 -> B.Builder
protoVarint = go
  where
    go !n
      | n < 0x80  = B.word8 (fromIntegral n)
      | otherwise =
          B.word8 (fromIntegral (n .&. 0x7F) .|. 0x80)
            <> go (n `shiftR` 7)

-- silence unused-import warning for BA / Word8.
_unusedBA :: BA.Bytes -> Int
_unusedBA = BA.length

_unusedW8 :: Word8 -> Word8
_unusedW8 = id
