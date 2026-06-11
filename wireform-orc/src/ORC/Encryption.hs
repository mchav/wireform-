{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- | ORC column encryption support (ORC v1.6+).

ORC's column encryption design differs from Parquet's:

* Each /encrypted column subtree/ has a randomly generated
  "local key". The local key is stored in the file footer
  encrypted by a "master key" provided by an external KMS.
* Per-stripe rotation: 'StripeInformation' carries an
  'encryptStripeId' counter (incremented per stripe) plus an
  'encryptedLocalKeys' array (one per encrypted variant). The
  actual stream-encryption key for a stripe is derived from
  @AES-CTR(localKey, encryptStripeId)@ rather than reused
  verbatim, which limits the impact of a key compromise to one
  stripe.
* Stream encryption is AES-CTR (no authentication tag); the
  per-stripe key + a derived IV from the stream offset are passed
  to a CTR keystream.

This module supplies:

* The cryptographic primitives ('encryptStripeKey',
  'deriveStreamIv', 'aesCtrXor') wired to the same @crypton@
  library Parquet's encryption uses.
* The protobuf encoders for the four messages a writer needs to
  emit in the footer's @Encryption@ field
  ('encodeEncryption', 'encodeEncryptionKey',
  'encodeEncryptionVariant', 'encodeDataMask').
* A @KeyProviderKind@ enum.

The actual integration into the whole-file writer (rotating keys
per stripe, encrypting each stream's bytes, populating the
@encrypted_local_keys@ on each 'StripeInformation') is composed
by the caller once they pick a KMS — this module deliberately
doesn't dictate how local keys are generated or where master
keys come from.
-}
module ORC.Encryption (
  -- * Algorithms
  EncryptionAlgorithm (..),
  KeyProviderKind (..),

  -- * Stripe-key derivation
  encryptStripeKey,
  deriveStreamIv,
  aesCtrXor,

  -- * Protobuf codec for the @Encryption@ footer field
  Encryption (..),
  EncryptionKey (..),
  EncryptionVariant (..),
  DataMask (..),
  encodeEncryption,
  encodeEncryptionKey,
  encodeEncryptionVariant,
  encodeDataMask,
  decodeEncryption,
  decodeEncryptionKey,
  decodeEncryptionVariant,
  decodeDataMask,
) where

import Crypto.Cipher.AES qualified as Crypto
import Crypto.Cipher.Types qualified as Cipher
import Crypto.Error (CryptoFailable (..))
import Data.ByteArray qualified as BA
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Word (Word32, Word64, Word8)
import ORC.Proto.Schema
import Wireform.Builder qualified as B


-- ============================================================
-- Algorithm and key provider enums
-- ============================================================

{- | The AES algorithm + key length used for stream encryption.
Matches @orc_proto.proto::EncryptionAlgorithm@.
-}
data EncryptionAlgorithm
  = AES_CTR_128
  | AES_CTR_192
  | AES_CTR_256
  | UnknownEncryption
  deriving (Show, Eq)


algorithmTag :: EncryptionAlgorithm -> Int
algorithmTag = \case
  UnknownEncryption -> 0
  AES_CTR_128 -> 1
  AES_CTR_192 -> 2
  AES_CTR_256 -> 3


{- | Inverse of 'algorithmTag'. Unknown tags are surfaced as
'UnknownEncryption' (matching the spec's @UNKNOWN_ENCRYPTION = 0@
fallback) rather than as a decode error, so a reader can tolerate
newer ORC writers that introduce additional algorithm tags.
-}
algorithmFromTag :: Int -> EncryptionAlgorithm
algorithmFromTag = \case
  1 -> AES_CTR_128
  2 -> AES_CTR_192
  3 -> AES_CTR_256
  _ -> UnknownEncryption


{- | Which KMS produced the master keys. Mirrors
@orc_proto.proto::KeyProviderKind@.
-}
data KeyProviderKind
  = ProviderUnknown
  | ProviderHadoop
  | ProviderAwsKms
  | ProviderGcpKms
  | ProviderAzureKms
  deriving (Show, Eq)


providerTag :: KeyProviderKind -> Int
providerTag = \case
  ProviderUnknown -> 0
  ProviderHadoop -> 1
  ProviderAwsKms -> 2
  ProviderGcpKms -> 3
  ProviderAzureKms -> 4


{- | Inverse of 'providerTag'; unknown values fall through to
'ProviderUnknown' for forward compatibility.
-}
providerFromTag :: Int -> KeyProviderKind
providerFromTag = \case
  1 -> ProviderHadoop
  2 -> ProviderAwsKms
  3 -> ProviderGcpKms
  4 -> ProviderAzureKms
  _ -> ProviderUnknown


-- ============================================================
-- Cryptographic primitives
-- ============================================================

{- | Derive a per-stripe encryption key from the column's local key
and the stripe ordinal, per the ORC encryption spec:

@stripeKey = AES-CTR(localKey, IV = (stripeId padded to 16 bytes))@

The result is used as the AES key for the stripe's stream
encryption. Rotating per stripe limits the blast radius of a
single key compromise.
-}
encryptStripeKey
  :: ByteString
  -- ^ local key (16 / 24 / 32 bytes)
  -> Word64
  -- ^ stripe id
  -> Either String ByteString
encryptStripeKey localKey stripeId =
  let !iv = stripeIdToIV stripeId
      !payload = BS.replicate (BS.length localKey) 0
  in case aesCtrCombine localKey iv payload of
       Left e -> Left e
       Right ks -> Right ks


{- | The 16-byte AES-CTR IV the spec derives from a stream's byte
offset within the stripe, per the same @stripe-id-as-IV@
construction:

@iv[0..7]   = stripeId (BE)
iv[8..15]  = streamOffset (BE)@
-}
deriveStreamIv :: Word64 -> Word64 -> ByteString
deriveStreamIv stripeId streamOffset =
  BL.toStrict $
    B.toLazyByteString $
      B.word64BE stripeId
        <> B.word64BE streamOffset


{- | XOR a payload with the AES-CTR keystream produced by
(key, iv). For ORC encryption this is the only cipher operation
on stream bytes - both encrypt and decrypt use the same call.
-}
aesCtrXor
  :: ByteString
  -- ^ key (16 / 24 / 32 bytes)
  -> ByteString
  -- ^ 16-byte IV
  -> ByteString
  -- ^ payload
  -> Either String ByteString
aesCtrXor = aesCtrCombine


aesCtrCombine
  :: ByteString
  -> ByteString
  -> ByteString
  -> Either String ByteString
aesCtrCombine key iv payload =
  withAes
    key
    (\(c :: Crypto.AES128) -> ctrCombine c iv payload)
    (\(c :: Crypto.AES192) -> ctrCombine c iv payload)
    (\(c :: Crypto.AES256) -> ctrCombine c iv payload)


ctrCombine
  :: forall c
   . Cipher.BlockCipher c
  => c
  -> ByteString
  -> ByteString
  -> Either String ByteString
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
  n -> Left ("ORC.Encryption: unsupported key length " ++ show n)


stripeIdToIV :: Word64 -> ByteString
stripeIdToIV sid =
  BL.toStrict $
    B.toLazyByteString $
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
  { encMasks :: ![DataMask]
  , encKeys :: ![EncryptionKey]
  , encVariants :: ![EncryptionVariant]
  , encKeyProvider :: !KeyProviderKind
  }
  deriving (Show, Eq)


data EncryptionKey = EncryptionKey
  { ekName :: !ByteString -- empty = field omitted
  , ekVersion :: !Word32
  , ekAlgorithm :: !EncryptionAlgorithm
  }
  deriving (Show, Eq)


data EncryptionVariant = EncryptionVariant
  { evRoot :: !Word32
  , evKey :: !Word32 -- index into Encryption.keys
  , evEncryptedKey :: !ByteString
  }
  deriving (Show, Eq)


data DataMask = DataMask
  { dmName :: !ByteString
  , dmParameters :: ![ByteString]
  , dmColumns :: ![Word32]
  }
  deriving (Show, Eq)


encodeEncryption :: Encryption -> ByteString
encodeEncryption e =
  BL.toStrict $
    B.toLazyByteString $
      foldMap
        (encodeLengthDelimBytes Encryption_Mask . encodeDataMask)
        (encMasks e)
        <> foldMap
          (encodeLengthDelimBytes Encryption_Key . encodeEncryptionKey)
          (encKeys e)
        <> foldMap
          (encodeLengthDelimBytes Encryption_Variants . encodeEncryptionVariant)
          (encVariants e)
        <> encodeVarintField
          Encryption_KeyProvider
          (fromIntegral (providerTag (encKeyProvider e)))


encodeEncryptionKey :: EncryptionKey -> ByteString
encodeEncryptionKey ek =
  BL.toStrict $
    B.toLazyByteString $
      optName
        <> encodeVarintField EncryptionKey_KeyVersion (fromIntegral (ekVersion ek))
        <> encodeVarintField
          EncryptionKey_Algorithm
          (fromIntegral (algorithmTag (ekAlgorithm ek)))
  where
    optName
      | BS.null (ekName ek) = mempty
      | otherwise = encodeLengthDelimBytes EncryptionKey_KeyName (ekName ek)


encodeEncryptionVariant :: EncryptionVariant -> ByteString
encodeEncryptionVariant ev =
  BL.toStrict $
    B.toLazyByteString $
      encodeVarintField EncryptionVariant_Root (fromIntegral (evRoot ev))
        <> encodeVarintField EncryptionVariant_Key (fromIntegral (evKey ev))
        <> encodeLengthDelimBytes EncryptionVariant_EncryptedKey (evEncryptedKey ev)


encodeDataMask :: DataMask -> ByteString
encodeDataMask dm =
  BL.toStrict $
    B.toLazyByteString $
      optName
        <> foldMap (encodeLengthDelimBytes DataMask_MaskParameters) (dmParameters dm)
        <> foldMap (encodeVarintField DataMask_Columns . fromIntegral) (dmColumns dm)
  where
    optName
      | BS.null (dmName dm) = mempty
      | otherwise = encodeLengthDelimBytes DataMask_Name (dmName dm)


-- ============================================================
-- Decoders (inverse of the encoders above)
-- ============================================================

{- | Parse an @Encryption@ protobuf message (the
@Footer.encryption@ field's value) into its typed representation.
Accepts the bytes wrapped in a 'Types.FooterEncryption' round-trip
via 'ORC.Footer'.
-}
decodeEncryption :: ByteString -> Either String Encryption
decodeEncryption bs =
  let empty_ =
        Encryption
          { encMasks = []
          , encKeys = []
          , encVariants = []
          , encKeyProvider = ProviderUnknown
          }
  in decodeMsg bs empty_ $ \e -> \case
       Encryption_Mask -> ReadBytesE $ \payload -> do
         m <- decodeDataMask payload
         Right e {encMasks = encMasks e ++ [m]}
       Encryption_Key -> ReadBytesE $ \payload -> do
         k <- decodeEncryptionKey payload
         Right e {encKeys = encKeys e ++ [k]}
       Encryption_Variants -> ReadBytesE $ \payload -> do
         v <- decodeEncryptionVariant payload
         Right e {encVariants = encVariants e ++ [v]}
       Encryption_KeyProvider -> ReadVarint $ \v ->
         e {encKeyProvider = providerFromTag (fromIntegral v)}
       _ -> SkipUnknown


decodeEncryptionKey :: ByteString -> Either String EncryptionKey
decodeEncryptionKey bs =
  decodeMsg bs (EncryptionKey BS.empty 0 UnknownEncryption) $ \k -> \case
    EncryptionKey_KeyName -> ReadBytes $ \v -> k {ekName = v}
    EncryptionKey_KeyVersion -> ReadVarint $ \v -> k {ekVersion = fromIntegral v}
    EncryptionKey_Algorithm -> ReadVarint $ \v ->
      k {ekAlgorithm = algorithmFromTag (fromIntegral v)}
    _ -> SkipUnknown


decodeEncryptionVariant :: ByteString -> Either String EncryptionVariant
decodeEncryptionVariant bs =
  decodeMsg bs (EncryptionVariant 0 0 BS.empty) $ \v -> \case
    EncryptionVariant_Root -> ReadVarint $ \x ->
      v {evRoot = fromIntegral x}
    EncryptionVariant_Key -> ReadVarint $ \x ->
      v {evKey = fromIntegral x}
    EncryptionVariant_EncryptedKey -> ReadBytes $ \x ->
      v {evEncryptedKey = x}
    _ -> SkipUnknown


decodeDataMask :: ByteString -> Either String DataMask
decodeDataMask bs =
  decodeMsg bs (DataMask BS.empty [] []) $ \d -> \case
    DataMask_Name -> ReadBytes $ \x -> d {dmName = x}
    DataMask_MaskParameters -> ReadBytes $ \x ->
      d {dmParameters = dmParameters d ++ [x]}
    DataMask_Columns -> ReadVarint $ \x ->
      d {dmColumns = dmColumns d ++ [fromIntegral x]}
    _ -> SkipUnknown


-- silence unused-import warning for BA / Word8.
_unusedBA :: BA.Bytes -> Int
_unusedBA = BA.length


_unusedW8 :: Word8 -> Word8
_unusedW8 = id
