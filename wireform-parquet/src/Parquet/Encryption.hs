{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
-- | Apache Parquet modular encryption (parquet-format/Encryption.md).
--
-- Implements the spec's two encryption algorithms:
--
-- - 'AesGcmV1' (full AES-GCM): every encrypted module - footer, column
--   metadata, column index, offset index, bloom filter, and each page -
--   is encrypted as @nonce(12) || ciphertext || tag(16)@. The data and the
--   AAD are both authenticated, so any tampering with the layout aborts
--   the read.
--
-- - 'AesGcmCtrV1' (mixed mode): metadata modules (footer, column metadata,
--   page headers, indexes, bloom filter) use AES-GCM as above; data
--   pages and dictionary pages use AES-CTR for speed (no authentication on
--   the data bytes themselves, only on the encrypted page header).
--
-- The 'AAD' (Additional Authenticated Data) for each module is the
-- concatenation of:
--
-- @
-- AAD = aad_prefix || aad_suffix
-- aad_suffix = aad_file_id(8) || module_type(1)
--              || row_group_ordinal(2) || column_ordinal(2)
--              || page_ordinal(2)
-- @
--
-- not all modules use every suffix component; this module exposes
-- 'buildAadSuffix' / 'buildAad' helpers that match the spec's wire layout.
--
-- The actual key material is supplied by the caller through
-- 'EncryptionKeys'; this module does not assume a particular KMS. The
-- opaque @key_metadata@ bytes that should be stored on the encrypted
-- column chunks (and on the Iceberg @data_file.key_metadata@ field) round-
-- trip unchanged through 'EncryptionConfig'.
module Parquet.Encryption
  ( -- * Algorithms
    EncryptionAlgorithm(..)
  , ModuleType(..)
  , moduleTypeId
    -- * AAD construction
  , buildAadSuffix
  , buildAad
    -- * Encrypt / decrypt
  , encryptModule
  , decryptModule
  , encryptModuleCtr
  , decryptModuleCtr
    -- * Key configuration
  , EncryptionKeys(..)
  , EncryptionConfig(..)
  , unencrypted
  ) where

import qualified Crypto.Cipher.AES as Crypto
import Crypto.Cipher.Types (AEADMode (..), BlockCipher, IV)
import qualified Crypto.Cipher.Types as Cipher
import Crypto.Error (CryptoFailable (..))
import qualified Crypto.Random.Types as RNG
import qualified Data.ByteArray as BA
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int16)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

-- ============================================================
-- Spec types
-- ============================================================

data EncryptionAlgorithm = AesGcmV1 | AesGcmCtrV1
  deriving (Show, Eq)

data ModuleType
  = ModuleFooter
  | ModuleColumnMetaData
  | ModuleDataPage
  | ModuleDictionaryPage
  | ModuleDataPageHeader
  | ModuleDictionaryPageHeader
  | ModuleColumnIndex
  | ModuleOffsetIndex
  | ModuleBloomFilterHeader
  | ModuleBloomFilterBitset
  deriving (Show, Eq)

-- | The byte constant the spec assigns to each module type.
moduleTypeId :: ModuleType -> Int
moduleTypeId = \case
  ModuleFooter                -> 0
  ModuleColumnMetaData        -> 1
  ModuleDataPage              -> 2
  ModuleDictionaryPage        -> 3
  ModuleDataPageHeader        -> 4
  ModuleDictionaryPageHeader  -> 5
  ModuleColumnIndex           -> 6
  ModuleOffsetIndex           -> 7
  ModuleBloomFilterHeader     -> 8
  ModuleBloomFilterBitset     -> 9

-- ============================================================
-- AAD framing
-- ============================================================

buildAadSuffix
  :: ByteString -- ^ aad_file_id (exactly 8 bytes; padded with zeros if shorter).
  -> ModuleType
  -> Int16      -- ^ row group ordinal.
  -> Int16      -- ^ column ordinal.
  -> Int16      -- ^ page ordinal.
  -> ByteString
buildAadSuffix fileId mt rg col pg =
  BL.toStrict $ B.toLazyByteString $
    B.byteString (BS.take 8 (BS.append fileId (BS.replicate 8 0)))
    <> B.word8 (fromIntegral (moduleTypeId mt))
    <> B.int16LE rg
    <> B.int16LE col
    <> B.int16LE pg

buildAad
  :: ByteString  -- ^ aad_prefix (caller-defined; can be empty).
  -> ByteString  -- ^ aad_suffix produced by 'buildAadSuffix'.
  -> ByteString
buildAad = (<>)

-- ============================================================
-- AES-GCM
-- ============================================================

-- | Encrypt a module with AES-GCM. Output: @nonce(12) || ciphertext || tag(16)@.
encryptModule
  :: RNG.MonadRandom m
  => ByteString -- ^ AES key (16, 24, or 32 bytes).
  -> ByteString -- ^ AAD.
  -> ByteString -- ^ Plaintext.
  -> m (Either String ByteString)
encryptModule key aad plaintext = do
  nonce <- RNG.getRandomBytes 12 :: RNG.MonadRandom m => m BS.ByteString
  pure (encryptGcmWithNonce key nonce aad plaintext)

decryptModule
  :: ByteString -- ^ key
  -> ByteString -- ^ aad
  -> ByteString -- ^ wire bytes (nonce || ciphertext || tag)
  -> Either String ByteString
decryptModule key aad bs = case BS.length bs of
  n | n < 28 -> Left "Parquet.Encryption: ciphertext too short"
    | otherwise ->
        let (nonce, rest) = BS.splitAt 12 bs
            (ct, tagBs)   = BS.splitAt (BS.length rest - 16) rest
         in withAes key
              (\(c :: Crypto.AES128) -> gcmDecrypt c nonce aad ct tagBs)
              (\(c :: Crypto.AES192) -> gcmDecrypt c nonce aad ct tagBs)
              (\(c :: Crypto.AES256) -> gcmDecrypt c nonce aad ct tagBs)

encryptGcmWithNonce :: ByteString -> ByteString -> ByteString -> ByteString -> Either String ByteString
encryptGcmWithNonce key nonce aad plaintext =
  withAes key
    (\(c :: Crypto.AES128) -> gcmEncrypt c nonce aad plaintext)
    (\(c :: Crypto.AES192) -> gcmEncrypt c nonce aad plaintext)
    (\(c :: Crypto.AES256) -> gcmEncrypt c nonce aad plaintext)

gcmEncrypt :: forall c. BlockCipher c => c -> ByteString -> ByteString -> ByteString -> Either String ByteString
gcmEncrypt cipher nonce aad plaintext = do
  -- AES-GCM uses a 12-byte nonce; aeadInit accepts any ByteArrayAccess
  -- so we pass the nonce bytes directly. The IV-length restriction in
  -- @makeIV@ doesn't apply to AEAD initialisation.
  aead0 <- adapt (Cipher.aeadInit AEAD_GCM cipher nonce)
  let aead1 = Cipher.aeadAppendHeader aead0 aad
      (ciphertext, aead2) = Cipher.aeadEncrypt aead1 plaintext
      tag = Cipher.aeadFinalize aead2 16
  Right $ BS.concat [nonce, ciphertext, BA.convert tag]

gcmDecrypt :: forall c. BlockCipher c => c -> ByteString -> ByteString -> ByteString -> ByteString -> Either String ByteString
gcmDecrypt cipher nonce aad ct tagBs = do
  aead0 <- adapt (Cipher.aeadInit AEAD_GCM cipher nonce)
  let aead1 = Cipher.aeadAppendHeader aead0 aad
      (plaintext, aead2) = Cipher.aeadDecrypt aead1 ct
      expected = Cipher.aeadFinalize aead2 16
  if BA.constEq expected tagBs
    then Right plaintext
    else Left "Parquet.Encryption: GCM tag mismatch"

-- ============================================================
-- AES-CTR (mixed-mode payload encryption)
-- ============================================================

-- | Encrypt a module with AES-CTR (no authentication). Wire format:
-- @nonce(12) || ciphertext@. Used for data\/dictionary pages in
-- 'AesGcmCtrV1'.
encryptModuleCtr
  :: RNG.MonadRandom m
  => ByteString -- ^ key
  -> ByteString -- ^ plaintext
  -> m (Either String ByteString)
encryptModuleCtr key plaintext = do
  nonce <- RNG.getRandomBytes 12 :: RNG.MonadRandom m => m BS.ByteString
  pure (ctrEncryptWithNonce key nonce plaintext)

decryptModuleCtr :: ByteString -> ByteString -> Either String ByteString
decryptModuleCtr key bs
  | BS.length bs < 12 = Left "Parquet.Encryption: CTR ciphertext too short"
  | otherwise =
      let (nonce, ct) = BS.splitAt 12 bs
       in withAes key
            (\(c :: Crypto.AES128) -> ctrCombine c nonce ct)
            (\(c :: Crypto.AES192) -> ctrCombine c nonce ct)
            (\(c :: Crypto.AES256) -> ctrCombine c nonce ct)

ctrEncryptWithNonce :: ByteString -> ByteString -> ByteString -> Either String ByteString
ctrEncryptWithNonce key nonce plaintext =
  fmap (BS.append nonce)
    (withAes key
      (\(c :: Crypto.AES128) -> ctrCombine c nonce plaintext)
      (\(c :: Crypto.AES192) -> ctrCombine c nonce plaintext)
      (\(c :: Crypto.AES256) -> ctrCombine c nonce plaintext))

ctrCombine :: forall c. BlockCipher c => c -> ByteString -> ByteString -> Either String ByteString
ctrCombine cipher nonce buf = do
  -- Per Parquet spec: the 16-byte AES-CTR IV is the 12-byte module nonce
  -- followed by a 4-byte counter starting at zero.
  iv <- mkIVFor cipher (BS.append nonce (BS.replicate 4 0))
  Right (Cipher.ctrCombine cipher iv buf)

-- ============================================================
-- Key configuration
-- ============================================================

data EncryptionKeys = EncryptionKeys
  { ekFooterKey  :: !ByteString
  , ekColumnKeys :: !(Map Text ByteString)
  } deriving (Show, Eq)

data EncryptionConfig = EncryptionConfig
  { encAlgorithm    :: !EncryptionAlgorithm
  , encKeys         :: !EncryptionKeys
  , encAadFileId    :: !ByteString
  , encAadPrefix    :: !ByteString
  , encKeyMetadata  :: !ByteString
  } deriving (Show, Eq)

unencrypted :: EncryptionConfig
unencrypted = EncryptionConfig
  { encAlgorithm   = AesGcmV1
  , encKeys        = EncryptionKeys BS.empty Map.empty
  , encAadFileId   = BS.empty
  , encAadPrefix   = BS.empty
  , encKeyMetadata = BS.empty
  }

-- ============================================================
-- Internals
-- ============================================================

-- | Dispatch on key length to one of the three concrete AES variants.
-- The three branches exist because @crypton@ uses different types for
-- AES-128 / 192 / 256 to encode key sizes statically; the call-site
-- continues with whichever branch was selected.
withAes
  :: ByteString
  -> (Crypto.AES128 -> Either String r)
  -> (Crypto.AES192 -> Either String r)
  -> (Crypto.AES256 -> Either String r)
  -> Either String r
withAes key k128 k192 k256 = case BS.length key of
  16 -> case Cipher.cipherInit key :: CryptoFailable Crypto.AES128 of
    CryptoPassed c -> k128 c
    CryptoFailed e -> Left ("Parquet.Encryption: AES128 init: " ++ show e)
  24 -> case Cipher.cipherInit key :: CryptoFailable Crypto.AES192 of
    CryptoPassed c -> k192 c
    CryptoFailed e -> Left ("Parquet.Encryption: AES192 init: " ++ show e)
  32 -> case Cipher.cipherInit key :: CryptoFailable Crypto.AES256 of
    CryptoPassed c -> k256 c
    CryptoFailed e -> Left ("Parquet.Encryption: AES256 init: " ++ show e)
  n  -> Left ("Parquet.Encryption: unsupported key length " ++ show n)

-- | Construct an IV whose phantom cipher type matches the supplied cipher.
-- Forces the @IV@'s polymorphic block size to the concrete one that
-- 'aeadInit' / 'ctrCombine' expect later.
mkIVFor :: forall c. BlockCipher c => c -> ByteString -> Either String (IV c)
mkIVFor _ bs = case Cipher.makeIV bs of
  Just v  -> Right v
  Nothing -> Left "Parquet.Encryption: invalid IV length"

adapt :: CryptoFailable a -> Either String a
adapt (CryptoPassed a) = Right a
adapt (CryptoFailed e) = Left ("Parquet.Encryption: " ++ show e)
