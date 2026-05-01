{-# LANGUAGE PatternSynonyms #-}
-- | Named protobuf field descriptors for every ORC @orc_proto.proto@
-- message this codebase encodes or decodes.
--
-- ORC's wire format is protobuf, so every field lives at an explicit
-- @(fieldNum, wireType)@ tag. Historically the writers and readers
-- here carried those numbers inline as @encodeVarintField 1 _@ /
-- @case (fieldNum, wireType) of (1, 0) -> _@. After the analogous
-- Parquet @field_id@/@precision@ bug
-- (@apache/parquet-format::SchemaElement@ field 8 vs 9), we centralise
-- every ORC field number here as a bidirectional 'PatternSynonym' so
-- the spec number only appears once and writer + reader reference the
-- same name.
--
-- Wire-type encodings (matches @Proto.Wire@ and the protobuf spec):
--
-- @
-- 0 = varint          (int32 / int64 / uint32 / uint64 / bool / enum)
-- 1 = 64-bit fixed    (fixed64 / sfixed64 / double)
-- 2 = length-delim    (string / bytes / embedded / packed repeated)
-- 5 = 32-bit fixed    (fixed32 / sfixed32 / float)
-- @
--
-- Each pattern synonym packs @(fieldNum, wireType)@ so the reader can
-- do
--
-- > case (fieldNum, wireType) of
-- >   Footer_HeaderLength -> ...
--
-- and the writer can destructure the same pattern through
-- 'protoField' /  'encodeVarintField' below.
--
-- /Scope./ Covers @Footer@, @PostScript@, @StripeInformation@,
-- @ORCType@, @MetadataEntry@, @ColumnStatistics@, @StripeFooter@,
-- @Stream@, @RowIndex@, @RowIndexEntry@, @BloomFilter@,
-- @BloomFilterIndex@, @Encryption@, @EncryptionKey@,
-- @EncryptionVariant@, @DataMask@. Fields we don't touch are
-- intentionally absent; add a one-liner when needed.
--
-- /Spec refs./ Field numbers below are quoted from
-- @apache/orc@ at @proto/orc_proto.proto@. The encryption messages
-- are from the ORC 1.6+ additions to the same file.
module ORC.Proto.Schema
  ( -- * Wire types
    WireType
  , wtVarint
  , wtFixed64
  , wtLenDelim
  , wtFixed32
    -- * Writer helpers
  , encodeVarintField
  , encodeLengthDelim
  , encodeLengthDelimBytes
  , encodePackedVarintField
  , encodePackedFixed64Field
  , protoTagByte
    -- * Footer
  , pattern Footer_HeaderLength
  , pattern Footer_ContentLength
  , pattern Footer_Stripes
  , pattern Footer_Types
  , pattern Footer_Metadata
  , pattern Footer_NumberOfRows
  , pattern Footer_Statistics
    -- * PostScript
  , pattern PostScript_FooterLength
  , pattern PostScript_Compression
  , pattern PostScript_CompressionBlockSize
  , pattern PostScript_Version
  , pattern PostScript_Magic
    -- * StripeInformation
  , pattern StripeInformation_Offset
  , pattern StripeInformation_IndexLength
  , pattern StripeInformation_DataLength
  , pattern StripeInformation_FooterLength
  , pattern StripeInformation_NumberOfRows
    -- * ORCType
  , pattern ORCType_Kind
  , pattern ORCType_Subtypes
  , pattern ORCType_FieldNames
    -- * UserMetadataItem
  , pattern MetadataEntry_Name
  , pattern MetadataEntry_Value
    -- * ColumnStatistics
  , pattern ColumnStatistics_NumberOfValues
  , pattern ColumnStatistics_HasNull
  , pattern ColumnStatistics_BytesOnDisk
    -- * StripeFooter
  , pattern StripeFooter_Streams
    -- * Stream
  , pattern Stream_Kind
  , pattern Stream_Column
  , pattern Stream_Length
    -- * RowIndex + RowIndexEntry
  , pattern RowIndex_Entry
  , pattern RowIndexEntry_Positions
  , pattern RowIndexEntry_Statistics
    -- * BloomFilter + BloomFilterIndex
  , pattern BloomFilter_NumHashFunctions
  , pattern BloomFilter_Bitset
  , pattern BloomFilterIndex_Entry
    -- * Encryption
  , pattern Encryption_Mask
  , pattern Encryption_Key
  , pattern Encryption_Variants
  , pattern Encryption_KeyProvider
    -- * EncryptionKey
  , pattern EncryptionKey_KeyName
  , pattern EncryptionKey_KeyVersion
  , pattern EncryptionKey_Algorithm
    -- * EncryptionVariant
  , pattern EncryptionVariant_Root
  , pattern EncryptionVariant_Key
  , pattern EncryptionVariant_EncryptedKey
    -- * DataMask
  , pattern DataMask_Name
  , pattern DataMask_MaskParameters
  , pattern DataMask_Columns
  ) where

import Data.Bits (shiftL, shiftR, (.|.), (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector.Unboxed as VU
import Data.Word (Word64)

-- | Protobuf wire type. We keep it as a 'Word64' to match the value we
-- get by masking the tag byte, so patterns can be compared without
-- conversion.
type WireType = Word64

wtVarint, wtFixed64, wtLenDelim, wtFixed32 :: WireType
wtVarint    = 0
wtFixed64   = 1
wtLenDelim  = 2
wtFixed32   = 5

-- ============================================================
-- Writer helpers
-- ============================================================

-- | Encode a varint-wire field. Takes the pattern-synonym-supplied
-- @(fieldNum, wireType)@ pair so the call site reads as
--
-- > encodeVarintField Footer_HeaderLength (orcHeaderLength f)
--
-- instead of @encodeVarintField 1 _@. The wire-type component of the
-- pattern is asserted to be 'wtVarint' — passing a length-delimited
-- field here is a writer bug, and we'd rather crash loudly at encode
-- time than silently corrupt the file.
encodeVarintField :: (Int, WireType) -> Word64 -> B.Builder
encodeVarintField (fieldNum, wt) val
  | wt == wtVarint = protoTagByte fieldNum wt <> putVarint val
  | otherwise      = error $ "ORC.Proto.Schema.encodeVarintField: "
                          ++ "field " ++ show fieldNum
                          ++ " is not a varint field (got wire type "
                          ++ show wt ++ ")"

-- | Encode a length-delimited field (string / bytes / embedded
-- message / packed repeated).
encodeLengthDelim :: (Int, WireType) -> B.Builder -> B.Builder
encodeLengthDelim (fieldNum, wt) content
  | wt == wtLenDelim =
      let !encoded    = BL.toStrict (B.toLazyByteString content)
          !contentLen = BS.length encoded
       in protoTagByte fieldNum wt
          <> putVarint (fromIntegral contentLen)
          <> B.byteString encoded
  | otherwise = error $ "ORC.Proto.Schema.encodeLengthDelim: "
                     ++ "field " ++ show fieldNum
                     ++ " is not a length-delimited field "
                     ++ "(got wire type " ++ show wt ++ ")"

-- | Variant of 'encodeLengthDelim' that takes a 'ByteString' payload
-- directly — useful when the caller already has the pre-encoded bytes
-- (e.g. the bloom-filter bit-set, a pre-serialised @ColumnStatistics@).
encodeLengthDelimBytes :: (Int, WireType) -> ByteString -> B.Builder
encodeLengthDelimBytes (fieldNum, wt) payload
  | wt == wtLenDelim =
         protoTagByte fieldNum wt
      <> putVarint (fromIntegral (BS.length payload))
      <> B.byteString payload
  | otherwise = error $ "ORC.Proto.Schema.encodeLengthDelimBytes: "
                     ++ "field " ++ show fieldNum
                     ++ " is not a length-delimited field "
                     ++ "(got wire type " ++ show wt ++ ")"

-- | Emit a packed repeated varint field. The payload is a single
-- length-delimited region with back-to-back varints.
encodePackedVarintField :: (Int, WireType) -> [Word64] -> B.Builder
encodePackedVarintField _ [] = mempty
encodePackedVarintField f xs =
  encodeLengthDelim f (foldMap putVarint xs)

-- | Encode a packed repeated @fixed64@ field. Empty input emits no
-- bytes, matching the protobuf packed-repeated convention.
encodePackedFixed64Field :: (Int, WireType) -> VU.Vector Word64 -> B.Builder
encodePackedFixed64Field f@(_, wt) xs
  | VU.null xs       = mempty
  | wt == wtLenDelim =
      encodeLengthDelim f (VU.foldl' (\b w -> b <> B.word64LE w) mempty xs)
  | otherwise = error $ "ORC.Proto.Schema.encodePackedFixed64Field: "
                     ++ "expected wtLenDelim, got " ++ show wt

-- | Low-level: emit the tag byte(s) for @(fieldNum, wireType)@.
protoTagByte :: Int -> WireType -> B.Builder
protoTagByte fieldNum wireType =
  putVarint (fromIntegral ((fieldNum `shiftL` 3) .|. fromIntegral wireType))

putVarint :: Word64 -> B.Builder
putVarint = go
  where
    go !v
      | v < 0x80  = B.word8 (fromIntegral v)
      | otherwise =
          B.word8 (fromIntegral (v .&. 0x7F) .|. 0x80)
            <> go (v `shiftR` 7)

-- ============================================================
-- Footer
-- ============================================================

-- Per @orc_proto.proto::Footer@:
--
--   1 headerLength, 2 contentLength, 3 stripes (StripeInformation),
--   4 types (Type), 5 metadata (UserMetadataItem), 6 numberOfRows,
--   7 statistics (ColumnStatistics), 8 rowIndexStride (skipped),
--   9 writer (skipped), 10+ encryption (in ORC 1.6+; handled via
--   ORC.Encryption's own encoders).

pattern Footer_HeaderLength :: (Int, WireType)
pattern Footer_HeaderLength  = (1, 0)

pattern Footer_ContentLength :: (Int, WireType)
pattern Footer_ContentLength = (2, 0)

pattern Footer_Stripes :: (Int, WireType)
pattern Footer_Stripes       = (3, 2)

pattern Footer_Types :: (Int, WireType)
pattern Footer_Types         = (4, 2)

pattern Footer_Metadata :: (Int, WireType)
pattern Footer_Metadata      = (5, 2)

pattern Footer_NumberOfRows :: (Int, WireType)
pattern Footer_NumberOfRows  = (6, 0)

pattern Footer_Statistics :: (Int, WireType)
pattern Footer_Statistics    = (7, 2)

-- ============================================================
-- PostScript
-- ============================================================

pattern PostScript_FooterLength :: (Int, WireType)
pattern PostScript_FooterLength         = (1, 0)

pattern PostScript_Compression :: (Int, WireType)
pattern PostScript_Compression          = (2, 0)

pattern PostScript_CompressionBlockSize :: (Int, WireType)
pattern PostScript_CompressionBlockSize = (3, 0)

pattern PostScript_Version :: (Int, WireType)
pattern PostScript_Version              = (4, 0)

pattern PostScript_Magic :: (Int, WireType)
pattern PostScript_Magic                = (5, 2)

-- ============================================================
-- StripeInformation
-- ============================================================

pattern StripeInformation_Offset :: (Int, WireType)
pattern StripeInformation_Offset        = (1, 0)

pattern StripeInformation_IndexLength :: (Int, WireType)
pattern StripeInformation_IndexLength   = (2, 0)

pattern StripeInformation_DataLength :: (Int, WireType)
pattern StripeInformation_DataLength    = (3, 0)

pattern StripeInformation_FooterLength :: (Int, WireType)
pattern StripeInformation_FooterLength  = (4, 0)

pattern StripeInformation_NumberOfRows :: (Int, WireType)
pattern StripeInformation_NumberOfRows  = (5, 0)

-- ============================================================
-- Type
-- ============================================================

pattern ORCType_Kind :: (Int, WireType)
pattern ORCType_Kind       = (1, 0)

pattern ORCType_Subtypes :: (Int, WireType)
pattern ORCType_Subtypes   = (2, 0)

pattern ORCType_FieldNames :: (Int, WireType)
pattern ORCType_FieldNames = (3, 2)

-- ============================================================
-- UserMetadataItem
-- ============================================================

pattern MetadataEntry_Name :: (Int, WireType)
pattern MetadataEntry_Name  = (1, 2)

pattern MetadataEntry_Value :: (Int, WireType)
pattern MetadataEntry_Value = (2, 2)

-- ============================================================
-- ColumnStatistics
-- ============================================================

pattern ColumnStatistics_NumberOfValues :: (Int, WireType)
pattern ColumnStatistics_NumberOfValues = (1, 0)

pattern ColumnStatistics_HasNull :: (Int, WireType)
pattern ColumnStatistics_HasNull        = (2, 0)

pattern ColumnStatistics_BytesOnDisk :: (Int, WireType)
pattern ColumnStatistics_BytesOnDisk    = (3, 0)

-- ============================================================
-- StripeFooter
-- ============================================================

pattern StripeFooter_Streams :: (Int, WireType)
pattern StripeFooter_Streams = (1, 2)

-- ============================================================
-- Stream
-- ============================================================

pattern Stream_Kind :: (Int, WireType)
pattern Stream_Kind   = (1, 0)

pattern Stream_Column :: (Int, WireType)
pattern Stream_Column = (2, 0)

pattern Stream_Length :: (Int, WireType)
pattern Stream_Length = (3, 0)

-- ============================================================
-- RowIndex / RowIndexEntry
-- ============================================================

pattern RowIndex_Entry :: (Int, WireType)
pattern RowIndex_Entry = (1, 2)

pattern RowIndexEntry_Positions :: (Int, WireType)
-- packed repeated uint64
pattern RowIndexEntry_Positions  = (1, 2)

pattern RowIndexEntry_Statistics :: (Int, WireType)
pattern RowIndexEntry_Statistics = (2, 2)

-- ============================================================
-- BloomFilter / BloomFilterIndex
-- ============================================================

pattern BloomFilter_NumHashFunctions :: (Int, WireType)
pattern BloomFilter_NumHashFunctions = (1, 0)

pattern BloomFilter_Bitset :: (Int, WireType)
-- packed repeated fixed64
pattern BloomFilter_Bitset           = (2, 2)

pattern BloomFilterIndex_Entry :: (Int, WireType)
pattern BloomFilterIndex_Entry       = (1, 2)

-- ============================================================
-- Encryption (ORC 1.6+)
-- ============================================================

pattern Encryption_Mask :: (Int, WireType)
pattern Encryption_Mask        = (1, 2)

pattern Encryption_Key :: (Int, WireType)
pattern Encryption_Key         = (2, 2)

pattern Encryption_Variants :: (Int, WireType)
pattern Encryption_Variants    = (3, 2)

pattern Encryption_KeyProvider :: (Int, WireType)
pattern Encryption_KeyProvider = (4, 0)

-- ============================================================
-- EncryptionKey
-- ============================================================

pattern EncryptionKey_KeyName :: (Int, WireType)
pattern EncryptionKey_KeyName    = (1, 2)

pattern EncryptionKey_KeyVersion :: (Int, WireType)
pattern EncryptionKey_KeyVersion = (2, 0)

pattern EncryptionKey_Algorithm :: (Int, WireType)
pattern EncryptionKey_Algorithm  = (3, 0)

-- ============================================================
-- EncryptionVariant
-- ============================================================

pattern EncryptionVariant_Root :: (Int, WireType)
pattern EncryptionVariant_Root         = (1, 0)

pattern EncryptionVariant_Key :: (Int, WireType)
pattern EncryptionVariant_Key          = (2, 0)

pattern EncryptionVariant_EncryptedKey :: (Int, WireType)
pattern EncryptionVariant_EncryptedKey = (3, 2)

-- ============================================================
-- DataMask
-- ============================================================

pattern DataMask_Name :: (Int, WireType)
pattern DataMask_Name           = (1, 2)

pattern DataMask_MaskParameters :: (Int, WireType)
pattern DataMask_MaskParameters = (2, 2)

pattern DataMask_Columns :: (Int, WireType)
pattern DataMask_Columns        = (3, 0)
