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
    -- * Decoder DSL
  , FieldAction (..)
  , decodeMsg
  , getVarint
  , getLenDelim
  , skipField
    -- * Footer
  , pattern Footer_HeaderLength
  , pattern Footer_ContentLength
  , pattern Footer_Stripes
  , pattern Footer_Types
  , pattern Footer_Metadata
  , pattern Footer_NumberOfRows
  , pattern Footer_Statistics
  , pattern Footer_Encryption
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
  , pattern ColumnStatistics_IntStatistics
  , pattern ColumnStatistics_DoubleStatistics
  , pattern ColumnStatistics_StringStatistics
  , pattern ColumnStatistics_BucketStatistics
  , pattern ColumnStatistics_DecimalStatistics
  , pattern ColumnStatistics_DateStatistics
  , pattern ColumnStatistics_BinaryStatistics
  , pattern ColumnStatistics_TimestampStatistics
    -- * IntegerStatistics
  , pattern IntegerStatistics_Minimum
  , pattern IntegerStatistics_Maximum
  , pattern IntegerStatistics_Sum
    -- * DoubleStatistics
  , pattern DoubleStatistics_Minimum
  , pattern DoubleStatistics_Maximum
  , pattern DoubleStatistics_Sum
    -- * StringStatistics
  , pattern StringStatistics_Minimum
  , pattern StringStatistics_Maximum
  , pattern StringStatistics_Sum
  , pattern StringStatistics_LowerBound
  , pattern StringStatistics_UpperBound
    -- * BinaryStatistics
  , pattern BinaryStatistics_Sum
    -- * BucketStatistics
  , pattern BucketStatistics_Count
    -- * DateStatistics
  , pattern DateStatistics_Minimum
  , pattern DateStatistics_Maximum
    -- * TimestampStatistics
  , pattern TimestampStatistics_Minimum
  , pattern TimestampStatistics_Maximum
  , pattern TimestampStatistics_MinimumUtc
  , pattern TimestampStatistics_MaximumUtc
    -- * DecimalStatistics
  , pattern DecimalStatistics_Minimum
  , pattern DecimalStatistics_Maximum
  , pattern DecimalStatistics_Sum
    -- * StripeFooter
  , pattern StripeFooter_Streams
  , pattern StripeFooter_Columns
    -- * ColumnEncoding
  , pattern ColumnEncoding_Kind
  , pattern ColumnEncoding_DictionarySize
  , pattern ColumnEncoding_BloomEncoding
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
  , pattern BloomFilter_Utf8Bitset
  , pattern BloomFilterIndex_Entry
  , encodeRepeatedFixed64Field
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
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.Vector.Unboxed as VU
import Data.Word (Word32, Word64)

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

-- | Encode an /unpacked/ repeated @fixed64@ field — one tag byte per
-- 8-byte little-endian element. This is what proto2 writers (including
-- the upstream ORC Java writer for the legacy @BLOOM_FILTER = 7@
-- stream) emit by default for @repeated fixed64@ without an explicit
-- @[packed=true]@ option.
encodeRepeatedFixed64Field :: (Int, WireType) -> VU.Vector Word64 -> B.Builder
encodeRepeatedFixed64Field (fieldNum, wt) xs
  | wt == wtFixed64 =
      VU.foldl' (\b w -> b <> protoTagByte fieldNum wt <> B.word64LE w)
                mempty xs
  | otherwise = error $ "ORC.Proto.Schema.encodeRepeatedFixed64Field: "
                     ++ "expected wtFixed64, got " ++ show wt

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
-- Decoder DSL
-- ============================================================

-- | Per-field reader action. The shape makes each @step@ equation a
-- direct statement of the field's wire shape ('ReadVarint' for
-- @uint64@, 'ReadBytes' for raw length-delimited, 'ReadNested' for
-- an embedded message) without the caller having to repeat the
-- protobuf plumbing.
--
-- @SkipUnknown@ is the fall-through for field numbers we don't
-- decode — the outer loop consumes the payload with 'skipField'.
--
-- Originally lived in "ORC.Footer"; lifted up here so
-- "ORC.Encryption" (and any other protobuf decoder in the ORC
-- package) can reuse it without pulling in the footer module.
data FieldAction a
  = ReadVarint  !(Word64     -> a)
  | ReadVarintE !(Word64     -> Either String a)
  | ReadFixed64 !(Word64     -> a)
    -- ^ Wire type 1 — eight little-endian bytes interpreted as
    -- a 'Word64'. Use 'GHC.Float.castWord64ToDouble' to recover
    -- a 'Double' (the only spec-defined consumer in ORC).
  | ReadFixed32 !(Word32     -> a)
    -- ^ Wire type 5 — four little-endian bytes.
  | ReadBytes   !(ByteString -> a)
  | ReadBytesE  !(ByteString -> Either String a)
  | forall b. ReadNested !(ByteString -> Either String b) !(b -> a)
  | SkipUnknown

-- | Generic protobuf message decoder: read field tags one by one and
-- interpret each via @step@. Unknown fields are skipped according to
-- their wire type.
decodeMsg
  :: ByteString
  -> a
  -> (a -> (Int, WireType) -> FieldAction a)
  -> Either String a
decodeMsg bs z step = go 0 z
  where
    !len = BS.length bs
    go !off !acc
      | off >= len = Right acc
      | otherwise = do
          (tag, off1) <- getVarint bs off len
          let !fn = fromIntegral (tag `shiftR` 3) :: Int
              !wt = tag .&. 7
          case step acc (fn, wt) of
            ReadVarint f -> do
              (v, off2) <- getVarint bs off1 len
              go off2 (f v)
            ReadVarintE f -> do
              (v, off2) <- getVarint bs off1 len
              acc' <- f v
              go off2 acc'
            ReadBytes f -> do
              (payload, off2) <- getLenDelim bs off1 len
              go off2 (f payload)
            ReadBytesE f -> do
              (payload, off2) <- getLenDelim bs off1 len
              acc' <- f payload
              go off2 acc'
            ReadNested decode f -> do
              (payload, off2) <- getLenDelim bs off1 len
              inner <- decode payload
              go off2 (f inner)
            ReadFixed64 f -> do
              if off1 + 8 > len
                then Left "ORC.Proto.Schema: truncated fixed64"
                else do
                  let !w = readLE64 bs off1
                  go (off1 + 8) (f w)
            ReadFixed32 f -> do
              if off1 + 4 > len
                then Left "ORC.Proto.Schema: truncated fixed32"
                else do
                  let !w = readLE32 bs off1
                  go (off1 + 4) (f w)
            SkipUnknown -> do
              off2 <- skipField wt bs off1 len
              go off2 acc

readLE32 :: ByteString -> Int -> Word32
readLE32 bs !off =
  let r i = fromIntegral (BSU.unsafeIndex bs (off + i)) :: Word32
  in r 0 .|. (r 1 `shiftL` 8) .|. (r 2 `shiftL` 16) .|. (r 3 `shiftL` 24)

readLE64 :: ByteString -> Int -> Word64
readLE64 bs !off =
  let lo = fromIntegral (readLE32 bs off)        :: Word64
      hi = fromIntegral (readLE32 bs (off + 4))  :: Word64
  in lo .|. (hi `shiftL` 32)

-- ============================================================
-- Wire primitives
-- ============================================================

getVarint :: ByteString -> Int -> Int -> Either String (Word64, Int)
getVarint bs !off !len = go off 0 0
  where
    go !pos !val !shift
      | pos >= len = Left "ORC.Proto.Schema: unexpected end of varint"
      | shift >= 64 = Left "ORC.Proto.Schema: varint too long"
      | otherwise =
          let !b = fromIntegral (BSU.unsafeIndex bs pos) :: Word64
              !val' = val .|. ((b .&. 0x7F) `shiftL` shift)
          in if b .&. 0x80 == 0
               then Right (val', pos + 1)
               else go (pos + 1) val' (shift + 7)

getLenDelim :: ByteString -> Int -> Int -> Either String (ByteString, Int)
getLenDelim bs !off !len = do
  (dlen, off') <- getVarint bs off len
  let !dataLen = fromIntegral dlen :: Int
  if off' + dataLen > len
    then Left "ORC.Proto.Schema: length-delimited data exceeds buffer"
    else Right (BSU.unsafeTake dataLen (BSU.unsafeDrop off' bs), off' + dataLen)

skipField :: WireType -> ByteString -> Int -> Int -> Either String Int
skipField wireType bs !off !len = case wireType of
  0 -> do (_, off') <- getVarint bs off len; Right off'
  1 -> if off + 8 <= len then Right (off + 8) else Left "ORC.Proto.Schema: truncated fixed64"
  2 -> do (_, off') <- getLenDelim bs off len; Right off'
  5 -> if off + 4 <= len then Right (off + 4) else Left "ORC.Proto.Schema: truncated fixed32"
  _ -> Left $ "ORC.Proto.Schema: unknown wire type " ++ show wireType

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

-- | @Footer.encryption@ (ORC 1.6+). Carries the serialized
-- @Encryption@ protobuf message as a length-delimited payload.
-- Field number 10 per the ORC protobuf spec.
pattern Footer_Encryption :: (Int, WireType)
pattern Footer_Encryption    = (10, 2)

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

pattern ColumnStatistics_IntStatistics :: (Int, WireType)
pattern ColumnStatistics_IntStatistics = (4, 2)

pattern ColumnStatistics_DoubleStatistics :: (Int, WireType)
pattern ColumnStatistics_DoubleStatistics = (5, 2)

pattern ColumnStatistics_StringStatistics :: (Int, WireType)
pattern ColumnStatistics_StringStatistics = (6, 2)

pattern ColumnStatistics_BucketStatistics :: (Int, WireType)
pattern ColumnStatistics_BucketStatistics = (7, 2)

pattern ColumnStatistics_DecimalStatistics :: (Int, WireType)
pattern ColumnStatistics_DecimalStatistics = (8, 2)

pattern ColumnStatistics_DateStatistics :: (Int, WireType)
pattern ColumnStatistics_DateStatistics = (9, 2)

pattern ColumnStatistics_BinaryStatistics :: (Int, WireType)
pattern ColumnStatistics_BinaryStatistics = (10, 2)

pattern ColumnStatistics_TimestampStatistics :: (Int, WireType)
pattern ColumnStatistics_TimestampStatistics = (11, 2)

-- ============================================================
-- IntegerStatistics
-- ============================================================

pattern IntegerStatistics_Minimum :: (Int, WireType)
pattern IntegerStatistics_Minimum = (1, 0)

pattern IntegerStatistics_Maximum :: (Int, WireType)
pattern IntegerStatistics_Maximum = (2, 0)

pattern IntegerStatistics_Sum :: (Int, WireType)
pattern IntegerStatistics_Sum = (3, 0)

-- ============================================================
-- DoubleStatistics
-- ============================================================

pattern DoubleStatistics_Minimum :: (Int, WireType)
pattern DoubleStatistics_Minimum = (1, 1)

pattern DoubleStatistics_Maximum :: (Int, WireType)
pattern DoubleStatistics_Maximum = (2, 1)

pattern DoubleStatistics_Sum :: (Int, WireType)
pattern DoubleStatistics_Sum = (3, 1)

-- ============================================================
-- StringStatistics
-- ============================================================

pattern StringStatistics_Minimum :: (Int, WireType)
pattern StringStatistics_Minimum = (1, 2)

pattern StringStatistics_Maximum :: (Int, WireType)
pattern StringStatistics_Maximum = (2, 2)

pattern StringStatistics_Sum :: (Int, WireType)
pattern StringStatistics_Sum = (3, 0)

pattern StringStatistics_LowerBound :: (Int, WireType)
pattern StringStatistics_LowerBound = (4, 2)

pattern StringStatistics_UpperBound :: (Int, WireType)
pattern StringStatistics_UpperBound = (5, 2)

-- ============================================================
-- BinaryStatistics
-- ============================================================

pattern BinaryStatistics_Sum :: (Int, WireType)
pattern BinaryStatistics_Sum = (1, 0)

-- ============================================================
-- BucketStatistics
-- ============================================================

pattern BucketStatistics_Count :: (Int, WireType)
pattern BucketStatistics_Count = (1, 2)

-- ============================================================
-- DateStatistics
-- ============================================================

pattern DateStatistics_Minimum :: (Int, WireType)
pattern DateStatistics_Minimum = (1, 0)

pattern DateStatistics_Maximum :: (Int, WireType)
pattern DateStatistics_Maximum = (2, 0)

-- ============================================================
-- TimestampStatistics
-- ============================================================

pattern TimestampStatistics_Minimum :: (Int, WireType)
pattern TimestampStatistics_Minimum = (1, 0)

pattern TimestampStatistics_Maximum :: (Int, WireType)
pattern TimestampStatistics_Maximum = (2, 0)

pattern TimestampStatistics_MinimumUtc :: (Int, WireType)
pattern TimestampStatistics_MinimumUtc = (3, 0)

pattern TimestampStatistics_MaximumUtc :: (Int, WireType)
pattern TimestampStatistics_MaximumUtc = (4, 0)

-- ============================================================
-- DecimalStatistics
-- ============================================================

pattern DecimalStatistics_Minimum :: (Int, WireType)
pattern DecimalStatistics_Minimum = (1, 2)

pattern DecimalStatistics_Maximum :: (Int, WireType)
pattern DecimalStatistics_Maximum = (2, 2)

pattern DecimalStatistics_Sum :: (Int, WireType)
pattern DecimalStatistics_Sum = (3, 2)

-- ============================================================
-- StripeFooter
-- ============================================================

pattern StripeFooter_Streams :: (Int, WireType)
pattern StripeFooter_Streams = (1, 2)

pattern StripeFooter_Columns :: (Int, WireType)
pattern StripeFooter_Columns = (2, 2)

-- ============================================================
-- ColumnEncoding
-- ============================================================
--
-- Per orc.proto:
--
-- @
-- message ColumnEncoding {
--   enum Kind { DIRECT = 0; DICTIONARY = 1;
--               DIRECT_V2 = 2; DICTIONARY_V2 = 3; }
--   required Kind kind = 1;
--   optional uint32 dictionarySize = 2;
--   optional bytes bloomEncoding = 3;
-- }
-- @

pattern ColumnEncoding_Kind :: (Int, WireType)
pattern ColumnEncoding_Kind = (1, 0)

pattern ColumnEncoding_DictionarySize :: (Int, WireType)
pattern ColumnEncoding_DictionarySize = (2, 0)

pattern ColumnEncoding_BloomEncoding :: (Int, WireType)
pattern ColumnEncoding_BloomEncoding = (3, 0)

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
-- Legacy @repeated fixed64 bitset = 2@. ORC's Java writer emits the
-- legacy @BLOOM_FILTER = 7@ stream as one tag-per-word (i.e.
-- /unpacked/ repeated fixed64, wire type 1), even though packed would
-- also be a valid encoding; the reader path tolerates either, but the
-- wire-type tag below is the unpacked form so 'encodeRepeatedFixed64Field'
-- can reuse it.
pattern BloomFilter_Bitset           = (2, 1)

pattern BloomFilter_Utf8Bitset :: (Int, WireType)
-- @optional bytes utf8bitset = 3@. ORC's Java writer packs the bit-set
-- as a little-endian byte run in this field whenever it emits a
-- @BLOOM_FILTER_UTF8 = 8@ stream (post-ORC-101).
pattern BloomFilter_Utf8Bitset       = (3, 2)

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
