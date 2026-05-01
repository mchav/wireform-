{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
-- | Read/write Apache ORC file footer.
--
-- ORC file layout ends with:
--   [protobuf Footer] [protobuf PostScript] [1-byte postscript length]
--
-- The PostScript contains: footerLength, compression, compressionBlockSize,
-- version, and the magic string "ORC".
--
-- All @(fieldNum, wireType)@ tags are named once in "ORC.Proto.Schema";
-- this module only references them by pattern synonym so writer + reader
-- can't drift.
module ORC.Footer
  ( readORCFooter
  , writeORCFooter
  , readORCCompression
  , orcMagic
  ) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word32, Word64)

import ORC.Proto.Schema
import ORC.Types

orcMagic :: ByteString
orcMagic = "ORC"

readORCFooter :: ByteString -> Either String ORCFooter
readORCFooter bs
  | BS.length bs < 4 = Left "ORC.Footer: input too short"
  | otherwise = do
      let !totalLen = BS.length bs
          !psLen = fromIntegral (BSU.unsafeIndex bs (totalLen - 1)) :: Int
      if psLen <= 0 || psLen >= totalLen - 1
        then Left "ORC.Footer: invalid postscript length"
        else do
          let !psStart = totalLen - 1 - psLen
              !psBytes = BSU.unsafeTake psLen (BSU.unsafeDrop psStart bs)
          ps <- decodePostScript psBytes
          let !magic = psMagic ps
          if magic /= orcMagic
            then Left $ "ORC.Footer: invalid magic (expected ORC, got " ++ show magic ++ ")"
            else do
              let !footerLen = fromIntegral (psFooterLength ps) :: Int
                  !footerStart = psStart - footerLen
              if footerLen <= 0 || footerStart < 0
                then Left "ORC.Footer: invalid footer length"
                else do
                  let !footerBytes = BSU.unsafeTake footerLen (BSU.unsafeDrop footerStart bs)
                  decodeFooter footerBytes

-- | Extract the compression kind from the PostScript.
readORCCompression :: ByteString -> Either String CompressionKind
readORCCompression bs
  | BS.length bs < 4 = Left "ORC.Footer: input too short for compression read"
  | otherwise = do
      let !totalLen = BS.length bs
          !psLen = fromIntegral (BSU.unsafeIndex bs (totalLen - 1)) :: Int
      if psLen <= 0 || psLen >= totalLen - 1
        then Left "ORC.Footer: invalid postscript length"
        else do
          let !psStart = totalLen - 1 - psLen
              !psBytes = BSU.unsafeTake psLen (BSU.unsafeDrop psStart bs)
          ps <- decodePostScript psBytes
          case compressionFromInt (psCompression ps) of
            Just ck -> Right ck
            Nothing -> Left $ "ORC.Footer: unknown compression kind " ++ show (psCompression ps)

writeORCFooter :: ORCFooter -> ByteString
writeORCFooter footer =
  let !footerBytes = encodeFooter footer
      !footerLen = BS.length footerBytes
      !psBytes = encodePostScript PostScript
        { psFooterLength = fromIntegral footerLen
        , psCompression = 0
        , psCompressionBlockSize = 262144
        , psVersion = V.fromList [0, 12]
        , psMagic = orcMagic
        }
      !psLen = BS.length psBytes
  in BL.toStrict $ B.toLazyByteString $
       B.byteString footerBytes
       <> B.byteString psBytes
       <> B.word8 (fromIntegral psLen)

-- Internal PostScript type
data PostScript = PostScript
  { psFooterLength         :: !Word64
  , psCompression          :: !Word64
  , psCompressionBlockSize :: !Word64
  , psVersion              :: !(Vector Word32)
  , psMagic                :: !ByteString
  }

------------------------------------------------------------------------
-- Protobuf encoding (manual, matching ORC .proto field numbers)
------------------------------------------------------------------------

encodeFooter :: ORCFooter -> ByteString
encodeFooter f = BL.toStrict $ B.toLazyByteString $ mconcat
  [ encodeVarintField Footer_HeaderLength  (orcHeaderLength f)
  , encodeVarintField Footer_ContentLength (orcContentLength f)
  , V.foldl' (\acc s -> acc <> encodeLengthDelim Footer_Stripes
                                 (encodeStripeInfo s))
      mempty (orcStripes f)
  , V.foldl' (\acc t -> acc <> encodeLengthDelim Footer_Types
                                 (encodeORCType t))
      mempty (orcTypes f)
  , V.foldl' (\acc (k, v) -> acc <> encodeLengthDelim Footer_Metadata
                                    (encodeMetadataEntry k v))
      mempty (orcMetadata f)
  , encodeVarintField Footer_NumberOfRows (orcNumberOfRows f)
  , V.foldl' (\acc s -> acc <> encodeLengthDelim Footer_Statistics
                                 (encodeColStats s))
      mempty (orcStatistics f)
  , case orcEncryption f of
      Nothing -> mempty
      Just (FooterEncryption encBs) ->
        encodeLengthDelim Footer_Encryption (B.byteString encBs)
  ]

encodeStripeInfo :: StripeInformation -> B.Builder
encodeStripeInfo si = mconcat
  [ encodeVarintField StripeInformation_Offset        (siOffset si)
  , encodeVarintField StripeInformation_IndexLength   (siIndexLength si)
  , encodeVarintField StripeInformation_DataLength    (siDataLength si)
  , encodeVarintField StripeInformation_FooterLength  (siFooterLength si)
  , encodeVarintField StripeInformation_NumberOfRows  (siNumberOfRows si)
  ]

encodeORCType :: ORCType -> B.Builder
encodeORCType ot = mconcat
  [ encodeVarintField ORCType_Kind
      (fromIntegral (typeKindToInt (otKind ot)) :: Word64)
  , V.foldl' (\acc st -> acc <> encodeVarintField ORCType_Subtypes
                                   (fromIntegral st :: Word64))
      mempty (otSubtypes ot)
  , V.foldl' (\acc fn -> acc <> encodeLengthDelim ORCType_FieldNames
                                   (B.byteString (TE.encodeUtf8 fn)))
      mempty (otFieldNames ot)
  ]

encodeMetadataEntry :: T.Text -> ByteString -> B.Builder
encodeMetadataEntry k v = mconcat
  [ encodeLengthDelim MetadataEntry_Name
      (B.byteString (TE.encodeUtf8 k))
  , encodeLengthDelim MetadataEntry_Value (B.byteString v)
  ]

encodeColStats :: ColumnStatistics -> B.Builder
encodeColStats cs = mconcat
  [ maybe mempty (encodeVarintField ColumnStatistics_NumberOfValues)
      (csNumberOfValues cs)
  , maybe mempty
      (\b -> encodeVarintField ColumnStatistics_HasNull
               (if b then 1 else 0 :: Word64))
      (csHasNull cs)
  , maybe mempty (encodeVarintField ColumnStatistics_BytesOnDisk)
      (csBytesOnDisk cs)
  ]

encodePostScript :: PostScript -> ByteString
encodePostScript ps = BL.toStrict $ B.toLazyByteString $ mconcat
  [ encodeVarintField PostScript_FooterLength         (psFooterLength ps)
  , encodeVarintField PostScript_Compression          (psCompression ps)
  , encodeVarintField PostScript_CompressionBlockSize (psCompressionBlockSize ps)
  , V.foldl' (\acc v -> acc <> encodeVarintField PostScript_Version
                                   (fromIntegral v :: Word64))
      mempty (psVersion ps)
  , encodeLengthDelim PostScript_Magic (B.byteString (psMagic ps))
  ]

------------------------------------------------------------------------
-- Protobuf decoding (manual varint + length-delimited)
------------------------------------------------------------------------

decodePostScript :: ByteString -> Either String PostScript
decodePostScript bs = decodeMsg bs (PostScript 0 0 0 V.empty BS.empty) step
  where
    step ps = \case
      PostScript_FooterLength         -> ReadVarint  $ \v -> ps { psFooterLength = v }
      PostScript_Compression          -> ReadVarint  $ \v -> ps { psCompression = v }
      PostScript_CompressionBlockSize -> ReadVarint  $ \v -> ps { psCompressionBlockSize = v }
      PostScript_Version              -> ReadVarint  $ \v -> ps { psVersion = V.snoc (psVersion ps) (fromIntegral v) }
      PostScript_Magic                -> ReadBytes   $ \v -> ps { psMagic = v }
      _                               -> SkipUnknown

decodeFooter :: ByteString -> Either String ORCFooter
decodeFooter bs = decodeMsg bs emptyFooter step
  where
    emptyFooter =
      ORCFooter 0 0 V.empty V.empty V.empty 0 V.empty Nothing
    step f = \case
      Footer_HeaderLength  -> ReadVarint $ \v    -> f { orcHeaderLength = v }
      Footer_ContentLength -> ReadVarint $ \v    -> f { orcContentLength = v }
      Footer_Stripes       -> ReadNested decodeStripeInfo $ \si ->
        f { orcStripes = V.snoc (orcStripes f) si }
      Footer_Types         -> ReadNested decodeORCType $ \t ->
        f { orcTypes = V.snoc (orcTypes f) t }
      Footer_Metadata      -> ReadNested decodeMetadataEntry $ \e ->
        f { orcMetadata = V.snoc (orcMetadata f) e }
      Footer_NumberOfRows  -> ReadVarint $ \v    -> f { orcNumberOfRows = v }
      Footer_Statistics    -> ReadNested decodeColStats $ \cs ->
        f { orcStatistics = V.snoc (orcStatistics f) cs }
      Footer_Encryption    -> ReadBytes $ \enc ->
        f { orcEncryption = Just (FooterEncryption enc) }
      _                    -> SkipUnknown

decodeStripeInfo :: ByteString -> Either String StripeInformation
decodeStripeInfo bs = decodeMsg bs (StripeInformation 0 0 0 0 0) step
  where
    step si = \case
      StripeInformation_Offset       -> ReadVarint $ \v -> si { siOffset = v }
      StripeInformation_IndexLength  -> ReadVarint $ \v -> si { siIndexLength = v }
      StripeInformation_DataLength   -> ReadVarint $ \v -> si { siDataLength = v }
      StripeInformation_FooterLength -> ReadVarint $ \v -> si { siFooterLength = v }
      StripeInformation_NumberOfRows -> ReadVarint $ \v -> si { siNumberOfRows = v }
      _                              -> SkipUnknown

decodeORCType :: ByteString -> Either String ORCType
decodeORCType bs = decodeMsg bs (ORCType TKBoolean V.empty V.empty) step
  where
    step ot = \case
      ORCType_Kind       -> ReadVarintE $ \v -> case intToTypeKind (fromIntegral v) of
        Just tk -> Right ot { otKind = tk }
        Nothing -> Left $ "ORC.Footer: invalid TypeKind " ++ show v
      ORCType_Subtypes   -> ReadVarint $ \v ->
        ot { otSubtypes = V.snoc (otSubtypes ot) (fromIntegral v) }
      ORCType_FieldNames -> ReadBytesE $ \v -> case TE.decodeUtf8' v of
        Right t -> Right ot { otFieldNames = V.snoc (otFieldNames ot) t }
        Left _  -> Left "ORC.Footer: invalid UTF-8 in field name"
      _                  -> SkipUnknown

decodeMetadataEntry :: ByteString -> Either String (T.Text, ByteString)
decodeMetadataEntry bs = decodeMsg bs (T.empty, BS.empty) step
  where
    step (k, v) = \case
      MetadataEntry_Name  -> ReadBytesE $ \bs' -> case TE.decodeUtf8' bs' of
        Right t -> Right (t, v)
        Left _  -> Left "ORC.Footer: invalid UTF-8 in metadata key"
      MetadataEntry_Value -> ReadBytes  $ \bs' -> (k, bs')
      _                   -> SkipUnknown

decodeColStats :: ByteString -> Either String ColumnStatistics
decodeColStats bs = decodeMsg bs (ColumnStatistics Nothing Nothing Nothing) step
  where
    step cs = \case
      ColumnStatistics_NumberOfValues -> ReadVarint $ \v -> cs { csNumberOfValues = Just v }
      ColumnStatistics_HasNull        -> ReadVarint $ \v -> cs { csHasNull = Just (v /= 0) }
      ColumnStatistics_BytesOnDisk    -> ReadVarint $ \v -> cs { csBytesOnDisk = Just v }
      _                               -> SkipUnknown

-- | Per-field reader action. The shape makes each @step@ equation a
-- direct statement of the field's wire shape ('ReadVarint' for uint64,
-- 'ReadBytes' for raw length-delimited, 'ReadNested' for an embedded
-- message) without the caller having to repeat the protobuf plumbing.
--
-- @SkipUnknown@ is the fall-through for field numbers we don't decode
-- — the outer loop consumes the payload with 'skipField'.
data FieldAction a
  = ReadVarint  !(Word64     -> a)
  | ReadVarintE !(Word64     -> Either String a)
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
  -> (a -> (Int, Word64) -> FieldAction a)
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
            SkipUnknown -> do
              off2 <- skipField wt bs off1 len
              go off2 acc

------------------------------------------------------------------------
-- Low-level protobuf primitives
------------------------------------------------------------------------

getVarint :: ByteString -> Int -> Int -> Either String (Word64, Int)
getVarint bs !off !len = go off 0 0
  where
    go !pos !val !shift
      | pos >= len = Left "ORC.Footer: unexpected end of varint"
      | shift >= 64 = Left "ORC.Footer: varint too long"
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
    then Left "ORC.Footer: length-delimited data exceeds buffer"
    else Right (BSU.unsafeTake dataLen (BSU.unsafeDrop off' bs), off' + dataLen)

skipField :: Word64 -> ByteString -> Int -> Int -> Either String Int
skipField wireType bs !off !len = case wireType of
  0 -> do (_, off') <- getVarint bs off len; Right off'
  1 -> if off + 8 <= len then Right (off + 8) else Left "ORC.Footer: truncated fixed64"
  2 -> do (_, off') <- getLenDelim bs off len; Right off'
  5 -> if off + 4 <= len then Right (off + 4) else Left "ORC.Footer: truncated fixed32"
  _ -> Left $ "ORC.Footer: unknown wire type " ++ show wireType

-- Protobuf encoding helpers: 'encodeVarintField' / 'encodeLengthDelim'
-- come from "ORC.Proto.Schema"; those versions take the pattern-synonym
-- @(fieldNum, wireType)@ pair directly.
