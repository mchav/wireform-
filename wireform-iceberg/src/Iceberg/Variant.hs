{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Apache Spark / Iceberg V3 / Parquet "Variant" binary encoding.
--
-- The Variant is a self-describing binary form for semi-structured
-- data (think: BSON, but with a deduplicated string dictionary and
-- offset-indexed object fields so deep field access is O(log n)
-- without scanning). Iceberg stores it as a Parquet group of
-- @{metadata: BINARY, value: BINARY}@; this module produces and
-- consumes those two byte-strings.
--
-- Spec:
-- <https://parquet.apache.org/docs/file-format/types/variantencoding/>.
--
-- Scope of /this/ module: the JSON-equivalent type set
-- (null, bool, integers, double, float, string, binary, array,
-- object) and a 'Variant' ↔ 'Aeson.Value' bridge. The temporal /
-- decimal / UUID primitive types in the spec are expected to land in a
-- follow-up that wires them into Iceberg's column-statistics flow;
-- they're called out at the type level via 'VUnsupportedPrimitive'
-- so callers can detect what they read but the encoder doesn't emit
-- them yet.
module Iceberg.Variant
  ( -- * Variant value
    Variant (..)
    -- * Binary encoding
  , encodeVariant
  , decodeVariant
    -- * JSON bridge
  , variantFromJSON
  , variantToJSON
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int8, Int16, Int32, Int64)
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Scientific (Scientific, floatingOrInteger)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Data.Vector (Vector)
import Data.Word (Word8, Word32, Word64)
import GHC.Float (castDoubleToWord64, castFloatToWord32, castWord32ToFloat, castWord64ToDouble)

-- ============================================================
-- Variant value
-- ============================================================

-- | Self-describing variant value. Mirrors the JSON-equivalent
-- subset of the Spark Variant spec. Decimal / temporal / UUID
-- primitives surface through 'VUnsupportedPrimitive' so a reader can
-- still report the type even when the writer doesn't emit it yet.
data Variant
  = VNull
  | VBool   !Bool
  | VInt8   !Int8
  | VInt16  !Int16
  | VInt32  !Int32
  | VInt64  !Int64
  | VFloat  !Float
  | VDouble !Double
  | VString !Text
  | VBinary !ByteString
  | VArray  !(Vector Variant)
  | VObject !(Map Text Variant)
  | VUnsupportedPrimitive !Word8 !ByteString
    -- ^ Caught when the decoder sees a primitive type ID this version
    --   doesn't model (decimals, dates, timestamps, time, uuid). The
    --   raw payload bytes are preserved so callers can hand them off
    --   downstream.
  deriving (Show, Eq)

-- ============================================================
-- Binary encoding
-- ============================================================

-- | Encode a 'Variant' as the @(metadata, value)@ byte-string pair
-- the spec requires. The metadata holds the deduplicated, sorted
-- string dictionary used for object field names; the value holds the
-- recursive value tree.
encodeVariant :: Variant -> (ByteString, ByteString)
encodeVariant v =
  let !dict = collectFieldNames v
      !meta = encodeMetadata dict
      !val  = encodeValue dict v
   in (meta, val)

-- | Walk the variant collecting the set of object-field names. We
-- emit them sorted so we can set the @sorted_strings@ flag and rely
-- on it for fast lookups in the decoder.
collectFieldNames :: Variant -> Vector Text
collectFieldNames root =
  let !sorted = L.nub (L.sort (go root []))
   in V.fromList sorted
  where
    go (VObject m) !acc = Map.foldrWithKey
                            (\k inner a -> k : go inner a) acc m
    go (VArray xs) !acc = V.foldr go acc xs
    go _ !acc = acc

-- | Encode the metadata dictionary.
--
-- Layout: @<header(1)> <dict_size> <offset_0> ... <offset_n> <bytes>@
-- where size and offsets are unsigned little-endian @offset_size@
-- bytes each. We pick the smallest @offset_size@ that fits both the
-- dictionary count and the total bytes length.
encodeMetadata :: Vector Text -> ByteString
encodeMetadata dict =
  let !utf8s = V.map TE.encodeUtf8 dict
      !n = V.length utf8s
      !bodyLen = V.foldl' (\acc b -> acc + BS.length b) 0 utf8s
      !maxVal = max n bodyLen
      !offsetSize
        | maxVal < 256        = 1
        | maxVal < 65536      = 2
        | maxVal < 16777216   = 3
        | otherwise           = 4
      !version = 1
      !sortedFlag = 1
      !header = fromIntegral version
                .|. (sortedFlag `shiftL` 4)
                .|. ((offsetSize - 1) `shiftL` 6)
      !offsets = V.scanl' (\acc b -> acc + BS.length b) 0 utf8s
      !payload = B.word8 (fromIntegral header)
              <> writeUleN offsetSize n
              <> V.foldl' (\acc o -> acc <> writeUleN offsetSize o) mempty offsets
              <> V.foldl' (\acc b -> acc <> B.byteString b) mempty utf8s
   in BL.toStrict (B.toLazyByteString payload)

-- | Encode one variant value. Recursive on arrays / objects.
encodeValue :: Vector Text -> Variant -> ByteString
encodeValue dict = BL.toStrict . B.toLazyByteString . goB
  where
    goB :: Variant -> B.Builder
    goB = \case
      VNull       -> primHeader 0
      VBool True  -> primHeader 1
      VBool False -> primHeader 2
      VInt8 v     -> primHeader 3 <> B.int8 v
      VInt16 v    -> primHeader 4 <> B.int16LE v
      VInt32 v    -> primHeader 5 <> B.int32LE v
      VInt64 v    -> primHeader 6 <> B.int64LE v
      VDouble v   -> primHeader 7 <> B.word64LE (castDoubleToWord64 v)
      VFloat v    -> primHeader 14 <> B.word32LE (castFloatToWord32 v)
      VString t   ->
        let !bs = TE.encodeUtf8 t
            !n  = BS.length bs
         in if n < 64
              then -- short_string: basic=1, value_header = length
                   B.word8 (fromIntegral ((n `shiftL` 2) .|. 1))
                   <> B.byteString bs
              else primHeader 16
                   <> B.word32LE (fromIntegral n)
                   <> B.byteString bs
      VBinary bs  ->
        primHeader 15
        <> B.word32LE (fromIntegral (BS.length bs))
        <> B.byteString bs
      VArray xs   -> encodeArray dict xs
      VObject m   -> encodeObject dict m
      VUnsupportedPrimitive tag payload ->
        -- emit verbatim so a roundtrip preserves what we don't model.
        B.word8 (fromIntegral ((tag `shiftL` 2) .|. 0))
        <> B.byteString payload

primHeader :: Int -> B.Builder
primHeader p = B.word8 (fromIntegral ((p `shiftL` 2) .|. 0))

-- | Encode an array. We always use 1-byte field offsets if the
-- payload fits in 255 bytes, else 4-byte. Likewise @is_large@ for the
-- element count.
encodeArray :: Vector Text -> Vector Variant -> B.Builder
encodeArray dict xs =
  let !n = V.length xs
      !encodedKids = V.map (encodeValue dict) xs
      !offsets = V.scanl' (\acc bs -> acc + BS.length bs) 0 encodedKids
      !lastOffset = V.unsafeIndex offsets n
      !offSize = pickWidth lastOffset
      !isLarge = if n >= 256 then 1 else 0 :: Int
      !valueHeader = (isLarge `shiftL` 2) .|. (offSize - 1)
      !valueMetadata = (valueHeader `shiftL` 2) .|. 3  -- basic_type=3
      !numElementsBytes = if isLarge == 1
                           then B.word32LE (fromIntegral n)
                           else B.word8 (fromIntegral n)
   in B.word8 (fromIntegral valueMetadata)
      <> numElementsBytes
      <> V.foldl' (\acc o -> acc <> writeUleN offSize o) mempty offsets
      <> V.foldl' (\acc bs -> acc <> B.byteString bs) mempty encodedKids

-- | Encode an object. Field ids are dictionary indices and must be
-- emitted in lexicographic order of their field names (which we get
-- for free from 'Map.toAscList').
encodeObject :: Vector Text -> Map Text Variant -> B.Builder
encodeObject dict m =
  let !pairs = Map.toAscList m -- already lex-sorted by key
      !n = length pairs
      -- Look up each key's dictionary index.
      !fieldIds = map (\(k, _) -> dictIndex dict k) pairs
      !maxFieldId = maximum (0 : fieldIds)
      !idSize = pickWidth maxFieldId
      !encodedKids = map (\(_, val) -> encodeValue dict val) pairs
      !offsets = scanlInt (+) 0 (map BS.length encodedKids)
      !lastOffset = if null offsets then 0 else last offsets
      !offSize = pickWidth lastOffset
      !isLarge = if n >= 256 then 1 else 0 :: Int
      !valueHeader = (isLarge `shiftL` 4)
                     .|. ((idSize - 1) `shiftL` 2)
                     .|. (offSize - 1)
      !valueMetadata = (valueHeader `shiftL` 2) .|. 2  -- basic_type=2
      !numElementsBytes = if isLarge == 1
                           then B.word32LE (fromIntegral n)
                           else B.word8 (fromIntegral n)
   in B.word8 (fromIntegral valueMetadata)
      <> numElementsBytes
      <> mconcat (map (writeUleN idSize) fieldIds)
      <> mconcat (map (writeUleN offSize) offsets)
      <> mconcat (map B.byteString encodedKids)

-- ============================================================
-- Binary decoding
-- ============================================================

decodeVariant :: ByteString -> ByteString -> Either String Variant
decodeVariant meta val = do
  dict <- decodeMetadata meta
  (v, _) <- decodeValueAt dict val 0
  Right v

decodeMetadata :: ByteString -> Either String (Vector Text)
decodeMetadata bs
  | BS.null bs = Left "Iceberg.Variant: empty metadata"
  | otherwise = do
      let !hdr = BS.index bs 0
          !version = fromIntegral hdr .&. 0x0F :: Int
          !offSize = ((fromIntegral hdr `shiftR` 6) .&. 0x03) + 1 :: Int
      if version /= 1
        then Left ("Iceberg.Variant: unsupported metadata version " ++ show version)
        else do
          (n, off1) <- readUle offSize bs 1
          let !numOffsets = n + 1
              !offsetsStart = off1
              !bytesStart = offsetsStart + numOffsets * offSize
          if bytesStart > BS.length bs
            then Left "Iceberg.Variant: truncated metadata"
            else do
              offs <- traverse (\i -> fst <$> readUle offSize bs (offsetsStart + i * offSize))
                               [0 .. numOffsets - 1]
              let !bytesRegion = BS.drop bytesStart bs
              entries <- mapM
                (\i -> do
                    let !lo = offs !! i
                        !hi = offs !! (i + 1)
                    if hi < lo || bytesStart + hi > BS.length bs
                      then Left "Iceberg.Variant: malformed offsets"
                      else case TE.decodeUtf8' (BS.take (hi - lo) (BS.drop lo bytesRegion)) of
                             Right t -> Right t
                             Left  e -> Left ("Iceberg.Variant: bad UTF-8: " ++ show e))
                [0 .. n - 1]
              Right (V.fromList entries)

decodeValueAt :: Vector Text -> ByteString -> Int -> Either String (Variant, Int)
decodeValueAt dict bs off
  | off >= BS.length bs = Left "Iceberg.Variant: ran off end of value"
  | otherwise = do
      let !meta = fromIntegral (BS.index bs off) :: Int
          !basic = meta .&. 0x03
          !hdr   = meta `shiftR` 2
      case basic of
        0 -> decodePrimitive hdr bs (off + 1)
        1 ->
          let !len = hdr in
          if off + 1 + len > BS.length bs
            then Left "Iceberg.Variant: short string runs past end"
            else case TE.decodeUtf8' (BS.take len (BS.drop (off + 1) bs)) of
                   Right t -> Right (VString t, off + 1 + len)
                   Left  e -> Left ("Iceberg.Variant: short-string UTF-8: " ++ show e)
        2 -> decodeObjectAt dict hdr bs (off + 1)
        3 -> decodeArrayAt  dict hdr bs (off + 1)
        _ -> Left ("Iceberg.Variant: unknown basic_type " ++ show basic)

decodePrimitive :: Int -> ByteString -> Int -> Either String (Variant, Int)
decodePrimitive ph bs off = case ph of
  0  -> Right (VNull, off)
  1  -> Right (VBool True, off)
  2  -> Right (VBool False, off)
  3  -> readBytes 1 bs off >>= \(v, n) -> Right (VInt8  (fromIntegral (BS.head v) :: Int8), n)
  4  -> readLE 2 bs off >>= \(v, n) -> Right (VInt16 (fromIntegral v), n)
  5  -> readLE 4 bs off >>= \(v, n) -> Right (VInt32 (fromIntegral v), n)
  6  -> readLE 8 bs off >>= \(v, n) -> Right (VInt64 (fromIntegral v), n)
  7  -> readLE 8 bs off >>= \(v, n) ->
          Right (VDouble (castWord64ToDouble (fromIntegral v)), n)
  14 -> readLE 4 bs off >>= \(v, n) ->
          Right (VFloat (castWord32ToFloat (fromIntegral v)), n)
  15 -> readBlob bs off >>= \(payload, n) -> Right (VBinary payload, n)
  16 -> readBlob bs off >>= \(payload, n) ->
          case TE.decodeUtf8' payload of
            Right t -> Right (VString t, n)
            Left  e -> Left ("Iceberg.Variant: long-string UTF-8: " ++ show e)
  other ->
    -- Forward-compat: keep raw bytes for unknown primitives so the
    -- caller can pass them through. We don't know the length so we
    -- treat the whole rest of the buffer as the payload, which is
    -- enough to surface the type to user code without panicking.
    Right (VUnsupportedPrimitive (fromIntegral other) (BS.drop off bs)
          , BS.length bs)
  where
    readBytes n bs' o
      | o + n > BS.length bs' = Left "Iceberg.Variant: short buffer"
      | otherwise = Right (BS.take n (BS.drop o bs'), o + n)
    readBlob bs' o = do
      (sz, o1) <- readLE 4 bs' o
      readBytes (fromIntegral sz) bs' o1

decodeObjectAt
  :: Vector Text -> Int -> ByteString -> Int
  -> Either String (Variant, Int)
decodeObjectAt dict hdr bs off = do
  let !offSize = (hdr .&. 0x03) + 1
      !idSize  = ((hdr `shiftR` 2) .&. 0x03) + 1
      !isLarge = (hdr `shiftR` 4) .&. 0x01
      !numWidth = if isLarge == 1 then 4 else 1
  (n, off1) <- readUle numWidth bs off
  let !idsStart = off1
      !offsetsStart = idsStart + n * idSize
      !valuesStart  = offsetsStart + (n + 1) * offSize
  fieldIds <- traverse
                (\i -> fst <$> readUle idSize bs (idsStart + i * idSize))
                [0 .. n - 1]
  fieldOffsets <- traverse
                    (\i -> fst <$> readUle offSize bs (offsetsStart + i * offSize))
                    [0 .. n]
  pairs <- traverse
             (\i -> do
                 let !o = valuesStart + (fieldOffsets !! i)
                 case dict V.!? (fieldIds !! i) of
                   Nothing  -> Left "Iceberg.Variant: object field id out of range"
                   Just key -> do
                     (val, _) <- decodeValueAt dict bs o
                     Right (key, val))
             [0 .. n - 1]
  let !lastOffset = if null fieldOffsets then 0 else last fieldOffsets
      !endOff = valuesStart + lastOffset
  Right (VObject (Map.fromList pairs), endOff)

decodeArrayAt
  :: Vector Text -> Int -> ByteString -> Int
  -> Either String (Variant, Int)
decodeArrayAt dict hdr bs off = do
  let !offSize = (hdr .&. 0x03) + 1
      !isLarge = (hdr `shiftR` 2) .&. 0x01
      !numWidth = if isLarge == 1 then 4 else 1
  (n, off1) <- readUle numWidth bs off
  let !offsetsStart = off1
      !valuesStart  = offsetsStart + (n + 1) * offSize
  fieldOffsets <- traverse
                    (\i -> fst <$> readUle offSize bs (offsetsStart + i * offSize))
                    [0 .. n]
  vals <- traverse
            (\i -> do
                let !o = valuesStart + (fieldOffsets !! i)
                (val, _) <- decodeValueAt dict bs o
                Right val)
            [0 .. n - 1]
  let !lastOffset = if null fieldOffsets then 0 else last fieldOffsets
      !endOff = valuesStart + lastOffset
  Right (VArray (V.fromList vals), endOff)

-- ============================================================
-- JSON bridge
-- ============================================================

-- | Convert a JSON value to a Variant. Numbers without a fractional
-- part fit into the smallest integer type that holds them; numbers
-- with a fractional part become 'VDouble'.
variantFromJSON :: Aeson.Value -> Variant
variantFromJSON = \case
  Aeson.Null      -> VNull
  Aeson.Bool b    -> VBool b
  Aeson.Number s  -> case floatingOrInteger s :: Either Double Integer of
    Right i  -> integerToVariant i
    Left  d  -> VDouble d
  Aeson.String t  -> VString t
  Aeson.Array xs  -> VArray (V.map variantFromJSON xs)
  Aeson.Object o  ->
    VObject (Map.fromList
              (map (\(k, v) -> (Key.toText k, variantFromJSON v)) (KM.toList o)))

integerToVariant :: Integer -> Variant
integerToVariant i
  | i >= toInteger (minBound :: Int8)  && i <= toInteger (maxBound :: Int8)  = VInt8  (fromInteger i)
  | i >= toInteger (minBound :: Int16) && i <= toInteger (maxBound :: Int16) = VInt16 (fromInteger i)
  | i >= toInteger (minBound :: Int32) && i <= toInteger (maxBound :: Int32) = VInt32 (fromInteger i)
  | i >= toInteger (minBound :: Int64) && i <= toInteger (maxBound :: Int64) = VInt64 (fromInteger i)
  | otherwise = VDouble (fromInteger i)

variantToJSON :: Variant -> Aeson.Value
variantToJSON = \case
  VNull       -> Aeson.Null
  VBool b     -> Aeson.Bool b
  VInt8 v     -> Aeson.toJSON v
  VInt16 v    -> Aeson.toJSON v
  VInt32 v    -> Aeson.toJSON v
  VInt64 v    -> Aeson.toJSON v
  VFloat v    -> Aeson.toJSON v
  VDouble v   -> Aeson.toJSON v
  VString t   -> Aeson.String t
  VBinary bs  -> Aeson.String (TE.decodeUtf8 (BS.takeWhile (const True) bs))
                 -- best-effort; binary in JSON is fundamentally lossy.
  VArray xs   -> Aeson.Array (V.map variantToJSON xs)
  VObject m   ->
    Aeson.Object (KM.fromList
                    (map (\(k, v) -> (Key.fromText k, variantToJSON v))
                         (Map.toList m)))
  VUnsupportedPrimitive tag _ ->
    Aeson.object [ "_unsupported_variant_primitive" Aeson..= tag ]

-- ============================================================
-- Helpers
-- ============================================================

-- | Lookup a name's index in the dictionary. Errors out of band with
-- 'error' because 'collectFieldNames' constructed the dictionary
-- from the same tree we're encoding, so a missing name is a writer
-- bug, not a user error.
dictIndex :: Vector Text -> Text -> Int
dictIndex dict t = case V.findIndex (== t) dict of
  Just i  -> i
  Nothing -> error ("Iceberg.Variant.dictIndex: '"
                     ++ T.unpack t ++ "' missing from dictionary")

-- | Number of bytes needed to encode @v@ as an unsigned little-endian
-- integer, in the @[1, 4]@ range the spec allows.
pickWidth :: Int -> Int
pickWidth v
  | v < 256        = 1
  | v < 65536      = 2
  | v < 16777216   = 3
  | otherwise      = 4

-- | Write an unsigned little-endian integer of @width@ bytes.
writeUleN :: Int -> Int -> B.Builder
writeUleN w v = go 0
  where
    go !i
      | i >= w = mempty
      | otherwise = B.word8 (fromIntegral ((v `shiftR` (i * 8)) .&. 0xFF))
                    <> go (i + 1)

-- | Read an unsigned little-endian integer of @width@ bytes, as 'Int'.
readUle :: Int -> ByteString -> Int -> Either String (Int, Int)
readUle w bs o
  | o + w > BS.length bs = Left "Iceberg.Variant: short buffer"
  | otherwise = Right (go 0 0, o + w)
  where
    go !i !acc
      | i >= w = acc
      | otherwise =
          let !b = fromIntegral (BS.index bs (o + i)) :: Int
           in go (i + 1) (acc .|. (b `shiftL` (i * 8)))

readLE :: Int -> ByteString -> Int -> Either String (Word64, Int)
readLE w bs o
  | o + w > BS.length bs = Left "Iceberg.Variant: short buffer"
  | otherwise = Right (go 0 0, o + w)
  where
    go !i !acc
      | i >= w = acc
      | otherwise =
          let !b = fromIntegral (BS.index bs (o + i)) :: Word64
           in go (i + 1) (acc .|. (b `shiftL` (i * 8)))

-- | Like 'scanl'' on lists but returns @[acc] ++ [acc + x | x <- xs]@
-- as a list of 'Int's. Avoids a Data.List.scanl import + an explicit
-- type annotation.
scanlInt :: (Int -> Int -> Int) -> Int -> [Int] -> [Int]
scanlInt f = goL
  where
    goL !acc xs0 = acc : case xs0 of
      []       -> []
      (x : xs) -> goL (f acc x) xs

-- Suppress unused-import warning: 'Scientific' is used in the 'case'
-- match but the type annotation can be elided.
_unusedScientific :: Scientific -> Scientific
_unusedScientific = id
