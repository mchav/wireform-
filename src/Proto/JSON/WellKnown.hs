{-# LANGUAGE BangPatterns #-}
-- | Proto3 canonical JSON mapping for well-known types.
--
-- These functions provide the canonical conversions specified by the
-- proto3 JSON specification. Use these when you need spec-compliant
-- JSON rather than the default field-level format.
module Proto.JSON.WellKnown
  ( timestampToJSON
  , timestampFromJSON
  , durationToJSON
  , durationFromJSON
  , fieldMaskToJSON
  , fieldMaskFromJSON
  , structToJSON
  , structFromJSON
  , valueToJSON
  , valueFromJSON
  , formatRfc3339
  , parseRfc3339
  ) where

import Data.Int (Int32, Int64)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V

import Proto.JSON
import Proto.Google.Protobuf.Timestamp (Timestamp(..))
import Proto.Google.Protobuf.Duration (Duration(..))
import Proto.Google.Protobuf.FieldMask (FieldMask(..))
import Proto.Google.Protobuf.Struct (Struct(..), Value(..), Value'Kind(..), NullValue(..), ListValue(..))

timestampToJSON :: Timestamp -> JsonValue
timestampToJSON (Timestamp s n) = JsonString (formatRfc3339 s n)

timestampFromJSON :: JsonValue -> Either String Timestamp
timestampFromJSON (JsonString t) = parseRfc3339 t
timestampFromJSON _ = Left "Expected RFC 3339 string for Timestamp"

formatRfc3339 :: Int64 -> Int32 -> Text
formatRfc3339 secs nanos =
  let (y, m, d, h, mi, s) = unixToDate secs
      dateStr = pad4 y <> "-" <> pad2 m <> "-" <> pad2 d
      timeStr = pad2 h <> ":" <> pad2 mi <> ":" <> pad2 s
      nanoStr = if nanos == 0 then ""
                else "." <> T.dropWhileEnd (== '0') (pad9 (fromIntegral nanos))
  in dateStr <> "T" <> timeStr <> nanoStr <> "Z"
  where
    pad2 n = let s = T.pack (show n) in T.replicate (2 - T.length s) "0" <> s
    pad4 n = let s = T.pack (show n) in T.replicate (4 - T.length s) "0" <> s
    pad9 n = let s = T.pack (show (abs n)) in T.replicate (9 - T.length s) "0" <> s

unixToDate :: Int64 -> (Int, Int, Int, Int, Int, Int)
unixToDate totalSecs =
  let s' = fromIntegral totalSecs :: Int
      (days, dayRem) = s' `divMod` 86400
      h = dayRem `div` 3600
      mi = (dayRem `mod` 3600) `div` 60
      sec = dayRem `mod` 60
      (y, m, d) = civilFromDays (days + 719468)
  in (y, m, d, h, mi, sec)

civilFromDays :: Int -> (Int, Int, Int)
civilFromDays z =
  let era = (if z >= 0 then z else z - 146096) `div` 146097
      doe = z - era * 146097
      yoe = (doe - doe `div` 1460 + doe `div` 36524 - doe `div` 146096) `div` 365
      y = yoe + era * 400
      doy = doe - (365 * yoe + yoe `div` 4 - yoe `div` 100)
      mp = (5 * doy + 2) `div` 153
      d = doy - (153 * mp + 2) `div` 5 + 1
      m = mp + (if mp < 10 then 3 else -9)
      y' = y + (if m <= 2 then 1 else 0)
  in (y', m, d)

parseRfc3339 :: Text -> Either String Timestamp
parseRfc3339 t = do
  let stripped = T.strip t
  case T.splitOn "T" stripped of
    [datePart, timePart'] -> do
      let timePart = if T.isSuffixOf "Z" timePart' || T.isSuffixOf "z" timePart'
                     then T.init timePart'
                     else timePart'
      (y, m, d) <- parseDate datePart
      (h, mi, s, ns) <- parseTime timePart
      let days = daysFromCivil y m d - 719468
          totalSecs = fromIntegral days * 86400 + fromIntegral h * 3600 +
                      fromIntegral mi * 60 + fromIntegral s
      Right (Timestamp totalSecs ns)
    _ -> Left ("Invalid RFC 3339 timestamp: " <> T.unpack t)

parseDate :: Text -> Either String (Int, Int, Int)
parseDate t = case T.splitOn "-" t of
  [ys, ms, ds] -> case (reads (T.unpack ys), reads (T.unpack ms), reads (T.unpack ds)) of
    ([(y,"")], [(m,"")], [(d,"")]) -> Right (y, m, d)
    _ -> Left "Invalid date"
  _ -> Left "Invalid date format"

parseTime :: Text -> Either String (Int, Int, Int, Int32)
parseTime t =
  let (wholePart, fracPart) = T.breakOn "." t
  in case T.splitOn ":" wholePart of
    [hs, ms, ss] -> case (reads (T.unpack hs), reads (T.unpack ms), reads (T.unpack ss)) of
      ([(h,"")], [(m,"")], [(s,"")]) -> do
        let nanos = parseFracNanos fracPart
        Right (h, m, s, nanos)
      _ -> Left "Invalid time"
    _ -> Left "Invalid time format"

parseFracNanos :: Text -> Int32
parseFracNanos t
  | T.null t = 0
  | T.head t == '.' =
      let digits = T.takeWhile (\c -> c >= '0' && c <= '9') (T.tail t)
          padded = digits <> T.replicate (9 - T.length digits) "0"
      in case reads (T.unpack (T.take 9 padded)) of
           [(n, "")] -> n
           _ -> 0
  | otherwise = 0

daysFromCivil :: Int -> Int -> Int -> Int
daysFromCivil y m d =
  let y' = y - (if m <= 2 then 1 else 0)
      era = (if y' >= 0 then y' else y' - 399) `div` 400
      yoe = y' - era * 400
      doy = (153 * (m + (if m > 2 then -3 else 9)) + 2) `div` 5 + d - 1
      doe = yoe * 365 + yoe `div` 4 - yoe `div` 100 + doy
  in era * 146097 + doe

-- Duration: "3.5s" format
durationToJSON :: Duration -> JsonValue
durationToJSON (Duration s n) =
  let secStr = T.pack (show s)
      nanoStr = if n == 0 then ""
                else "." <> T.dropWhileEnd (== '0') (pad9 (abs (fromIntegral n)))
  in JsonString (secStr <> nanoStr <> "s")
  where
    pad9 :: Int -> Text
    pad9 x = let str = T.pack (show x) in T.replicate (9 - T.length str) "0" <> str

durationFromJSON :: JsonValue -> Either String Duration
durationFromJSON (JsonString t) = parseDuration t
durationFromJSON _ = Left "Expected duration string"

parseDuration :: Text -> Either String Duration
parseDuration t = do
  let stripped = T.strip t
  case T.stripSuffix "s" stripped of
    Nothing -> Left ("Duration must end with 's': " <> T.unpack t)
    Just numPart -> case T.breakOn "." numPart of
      (wholePart, fracPart) -> do
        secs <- case reads (T.unpack wholePart) of
          [(s, "")] -> Right s
          _ -> Left ("Invalid duration seconds: " <> T.unpack wholePart)
        let nanos = parseFracNanos fracPart
        Right (Duration secs nanos)

-- FieldMask: comma-separated paths
fieldMaskToJSON :: FieldMask -> JsonValue
fieldMaskToJSON (FieldMask ps) = JsonString (T.intercalate "," (V.toList ps))

fieldMaskFromJSON :: JsonValue -> Either String FieldMask
fieldMaskFromJSON (JsonString t)
  | T.null t  = Right (FieldMask V.empty)
  | otherwise = Right (FieldMask (V.fromList (T.splitOn "," t)))
fieldMaskFromJSON _ = Left "Expected string for FieldMask"

-- Struct/Value: native JSON
structToJSON :: Struct -> JsonValue
structToJSON (Struct fs) = JsonObject (fmap valueToJSON fs)

structFromJSON :: JsonValue -> Either String Struct
structFromJSON (JsonObject m) = Right (Struct (fmap jsonToValue m))
structFromJSON _ = Left "Expected object for Struct"

valueToJSON :: Value -> JsonValue
valueToJSON (Value Nothing) = JsonNull
valueToJSON (Value (Just vk)) = case vk of
  Value'Kind'NullValue _   -> JsonNull
  Value'Kind'NumberValue d -> JsonNumber d
  Value'Kind'StringValue s -> JsonString s
  Value'Kind'BoolValue b   -> JsonBool b
  Value'Kind'StructValue s -> structToJSON s
  Value'Kind'ListValue l   -> JsonArray (V.toList (fmap valueToJSON (listValueValues l)))

valueFromJSON :: JsonValue -> Either String Value
valueFromJSON jv = Right (jsonToValue jv)

jsonToValue :: JsonValue -> Value
jsonToValue JsonNull = Value (Just (Value'Kind'NullValue NullValue'NullValue))
jsonToValue (JsonBool b) = Value (Just (Value'Kind'BoolValue b))
jsonToValue (JsonNumber n) = Value (Just (Value'Kind'NumberValue n))
jsonToValue (JsonString s) = Value (Just (Value'Kind'StringValue s))
jsonToValue (JsonArray vs) = Value (Just (Value'Kind'ListValue (ListValue (V.fromList (fmap jsonToValue vs)))))
jsonToValue (JsonObject m) = Value (Just (Value'Kind'StructValue (Struct (fmap jsonToValue m))))
