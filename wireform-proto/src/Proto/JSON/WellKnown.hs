{-# LANGUAGE BangPatterns #-}
-- | Proto3 canonical JSON mapping for well-known types.
--
-- These functions provide the canonical conversions specified by the
-- proto3 JSON specification.
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

    -- * Wrapper types
    -- | Per the proto3 JSON spec, every @google.protobuf.XValue@
    -- wrapper serialises as just its inner value (rather than the
    -- generic @{"value": ...}@ shape the pre-generated @ToJSON@
    -- instances produce). The 'wrap*' helpers go in the encode
    -- direction; the 'unwrap*' helpers parse a bare JSON value
    -- and construct the wrapper. 64-bit integer wrappers
    -- additionally string-encode their inner value.
  , wrapBoolValue
  , wrapInt32Value
  , wrapInt64Value
  , wrapUInt32Value
  , wrapUInt64Value
  , wrapFloatValue
  , wrapDoubleValue
  , wrapStringValue
  , wrapBytesValue
  , unwrapBoolValue
  , unwrapInt32Value
  , unwrapInt64Value
  , unwrapUInt32Value
  , unwrapUInt64Value
  , unwrapFloatValue
  , unwrapDoubleValue
  , unwrapStringValue
  , unwrapBytesValue

    -- * Empty / NullValue
  , emptyToJSON
  , emptyFromJSON
  , nullValueToJSON
  , nullValueFromJSON

    -- * Any
  , anyToJSON
  , anyFromJSON
  ) where

import Data.Bifunctor (bimap)
import Data.Char (isDigit)
import Data.Int (Int32, Int64)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Scientific (fromFloatDigits, toRealFloat)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified Data.Vector as V

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as AesonKM

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Text.Encoding as TE

import Proto.Google.Protobuf.Timestamp
import Proto.Google.Protobuf.Duration
import Proto.Google.Protobuf.FieldMask
import Proto.Google.Protobuf.Struct
import qualified Proto.Google.Protobuf.Wrappers as W
import qualified Proto.Google.Protobuf.Empty    as Empty
import qualified Proto.Google.Protobuf.Any      as Any

-- Timestamp: RFC 3339 format "YYYY-MM-DDThh:mm:ss[.nnn]Z"

timestampToJSON :: Timestamp -> Aeson.Value
timestampToJSON ts = Aeson.String (formatRfc3339 (timestampSeconds ts) (timestampNanos ts))

timestampFromJSON :: Aeson.Value -> Either String Timestamp
timestampFromJSON (Aeson.String t) = parseRfc3339 t
timestampFromJSON _ = Left "Expected RFC 3339 string for Timestamp"

formatRfc3339 :: Int64 -> Int32 -> Text
formatRfc3339 secs nanos =
  let !civil = unixToCivil secs
      dateStr = padInt 4 (cvYear civil) <> "-" <> padInt 2 (cvMonth civil) <> "-" <> padInt 2 (cvDay civil)
      timeStr = padInt 2 (cvHour civil) <> ":" <> padInt 2 (cvMinute civil) <> ":" <> padInt 2 (cvSecond civil)
      nanoStr
        | nanos == 0 = ""
        | otherwise  = "." <> T.dropWhileEnd (== '0') (padInt 9 (fromIntegral (abs nanos)))
  in dateStr <> "T" <> timeStr <> nanoStr <> "Z"

data CivilTime = CivilTime
  { cvYear   :: {-# UNPACK #-} !Int
  , cvMonth  :: {-# UNPACK #-} !Int
  , cvDay    :: {-# UNPACK #-} !Int
  , cvHour   :: {-# UNPACK #-} !Int
  , cvMinute :: {-# UNPACK #-} !Int
  , cvSecond :: {-# UNPACK #-} !Int
  }

unixToCivil :: Int64 -> CivilTime
unixToCivil totalSecs =
  let !s' = fromIntegral totalSecs :: Int
      (!days, !dayRem) = s' `quotRem` 86400
      (!h, !hmRem)     = dayRem `quotRem` 3600
      (!mi, !sec)      = hmRem `quotRem` 60
      !date            = civilFromDays (days + 719468)
  in CivilTime (cdYear date) (cdMonth date) (cdDay date) h mi sec

data CivilDate = CivilDate
  { cdYear  :: {-# UNPACK #-} !Int
  , cdMonth :: {-# UNPACK #-} !Int
  , cdDay   :: {-# UNPACK #-} !Int
  }

civilFromDays :: Int -> CivilDate
civilFromDays z =
  let !era = (if z >= 0 then z else z - 146096) `quot` 146097
      !doe = z - era * 146097
      !yoe = (doe - doe `quot` 1460 + doe `quot` 36524 - doe `quot` 146096) `quot` 365
      !y   = yoe + era * 400
      !doy = doe - (365 * yoe + yoe `quot` 4 - yoe `quot` 100)
      !mp  = (5 * doy + 2) `quot` 153
      !d   = doy - (153 * mp + 2) `quot` 5 + 1
      !m   = mp + (if mp < 10 then 3 else -9)
      !y'  = y + (if m <= 2 then 1 else 0)
  in CivilDate y' m d

daysFromCivil :: Int -> Int -> Int -> Int
daysFromCivil y m d =
  let !y'  = y - (if m <= 2 then 1 else 0)
      !era = (if y' >= 0 then y' else y' - 399) `quot` 400
      !yoe = y' - era * 400
      !doy = (153 * (m + (if m > 2 then -3 else 9)) + 2) `quot` 5 + d - 1
      !doe = yoe * 365 + yoe `quot` 4 - yoe `quot` 100 + doy
  in era * 146097 + doe

padInt :: Int -> Int -> Text
padInt width n =
  let !raw = intToText n
      !pad = width - T.length raw
  in if pad <= 0 then raw else T.replicate pad "0" <> raw

intToText :: Int -> Text
intToText n
  | n < 0     = "-" <> intToText (negate n)
  | n < 10    = T.singleton (digit n)
  | otherwise = go T.empty n
  where
    go !acc 0 = acc
    go !acc v =
      let (!q, !r) = v `quotRem` 10
      in go (T.cons (digit r) acc) q
    digit i = toEnum (i + 48)

parseRfc3339 :: Text -> Either String Timestamp
parseRfc3339 t = do
  let stripped = T.strip t
  case T.breakOn "T" stripped of
    (datePart, rest)
      | T.null rest -> Left "Invalid RFC 3339 timestamp: missing T separator"
      | otherwise -> do
          let timePart' = T.drop 1 rest
              timePart = T.dropWhileEnd (\c -> c == 'Z' || c == 'z') timePart'
          date <- parseDate datePart
          time <- parseTime timePart
          let !days = daysFromCivil (pdYear date) (pdMonth date) (pdDay date) - 719468
              !totalSecs = fromIntegral days * 86400 + fromIntegral (ptHour time) * 3600 +
                           fromIntegral (ptMinute time) * 60 + fromIntegral (ptSecond time)
          Right Timestamp
            { timestampSeconds = totalSecs
            , timestampNanos = ptNanos time
            , timestampUnknownFields = []
            }

data ParsedDate = ParsedDate
  { pdYear  :: {-# UNPACK #-} !Int
  , pdMonth :: {-# UNPACK #-} !Int
  , pdDay   :: {-# UNPACK #-} !Int
  }

data ParsedTime = ParsedTime
  { ptHour   :: {-# UNPACK #-} !Int
  , ptMinute :: {-# UNPACK #-} !Int
  , ptSecond :: {-# UNPACK #-} !Int
  , ptNanos  :: {-# UNPACK #-} !Int32
  }

parseDate :: Text -> Either String ParsedDate
parseDate t = case T.splitOn "-" t of
  [ys, ms, ds] -> do
    y <- readInt ys
    m <- readInt ms
    d <- readInt ds
    Right (ParsedDate y m d)
  _ -> Left "Invalid date format"

parseTime :: Text -> Either String ParsedTime
parseTime t =
  let (wholePart, fracPart) = T.breakOn "." t
  in case T.splitOn ":" wholePart of
    [hs, ms, ss] -> do
      h <- readInt hs
      m <- readInt ms
      s <- readInt ss
      let !nanos = parseFracNanos fracPart
      Right (ParsedTime h m s nanos)
    _ -> Left "Invalid time format"

parseFracNanos :: Text -> Int32
parseFracNanos t
  | T.null t  = 0
  | T.head t == '.' =
      let digits = T.takeWhile isDigit (T.tail t)
          padded = T.take 9 (digits <> T.replicate (9 - T.length digits) "0")
      in case readInt padded of
           Right n -> fromIntegral n
           Left _  -> 0
  | otherwise = 0

readInt :: Text -> Either String Int
readInt t = case TR.signed TR.decimal t of
  Right (n, rest) | T.null rest -> Right n
  Right (_, rest) -> Left ("Trailing chars: " <> T.unpack rest)
  Left e -> Left e

-- Duration: "3.5s" format

durationToJSON :: Duration -> Aeson.Value
durationToJSON dur =
  let !s = durationSeconds dur
      !n = durationNanos dur
      secStr = intToText (fromIntegral s)
      nanoStr
        | n == 0    = ""
        | otherwise = "." <> T.dropWhileEnd (== '0') (padInt 9 (fromIntegral (abs n)))
  in Aeson.String (secStr <> nanoStr <> "s")

durationFromJSON :: Aeson.Value -> Either String Duration
durationFromJSON (Aeson.String t) = parseDuration t
durationFromJSON _ = Left "Expected duration string"

parseDuration :: Text -> Either String Duration
parseDuration t = do
  let stripped = T.strip t
  case T.stripSuffix "s" stripped of
    Nothing -> Left "Duration must end with 's'"
    Just numPart -> case T.breakOn "." numPart of
      (wholePart, fracPart) -> do
        secs <- readInt wholePart
        let !nanos = parseFracNanos fracPart
        Right Duration
          { durationSeconds = fromIntegral secs
          , durationNanos = nanos
          , durationUnknownFields = []
          }

-- FieldMask: comma-separated paths

fieldMaskToJSON :: FieldMask -> Aeson.Value
fieldMaskToJSON fm = Aeson.String (T.intercalate "," (V.toList (fieldMaskPaths fm)))

fieldMaskFromJSON :: Aeson.Value -> Either String FieldMask
fieldMaskFromJSON (Aeson.String t)
  | T.null t  = Right (FieldMask { fieldMaskPaths = V.empty, fieldMaskUnknownFields = [] })
  | otherwise = Right (FieldMask { fieldMaskPaths = V.fromList (T.splitOn "," t), fieldMaskUnknownFields = [] })
fieldMaskFromJSON _ = Left "Expected string for FieldMask"

-- Struct/Value: native JSON

structToJSON :: Struct -> Aeson.Value
structToJSON s =
  Aeson.Object (AesonKM.fromList
    (fmap (bimap AesonKey.fromText valueToJSON) (Map.toList (structFields s))))

structFromJSON :: Aeson.Value -> Either String Struct
structFromJSON (Aeson.Object o) =
  Right defaultStruct { structFields = Map.fromList
    (fmap (bimap AesonKey.toText jsonToValue) (AesonKM.toList o)) }
structFromJSON _ = Left "Expected object for Struct"

valueToJSON :: Value -> Aeson.Value
valueToJSON v = case valueKind v of
  Nothing -> Aeson.Null
  Just vk -> case vk of
    Value'Kind'NullValue _   -> Aeson.Null
    Value'Kind'NumberValue d -> Aeson.Number (fromFloatDigits d)
    Value'Kind'StringValue s -> Aeson.String s
    Value'Kind'BoolValue b   -> Aeson.Bool b
    Value'Kind'StructValue s -> structToJSON s
    Value'Kind'ListValue l   -> Aeson.Array (fmap valueToJSON (listValueValues l))

valueFromJSON :: Aeson.Value -> Either String Value
valueFromJSON jv = Right (jsonToValue jv)

jsonToValue :: Aeson.Value -> Value
jsonToValue Aeson.Null = defaultValue { valueKind = Just (Value'Kind'NullValue NullValue'NullValue) }
jsonToValue (Aeson.Bool b) = defaultValue { valueKind = Just (Value'Kind'BoolValue b) }
jsonToValue (Aeson.Number n) = defaultValue { valueKind = Just (Value'Kind'NumberValue (toRealFloat n)) }
jsonToValue (Aeson.String s) = defaultValue { valueKind = Just (Value'Kind'StringValue s) }
jsonToValue (Aeson.Array vs) = defaultValue { valueKind = Just (Value'Kind'ListValue (defaultListValue { listValueValues = fmap jsonToValue vs })) }
jsonToValue (Aeson.Object o) = defaultValue { valueKind = Just (Value'Kind'StructValue (defaultStruct { structFields = Map.fromList (fmap (bimap AesonKey.toText jsonToValue) (AesonKM.toList o)) })) }

-- ---------------------------------------------------------------------------
-- Wrappers (proto3 spec: emit just the inner value, not @{"value": ...}@)
-- ---------------------------------------------------------------------------

-- Encoders unwrap to the inner value, applying proto3-canonical
-- per-scalar conversions where needed (string-form 64-bit ints,
-- NaN/Infinity floats, base64 bytes).

wrapBoolValue :: W.BoolValue -> Aeson.Value
wrapBoolValue = Aeson.Bool . W.boolValueValue

wrapInt32Value :: W.Int32Value -> Aeson.Value
wrapInt32Value = Aeson.toJSON . W.int32ValueValue

wrapInt64Value :: W.Int64Value -> Aeson.Value
wrapInt64Value = Aeson.String . T.pack . show . W.int64ValueValue

wrapUInt32Value :: W.UInt32Value -> Aeson.Value
wrapUInt32Value = Aeson.toJSON . W.uInt32ValueValue

wrapUInt64Value :: W.UInt64Value -> Aeson.Value
wrapUInt64Value = Aeson.String . T.pack . show . W.uInt64ValueValue

wrapFloatValue :: W.FloatValue -> Aeson.Value
wrapFloatValue = floatLikeToJSON . realToFrac . W.floatValueValue

wrapDoubleValue :: W.DoubleValue -> Aeson.Value
wrapDoubleValue = floatLikeToJSON . W.doubleValueValue

wrapStringValue :: W.StringValue -> Aeson.Value
wrapStringValue = Aeson.String . W.stringValueValue

wrapBytesValue :: W.BytesValue -> Aeson.Value
wrapBytesValue = Aeson.String . TE.decodeUtf8 . Base64.encode . W.bytesValueValue

floatLikeToJSON :: Double -> Aeson.Value
floatLikeToJSON d
  | isNaN d      = Aeson.String "NaN"
  | isInfinite d = Aeson.String (if d > 0 then "Infinity" else "-Infinity")
  | otherwise    = Aeson.Number (fromFloatDigits d)

-- Decoders parse a bare JSON value and construct the wrapper.

unwrapBoolValue :: Aeson.Value -> Either String W.BoolValue
unwrapBoolValue (Aeson.Bool b) = Right W.defaultBoolValue { W.boolValueValue = b }
unwrapBoolValue _              = Left "Expected JSON Bool for BoolValue"

unwrapInt32Value :: Aeson.Value -> Either String W.Int32Value
unwrapInt32Value v = case parseIntegral v of
  Right n -> Right W.defaultInt32Value { W.int32ValueValue = fromIntegral (n :: Int64) }
  Left e  -> Left e

unwrapInt64Value :: Aeson.Value -> Either String W.Int64Value
unwrapInt64Value v = case parseIntegral v of
  Right n -> Right W.defaultInt64Value { W.int64ValueValue = n }
  Left e  -> Left e

unwrapUInt32Value :: Aeson.Value -> Either String W.UInt32Value
unwrapUInt32Value v = case parseIntegral v of
  Right n -> Right W.defaultUInt32Value
    { W.uInt32ValueValue = fromIntegral (n :: Int64) }
  Left e  -> Left e

unwrapUInt64Value :: Aeson.Value -> Either String W.UInt64Value
unwrapUInt64Value v = case parseIntegral v of
  Right n -> Right W.defaultUInt64Value
    { W.uInt64ValueValue = fromIntegral (n :: Int64) }
  Left e  -> Left e

unwrapFloatValue :: Aeson.Value -> Either String W.FloatValue
unwrapFloatValue v = case parseFloating v of
  Right d -> Right W.defaultFloatValue { W.floatValueValue = realToFrac d }
  Left e  -> Left e

unwrapDoubleValue :: Aeson.Value -> Either String W.DoubleValue
unwrapDoubleValue v = case parseFloating v of
  Right d -> Right W.defaultDoubleValue { W.doubleValueValue = d }
  Left e  -> Left e

unwrapStringValue :: Aeson.Value -> Either String W.StringValue
unwrapStringValue (Aeson.String s) =
  Right W.defaultStringValue { W.stringValueValue = s }
unwrapStringValue _ = Left "Expected JSON String for StringValue"

unwrapBytesValue :: Aeson.Value -> Either String W.BytesValue
unwrapBytesValue (Aeson.String s) =
  case Base64.decode (TE.encodeUtf8 s) of
    Right bs -> Right W.defaultBytesValue { W.bytesValueValue = bs }
    Left e   -> Left ("invalid base64 for BytesValue: " <> e)
unwrapBytesValue _ = Left "Expected JSON String for BytesValue"

parseIntegral :: Aeson.Value -> Either String Int64
parseIntegral (Aeson.String s) = case TR.signed TR.decimal s of
  Right (n, rest) | T.null rest -> Right n
  _ -> Left "Invalid integer string"
parseIntegral (Aeson.Number n) = Right (round n)
parseIntegral _ = Left "Expected JSON String or Number"

parseFloating :: Aeson.Value -> Either String Double
parseFloating (Aeson.Number n) = Right (toRealFloat n)
parseFloating (Aeson.String "NaN")       = Right (0 / 0)
parseFloating (Aeson.String "Infinity")  = Right (1 / 0)
parseFloating (Aeson.String "-Infinity") = Right (negate (1 / 0))
parseFloating _ = Left "Expected JSON Number or {NaN,Infinity}"

-- ---------------------------------------------------------------------------
-- Empty / NullValue / Any
-- ---------------------------------------------------------------------------

emptyToJSON :: Empty.Empty -> Aeson.Value
emptyToJSON _ = Aeson.Object AesonKM.empty

emptyFromJSON :: Aeson.Value -> Either String Empty.Empty
emptyFromJSON (Aeson.Object _) = Right Empty.defaultEmpty
emptyFromJSON _                = Left "Expected JSON Object for Empty"

-- | 'NullValue' is the proto3 enum @NULL_VALUE = 0@; it serialises
-- as JSON @null@. We import it from "Proto.Google.Protobuf.Struct"
-- (since that's where the codegen put it).
nullValueToJSON :: NullValue -> Aeson.Value
nullValueToJSON _ = Aeson.Null

nullValueFromJSON :: Aeson.Value -> Either String NullValue
nullValueFromJSON Aeson.Null = Right NullValue'NullValue
nullValueFromJSON _          = Left "Expected JSON null for NullValue"

-- | @google.protobuf.Any@ JSON shape:
-- @{"@type": "type.googleapis.com/...", ...other fields embedded...}@.
-- Implementing the embedded-fields side requires a runtime type
-- registry; for now we support only the round-trip-as-@\{"@type":\}@-and-
-- @value@ degenerate form, which the conformance suite uses for
-- some of its Any tests.
anyToJSON :: Any.Any -> Aeson.Value
anyToJSON a = Aeson.Object (AesonKM.fromList
  [ (AesonKey.fromText (T.pack "@type"), Aeson.String (Any.anyTypeUrl a))
  , (AesonKey.fromText (T.pack "value"),
      Aeson.String (TE.decodeUtf8 (Base64.encode (Any.anyValue a))))
  ])

anyFromJSON :: Aeson.Value -> Either String Any.Any
anyFromJSON (Aeson.Object o) = do
  let look k = AesonKM.lookup (AesonKey.fromText (T.pack k)) o
  ty <- case look "@type" of
    Just (Aeson.String s) -> Right s
    _                     -> Left "Any: missing or non-string @type"
  bs <- case look "value" of
    Just (Aeson.String s) -> case Base64.decode (TE.encodeUtf8 s) of
      Right bs -> Right bs
      Left e   -> Left ("Any: invalid base64 value: " <> e)
    Nothing               -> Right BS.empty
    _                     -> Left "Any: non-string value"
  Right Any.defaultAny { Any.anyTypeUrl = ty, Any.anyValue = bs }
anyFromJSON _ = Left "Expected JSON Object for Any"
