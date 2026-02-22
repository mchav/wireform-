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

import Proto.Google.Protobuf.Timestamp
import Proto.Google.Protobuf.Duration
import Proto.Google.Protobuf.FieldMask
import Proto.Google.Protobuf.Struct

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
