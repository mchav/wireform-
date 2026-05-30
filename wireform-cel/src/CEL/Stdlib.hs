{-# LANGUAGE BangPatterns #-}

-- | The CEL standard library: arithmetic and comparison operators, the
-- membership / indexing operators, type conversions, and the built-in
-- functions (size, string functions, regex matching, and the timestamp /
-- duration accessors).
--
-- Semantics follow the "Standard Definitions" section of the CEL language
-- definition: numeric operators check for overflow, division / modulus by
-- zero are errors, ordering and equality treat the numeric types as a single
-- number line, and @NaN@ is unordered and never equal.
--
-- Named (Joda / IANA) timezones are not yet supported by the date/time
-- accessors; @UTC@ and fixed @±HH:MM@ offsets are. Passing an unsupported
-- timezone name yields an error rather than an incorrect result.
module CEL.Stdlib
  ( arith
  , negateValue
  , notValue
  , ordCompare
  , inOp
  , indexValue
  , selectField
  , callFunction
  , sizeOf
  ) where

import qualified Data.ByteString as BS
import Data.Char (intToDigit, isDigit)
import Data.Int (Int64)
import Data.Ratio ((%))
import Numeric (floatToDigits)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Builder.Int as TBI
import qualified Data.Text.Read as TR
import Data.Time.Calendar (dayOfWeek, toGregorian)
import Data.Time.Calendar.OrdinalDate (toOrdinalDate)
import Data.Time.Clock (UTCTime (..), diffTimeToPicoseconds)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import Data.Time.LocalTime (timeZoneMinutes)
import Data.Time.Zones (timeZoneForUTCTime)
import Data.Time.Zones.All (fromTZName, tzByLabel)
import qualified Data.Vector as V
import Data.Word (Word64)
import Text.Regex.TDFA (Regex, makeRegexM, matchTest)

import CEL.Error
import CEL.Syntax (ArithOp (..), RelOp (..))
import CEL.Value

----------------------------------------------------------------------
-- Integer helpers
----------------------------------------------------------------------

i64min, i64max, u64max :: Integer
i64min = toInteger (minBound :: Int64)
i64max = toInteger (maxBound :: Int64)
u64max = toInteger (maxBound :: Word64)

mkInt :: Integer -> Either CelError Value
mkInt r
  | r < i64min || r > i64max = Left (overflow "integer")
  | otherwise = Right (VInt (fromInteger r))

mkUInt :: Integer -> Either CelError Value
mkUInt r
  | r < 0 || r > u64max = Left (overflow "unsigned integer")
  | otherwise = Right (VUInt (fromInteger r))

----------------------------------------------------------------------
-- Arithmetic
----------------------------------------------------------------------

-- | Evaluate a binary arithmetic operator.
arith :: ArithOp -> Value -> Value -> Either CelError Value
arith op a b = case op of
  Add -> add a b
  Sub -> sub a b
  Mul -> mul a b
  Div -> divide a b
  Mod -> modulo a b

add :: Value -> Value -> Either CelError Value
add (VInt x) (VInt y) = mkInt (toInteger x + toInteger y)
add (VUInt x) (VUInt y) = mkUInt (toInteger x + toInteger y)
add (VDouble x) (VDouble y) = Right (VDouble (x + y))
add (VString x) (VString y) = Right (VString (x <> y))
add (VBytes x) (VBytes y) = Right (VBytes (x <> y))
add (VList x) (VList y) = Right (VList (x <> y))
add (VTimestamp t) (VDuration d) = addTsDur t d
add (VDuration d) (VTimestamp t) = addTsDur t d
add (VDuration x) (VDuration y) = mkDurationNanos (durationNanos x + durationNanos y)
add _ _ = Left (noOverload "_+_")

sub :: Value -> Value -> Either CelError Value
sub (VInt x) (VInt y) = mkInt (toInteger x - toInteger y)
sub (VUInt x) (VUInt y) = mkUInt (toInteger x - toInteger y)
sub (VDouble x) (VDouble y) = Right (VDouble (x - y))
sub (VTimestamp x) (VTimestamp y) = mkDurationNanos (timestampNanos x - timestampNanos y)
sub (VTimestamp t) (VDuration d) = mkTimestampNanos (timestampNanos t - durationNanos d)
sub (VDuration x) (VDuration y) = mkDurationNanos (durationNanos x - durationNanos y)
sub _ _ = Left (noOverload "_-_")

mul :: Value -> Value -> Either CelError Value
mul (VInt x) (VInt y) = mkInt (toInteger x * toInteger y)
mul (VUInt x) (VUInt y) = mkUInt (toInteger x * toInteger y)
mul (VDouble x) (VDouble y) = Right (VDouble (x * y))
mul _ _ = Left (noOverload "_*_")

divide :: Value -> Value -> Either CelError Value
divide (VInt _) (VInt 0) = Left (divByZero "divide by zero")
divide (VInt x) (VInt y)
  | x == minBound && y == (-1) = Left (overflow "integer")
  | otherwise = Right (VInt (x `quot` y))
divide (VUInt _) (VUInt 0) = Left (divByZero "divide by zero")
divide (VUInt x) (VUInt y) = Right (VUInt (x `quot` y))
divide (VDouble x) (VDouble y) = Right (VDouble (x / y))
divide _ _ = Left (noOverload "_/_")

modulo :: Value -> Value -> Either CelError Value
modulo (VInt _) (VInt 0) = Left (divByZero "modulus by zero")
modulo (VInt x) (VInt y)
  | x == minBound && y == (-1) = Right (VInt 0)
  | otherwise = Right (VInt (x `rem` y))
modulo (VUInt _) (VUInt 0) = Left (divByZero "modulus by zero")
modulo (VUInt x) (VUInt y) = Right (VUInt (x `rem` y))
modulo _ _ = Left (noOverload "_%_")

-- | Unary negation.
negateValue :: Value -> Either CelError Value
negateValue (VInt x)
  | x == minBound = Left (overflow "integer")
  | otherwise = Right (VInt (negate x))
negateValue (VDouble x) = Right (VDouble (negate x))
negateValue _ = Left (noOverload "-_")

-- | Logical NOT.
notValue :: Value -> Either CelError Value
notValue (VBool b) = Right (VBool (not b))
notValue _ = Left (noOverload "!_")

----------------------------------------------------------------------
-- Timestamp / duration range construction
----------------------------------------------------------------------

-- Valid timestamp seconds range for [0001-01-01T00:00:00Z,
-- 9999-12-31T23:59:59.999999999Z].
tsSecMin, tsSecMax :: Integer
tsSecMin = -62135596800
tsSecMax = 253402300799

mkTimestampNanos :: Integer -> Either CelError Value
mkTimestampNanos total =
  let (s, n) = total `divMod` 1000000000
   in if s < tsSecMin || s > tsSecMax
        then Left (overflow "timestamp")
        else Right (VTimestamp (Timestamp (fromInteger s) (fromInteger n)))

mkDurationNanos :: Integer -> Either CelError Value
mkDurationNanos total
  | total < toInteger (minBound :: Int64) || total > toInteger (maxBound :: Int64) =
      Left (overflow "duration")
  | otherwise =
      let (s, n) = total `quotRem` 1000000000
       in Right (VDuration (Duration (fromInteger s) (fromInteger n)))

addTsDur :: Timestamp -> Duration -> Either CelError Value
addTsDur t d = mkTimestampNanos (timestampNanos t + durationNanos d)

----------------------------------------------------------------------
-- Comparison / membership / indexing
----------------------------------------------------------------------

-- | Ordering comparison (@<@, @<=@, @>@, @>=@). Cross-type numeric ordering is
-- supported; @NaN@ comparisons are @false@; incompatible types are a
-- no-overload error.
ordCompare :: RelOp -> Value -> Value -> Either CelError Value
ordCompare op a b
  | isNumeric a && isNumeric b = decide
  | sameOrderable a b = decide
  | otherwise = Left (noOverload (relName op))
  where
    decide = case compareValues a b of
      Just o -> Right (VBool (applyOrd op o))
      Nothing -> Right (VBool False)

applyOrd :: RelOp -> Ordering -> Bool
applyOrd Lt o = o == LT
applyOrd Le o = o /= GT
applyOrd Gt o = o == GT
applyOrd Ge o = o /= LT
applyOrd _ _ = False

relName :: RelOp -> Text
relName = \case
  Lt -> "_<_"
  Le -> "_<=_"
  Gt -> "_>_"
  Ge -> "_>=_"
  Eq -> "_==_"
  Ne -> "_!=_"
  In -> "@in"

sameOrderable :: Value -> Value -> Bool
sameOrderable a b = case (a, b) of
  (VBool _, VBool _) -> True
  (VString _, VString _) -> True
  (VBytes _, VBytes _) -> True
  (VTimestamp _, VTimestamp _) -> True
  (VDuration _, VDuration _) -> True
  _ -> False

-- | The @in@ operator: membership in a list, or key membership in a map.
inOp :: Value -> Value -> Either CelError Value
inOp x (VList v) = Right (VBool (V.any (valueEq x) v))
inOp x (VMap m) = Right (VBool (maybe False (const True) (celMapLookup x m)))
inOp _ _ = Left (noOverload "@in")

-- | Indexing: @list[int]@ and @map[key]@.
indexValue :: Value -> Value -> Either CelError Value
indexValue (VList v) i = case asListIndex i of
  Just k
    | k < 0 || k >= V.length v -> Left (CelError ErrNoSuchKey "index out of bounds")
    | otherwise -> Right (v V.! k)
  Nothing -> Left (noOverload "_[_]")
indexValue (VMap m) k = case celMapLookup k m of
  Just val -> Right val
  Nothing -> Left (noSuchKey (renderKey k))
indexValue _ _ = Left (noOverload "_[_]")

asListIndex :: Value -> Maybe Int
asListIndex (VInt n) = Just (fromIntegral n)
asListIndex (VUInt n) = Just (fromIntegral n)
asListIndex (VDouble d)
  | not (isNaN d || isInfinite d) && fromIntegral (truncate d :: Integer) == d =
      Just (truncate d)
asListIndex _ = Nothing

renderKey :: Value -> Text
renderKey = \case
  VString s -> s
  VInt i -> intToText i
  VUInt u -> intToText u
  VBool b -> if b then "true" else "false"
  v -> T.pack (show v)

-- | Field selection @e.f@. Only maps support selection in this
-- implementation (there is no protobuf message support yet); selecting a
-- missing map key is a no-such-key error.
selectField :: Value -> Text -> Either CelError Value
selectField (VMap m) f = case celMapLookup (VString f) m of
  Just v -> Right v
  Nothing -> Left (noSuchKey f)
selectField _ f = Left (noSuchField f)

----------------------------------------------------------------------
-- size
----------------------------------------------------------------------

-- | The @size@ of a string (code points), bytes (octets), list, or map.
sizeOf :: Value -> Either CelError Value
sizeOf = \case
  VString s -> Right (VInt (fromIntegral (T.length s)))
  VBytes b -> Right (VInt (fromIntegral (BS.length b)))
  VList v -> Right (VInt (fromIntegral (V.length v)))
  VMap m -> Right (VInt (fromIntegral (celMapSize m)))
  _ -> Left (noOverload "size")

----------------------------------------------------------------------
-- Standard functions
----------------------------------------------------------------------

-- | Dispatch a standard function or conversion by name and evaluated
-- arguments. The receiver of a method-style call is passed as the first
-- argument.
callFunction :: Text -> [Value] -> Either CelError Value
callFunction name args = case (name, args) of
  ("size", [v]) -> sizeOf v
  ("type", [v]) -> Right (VType (typeOf v))
  ("dyn", [v]) -> Right v
  -- Boolean
  ("bool", [VBool b]) -> Right (VBool b)
  ("bool", [VString s]) -> parseBoolText s
  -- Bytes
  ("bytes", [VBytes b]) -> Right (VBytes b)
  ("bytes", [VString s]) -> Right (VBytes (TE.encodeUtf8 s))
  -- Double
  ("double", [VDouble d]) -> Right (VDouble d)
  ("double", [VInt i]) -> Right (VDouble (fromIntegral i))
  ("double", [VUInt u]) -> Right (VDouble (fromIntegral u))
  ("double", [VString s]) -> parseDoubleText s
  -- Int
  ("int", [VInt i]) -> Right (VInt i)
  ("int", [VUInt u]) -> if toInteger u > i64max then Left (overflow "integer") else Right (VInt (fromIntegral u))
  ("int", [VDouble d]) -> doubleToInt d
  ("int", [VString s]) -> parseIntText s
  ("int", [VTimestamp t]) -> Right (VInt (tsSeconds t))
  -- Uint
  ("uint", [VUInt u]) -> Right (VUInt u)
  ("uint", [VInt i]) -> if i < 0 then Left (overflow "unsigned integer") else Right (VUInt (fromIntegral i))
  ("uint", [VDouble d]) -> doubleToUint d
  ("uint", [VString s]) -> parseUintText s
  -- String
  ("string", [VString s]) -> Right (VString s)
  ("string", [VBool b]) -> Right (VString (if b then "true" else "false"))
  ("string", [VInt i]) -> Right (VString (intToText i))
  ("string", [VUInt u]) -> Right (VString (intToText u))
  ("string", [VDouble d]) -> Right (VString (doubleToText d))
  ("string", [VBytes b]) -> bytesToString b
  ("string", [VTimestamp t]) -> Right (VString (formatTimestamp t))
  ("string", [VDuration d]) -> Right (VString (formatDuration (durationNanos d)))
  -- Duration / timestamp
  ("duration", [VDuration d]) -> Right (VDuration d)
  ("duration", [VString s]) -> parseDurationText s
  ("timestamp", [VTimestamp t]) -> Right (VTimestamp t)
  ("timestamp", [VString s]) -> parseTimestampText s
  ("timestamp", [VInt i]) -> mkTimestampNanos (toInteger i * 1000000000)
  -- String functions
  ("contains", [VString s, VString needle]) -> Right (VBool (needle `T.isInfixOf` s))
  ("startsWith", [VString s, VString p]) -> Right (VBool (p `T.isPrefixOf` s))
  ("endsWith", [VString s, VString p]) -> Right (VBool (p `T.isSuffixOf` s))
  ("contains", [VBytes s, VBytes needle]) -> Right (VBool (needle `BS.isInfixOf` s))
  ("startsWith", [VBytes s, VBytes p]) -> Right (VBool (p `BS.isPrefixOf` s))
  ("endsWith", [VBytes s, VBytes p]) -> Right (VBool (p `BS.isSuffixOf` s))
  ("matches", [VString s, VString p]) -> matchesRegex s p
  -- Timestamp accessors
  ("getFullYear", _) -> tsAccessor args (\(y, _, _, _, _, _, _, _) -> y)
  ("getMonth", _) -> tsAccessor args (\(_, mo, _, _, _, _, _, _) -> mo - 1)
  ("getDate", _) -> tsAccessor args (\(_, _, d, _, _, _, _, _) -> d)
  ("getDayOfMonth", _) -> tsAccessor args (\(_, _, d, _, _, _, _, _) -> d - 1)
  ("getDayOfYear", _) -> tsAccessor args (\(_, _, _, _, _, _, doy, _) -> doy)
  ("getDayOfWeek", _) -> tsAccessor args (\(_, _, _, _, _, _, _, dow) -> dow)
  ("getHours", [VDuration d]) -> Right (VInt (fromInteger (durationNanos d `quot` 3600000000000)))
  ("getMinutes", [VDuration d]) -> Right (VInt (fromInteger (durationNanos d `quot` 60000000000)))
  ("getSeconds", [VDuration d]) -> Right (VInt (fromInteger (durationNanos d `quot` 1000000000)))
  ("getMilliseconds", [VDuration d]) ->
    Right (VInt (fromInteger ((durationNanos d `quot` 1000000) `rem` 1000)))
  ("getHours", _) -> tsAccessor args (\(_, _, _, h, _, _, _, _) -> h)
  ("getMinutes", _) -> tsAccessor args (\(_, _, _, _, mi, _, _, _) -> mi)
  ("getSeconds", _) -> tsAccessor args (\(_, _, _, _, _, se, _, _) -> se)
  ("getMilliseconds", [VTimestamp t]) -> Right (VInt (fromIntegral (tsNanos t `div` 1000000)))
  _ -> Left (noOverload name)

----------------------------------------------------------------------
-- Conversions
----------------------------------------------------------------------

parseBoolText :: Text -> Either CelError Value
parseBoolText s
  | s `elem` ["true", "True", "TRUE", "t", "T", "1"] = Right (VBool True)
  | s `elem` ["false", "False", "FALSE", "f", "F", "0"] = Right (VBool False)
  | otherwise = Left (conversion ("cannot convert string to bool: " <> s))

parseDoubleText :: Text -> Either CelError Value
parseDoubleText s0 =
  let s = T.strip s0
   in case s of
        "Infinity" -> Right (VDouble (1 / 0))
        "+Infinity" -> Right (VDouble (1 / 0))
        "-Infinity" -> Right (VDouble (-1 / 0))
        "NaN" -> Right (VDouble (0 / 0))
        _ -> case parseDecimalRational s of
          Just r -> Right (VDouble (fromRational r))
          Nothing -> Left (conversion ("cannot convert string to double: " <> s0))

-- Parse a decimal string @[sign] digits [. digits] [(e|E) [sign] digits]@ into
-- an exact 'Rational', so the subsequent 'fromRational' is correctly rounded.
parseDecimalRational :: Text -> Maybe Rational
parseDecimalRational t0 =
  let (sign, t1) = case T.uncons t0 of
        Just ('-', r) -> (-1, r)
        Just ('+', r) -> (1, r)
        _ -> (1, t0)
      (intPart, t2) = T.span isDigit t1
      (fracPart, t3) = case T.uncons t2 of
        Just ('.', r) -> T.span isDigit r
        _ -> (T.empty, t2)
      (expVal, t4) = parseExp t3
   in if not (T.null intPart && T.null fracPart) && T.null t4
        then
          let digits = intPart <> fracPart
              mantissa = readIntegerText digits
              scale = toInteger (T.length fracPart)
              value = (mantissa % 1) * (10 ^^ (expVal - scale))
           in Just (sign * value)
        else Nothing
  where
    parseExp t = case T.uncons t of
      Just (e, r)
        | e == 'e' || e == 'E' ->
            let (s', r') = case T.uncons r of
                  Just ('-', rr) -> (-1, rr)
                  Just ('+', rr) -> (1, rr)
                  _ -> (1, r)
                (ds, r'') = T.span isDigit r'
             in if T.null ds then (0, t) else (s' * fromIntegral (readIntegerText ds), r'')
      _ -> (0, t)

readIntegerText :: Text -> Integer
readIntegerText t = T.foldl' (\acc c -> acc * 10 + toInteger (fromEnum c - fromEnum '0')) 0 t

parseIntText :: Text -> Either CelError Value
parseIntText s0 =
  let s = T.strip s0
   in case TR.signed TR.decimal s of
        Right (n, rest) | T.null rest -> mkInt n
        _ -> Left (conversion ("cannot convert string to int: " <> s0))

parseUintText :: Text -> Either CelError Value
parseUintText s0 =
  let s = T.strip s0
   in if T.isPrefixOf "-" s
        then Left (conversion ("cannot convert string to uint: " <> s0))
        else case TR.decimal s of
          Right (n, rest) | T.null rest -> mkUInt n
          _ -> Left (conversion ("cannot convert string to uint: " <> s0))

doubleToInt :: Double -> Either CelError Value
doubleToInt d
  | isNaN d || isInfinite d = Left (conversion "cannot convert double to int")
  -- The valid range is (minInt, maxInt) non-inclusive (the conservative
  -- round-trip bound from the spec), so exactly +-2^63 is also out of range.
  | d >= 9223372036854775808.0 = Left (overflow "integer")
  | d <= -9223372036854775808.0 = Left (overflow "integer")
  | otherwise = Right (VInt (truncate d))

doubleToUint :: Double -> Either CelError Value
doubleToUint d
  | isNaN d || isInfinite d = Left (conversion "cannot convert double to uint")
  | d < 0 = Left (overflow "unsigned integer")
  | d >= 18446744073709551616.0 = Left (overflow "unsigned integer")
  | otherwise = Right (VUInt (truncate d))

bytesToString :: BS.ByteString -> Either CelError Value
bytesToString b = case TE.decodeUtf8' b of
  Right t -> Right (VString t)
  Left _ -> Left (conversion "invalid UTF-8 in bytes to string conversion")

matchesRegex :: Text -> Text -> Either CelError Value
matchesRegex s p
  | T.null p = Right (VBool True) -- the empty pattern matches at any position
  | otherwise = case makeRegexM (T.unpack p) :: Maybe Regex of
      Nothing -> Left (invalidArg ("invalid regular expression: " <> p))
      Just re -> Right (VBool (matchTest re (T.unpack s)))

----------------------------------------------------------------------
-- Text formatting helpers
----------------------------------------------------------------------

intToText :: Integral a => a -> Text
intToText = TL.toStrict . TB.toLazyText . TBI.decimal

-- | Format a double the way reference CEL (Go's @strconv.FormatFloat(_, 'g',
-- -1, 64)@) does: shortest round-tripping decimal, switching to scientific
-- notation when the decimal exponent is @< -4@ or @>= 21@.
doubleToText :: Double -> Text
doubleToText d
  | isNaN d = "NaN"
  | isInfinite d = if d > 0 then "+Inf" else "-Inf"
  | d == 0 = if isNegativeZero d then "-0" else "0"
  | otherwise =
      let sign = if d < 0 then "-" else ""
          (ds, n) = floatToDigits 10 (abs d)
          digits = map intToDigit ds
          expo = n - 1 -- power of ten of the leading digit
       in T.pack (sign ++ if expo < -4 || expo >= 21 then sci digits expo else fixed digits n)
  where
    sci [] _ = "0"
    sci (d1 : rest) expo =
      let mant = d1 : (if null rest then "" else '.' : rest)
       in mant ++ "e" ++ expSign expo ++ pad2 (abs expo)
    expSign e = if e < 0 then "-" else "+"
    pad2 e = let s = show e in if length s < 2 then replicate (2 - length s) '0' ++ s else s
    fixed digits n
      | n <= 0 = "0." ++ replicate (negate n) '0' ++ digits
      | n >= length digits = digits ++ replicate (n - length digits) '0'
      | otherwise = let (a, b) = splitAt n digits in a ++ "." ++ b

----------------------------------------------------------------------
-- Duration parsing / formatting
----------------------------------------------------------------------

-- | Format a duration (given as total nanoseconds) as seconds with an @s@
-- suffix, e.g. @"60.001s"@.
formatDuration :: Integer -> Text
formatDuration total =
  let neg = total < 0
      a = abs total
      (secs, nanos) = a `divMod` 1000000000
      sign = if neg then "-" else ""
      secsT = intToText secs
      fracT = if nanos == 0 then "" else "." <> trimTrailingZeros (padLeft 9 (intToText nanos))
   in sign <> secsT <> fracT <> "s"

padLeft :: Int -> Text -> Text
padLeft n t = T.replicate (max 0 (n - T.length t)) "0" <> t

trimTrailingZeros :: Text -> Text
trimTrailingZeros = T.dropWhileEnd (== '0')

-- | Parse a CEL duration string such as @"1h30m"@, @"-1.5h"@, @"1h34us"@, or
-- @"0"@.
parseDurationText :: Text -> Either CelError Value
parseDurationText input0 =
  let input = T.strip input0
      (neg, rest) = case T.uncons input of
        Just ('-', r) -> (True, r)
        Just ('+', r) -> (False, r)
        _ -> (False, input)
   in if rest == "0"
        then Right (VDuration (Duration 0 0))
        else
          if T.null rest
            then Left (conversion ("invalid duration string: " <> input0))
            else do
              nanos <- parseUnits rest 0
              let signed = if neg then negate nanos else nanos
              mkDurationNanos signed

-- Parse one-or-more (number, unit) segments accumulating nanoseconds.
parseUnits :: Text -> Integer -> Either CelError Integer
parseUnits t acc
  | T.null t = Right acc
  | otherwise =
      let (numText, t1) = T.span (\c -> isDigit c || c == '.') t
          (unitText, t2) = T.span (\c -> c >= 'a' && c <= 'z') t1
       in if T.null numText
            then dErr
            else case unitNanos unitText of
              Nothing -> dErr
              Just per -> case parseNumberRational numText of
                Nothing -> dErr
                Just q -> parseUnits t2 (acc + roundRational (q * fromInteger per))
  where
    dErr = Left (conversion "invalid duration string")

unitNanos :: Text -> Maybe Integer
unitNanos u = case u of
  "h" -> Just 3600000000000
  "m" -> Just 60000000000
  "s" -> Just 1000000000
  "ms" -> Just 1000000
  "us" -> Just 1000
  "ns" -> Just 1
  _ -> Nothing

parseNumberRational :: Text -> Maybe Rational
parseNumberRational t = case TR.rational t of
  Right (q, rest) | T.null rest -> Just q
  _ -> Nothing

roundRational :: Rational -> Integer
roundRational = round

----------------------------------------------------------------------
-- Timestamp parsing / formatting / accessors
----------------------------------------------------------------------

formatTimestamp :: Timestamp -> Text
formatTimestamp (Timestamp secs nanos) =
  let utct = posixSecondsToUTCTime (fromIntegral secs)
      base = formatTime defaultTimeLocale "%04Y-%m-%dT%H:%M:%S" utct
      frac =
        if nanos == 0
          then ""
          else "." ++ T.unpack (trimTrailingZeros (padLeft 9 (intToText (toInteger nanos))))
   in T.pack (base ++ frac ++ "Z")

parseTimestampText :: Text -> Either CelError Value
parseTimestampText s =
  let str = T.unpack s
      tryFmt fmt = parseTimeM True defaultTimeLocale fmt str :: Maybe UTCTime
      parsed = firstJust (map tryFmt timestampFormats)
   in case parsed of
        Nothing -> Left (conversion ("cannot parse timestamp: " <> s))
        Just utct ->
          let posix = utcTimeToPOSIXSeconds utct
              r = toRational posix
              secs = floor r :: Integer
              nanos = round ((r - fromInteger secs) * 1000000000) :: Integer
              (secs', nanos') = if nanos >= 1000000000 then (secs + 1, nanos - 1000000000) else (secs, nanos)
           in if secs' < tsSecMin || secs' > tsSecMax
                then Left (overflow "timestamp")
                else Right (VTimestamp (Timestamp (fromInteger secs') (fromInteger nanos')))

timestampFormats :: [String]
timestampFormats =
  [ "%Y-%m-%dT%H:%M:%S%Q%Ez"
  , "%Y-%m-%dT%H:%M:%S%QZ"
  , "%Y-%m-%dT%H:%M:%S%Qz"
  ]

firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (Just x : _) = Just x
firstJust (Nothing : rest) = firstJust rest

-- (year, month, day, hour, minute, second, dayOfYear0, dayOfWeek0)
type TsParts = (Int, Int, Int, Int, Int, Int, Int, Int)

tsAccessor :: [Value] -> (TsParts -> Int) -> Either CelError Value
tsAccessor args sel = case args of
  [VTimestamp t] -> withParts t 0
  [VTimestamp t, VString tz] -> do
    off <- resolveOffset tz t
    withParts t off
  _ -> Left (noOverload "timestamp accessor")
  where
    withParts t off = Right (VInt (fromIntegral (sel (timestampParts t off))))

timestampParts :: Timestamp -> Int -> TsParts
timestampParts (Timestamp secs _) off =
  let utct = posixSecondsToUTCTime (fromIntegral (toInteger secs + toInteger off))
      day = utctDay utct
      (y, mo, d) = toGregorian day
      secsOfDay = fromInteger (diffTimeToPicoseconds (utctDayTime utct) `div` 1000000000000) :: Int
      hh = secsOfDay `div` 3600
      mm = (secsOfDay `div` 60) `mod` 60
      ss = secsOfDay `mod` 60
      (_, ord) = toOrdinalDate day
      dow = fromEnum (dayOfWeek day) `mod` 7
   in (fromInteger y, mo, d, hh, mm, ss, ord - 1, dow)

-- | Resolve a timezone to its offset in seconds /at the given instant/.
-- Supports @UTC@, the empty string (UTC), fixed @±HH:MM@ offsets (the sign is
-- optional), and named IANA/Joda zones (with their DST rules applied at the
-- instant in question) via the bundled zone database.
resolveOffset :: Text -> Timestamp -> Either CelError Int
resolveOffset tz ts
  | tz == "" || tz == "UTC" = Right 0
  | Just off <- fixedOffset tz = Right off
  | otherwise = case fromTZName (TE.encodeUtf8 tz) of
      Just label ->
        let zone = tzByLabel label
            utct = posixSecondsToUTCTime (fromIntegral (tsSeconds ts))
         in Right (timeZoneMinutes (timeZoneForUTCTime zone utct) * 60)
      Nothing -> Left (invalidArg ("unsupported timezone: " <> tz))

fixedOffset :: Text -> Maybe Int
fixedOffset t0 =
  let (neg, t) = case T.uncons t0 of
        Just ('+', r) -> (False, r)
        Just ('-', r) -> (True, r)
        _ -> (False, t0)
   in case T.splitOn ":" t of
        [hh, mm]
          | Right (h, hr) <- TR.decimal hh
          , T.null hr
          , Right (m, mr) <- TR.decimal mm
          , T.null mr ->
              let off = h * 3600 + m * 60
               in Just (if neg then negate off else off)
        _ -> Nothing
