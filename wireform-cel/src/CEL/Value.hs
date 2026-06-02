-- | Runtime value model for the Common Expression Language (CEL).
--
-- This module defines 'Value', the dynamically-typed value that a CEL
-- expression evaluates to, the 'CelType' tag returned by the @type()@
-- function, and the equality / ordering semantics mandated by the CEL
-- language definition.
--
-- The semantics implemented here follow
-- <https://github.com/google/cel-spec/blob/master/doc/langdef.md>:
--
--   * Numeric values (@int@, @uint@, @double@) are compared as though they
--     lie on a single continuous number line, so @1 == 1u == 1.0@ all hold
--     and ordering works across the numeric types
--     ('compareValues' / 'valueEq').
--   * @NaN@ never compares equal to anything (including itself) and is
--     unordered with respect to every value.
--   * Equality is heterogeneous: comparing two values of different,
--     non-numeric types yields @false@ rather than an error.
--   * @timestamp@ and @duration@ are the built-in abstract types backed by
--     @google.protobuf.Timestamp@ / @google.protobuf.Duration@.
module CEL.Value
  ( Value (..)
  , CelType (..)
  , Timestamp (..)
  , Duration (..)
  , CelMap
  , celMap
  , celMapFromList
  , celMapEntries
  , celMapLookup
  , celMapSize
  , typeOf
  , typeName
  , typeNameText
    -- * Equality and ordering
  , valueEq
  , compareValues
  , isNumeric
    -- * Numeric helpers
  , durationNanos
  , timestampNanos
  ) where

import Control.DeepSeq (NFData (..))
import Data.ByteString (ByteString)
import Data.Int (Int32, Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word64)
import GHC.Generics (Generic)

-- | A @google.protobuf.Duration@: a signed span of seconds plus nanoseconds.
-- The nanosecond component carries the same sign as 'durSeconds' for non-zero
-- values (matching the protobuf normalization rules).
data Duration = Duration
  { durSeconds :: {-# UNPACK #-} !Int64
  , durNanos   :: {-# UNPACK #-} !Int32
  }
  deriving stock (Show, Generic)

instance NFData Duration

-- | A @google.protobuf.Timestamp@: seconds since the Unix epoch plus a
-- non-negative nanosecond component in @[0, 1e9)@.
data Timestamp = Timestamp
  { tsSeconds :: {-# UNPACK #-} !Int64
  , tsNanos   :: {-# UNPACK #-} !Int32
  }
  deriving stock (Show, Generic)

instance NFData Timestamp

-- | Total nanoseconds represented by a duration.
durationNanos :: Duration -> Integer
durationNanos (Duration s n) = fromIntegral s * 1000000000 + fromIntegral n

-- | Total nanoseconds since the Unix epoch represented by a timestamp.
timestampNanos :: Timestamp -> Integer
timestampNanos (Timestamp s n) = fromIntegral s * 1000000000 + fromIntegral n

instance Eq Duration where
  a == b = durationNanos a == durationNanos b

instance Ord Duration where
  compare a b = compare (durationNanos a) (durationNanos b)

instance Eq Timestamp where
  a == b = timestampNanos a == timestampNanos b

instance Ord Timestamp where
  compare a b = compare (timestampNanos a) (timestampNanos b)

-- | The runtime type of a CEL value, as returned by the @type()@ function.
-- 'TyMessage' carries the fully-qualified name of an opaque/abstract type
-- (currently only used for the @type@ values of the well-known abstract
-- types).
data CelType
  = TyNull
  | TyBool
  | TyInt
  | TyUInt
  | TyDouble
  | TyString
  | TyBytes
  | TyList
  | TyMap
  | TyType
  | TyTimestamp
  | TyDuration
  | TyMessage !Text
  deriving stock (Eq, Show, Generic)

instance NFData CelType

-- | The canonical CEL name of a type, e.g. @"int"@, @"null_type"@,
-- @"google.protobuf.Timestamp"@.
typeNameText :: CelType -> Text
typeNameText = \case
  TyNull -> "null_type"
  TyBool -> "bool"
  TyInt -> "int"
  TyUInt -> "uint"
  TyDouble -> "double"
  TyString -> "string"
  TyBytes -> "bytes"
  TyList -> "list"
  TyMap -> "map"
  TyType -> "type"
  TyTimestamp -> "google.protobuf.Timestamp"
  TyDuration -> "google.protobuf.Duration"
  TyMessage n -> n

-- | Alias for 'typeNameText'.
typeName :: CelType -> Text
typeName = typeNameText

-- | A CEL value.
--
-- Maps preserve insertion order and reject duplicate keys at construction
-- time (see 'celMap'). Map keys are restricted by the language to @int@,
-- @uint@, @bool@, and @string@, but the representation does not enforce that
-- invariant; the evaluator does when constructing map literals.
data Value
  = VNull
  | VBool      !Bool
  | VInt       {-# UNPACK #-} !Int64
  | VUInt      {-# UNPACK #-} !Word64
  | VDouble    {-# UNPACK #-} !Double
  | VString    !Text
  | VBytes     !ByteString
  | VList      !(Vector Value)
  | VMap       !CelMap
  | VType      !CelType
  | VTimestamp !Timestamp
  | VDuration  !Duration
  deriving stock (Show, Generic)

instance NFData Value

-- | An insertion-ordered association of CEL keys to CEL values with unique
-- keys (uniqueness determined by 'valueEq', so @1@ and @1u@ are the same key).
newtype CelMap = CelMap { celMapEntries :: [(Value, Value)] }
  deriving stock (Show, Generic)

instance NFData CelMap

-- | Number of entries in a map.
celMapSize :: CelMap -> Int
celMapSize (CelMap es) = length es

-- | Build a map from insertion-ordered entries, returning 'Left' on a
-- duplicate key (per CEL semantics, duplicate map keys are an error).
celMap :: [(Value, Value)] -> Either Text CelMap
celMap = go []
  where
    go acc [] = Right (CelMap (reverse acc))
    go acc ((k, v) : rest)
      | any (\(k', _) -> valueEq k k') acc =
          Left ("duplicate key in map: " <> T.pack (show k))
      | otherwise = go ((k, v) : acc) rest

-- | Build a map from entries, keeping the last value for duplicate keys.
-- Used where the caller has already validated uniqueness.
celMapFromList :: [(Value, Value)] -> CelMap
celMapFromList = CelMap

-- | Look up a key in a map using CEL key equality (numeric cross-type aware).
celMapLookup :: Value -> CelMap -> Maybe Value
celMapLookup k (CelMap es) = go es
  where
    go [] = Nothing
    go ((k', v) : rest)
      | valueEq k k' = Just v
      | otherwise = go rest

-- | The runtime 'CelType' of a value.
typeOf :: Value -> CelType
typeOf = \case
  VNull -> TyNull
  VBool _ -> TyBool
  VInt _ -> TyInt
  VUInt _ -> TyUInt
  VDouble _ -> TyDouble
  VString _ -> TyString
  VBytes _ -> TyBytes
  VList _ -> TyList
  VMap _ -> TyMap
  VType _ -> TyType
  VTimestamp _ -> TyTimestamp
  VDuration _ -> TyDuration

instance Eq Value where
  (==) = valueEq

-- | Is this value one of the numeric types (@int@, @uint@, @double@)?
isNumeric :: Value -> Bool
isNumeric = \case
  VInt _ -> True
  VUInt _ -> True
  VDouble _ -> True
  _ -> False

-- Internal three-way numeric representation for cross-type comparison.
data Num3 = NI !Int64 | NU !Word64 | ND !Double

toNum :: Value -> Maybe Num3
toNum = \case
  VInt i -> Just (NI i)
  VUInt u -> Just (NU u)
  VDouble d -> Just (ND d)
  _ -> Nothing

invert :: Ordering -> Ordering
invert LT = GT
invert GT = LT
invert EQ = EQ

-- | Compare two numbers on the shared number line. 'Nothing' indicates the
-- comparison is undefined, which only happens when a @NaN@ is involved.
cmpNum :: Num3 -> Num3 -> Maybe Ordering
cmpNum a b = case (a, b) of
  (NI x, NI y) -> Just (compare x y)
  (NU x, NU y) -> Just (compare x y)
  (ND x, ND y)
    | isNaN x || isNaN y -> Nothing
    | otherwise -> Just (compare x y)
  (NI x, NU y) -> Just (cmpIntUint x y)
  (NU x, NI y) -> Just (invert (cmpIntUint y x))
  (NI x, ND y) -> cmpIntDouble x y
  (ND x, NI y) -> invert <$> cmpIntDouble y x
  (NU x, ND y) -> cmpUintDouble x y
  (ND x, NU y) -> invert <$> cmpUintDouble y x

cmpIntUint :: Int64 -> Word64 -> Ordering
cmpIntUint x y
  | x < 0 = LT
  | otherwise = compare (fromIntegral x :: Word64) y

-- Cross-type comparison follows reference CEL (cel-go): the value with the
-- larger magnitude type is bounds-checked first, then the integer is converted
-- to 'Double' and compared. This is intentionally lossy at the extreme ends of
-- the @int64@ / @uint64@ range (e.g. @9223372036854775807@ compares equal to
-- @9223372036854775808.0@), matching every conformant runtime.
cmpIntDouble :: Int64 -> Double -> Maybe Ordering
cmpIntDouble x y
  | isNaN y = Nothing
  | y < fromIntegral (minBound :: Int64) = Just GT
  | y > fromIntegral (maxBound :: Int64) = Just LT
  | otherwise = Just (compare (fromIntegral x) y)

cmpUintDouble :: Word64 -> Double -> Maybe Ordering
cmpUintDouble x y
  | isNaN y = Nothing
  | y < 0 = Just GT
  | y > fromIntegral (maxBound :: Word64) = Just LT
  | otherwise = Just (compare (fromIntegral x) y)

-- | Heterogeneous CEL equality. Always total: differing non-numeric types
-- compare unequal rather than erroring. @NaN@ is unequal to everything.
valueEq :: Value -> Value -> Bool
valueEq a b
  | Just x <- toNum a, Just y <- toNum b = cmpNum x y == Just EQ
valueEq a b = case (a, b) of
  (VNull, VNull) -> True
  (VBool x, VBool y) -> x == y
  (VString x, VString y) -> x == y
  (VBytes x, VBytes y) -> x == y
  (VTimestamp x, VTimestamp y) -> x == y
  (VDuration x, VDuration y) -> x == y
  (VType x, VType y) -> typeNameText x == typeNameText y
  (VList x, VList y) ->
    V.length x == V.length y && V.and (V.zipWith valueEq x y)
  (VMap x, VMap y) -> mapEq x y
  _ -> False

mapEq :: CelMap -> CelMap -> Bool
mapEq (CelMap xs) my@(CelMap ys) =
  length xs == length ys && all entryMatches xs
  where
    entryMatches (k, v) = case celMapLookup k my of
      Just v' -> valueEq v v'
      Nothing -> False

-- | Compare two values for ordering. 'Nothing' is returned when the values
-- are not comparable: either they have incompatible (non-numeric, different)
-- types, or a @NaN@ is involved. Callers distinguish "no overload" from
-- "unordered @NaN@" using 'isNumeric' and type checks.
compareValues :: Value -> Value -> Maybe Ordering
compareValues a b
  | Just x <- toNum a, Just y <- toNum b = cmpNum x y
compareValues a b = case (a, b) of
  (VBool x, VBool y) -> Just (compare x y)
  (VString x, VString y) -> Just (compare x y)
  (VBytes x, VBytes y) -> Just (compare x y)
  (VTimestamp x, VTimestamp y) -> Just (compare x y)
  (VDuration x, VDuration y) -> Just (compare x y)
  _ -> Nothing
