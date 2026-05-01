-- | Iceberg predicate expressions and inclusive/strict bound evaluators.
--
-- This module mirrors the core Java @org.apache.iceberg.expressions@ API:
-- a small first-order predicate language whose terms are unbound or bound
-- references to schema fields, plus boolean connectives.
--
-- Two evaluators are exposed:
--
-- - 'evaluateInclusive' returns 'True' if the predicate /might/ match any
--   row in a file given column lower\/upper bounds and null counts.
-- - 'evaluateStrict' returns 'True' if the predicate /must/ match every row.
--
-- These are the building blocks needed for manifest pruning and projection
-- elimination.
module Iceberg.Expression
  ( Expression(..)
  , Predicate(..)
  , Operation(..)
  , Literal(..)
    -- * Construction helpers
  , equal
  , notEqual
  , lessThan
  , lessThanOrEq
  , greaterThan
  , greaterThanOrEq
  , isNull
  , notNull
  , isNan
  , notNan
  , inSet
  , notInSet
  , startsWith
  , notStartsWith
  , true
  , false
  , and_
  , or_
  , not_
    -- * Manifest pruning context
  , FileMetrics(..)
  , emptyFileMetrics
  , evaluateInclusive
  , evaluateStrict
  ) where

import Data.Bits (shiftR)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Int (Int32, Int64)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified GHC.Float as GF

import Iceberg.SingleValue (compareSingleValueBy)
import Iceberg.Types (IcebergType (..), Schema (..), StructField (..))

-- ============================================================
-- AST
-- ============================================================

data Operation
  = OpEq
  | OpNotEq
  | OpLt
  | OpLtEq
  | OpGt
  | OpGtEq
  | OpIsNull
  | OpNotNull
  | OpIsNan
  | OpNotNan
  | OpIn
  | OpNotIn
  | OpStartsWith
  | OpNotStartsWith
  deriving (Show, Eq)

data Literal
  = LBool      !Bool
  | LInt       !Int32
  | LLong      !Int64
  | LFloat     !Float
  | LDouble    !Double
  | LString    !Text
  | LBytes     !ByteString
  | LSet       !(Vector Literal)
  deriving (Show, Eq)

-- | Bound predicate: a comparison between a named column and a literal
-- (or set/null for unary operators).
data Predicate = Predicate
  { predOp    :: !Operation
  , predField :: !Text
  , predLits  :: !(Vector Literal)
    -- ^ Empty for null/nan checks, one for binary ops, n for IN/NOT IN.
  } deriving (Show, Eq)

data Expression
  = ETrue
  | EFalse
  | EPredicate !Predicate
  | EAnd !Expression !Expression
  | EOr !Expression !Expression
  | ENot !Expression
  deriving (Show, Eq)

-- ============================================================
-- Smart constructors
-- ============================================================

equal, notEqual, lessThan, lessThanOrEq, greaterThan, greaterThanOrEq
  :: Text -> Literal -> Expression
equal f l        = EPredicate (Predicate OpEq f (V.singleton l))
notEqual f l     = EPredicate (Predicate OpNotEq f (V.singleton l))
lessThan f l     = EPredicate (Predicate OpLt f (V.singleton l))
lessThanOrEq f l = EPredicate (Predicate OpLtEq f (V.singleton l))
greaterThan f l  = EPredicate (Predicate OpGt f (V.singleton l))
greaterThanOrEq f l = EPredicate (Predicate OpGtEq f (V.singleton l))

isNull, notNull, isNan, notNan :: Text -> Expression
isNull f  = EPredicate (Predicate OpIsNull f V.empty)
notNull f = EPredicate (Predicate OpNotNull f V.empty)
isNan f   = EPredicate (Predicate OpIsNan f V.empty)
notNan f  = EPredicate (Predicate OpNotNan f V.empty)

inSet, notInSet :: Text -> Vector Literal -> Expression
inSet    f xs = EPredicate (Predicate OpIn    f xs)
notInSet f xs = EPredicate (Predicate OpNotIn f xs)

startsWith, notStartsWith :: Text -> Text -> Expression
startsWith    f t = EPredicate (Predicate OpStartsWith    f (V.singleton (LString t)))
notStartsWith f t = EPredicate (Predicate OpNotStartsWith f (V.singleton (LString t)))

true, false :: Expression
true  = ETrue
false = EFalse

and_ :: Expression -> Expression -> Expression
and_ ETrue e   = e
and_ e ETrue   = e
and_ EFalse _  = EFalse
and_ _ EFalse  = EFalse
and_ a b       = EAnd a b

or_ :: Expression -> Expression -> Expression
or_ EFalse e  = e
or_ e EFalse  = e
or_ ETrue _   = ETrue
or_ _ ETrue   = ETrue
or_ a b       = EOr a b

not_ :: Expression -> Expression
not_ ETrue  = EFalse
not_ EFalse = ETrue
not_ (ENot e) = e
not_ e = ENot e

-- ============================================================
-- File metrics for evaluating against a manifest entry
-- ============================================================

data FileMetrics = FileMetrics
  { fmRecordCount  :: !Int64
  , fmValueCounts  :: !(Map Int Int64)
  , fmNullCounts   :: !(Map Int Int64)
  , fmNanCounts    :: !(Map Int Int64)
  , fmLowerBounds  :: !(Map Int ByteString)
  , fmUpperBounds  :: !(Map Int ByteString)
  } deriving (Show, Eq)

emptyFileMetrics :: Int64 -> FileMetrics
emptyFileMetrics n = FileMetrics
  { fmRecordCount = n
  , fmValueCounts = Map.empty
  , fmNullCounts  = Map.empty
  , fmNanCounts   = Map.empty
  , fmLowerBounds = Map.empty
  , fmUpperBounds = Map.empty
  }

-- | Inclusive evaluation: returns 'True' if the file might contain a row
-- that satisfies the predicate. The pruning-friendly direction.
evaluateInclusive :: Schema -> FileMetrics -> Expression -> Bool
evaluateInclusive schema fm = go
  where
    go ETrue           = True
    go EFalse          = False
    go (EAnd a b)      = go a && go b
    go (EOr a b)       = go a || go b
    go (ENot e)        = not (go e) -- conservative; over-approximates
    go (EPredicate p)  = inclusivePredicate schema fm p

-- | Strict evaluation: returns 'True' iff /every/ row in the file satisfies
-- the predicate. Useful for projection elimination ("if every row matches,
-- the predicate is redundant").
evaluateStrict :: Schema -> FileMetrics -> Expression -> Bool
evaluateStrict schema fm = go
  where
    go ETrue           = True
    go EFalse          = False
    go (EAnd a b)      = go a && go b
    go (EOr a b)       = go a || go b
    go (ENot e)        = not (evaluateInclusive schema fm e)
    go (EPredicate p)  = strictPredicate schema fm p

-- ============================================================
-- Predicate evaluation
-- ============================================================

inclusivePredicate :: Schema -> FileMetrics -> Predicate -> Bool
inclusivePredicate schema fm p = case lookupField schema (predField p) of
  Nothing -> True
  Just (fid, ty) -> case predOp p of
    OpIsNull   -> hasAnyNull fm fid
    OpNotNull  -> hasAnyNonNull fm fid
    OpIsNan    -> hasAnyNan fm fid
    OpNotNan   -> True
    OpEq       -> withLit p $ \lit -> rangeCovers ty fm fid lit lit
    OpNotEq    -> True
    OpLt       -> withLit p $ \lit -> rangeStartsBefore ty fm fid lit False
    OpLtEq     -> withLit p $ \lit -> rangeStartsBefore ty fm fid lit True
    OpGt       -> withLit p $ \lit -> rangeEndsAfter   ty fm fid lit False
    OpGtEq     -> withLit p $ \lit -> rangeEndsAfter   ty fm fid lit True
    OpStartsWith    -> withLit p $ \lit -> rangeMatchesPrefix ty fm fid lit
    OpNotStartsWith -> True
    OpIn       -> any (\lit -> rangeCovers ty fm fid lit lit) (V.toList (predLits p))
                  || V.null (predLits p)
    OpNotIn    -> True

strictPredicate :: Schema -> FileMetrics -> Predicate -> Bool
strictPredicate schema fm p = case lookupField schema (predField p) of
  Nothing -> False
  Just (fid, ty) -> case predOp p of
    OpIsNull   -> not (hasAnyNonNull fm fid)
    OpNotNull  -> not (hasAnyNull fm fid)
    OpEq       -> withLitFalse p $ \lit -> rangeIs ty fm fid lit
    OpLt       -> withLitFalse p $ \lit -> rangeStrictlyBelow ty fm fid lit False
    OpLtEq     -> withLitFalse p $ \lit -> rangeStrictlyBelow ty fm fid lit True
    OpGt       -> withLitFalse p $ \lit -> rangeStrictlyAbove ty fm fid lit False
    OpGtEq     -> withLitFalse p $ \lit -> rangeStrictlyAbove ty fm fid lit True
    OpStartsWith -> withLitFalse p $ \lit -> rangeAlwaysHasPrefix ty fm fid lit
    _          -> False

withLit :: Predicate -> (Literal -> Bool) -> Bool
withLit p f = case V.uncons (predLits p) of
  Just (lit, _) -> f lit
  Nothing       -> True

withLitFalse :: Predicate -> (Literal -> Bool) -> Bool
withLitFalse p f = case V.uncons (predLits p) of
  Just (lit, _) -> f lit
  Nothing       -> False

-- ============================================================
-- Field/range helpers
-- ============================================================

lookupField :: Schema -> Text -> Maybe (Int, IcebergType)
lookupField schema name =
  case V.find (\sf -> sfName sf == name) (schemaFields schema) of
    Just sf -> Just (sfId sf, sfType sf)
    Nothing -> Nothing

hasAnyNull :: FileMetrics -> Int -> Bool
hasAnyNull fm fid = case Map.lookup fid (fmNullCounts fm) of
  Just c -> c > 0
  Nothing -> True

hasAnyNonNull :: FileMetrics -> Int -> Bool
hasAnyNonNull fm fid =
  case (Map.lookup fid (fmNullCounts fm), Map.lookup fid (fmValueCounts fm)) of
    (Just nc, Just vc) -> nc < vc
    _                  -> True

hasAnyNan :: FileMetrics -> Int -> Bool
hasAnyNan fm fid = case Map.lookup fid (fmNanCounts fm) of
  Just c -> c > 0
  Nothing -> True

bounds :: FileMetrics -> Int -> (Maybe ByteString, Maybe ByteString)
bounds fm fid = (Map.lookup fid (fmLowerBounds fm), Map.lookup fid (fmUpperBounds fm))

literalBytes :: IcebergType -> Literal -> Maybe ByteString
literalBytes ty lit = case (ty, lit) of
  (TBoolean, LBool b)         -> Just (if b then "\1" else "\0")
  (TInt,     LInt n)          -> Just (intLE n)
  (TDate,    LInt n)          -> Just (intLE n)
  (TLong,    LLong n)         -> Just (longLE n)
  (TTimestamp, LLong n)       -> Just (longLE n)
  (TTimestampTz, LLong n)     -> Just (longLE n)
  (TTimestampNs, LLong n)     -> Just (longLE n)
  (TTimestampTzNs, LLong n)   -> Just (longLE n)
  (TTime, LLong n)            -> Just (longLE n)
  (TFloat, LFloat f)          -> Just (intLE (fromIntegral (GF.castFloatToWord32 f)))
  (TDouble, LDouble d)        -> Just (longLE (fromIntegral (GF.castDoubleToWord64 d)))
  (TString, LString t)        -> Just (TE.encodeUtf8 t)
  (TBinary, LBytes b)         -> Just b
  (TFixed _, LBytes b)        -> Just b
  (TUuid, LBytes b)           -> Just b
  _                            -> Nothing

intLE :: Int32 -> ByteString
intLE n = BS.pack [fromIntegral (fromIntegral n `shiftR` (8 * i) :: Int) | i <- [0 .. 3 :: Int]]

longLE :: Int64 -> ByteString
longLE n = BS.pack [fromIntegral (fromIntegral n `shiftR` (8 * i) :: Int) | i <- [0 .. 7 :: Int]]

rangeCovers :: IcebergType -> FileMetrics -> Int -> Literal -> Literal -> Bool
rangeCovers ty fm fid lo hi = fromMaybeTrue $ do
  let (mlb, mub) = bounds fm fid
  loB <- literalBytes ty lo
  hiB <- literalBytes ty hi
  case (mlb, mub) of
    (Just lb, Just ub) -> Just $
      compareTy ty lb hiB /= GT && compareTy ty ub loB /= LT
    _ -> Just True

rangeIs :: IcebergType -> FileMetrics -> Int -> Literal -> Bool
rangeIs ty fm fid lit = fromMaybeFalse $ do
  let (mlb, mub) = bounds fm fid
  litB <- literalBytes ty lit
  lb <- mlb
  ub <- mub
  Just (compareTy ty lb litB == EQ && compareTy ty ub litB == EQ)

rangeStartsBefore :: IcebergType -> FileMetrics -> Int -> Literal -> Bool {- inclusive? -} -> Bool
rangeStartsBefore ty fm fid lit inclusive = fromMaybeTrue $ do
  let (mlb, _) = bounds fm fid
  litB <- literalBytes ty lit
  lb <- mlb
  let c = compareTy ty lb litB
  Just $ if inclusive then c /= GT else c == LT

rangeEndsAfter :: IcebergType -> FileMetrics -> Int -> Literal -> Bool -> Bool
rangeEndsAfter ty fm fid lit inclusive = fromMaybeTrue $ do
  let (_, mub) = bounds fm fid
  litB <- literalBytes ty lit
  ub <- mub
  let c = compareTy ty ub litB
  Just $ if inclusive then c /= LT else c == GT

rangeStrictlyBelow :: IcebergType -> FileMetrics -> Int -> Literal -> Bool -> Bool
rangeStrictlyBelow ty fm fid lit inclusive = fromMaybeFalse $ do
  let (_, mub) = bounds fm fid
  litB <- literalBytes ty lit
  ub <- mub
  let c = compareTy ty ub litB
  Just $ if inclusive then c /= GT else c == LT

rangeStrictlyAbove :: IcebergType -> FileMetrics -> Int -> Literal -> Bool -> Bool
rangeStrictlyAbove ty fm fid lit inclusive = fromMaybeFalse $ do
  let (mlb, _) = bounds fm fid
  litB <- literalBytes ty lit
  lb <- mlb
  let c = compareTy ty lb litB
  Just $ if inclusive then c /= LT else c == GT

-- | The file's range overlaps the half-open prefix range
-- @[prefix, prefix + 1)@. Used for inclusive @startsWith@: returns 'True'
-- when the lower bound is &le; (prefix + max-byte) and upper bound is &ge;
-- prefix.
rangeMatchesPrefix :: IcebergType -> FileMetrics -> Int -> Literal -> Bool
rangeMatchesPrefix _ fm fid lit = fromMaybeTrue $ do
  let (mlb, mub) = bounds fm fid
  prefix <- literalRawBytes lit
  case (mlb, mub) of
    (Just lb, Just ub) -> Just $
      let lbPrefix = BS.take (BS.length prefix) lb
          ubPrefix = BS.take (BS.length prefix) ub
       in lbPrefix <= prefix && prefix <= ubPrefix
    _ -> Just True

-- | Both bounds start with the prefix &rArr; every row in the file does.
rangeAlwaysHasPrefix :: IcebergType -> FileMetrics -> Int -> Literal -> Bool
rangeAlwaysHasPrefix _ fm fid lit = fromMaybeFalse $ do
  let (mlb, mub) = bounds fm fid
  prefix <- literalRawBytes lit
  lb <- mlb
  ub <- mub
  Just $ BS.isPrefixOf prefix lb && BS.isPrefixOf prefix ub

literalRawBytes :: Literal -> Maybe ByteString
literalRawBytes (LString t) = Just (TE.encodeUtf8 t)
literalRawBytes (LBytes b)  = Just b
literalRawBytes _           = Nothing

compareTy :: IcebergType -> ByteString -> ByteString -> Ordering
compareTy ty a b = case compareSingleValueBy ty a b of
  Right o -> o
  Left _  -> compare a b

fromMaybeFalse :: Maybe Bool -> Bool
fromMaybeFalse = maybe False id

fromMaybeTrue :: Maybe Bool -> Bool
fromMaybeTrue = maybe True id
