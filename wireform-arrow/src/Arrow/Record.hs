{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | @hasql@-shaped encoder / decoder combinators for Arrow's
-- columnar data model.
--
-- Four complementary abstractions:
--
-- * 'Encoder' @a@ — column-level encoder, 'Contravariant'.
--   Primitives 'int32E', 'utf8E', 'boolE', … reshape with
--   @'contramap' :: (a -> b) -> Encoder b -> Encoder a@ and
--   @'nullable' :: Encoder a -> Encoder (Maybe a)@.
--
-- * 'Decoder' @a@ — column-level decoder, 'Functor'. Mirror set
--   of primitives ('int32D', …) with @'nullableD'@.
--
-- * 'RowEncoder' @r@ — record-level encoder. Combine
--   'fieldE' calls via 'Semigroup' @<>@.
--
-- * 'RowDecoder' @r@ — 'Applicative' row decoder. Build with
--   @<$>@ + @<*>@ + 'columnD'.
--
-- * 'Table' @r@ — pairs the two for round-trip use.
--
-- == Example
--
-- @
-- data Trade = Trade { sym :: Text, qty :: Int32, note :: Maybe Text }
--
-- tradeTable :: 'Table' Trade
-- tradeTable = 'table' enc dec
--   where
--     enc = 'fieldE' "sym"  sym  'utf8E'
--        <> 'fieldE' "qty"  qty  'int32E'
--        <> 'fieldE' "note" note ('nullable' 'utf8E')
--     dec = Trade
--         \<$\> 'columnD' "sym"  'utf8D'
--         \<*\> 'columnD' "qty"  'int32D'
--         \<*\> 'columnD' "note" ('nullableD' 'utf8D')
--
-- encoded = 'encodeTable' tradeTable tradesVec
-- @
module Arrow.Record
  ( -- * Column-level encoder
    Encoder
  , encoderType
  , encoderNullable
  , runEncoder
  , contramapE
  , nullable
    -- ** Primitive encoders
  , int8E, int16E, int32E, int64E
  , word8E, word16E, word32E, word64E
  , floatE, doubleE
  , boolE
  , utf8E
  , binaryE
  , date32E
  , timestampE
    -- * Column-level decoder
  , Decoder
  , decoderType
  , runDecoder
  , nullableD
    -- ** Primitive decoders
  , int8D, int16D, int32D, int64D
  , word8D, word16D, word32D, word64D
  , floatD, doubleD
  , boolD
  , utf8D
  , binaryD
  , date32D
  , timestampD
    -- * Row-level encoder
  , RowEncoder
  , rowEncoderFields
  , runRowEncoder
  , fieldE
    -- * Row-level decoder
  , RowDecoder
  , runRowDecoder
  , rowDecoderRequiredColumns
  , columnD
    -- * Table (round-trip pair)
  , Table (..)
  , table
  , tableSchema
  , tableRequiredColumns
  , encodeTable
  , decodeTable
  ) where

import Data.ByteString (ByteString)
import Data.Functor.Contravariant (Contravariant (..))
import Data.Int (Int8, Int16, Int32, Int64)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP
import Data.Word (Word8, Word16, Word32, Word64)

import Arrow.Column (ColumnArray (..))
import Arrow.Types
  ( ArrowType (..)
  , DateUnit (..)
  , Endianness (..)
  , Field (..)
  , Precision (..)
  , Schema (..)
  , TimeUnit (..)
  )

-- ============================================================
-- Encoder
-- ============================================================

-- | Serialises a vector of Haskell values as an Arrow column.
--
-- Pairs the Arrow type (used to populate the schema) with two
-- column-builder functions: one for required input, one for
-- 'Maybe'-wrapped input. Primitive encoders fill both;
-- 'contramap' threads a projection through both so a derived
-- encoder remains liftable via 'nullable'.
data Encoder a = Encoder
  { encoderType        :: !ArrowType
  , encoderNullable    :: !Bool
  , encoderRequired    :: !(V.Vector a -> ColumnArray)
  , encoderOptional    :: !(V.Vector (Maybe a) -> ColumnArray)
  }

-- | Encode a non-nullable column.
runEncoder :: Encoder a -> V.Vector a -> ColumnArray
runEncoder = encoderRequired

instance Contravariant Encoder where
  contramap f (Encoder ty nu req opt) = Encoder
    { encoderType      = ty
    , encoderNullable  = nu
    , encoderRequired  = req . V.map f
    , encoderOptional  = opt . V.map (fmap f)
    }

-- | Alias for 'contramap' that reads more naturally at call
-- sites.
contramapE :: (a -> b) -> Encoder b -> Encoder a
contramapE = contramap

-- | Lift an encoder to build a nullable Arrow column. The
-- inner encoder's @Maybe@-builder becomes the new required
-- builder; a second 'nullable' wrap is rejected at runtime
-- since Arrow has no nested-null representation.
nullable :: Encoder a -> Encoder (Maybe a)
nullable e = Encoder
  { encoderType      = encoderType e
  , encoderNullable  = True
  , encoderRequired  = encoderOptional e
  , encoderOptional  = \_ ->
      error "Arrow.Record.nullable: Arrow has no nested-null \
            \representation; don't wrap 'nullable' twice"
  }

-- Internal: primitive-encoder constructor.
mkE
  :: ArrowType
  -> (V.Vector a -> ColumnArray)
  -> (V.Vector (Maybe a) -> ColumnArray)
  -> Encoder a
mkE ty req opt = Encoder ty False req opt

-- ============================================================
-- Primitive encoders
-- ============================================================

int8E :: Encoder Int8
int8E = mkE (AInt 8 True) (ColInt8 . VP.convert) ColInt8Maybe

int16E :: Encoder Int16
int16E = mkE (AInt 16 True) (ColInt16 . VP.convert) ColInt16Maybe

int32E :: Encoder Int32
int32E = mkE (AInt 32 True) (ColInt32 . VP.convert) ColInt32Maybe

int64E :: Encoder Int64
int64E = mkE (AInt 64 True) (ColInt64 . VP.convert) ColInt64Maybe

word8E :: Encoder Word8
word8E = mkE (AInt 8 False) (ColUInt8 . VP.convert) ColUInt8Maybe

word16E :: Encoder Word16
word16E = mkE (AInt 16 False) (ColUInt16 . VP.convert) ColUInt16Maybe

word32E :: Encoder Word32
word32E = mkE (AInt 32 False) (ColUInt32 . VP.convert) ColUInt32Maybe

word64E :: Encoder Word64
word64E = mkE (AInt 64 False) (ColUInt64 . VP.convert) ColUInt64Maybe

floatE :: Encoder Float
floatE = mkE (AFloatingPoint Single) (ColFloat . VP.convert) ColFloatMaybe

doubleE :: Encoder Double
doubleE = mkE (AFloatingPoint DoublePrecision) (ColDouble . VP.convert) ColDoubleMaybe

boolE :: Encoder Bool
boolE = mkE ABool ColBool ColBoolMaybe

utf8E :: Encoder Text
utf8E = mkE AUtf8 ColUtf8 ColUtf8Maybe

binaryE :: Encoder ByteString
binaryE = mkE ABinary ColBinary ColBinaryMaybe

-- | Days since Unix epoch (INT32). Arrow logical @Date(DateDay)@.
date32E :: Encoder Int32
date32E = mkE (ADate DateDay) (ColDate32 . VP.convert) ColDate32Maybe

-- | Microseconds since Unix epoch (INT64, no timezone). Arrow
-- logical @Timestamp(Microsecond, None)@.
timestampE :: Encoder Int64
timestampE = mkE (ATimestamp Microsecond Nothing) (ColTimestamp . VP.convert) ColTimestampMaybe

-- ============================================================
-- Decoder
-- ============================================================

-- | Materialises a Haskell vector from an Arrow column.
--
-- The decoder stores two extractors: one for the non-nullable
-- 'ColumnArray' shape its 'decoderType' advertises, one for the
-- matching @Col*Maybe@. 'nullableD' flips to the second path so
-- 'fmap' composes through both in lockstep.
data Decoder a = Decoder
  { decoderType      :: !ArrowType
  , decoderRequired  :: !(ColumnArray -> Either String (V.Vector a))
  , decoderOptional  :: !(ColumnArray -> Either String (V.Vector (Maybe a)))
  }

instance Functor Decoder where
  fmap f (Decoder ty req opt) = Decoder
    { decoderType     = ty
    , decoderRequired = fmap (V.map f) . req
    , decoderOptional = fmap (V.map (fmap f)) . opt
    }

-- | Decode a non-nullable column into a vector of values.
runDecoder :: Decoder a -> ColumnArray -> Either String (V.Vector a)
runDecoder = decoderRequired

-- | Lift a 'Decoder' to recognise nullable columns. Wrapping
-- twice is a runtime error — Arrow has no nested-null
-- representation.
nullableD :: Decoder a -> Decoder (Maybe a)
nullableD d = Decoder
  { decoderType     = decoderType d
  , decoderRequired = decoderOptional d
  , decoderOptional = \_ ->
      Left "Arrow.Record.nullableD: Arrow has no nested-null \
            \representation; don't wrap 'nullableD' twice"
  }

-- Internal: primitive-decoder constructor.
mkD
  :: ArrowType
  -> (ColumnArray -> Either String (V.Vector a))
  -> (ColumnArray -> Either String (V.Vector (Maybe a)))
  -> Decoder a
mkD = Decoder

-- ============================================================
-- Primitive decoders
-- ============================================================

int8D :: Decoder Int8
int8D = mkD (AInt 8 True)
  (expectCol "ColInt8"      $ \case ColInt8      v -> Right (VP.convert v); o -> expectErr "ColInt8" o)
  (expectCol "ColInt8Maybe" $ \case ColInt8Maybe v -> Right v;              o -> expectErr "ColInt8Maybe" o)

int16D :: Decoder Int16
int16D = mkD (AInt 16 True)
  (expectCol "ColInt16"      $ \case ColInt16      v -> Right (VP.convert v); o -> expectErr "ColInt16" o)
  (expectCol "ColInt16Maybe" $ \case ColInt16Maybe v -> Right v;               o -> expectErr "ColInt16Maybe" o)

int32D :: Decoder Int32
int32D = mkD (AInt 32 True)
  (expectCol "ColInt32"      $ \case ColInt32      v -> Right (VP.convert v); o -> expectErr "ColInt32" o)
  (expectCol "ColInt32Maybe" $ \case ColInt32Maybe v -> Right v;               o -> expectErr "ColInt32Maybe" o)

int64D :: Decoder Int64
int64D = mkD (AInt 64 True)
  (expectCol "ColInt64"      $ \case ColInt64      v -> Right (VP.convert v); o -> expectErr "ColInt64" o)
  (expectCol "ColInt64Maybe" $ \case ColInt64Maybe v -> Right v;               o -> expectErr "ColInt64Maybe" o)

word8D :: Decoder Word8
word8D = mkD (AInt 8 False)
  (expectCol "ColUInt8"      $ \case ColUInt8      v -> Right (VP.convert v); o -> expectErr "ColUInt8" o)
  (expectCol "ColUInt8Maybe" $ \case ColUInt8Maybe v -> Right v;               o -> expectErr "ColUInt8Maybe" o)

word16D :: Decoder Word16
word16D = mkD (AInt 16 False)
  (expectCol "ColUInt16"      $ \case ColUInt16      v -> Right (VP.convert v); o -> expectErr "ColUInt16" o)
  (expectCol "ColUInt16Maybe" $ \case ColUInt16Maybe v -> Right v;               o -> expectErr "ColUInt16Maybe" o)

word32D :: Decoder Word32
word32D = mkD (AInt 32 False)
  (expectCol "ColUInt32"      $ \case ColUInt32      v -> Right (VP.convert v); o -> expectErr "ColUInt32" o)
  (expectCol "ColUInt32Maybe" $ \case ColUInt32Maybe v -> Right v;               o -> expectErr "ColUInt32Maybe" o)

word64D :: Decoder Word64
word64D = mkD (AInt 64 False)
  (expectCol "ColUInt64"      $ \case ColUInt64      v -> Right (VP.convert v); o -> expectErr "ColUInt64" o)
  (expectCol "ColUInt64Maybe" $ \case ColUInt64Maybe v -> Right v;               o -> expectErr "ColUInt64Maybe" o)

floatD :: Decoder Float
floatD = mkD (AFloatingPoint Single)
  (expectCol "ColFloat"      $ \case ColFloat      v -> Right (VP.convert v); o -> expectErr "ColFloat" o)
  (expectCol "ColFloatMaybe" $ \case ColFloatMaybe v -> Right v;               o -> expectErr "ColFloatMaybe" o)

doubleD :: Decoder Double
doubleD = mkD (AFloatingPoint DoublePrecision)
  (expectCol "ColDouble"      $ \case ColDouble      v -> Right (VP.convert v); o -> expectErr "ColDouble" o)
  (expectCol "ColDoubleMaybe" $ \case ColDoubleMaybe v -> Right v;               o -> expectErr "ColDoubleMaybe" o)

boolD :: Decoder Bool
boolD = mkD ABool
  (expectCol "ColBool"      $ \case ColBool      v -> Right v; o -> expectErr "ColBool" o)
  (expectCol "ColBoolMaybe" $ \case ColBoolMaybe v -> Right v; o -> expectErr "ColBoolMaybe" o)

utf8D :: Decoder Text
utf8D = mkD AUtf8
  (expectCol "ColUtf8"      $ \case ColUtf8      v -> Right v; o -> expectErr "ColUtf8" o)
  (expectCol "ColUtf8Maybe" $ \case ColUtf8Maybe v -> Right v; o -> expectErr "ColUtf8Maybe" o)

binaryD :: Decoder ByteString
binaryD = mkD ABinary
  (expectCol "ColBinary"      $ \case ColBinary      v -> Right v; o -> expectErr "ColBinary" o)
  (expectCol "ColBinaryMaybe" $ \case ColBinaryMaybe v -> Right v; o -> expectErr "ColBinaryMaybe" o)

date32D :: Decoder Int32
date32D = mkD (ADate DateDay)
  (expectCol "ColDate32"      $ \case ColDate32      v -> Right (VP.convert v); o -> expectErr "ColDate32" o)
  (expectCol "ColDate32Maybe" $ \case ColDate32Maybe v -> Right v;               o -> expectErr "ColDate32Maybe" o)

timestampD :: Decoder Int64
timestampD = mkD (ATimestamp Microsecond Nothing)
  (expectCol "ColTimestamp"      $ \case ColTimestamp      v -> Right (VP.convert v); o -> expectErr "ColTimestamp" o)
  (expectCol "ColTimestampMaybe" $ \case ColTimestampMaybe v -> Right v;               o -> expectErr "ColTimestampMaybe" o)

-- Internal helpers shared by every primitive decoder.
expectCol :: String -> (ColumnArray -> Either String b) -> ColumnArray -> Either String b
expectCol _ k = k

expectErr :: String -> ColumnArray -> Either String a
expectErr want got =
  Left $ "Arrow.Record: expected " ++ want ++ ", got " ++ colTag got

colTag :: ColumnArray -> String
colTag = takeWhile (/= ' ') . show

-- ============================================================
-- RowEncoder
-- ============================================================

-- | A record-level encoder: produces a 'V.Vector ColumnArray' +
-- its 'Field' list from a 'V.Vector' of records.
--
-- 'RowEncoder' is 'Contravariant' and a 'Semigroup' / 'Monoid'.
-- Combine 'fieldE' calls with @<>@:
--
-- @
-- enc = 'fieldE' "sym" sym utf8E <> 'fieldE' "qty" qty int32E
-- @
data RowEncoder r = RowEncoder
  { rowEncoderFields :: ![Field]
    -- ^ 'Field' entries in declaration order.
  , runRowEncoder    :: !(V.Vector r -> [ColumnArray])
    -- ^ One 'ColumnArray' per field, parallel to
    -- 'rowEncoderFields'.
  }

instance Contravariant RowEncoder where
  contramap f (RowEncoder flds run) =
    RowEncoder flds (run . V.map f)

instance Semigroup (RowEncoder r) where
  RowEncoder fl rl <> RowEncoder fr rr =
    RowEncoder (fl ++ fr) (\v -> rl v ++ rr v)

instance Monoid (RowEncoder r) where
  mempty = RowEncoder [] (const [])

-- | Build a 'RowEncoder' for a single field: name + selector +
-- column encoder.
--
-- @
-- fieldE "sym" tradeSym utf8E  :: RowEncoder Trade
-- @
fieldE :: Text -> (r -> a) -> Encoder a -> RowEncoder r
fieldE name sel enc = RowEncoder
  { rowEncoderFields = [Field
      { fieldName       = name
      , fieldNullable   = encoderNullable enc
      , fieldType       = encoderType enc
      , fieldChildren   = V.empty
      , fieldDictionary = Nothing
      }]
  , runRowEncoder = \rs -> [runEncoder enc (V.map sel rs)]
  }

-- ============================================================
-- RowDecoder
-- ============================================================

-- | Row decoder. Looks up named columns in a
-- 'V.Vector ColumnArray' (keyed by the schema's field names) and
-- runs the matching 'Decoder' on each.
--
-- 'RowDecoder' is an 'Applicative': combine several 'columnD'
-- calls with @<$>@ + @<*>@ to build a record.
data RowDecoder r = RowDecoder
  { rowDecoderRequiredColumns :: ![Text]
    -- ^ Names of columns the decoder consults when run.
    -- Order matches first appearance in the applicative chain;
    -- duplicates removed. Useful for column projection: a
    -- caller can ask the source format to only materialise
    -- these columns rather than the whole record batch.
  , runRowDecoder :: !(V.Vector Field -> V.Vector ColumnArray -> Either String (V.Vector r))
  }

instance Functor RowDecoder where
  fmap f (RowDecoder cs0 run) = RowDecoder cs0 $ \fs cs ->
    V.map f <$> run fs cs

instance Applicative RowDecoder where
  pure x = RowDecoder [] $ \_fs cs ->
    -- Length comes from the first column; an empty batch yields
    -- V.empty. If callers need a fixed row count with no
    -- columns they can rely on 'pure' inside an outer
    -- 'liftA2'-chained RowDecoder that has at least one column.
    let !n = if V.null cs then 0 else columnLen (V.head cs)
    in  Right (V.replicate n x)
  RowDecoder cF runF <*> RowDecoder cX runX = RowDecoder
    (mergeRequired cF cX) $ \fs cs -> do
      fvec <- runF fs cs
      xvec <- runX fs cs
      if V.length fvec /= V.length xvec
        then Left $ "Arrow.Record.<*>: column length mismatch ("
                    ++ show (V.length fvec) ++ " vs " ++ show (V.length xvec) ++ ")"
        else Right (V.zipWith ($) fvec xvec)

-- | Order-preserving union of two 'rowDecoderRequiredColumns'
-- lists. Used by the Applicative instance to maintain the
-- "first appearance" order as decoders are combined.
mergeRequired :: [Text] -> [Text] -> [Text]
mergeRequired xs ys = xs ++ filter (`notElem` xs) ys

-- | Decode the named column via the supplied 'Decoder'. Looks
-- the column up by 'Field' name in the schema the caller passes
-- to 'runRowDecoder'; returns 'Left' if the name isn't present.
columnD :: Text -> Decoder a -> RowDecoder a
columnD name d = RowDecoder [name] $ \fs cs -> do
  idx <- case V.findIndex ((== name) . fieldName) fs of
    Just i  -> Right i
    Nothing -> Left $ "Arrow.Record.columnD: no column named " ++ show name
  let !col = V.unsafeIndex cs idx
  case runDecoder d col of
    Right vs -> Right vs
    Left  e  -> Left $ "Arrow.Record.columnD " ++ show name ++ ": " ++ e

-- Tiny helper: vector-length accessor that works on all
-- ColumnArray shapes. We re-implement here to avoid pulling in
-- Arrow.Column.columnLength's bigger definition (its
-- dependency closure is heavier).
columnLen :: ColumnArray -> Int
columnLen = \case
  ColInt8 v     -> VP.length v
  ColInt16 v    -> VP.length v
  ColInt32 v    -> VP.length v
  ColInt64 v    -> VP.length v
  ColUInt8 v    -> VP.length v
  ColUInt16 v   -> VP.length v
  ColUInt32 v   -> VP.length v
  ColUInt64 v   -> VP.length v
  ColFloat v    -> VP.length v
  ColDouble v   -> VP.length v
  ColBool v     -> V.length v
  ColUtf8 v     -> V.length v
  ColBinary v   -> V.length v
  ColInt8Maybe v    -> V.length v
  ColInt16Maybe v   -> V.length v
  ColInt32Maybe v   -> V.length v
  ColInt64Maybe v   -> V.length v
  ColUInt8Maybe v   -> V.length v
  ColUInt16Maybe v  -> V.length v
  ColUInt32Maybe v  -> V.length v
  ColUInt64Maybe v  -> V.length v
  ColFloatMaybe v   -> V.length v
  ColDoubleMaybe v  -> V.length v
  ColBoolMaybe v    -> V.length v
  ColUtf8Maybe v    -> V.length v
  ColBinaryMaybe v  -> V.length v
  ColDate32 v       -> VP.length v
  ColDate32Maybe v  -> V.length v
  ColTimestamp v    -> VP.length v
  ColTimestampMaybe v -> V.length v
  _ -> 0

-- ============================================================
-- Table
-- ============================================================

-- | Pairs a 'RowEncoder' with a 'RowDecoder' for one Haskell
-- record type. This is the handle you pass to the top-level
-- encode / decode helpers below.
data Table r = Table
  { tableEncode :: !(RowEncoder r)
  , tableDecode :: !(RowDecoder r)
  }

-- | Smart constructor. Equivalent to @Table enc dec@ but reads
-- better in call-site positions.
table :: RowEncoder r -> RowDecoder r -> Table r
table = Table

-- | Schema implied by the 'RowEncoder'.
tableSchema :: Table r -> Schema
tableSchema t = Schema
  { arrowFields     = V.fromList (rowEncoderFields (tableEncode t))
  , arrowEndianness = Little
  }

-- | Names of the columns the 'Table''s decoder needs. Equivalent
-- to @'rowDecoderRequiredColumns' . 'tableDecode'@; surfaced
-- here so callers can drive column projection through a
-- 'Table' without unpacking the inner 'RowDecoder'.
tableRequiredColumns :: Table r -> [Text]
tableRequiredColumns = rowDecoderRequiredColumns . tableDecode

-- | Encode a vector of records as an Arrow batch + its schema.
-- The schema comes from 'tableSchema'; the batch is parallel to
-- 'arrowFields' of that schema.
encodeTable :: Table r -> V.Vector r -> (Schema, V.Vector ColumnArray)
encodeTable t rs =
  ( tableSchema t
  , V.fromList (runRowEncoder (tableEncode t) rs)
  )

-- | Decode an Arrow batch into a vector of records. Looks up
-- columns by schema field name; returns 'Left' on missing
-- columns or type mismatches.
decodeTable
  :: Table r
  -> Schema
  -> V.Vector ColumnArray
  -> Either String (V.Vector r)
decodeTable t sch cs =
  runRowDecoder (tableDecode t) (arrowFields sch) cs

-- Map import is kept for future @byIndex@ variants.
_mapShim :: Map.Map Text Int
_mapShim = Map.empty
