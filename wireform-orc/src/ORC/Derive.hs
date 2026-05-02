{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Annotation-driven Template Haskell deriver for Apache ORC.
--
-- ORC is a /columnar/ format: records become rows in a columnar
-- stripe rather than recursive trees. This module therefore models
-- two layers of typeclasses:
--
-- * 'ToORCLeaf' \/ 'FromORCLeaf' for individual scalar columns
--   (one per ORC leaf 'TypeKind').
-- * 'ToORC' \/ 'FromORC' for whole rows: a record value projects to
--   a positional 'V.Vector' of 'LeafValue', one entry per non-skipped
--   field in declaration order.
--
-- Companion class 'HasORCSchema' produces the @V.Vector ORCType@ that
-- "ORC.HighLevel.encodeORC" expects: index 0 is the synthetic root
-- struct, and indices @1..N@ are the leaves in field-declaration order
-- (matching the @V.Vector LeafValue@ shape that 'toORCRow' produces).
--
-- = Pragmatic split: row codec, not full file codec
--
-- ORC's wire format is stripe-oriented: per-stripe streams hold
-- columnar payloads (DATA, LENGTH, PRESENT, etc.) and the deriver
-- /cannot/ produce a single \"row codec\" the way a recursive format
-- like CBOR or BSON can. The layering above is therefore intentional:
-- the deriver gives you (a) the schema and (b) a per-row scalar
-- projection, and the caller bundles a batch of rows into a stripe by
-- pivoting the @V.Vector LeafValue@ rows into per-column vectors and
-- calling the encoders in "ORC.Write" (@encodeIntColumn@,
-- @encodeStringDirectColumn@, @encodeBooleanRLE@, …). The reverse
-- pipeline pairs 'fromORCRow' with the column readers in "ORC.Read".
--
-- = Encoding shape
--
-- * 'TypeShapeNewtype' — pass-through 'ToORCLeaf' \/ 'FromORCLeaf'
--   instances on the newtype, delegating to the inner field's
--   instance.
-- * 'TypeShapeRecord'  — 'ToORC' \/ 'FromORC' \/ 'HasORCSchema'
--   instances. Record fields must have a 'ToORCLeaf' \/ 'FromORCLeaf'
--   instance (one of the supported flat scalars or a user-derived
--   newtype thereof). 'Maybe' fields project to the matching leaf
--   constructor with a 'Nothing' payload.
-- * 'TypeShapeEnum' \/ 'TypeShapeSum' — rejected at splice time. ORC
--   models alternatives as union or struct columns with side-channel
--   discriminators that this row-shaped deriver does not attempt to
--   manage.
--
-- = Modifiers honoured (resolved against 'backendOrc')
--
-- * 'rename', 'renameStyle', 'renameWith' — used as the column name
--   in 'ORCType.otFieldNames' for the schema. The wire layout is
--   positional, so renames affect only the schema metadata.
-- * 'skip' — drop the column entirely from both 'toORCRow' and the
--   schema. The decoder fills the field from the named 'defaults'
--   function on read.
-- * 'defaults' — the default value used for skipped fields when
--   decoding.
-- * 'coerced' — the field is encoded \/ decoded via the named target
--   type's 'ToORCLeaf' \/ 'FromORCLeaf'; @Data.Coerce.coerce@ wraps
--   the boundary in both directions.
module ORC.Derive
  ( -- * Per-leaf scalar projection
    LeafValue (..)
  , ToORCLeaf (..)
  , FromORCLeaf (..)

    -- * Per-row record projection
  , ToORC (..)
  , FromORC (..)
  , HasORCSchema (..)

    -- * Derivers
  , deriveORC
  , deriveToORC
  , deriveFromORC
  , deriveHasORCSchema

    -- * Schema reflection
  , orcSchemaFor
  ) where

import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Word (Word32)
import Language.Haskell.TH

import ORC.Types (ORCType (..), TypeKind (..))

import Wireform.Derive.Backend (backendOrc)
import Wireform.Derive.ModifierInfo
import Wireform.Derive.TypeInfo

-- ---------------------------------------------------------------------------
-- LeafValue
-- ---------------------------------------------------------------------------

-- | A single ORC column-cell value: one of the flat scalar
-- 'TypeKind's that the row codec supports, with explicit 'Nothing'
-- representing a null entry (ORC's @PRESENT@ stream marks the row
-- as absent).
--
-- The constructor encodes both /which/ ORC column kind the cell
-- belongs to and /whether/ that cell carries a value. Mismatches
-- between the schema's expected 'TypeKind' and a row's actual
-- 'LeafValue' constructor surface as a decode error.
data LeafValue
  = LVBool   !(Maybe Bool)
  | LVInt8   !(Maybe Int8)
  | LVInt16  !(Maybe Int16)
  | LVInt32  !(Maybe Int32)
  | LVInt64  !(Maybe Int64)
  | LVFloat  !(Maybe Float)
  | LVDouble !(Maybe Double)
  | LVText   !(Maybe Text)
  | LVBytes  !(Maybe ByteString)
  deriving stock (Eq, Show)

-- ---------------------------------------------------------------------------
-- Leaf-level classes
-- ---------------------------------------------------------------------------

-- | Scalar Haskell values that map to a single ORC leaf column.
--
-- Instances supplied for the flat primitives ORC can natively
-- represent: 'Bool', 'Int8' \/ 'Int16' \/ 'Int32' \/ 'Int64', 'Int'
-- (as 'TKLong'), 'Float', 'Double', 'Text', and 'ByteString'. A
-- standalone 'Maybe' instance lifts any leaf into its nullable
-- variant; the 'Nothing' branch projects to the matching @LV*
-- Nothing@ leaf without inspecting a value.
class ToORCLeaf a where
  -- | Project a value into its 'LeafValue' shape (always non-null).
  toORCLeaf     :: a -> LeafValue
  -- | The null leaf for this scalar kind. Used by the 'Maybe'
  -- instance to materialise an empty cell without a witness value.
  toORCLeafNull :: proxy a -> LeafValue
  -- | The ORC 'TypeKind' the schema should advertise for this leaf.
  orcLeafKind   :: proxy a -> TypeKind

-- | Inverse of 'ToORCLeaf'. The 'Maybe' instance maps any null leaf
-- to 'Right Nothing' regardless of constructor, so an optional
-- field round-trips even when the cell is absent.
class FromORCLeaf a where
  fromORCLeaf :: LeafValue -> Either String a

-- ---------------------------------------------------------------------------
-- Scalar instances
-- ---------------------------------------------------------------------------

instance ToORCLeaf Bool where
  toORCLeaf b   = LVBool (Just b)
  toORCLeafNull _ = LVBool Nothing
  orcLeafKind   _ = TKBoolean

instance FromORCLeaf Bool where
  fromORCLeaf = \case
    LVBool (Just b) -> Right b
    LVBool Nothing  -> Left "ORC.Derive: null value for non-optional Bool"
    v -> Left ("ORC.Derive: expected LVBool, got " <> leafCtor v)

instance ToORCLeaf Int8 where
  toORCLeaf n   = LVInt8 (Just n)
  toORCLeafNull _ = LVInt8 Nothing
  orcLeafKind   _ = TKByte

instance FromORCLeaf Int8 where
  fromORCLeaf = \case
    LVInt8 (Just n) -> Right n
    LVInt8 Nothing  -> Left "ORC.Derive: null value for non-optional Int8"
    v -> Left ("ORC.Derive: expected LVInt8, got " <> leafCtor v)

instance ToORCLeaf Int16 where
  toORCLeaf n   = LVInt16 (Just n)
  toORCLeafNull _ = LVInt16 Nothing
  orcLeafKind   _ = TKShort

instance FromORCLeaf Int16 where
  fromORCLeaf = \case
    LVInt16 (Just n) -> Right n
    LVInt16 Nothing  -> Left "ORC.Derive: null value for non-optional Int16"
    v -> Left ("ORC.Derive: expected LVInt16, got " <> leafCtor v)

instance ToORCLeaf Int32 where
  toORCLeaf n   = LVInt32 (Just n)
  toORCLeafNull _ = LVInt32 Nothing
  orcLeafKind   _ = TKInt

instance FromORCLeaf Int32 where
  fromORCLeaf = \case
    LVInt32 (Just n) -> Right n
    LVInt32 Nothing  -> Left "ORC.Derive: null value for non-optional Int32"
    v -> Left ("ORC.Derive: expected LVInt32, got " <> leafCtor v)

instance ToORCLeaf Int64 where
  toORCLeaf n   = LVInt64 (Just n)
  toORCLeafNull _ = LVInt64 Nothing
  orcLeafKind   _ = TKLong

instance FromORCLeaf Int64 where
  fromORCLeaf = \case
    LVInt64 (Just n) -> Right n
    LVInt64 Nothing  -> Left "ORC.Derive: null value for non-optional Int64"
    v -> Left ("ORC.Derive: expected LVInt64, got " <> leafCtor v)

-- | Native 'Int' rides the @LVInt64@ slot. ORC has no signed
-- integer kind narrower than 'TKLong' that would faithfully cover
-- the full range, and 64-bit is the platform width on every host
-- this package targets.
instance ToORCLeaf Int where
  toORCLeaf n   = LVInt64 (Just (fromIntegral n))
  toORCLeafNull _ = LVInt64 Nothing
  orcLeafKind   _ = TKLong

instance FromORCLeaf Int where
  fromORCLeaf v = fromIntegral <$> (fromORCLeaf v :: Either String Int64)

instance ToORCLeaf Float where
  toORCLeaf f   = LVFloat (Just f)
  toORCLeafNull _ = LVFloat Nothing
  orcLeafKind   _ = TKFloat

instance FromORCLeaf Float where
  fromORCLeaf = \case
    LVFloat (Just f)  -> Right f
    LVFloat Nothing   -> Left "ORC.Derive: null value for non-optional Float"
    LVDouble (Just d) -> Right (realToFrac d)
    v -> Left ("ORC.Derive: expected LVFloat, got " <> leafCtor v)

instance ToORCLeaf Double where
  toORCLeaf d   = LVDouble (Just d)
  toORCLeafNull _ = LVDouble Nothing
  orcLeafKind   _ = TKDouble

instance FromORCLeaf Double where
  fromORCLeaf = \case
    LVDouble (Just d) -> Right d
    LVDouble Nothing  -> Left "ORC.Derive: null value for non-optional Double"
    LVFloat (Just f)  -> Right (realToFrac f)
    v -> Left ("ORC.Derive: expected LVDouble, got " <> leafCtor v)

instance ToORCLeaf Text where
  toORCLeaf t   = LVText (Just t)
  toORCLeafNull _ = LVText Nothing
  orcLeafKind   _ = TKString

instance FromORCLeaf Text where
  fromORCLeaf = \case
    LVText (Just t) -> Right t
    LVText Nothing  -> Left "ORC.Derive: null value for non-optional Text"
    v -> Left ("ORC.Derive: expected LVText, got " <> leafCtor v)

instance ToORCLeaf ByteString where
  toORCLeaf b   = LVBytes (Just b)
  toORCLeafNull _ = LVBytes Nothing
  orcLeafKind   _ = TKBinary

instance FromORCLeaf ByteString where
  fromORCLeaf = \case
    LVBytes (Just b) -> Right b
    LVBytes Nothing  -> Left "ORC.Derive: null value for non-optional ByteString"
    v -> Left ("ORC.Derive: expected LVBytes, got " <> leafCtor v)

-- | Optional projection. 'Nothing' projects to the matching @LV*
-- Nothing@ leaf for the inner type's kind; 'Just' delegates to the
-- inner instance. On decode, /any/ null leaf decodes to 'Nothing'
-- (so a row whose constructor mismatches the schema still
-- round-trips a null cell).
instance ToORCLeaf a => ToORCLeaf (Maybe a) where
  toORCLeaf      = \case
    Nothing -> toORCLeafNull (Proxy :: Proxy a)
    Just x  -> toORCLeaf x
  toORCLeafNull _ = toORCLeafNull (Proxy :: Proxy a)
  orcLeafKind   _ = orcLeafKind   (Proxy :: Proxy a)

instance FromORCLeaf a => FromORCLeaf (Maybe a) where
  fromORCLeaf v = case v of
    LVBool   Nothing -> Right Nothing
    LVInt8   Nothing -> Right Nothing
    LVInt16  Nothing -> Right Nothing
    LVInt32  Nothing -> Right Nothing
    LVInt64  Nothing -> Right Nothing
    LVFloat  Nothing -> Right Nothing
    LVDouble Nothing -> Right Nothing
    LVText   Nothing -> Right Nothing
    LVBytes  Nothing -> Right Nothing
    _                -> Just <$> fromORCLeaf v

-- ---------------------------------------------------------------------------
-- Row-level classes
-- ---------------------------------------------------------------------------

-- | A record type that projects to a row of ORC scalar cells.
--
-- The output 'V.Vector' has one entry per /non-skipped/ field, in
-- declaration order. The schema attached via 'HasORCSchema' lays out
-- the leaves in the same order, with the synthetic root-struct
-- 'ORCType' at index 0.
class ToORC a where
  toORCRow :: a -> V.Vector LeafValue

-- | Inverse of 'ToORC'. Extra leaf cells past the field count are
-- ignored; missing cells produce a 'Left'.
class FromORC a where
  fromORCRow :: V.Vector LeafValue -> Either String a

-- | The ORC schema for a record type, as the flat 'V.Vector ORCType'
-- that "ORC.HighLevel.encodeORC" consumes:
--
-- * Index 0: synthetic root @TKStruct@ whose @otSubtypes@ are the
--   ids @[1..N]@ of the leaf children and whose @otFieldNames@ are
--   the (rename-resolved) selector names.
-- * Indices 1..N: per-field leaves with @otSubtypes = empty@ and
--   @otFieldNames = empty@.
class HasORCSchema a where
  orcSchema :: proxy a -> V.Vector ORCType

-- ---------------------------------------------------------------------------
-- Public deriver entry points
-- ---------------------------------------------------------------------------

-- | Derive the full ORC instance set for a type.
--
-- * Records: 'ToORC' + 'FromORC' + 'HasORCSchema'.
-- * Newtypes: 'ToORCLeaf' + 'FromORCLeaf' pass-through.
-- * Enums and sums: rejected at splice time.
deriveORC :: Name -> Q [Dec]
deriveORC nm = do
  ti <- reifyTypeInfo nm
  case typeInfoShape ti of
    TypeShapeNewtype c -> deriveNewtypeLeaf ti c
    TypeShapeRecord  c -> do
      to     <- deriveToORCFromTI ti c
      from   <- deriveFromORCFromTI ti c
      schema <- deriveHasORCSchemaFromTI ti c
      pure (to ++ from ++ schema)
    TypeShapeEnum    _ -> fail (rejectMsg nm "enum")
    TypeShapeSum     _ -> fail (rejectMsg nm "sum")

-- | Derive only 'ToORC' for a record type. Fails on non-records.
deriveToORC :: Name -> Q [Dec]
deriveToORC nm = do
  ti <- reifyTypeInfo nm
  case typeInfoShape ti of
    TypeShapeRecord c -> deriveToORCFromTI ti c
    TypeShapeNewtype _ -> fail (newtypeNotRecord nm)
    TypeShapeEnum    _ -> fail (rejectMsg nm "enum")
    TypeShapeSum     _ -> fail (rejectMsg nm "sum")

-- | Derive only 'FromORC' for a record type. Fails on non-records.
deriveFromORC :: Name -> Q [Dec]
deriveFromORC nm = do
  ti <- reifyTypeInfo nm
  case typeInfoShape ti of
    TypeShapeRecord c -> deriveFromORCFromTI ti c
    TypeShapeNewtype _ -> fail (newtypeNotRecord nm)
    TypeShapeEnum    _ -> fail (rejectMsg nm "enum")
    TypeShapeSum     _ -> fail (rejectMsg nm "sum")

-- | Derive only 'HasORCSchema' for a record type. Fails on
-- non-records.
deriveHasORCSchema :: Name -> Q [Dec]
deriveHasORCSchema nm = do
  ti <- reifyTypeInfo nm
  case typeInfoShape ti of
    TypeShapeRecord c -> deriveHasORCSchemaFromTI ti c
    TypeShapeNewtype _ -> fail (newtypeNotRecord nm)
    TypeShapeEnum    _ -> fail (rejectMsg nm "enum")
    TypeShapeSum     _ -> fail (rejectMsg nm "sum")

-- | Splice the @V.Vector ORCType@ schema for a record type at use
-- site. Equivalent to @orcSchema ('Proxy' :: 'Proxy' T)@; provided
-- so callers can spell the schema without the proxy boilerplate.
--
-- The named type must already have a 'HasORCSchema' instance,
-- typically emitted by 'deriveORC'.
orcSchemaFor :: Name -> Q Exp
orcSchemaFor nm = do
  ti <- reifyTypeInfo nm
  let typ = applyTypeArgs (ConT (typeInfoName ti)) (typeInfoVarTypes ti)
  [| orcSchema (Proxy :: Proxy $(pure typ)) |]

-- ---------------------------------------------------------------------------
-- Records → ToORC / FromORC / HasORCSchema
-- ---------------------------------------------------------------------------

deriveToORCFromTI :: TypeInfo -> ConInfo -> Q [Dec]
deriveToORCFromTI ti c = do
  let typ = applyTypeArgs (ConT (typeInfoName ti)) (typeInfoVarTypes ti)
  body <- toORCRecord c
  let decl = InstanceD Nothing []
              (AppT (ConT ''ToORC) typ)
              [FunD 'toORCRow [Clause [] (NormalB body) []]]
  pure [decl]

deriveFromORCFromTI :: TypeInfo -> ConInfo -> Q [Dec]
deriveFromORCFromTI ti c = do
  let typ = applyTypeArgs (ConT (typeInfoName ti)) (typeInfoVarTypes ti)
  body <- fromORCRecord c
  let decl = InstanceD Nothing []
              (AppT (ConT ''FromORC) typ)
              [FunD 'fromORCRow [Clause [] (NormalB body) []]]
  pure [decl]

deriveHasORCSchemaFromTI :: TypeInfo -> ConInfo -> Q [Dec]
deriveHasORCSchemaFromTI ti c = do
  let typ = applyTypeArgs (ConT (typeInfoName ti)) (typeInfoVarTypes ti)
  body <- recordSchemaExp c
  let decl = InstanceD Nothing []
              (AppT (ConT ''HasORCSchema) typ)
              [FunD 'orcSchema [Clause [WildP] (NormalB body) []]]
  pure [decl]

-- | Build the @V.Vector LeafValue@ for a record value.
--
-- @
-- toORCRow x =
--   V.fromList
--     [ toORCLeaf (selector1 x)
--     , toORCLeaf (selector2 x)
--     , …
--     ]
-- @
--
-- Skipped fields drop out of the list; coerced fields cast through
-- the named target before calling 'toORCLeaf'.
toORCRecord :: ConInfo -> Q Exp
toORCRecord c = do
  x      <- newName "x"
  pieces <- mapM (toORCField (varE x)) (conInfoFields c)
  lamE [varP x] [| V.fromList $(pure (ListE (concat pieces))) |]

toORCField :: Q Exp -> FieldInfo -> Q [Exp]
toORCField varExp (FieldInfo mSel ty) = do
  selName <- requireSelector mSel
  mi      <- reifyModifierInfoFor backendOrc selName
  if miSkip mi
    then pure []
    else do
      let getter = appE (varE selName) varExp
      e <- case miCoerce mi of
        Nothing      -> [| toORCLeaf $getter |]
        Just tgtName ->
          let tgtTy = ConT tgtName
              srcTy = ty
          in  [| toORCLeaf
                   ((coerce :: $(pure srcTy) -> $(pure tgtTy)) $getter) |]
      pure [e]

-- | Build a 'FromORC' parser. Walks the field list assembling
-- @Ctor \<$\> p_0 \<*\> p_1 \<*\> …@ where each @p_i@ either reads
-- the next 'LeafValue' off the input vector or, for skipped fields,
-- substitutes the @defaults@ value.
fromORCRecord :: ConInfo -> Q Exp
fromORCRecord c = do
  v <- newName "v"
  bodyE <- buildFromSequence v (conInfoName c) (conInfoFields c)
  lamE [varP v] (pure bodyE)

buildFromSequence :: Name -> Name -> [FieldInfo] -> Q Exp
buildFromSequence v conName = go 0 []
  where
    go :: Int -> [Name] -> [FieldInfo] -> Q Exp
    go _ acc [] =
      let !assemble = foldl (\e nm -> AppE e (VarE nm))
                            (ConE conName)
                            (reverse acc)
      in  [| Right $(pure assemble) |]
    go pos acc (f : fs) = do
      vName <- newName "f"
      (cellExp, advance) <- fromORCField v pos f
      restExp            <- go (pos + advance) (vName : acc) fs
      [| $(pure cellExp) >>= \ $(varP vName) -> $(pure restExp) |]

-- | Produce @(parserExp, advance)@ for one field. 'advance' is 0 for
-- skipped fields (no leaf is consumed) and 1 otherwise.
fromORCField :: Name -> Int -> FieldInfo -> Q (Exp, Int)
fromORCField v pos (FieldInfo mSel ty) = do
  selName <- requireSelector mSel
  mi      <- reifyModifierInfoFor backendOrc selName
  if miSkip mi
    then case miDefaults mi of
      Just defNm -> do
        e <- [| Right $(varE defNm) |]
        pure (e, 0)
      Nothing -> do
        let msg = "ORC.Derive: missing 'defaults' for skipped field "
                    ++ nameBase selName
        e <- [| Left $(litE (stringL msg)) |]
        pure (e, 0)
    else do
      let posLit = litE (integerL (fromIntegral pos))
          selBase = nameBase selName
      base <- [| if V.length $(varE v) > $posLit
                   then $(parseAtPos v pos mi ty)
                   else Left ("ORC.Derive: row missing leaf at index "
                              ++ show ($posLit :: Int)
                              ++ " for field "
                              ++ $(litE (stringL selBase))) |]
      pure (base, 1)

-- | Build the per-position parser for a non-skipped field. Honours
-- 'coerced' by routing through the target type's 'FromORCLeaf'.
parseAtPos :: Name -> Int -> ModifierInfo -> Type -> Q Exp
parseAtPos v pos mi ty = do
  let posLit = litE (integerL (fromIntegral pos))
  case miCoerce mi of
    Nothing ->
      [| fromORCLeaf ($(varE v) V.! $posLit) |]
    Just tgtName ->
      let tgtTy = ConT tgtName
          srcTy = ty
      in  [| fmap (coerce :: $(pure tgtTy) -> $(pure srcTy))
              ((fromORCLeaf ($(varE v) V.! $posLit))
                  :: Either String $(pure tgtTy)) |]

-- ---------------------------------------------------------------------------
-- Schema body
-- ---------------------------------------------------------------------------

-- | Produce the @V.Vector ORCType@ schema body. The root struct is
-- index 0 and lists @otSubtypes = [1..N]@; the leaf children follow
-- in the same field order.
recordSchemaExp :: ConInfo -> Q Exp
recordSchemaExp c = do
  allPairs <- mapM
    (\f -> do
        selName <- requireSelector (fieldInfoName f)
        mi <- reifyModifierInfoFor backendOrc selName
        pure (f, mi))
    (conInfoFields c)
  let pairs = filter (\(_, mi) -> not (miSkip mi)) allPairs
  nameExps <- mapM (uncurry resolveFieldName) pairs
  leafExps <- mapM (uncurry leafTypeExp) pairs
  let n      = length pairs
      nLit   = litE (integerL (fromIntegral n))
  [| let !ids   = V.enumFromN (1 :: Word32) $nLit
         !names = V.fromList $(pure (ListE nameExps))
         !root  = ORCType
           { otKind       = TKStruct
           , otSubtypes   = ids
           , otFieldNames = names
           }
     in V.fromList (root : $(pure (ListE leafExps))) |]

-- | Compile the (rename-resolved) field name expression for one
-- field. The name appears verbatim in @otFieldNames@.
resolveFieldName :: FieldInfo -> ModifierInfo -> Q Exp
resolveFieldName (FieldInfo mSel _) mi = do
  selName <- requireSelector mSel
  renderWireKey mi (T.pack (nameBase selName))

-- | Build the @ORCType { otKind = …, … }@ expression for one leaf
-- field. Coerced fields use the target type's 'orcLeafKind'.
leafTypeExp :: FieldInfo -> ModifierInfo -> Q Exp
leafTypeExp (FieldInfo _ ty) mi = case miCoerce mi of
  Nothing      ->
    [| ORCType
         { otKind       = orcLeafKind (Proxy :: Proxy $(pure ty))
         , otSubtypes   = V.empty
         , otFieldNames = V.empty
         } |]
  Just tgtName ->
    let tgtTy = ConT tgtName
    in  [| ORCType
             { otKind       = orcLeafKind (Proxy :: Proxy $(pure tgtTy))
             , otSubtypes   = V.empty
             , otFieldNames = V.empty
             } |]

-- ---------------------------------------------------------------------------
-- Newtypes → ToORCLeaf / FromORCLeaf pass-through
-- ---------------------------------------------------------------------------

deriveNewtypeLeaf :: TypeInfo -> ConInfo -> Q [Dec]
deriveNewtypeLeaf ti c = do
  let typ = applyTypeArgs (ConT (typeInfoName ti)) (typeInfoVarTypes ti)
  innerTy <- case conInfoFields c of
    [FieldInfo _ ty] -> pure ty
    _ -> fail $
      "ORC.Derive: newtype " ++ show (typeInfoName ti)
        ++ " must have exactly one field"
  -- ToORCLeaf instance: every method delegates to the inner type's
  -- instance, with @coerce@ on the value side.
  toBody <-
    [| toORCLeaf
         . (coerce :: $(pure typ) -> $(pure innerTy)) |]
  toNullBody <-
    [| toORCLeafNull (Proxy :: Proxy $(pure innerTy)) |]
  kindBody <-
    [| orcLeafKind (Proxy :: Proxy $(pure innerTy)) |]
  fromBody <-
    [| fmap (coerce :: $(pure innerTy) -> $(pure typ))
         . (fromORCLeaf :: LeafValue -> Either String $(pure innerTy)) |]
  let toInst = InstanceD Nothing []
                  (AppT (ConT ''ToORCLeaf) typ)
                  [ FunD 'toORCLeaf
                      [Clause [] (NormalB toBody) []]
                  , FunD 'toORCLeafNull
                      [Clause [WildP] (NormalB toNullBody) []]
                  , FunD 'orcLeafKind
                      [Clause [WildP] (NormalB kindBody) []]
                  ]
      fromInst = InstanceD Nothing []
                  (AppT (ConT ''FromORCLeaf) typ)
                  [FunD 'fromORCLeaf
                      [Clause [] (NormalB fromBody) []]]
  pure [toInst, fromInst]

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

leafCtor :: LeafValue -> String
leafCtor = \case
  LVBool   _ -> "LVBool"
  LVInt8   _ -> "LVInt8"
  LVInt16  _ -> "LVInt16"
  LVInt32  _ -> "LVInt32"
  LVInt64  _ -> "LVInt64"
  LVFloat  _ -> "LVFloat"
  LVDouble _ -> "LVDouble"
  LVText   _ -> "LVText"
  LVBytes  _ -> "LVBytes"

requireSelector :: Maybe Name -> Q Name
requireSelector (Just n) = pure n
requireSelector Nothing  =
  fail "ORC.Derive: cannot derive ORC instances for a positional \
       \(non-record) constructor; ORC columns are addressed by name."

applyTypeArgs :: Type -> [Type] -> Type
applyTypeArgs = foldl AppT

rejectMsg :: Name -> String -> String
rejectMsg nm shape =
  "ORC.Derive: cannot derive ORC instances for " ++ shape
    ++ " type " ++ show nm
    ++ ". ORC has no first-class column-level union representation; "
    ++ "model alternatives as a discriminator + payload columns by hand."

newtypeNotRecord :: Name -> String
newtypeNotRecord nm =
  "ORC.Derive: " ++ show nm ++ " is a newtype; only records carry "
    ++ "ToORC / FromORC / HasORCSchema instances. Use 'deriveORC' "
    ++ "to emit pass-through ToORCLeaf / FromORCLeaf instances on "
    ++ "newtypes."
