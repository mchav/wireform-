{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Annotation-driven Template Haskell deriver for Cap'n Proto.
--
-- Cap'n Proto has no generic typeclass in this package, so the
-- 'ToCapnProto' / 'FromCapnProto' classes the deriver targets are
-- also defined here.
--
-- Encoding shape:
--
-- * 'TypeShapeNewtype' — pass-through to the inner field's instance.
-- * 'TypeShapeRecord'  — Cap'n Proto 'CP.Struct' with two sections.
--   Each Haskell field is classified by its declared type:
--
--       * Known scalar types ('Bool', fixed-width 'Int*' / 'Word*',
--         'Float', 'Double') land in the /data/ section.
--       * Everything else ('Text', 'ByteString', lists, vectors,
--         nested structs, user-defined types, etc.) lands in the
--         /pointer/ section.
--       * @Maybe a@ classifies the same way as @a@. 'Nothing' encodes
--         as 'CP.Void' in whichever section the field belongs to.
--       * The 'coerced' modifier reroutes the field through its
--         named target type for both the slot-classification and the
--         actual encode/decode bridge.
--
--   Within each section fields keep their /declaration order/. The
--   @tag N@ modifier is reserved for enum ordinal overrides; struct
--   fields are positional. 'skip'ped fields contribute to neither
--   section and are reconstructed on decode from 'defaults'.
-- * 'TypeShapeEnum'    — encoded as a 'CP.Enum' carrying the
--   constructor's zero-based ordinal, overridable with @tag N@.
-- * 'TypeShapeSum'     — rejected at splice time. Cap'n Proto unions
--   require schema-side discriminant metadata not captured at the
--   value level in 'CP.Value'.
--
-- Modifiers honoured: 'tag' (enum ordinal override), 'skip',
-- 'defaults', 'optional', 'coerced'. 'rename' is ignored because
-- Cap'n Proto structs are positional, not keyed.
module CapnProto.Derive
  ( -- * Classes
    ToCapnProto (..)
  , FromCapnProto (..)

    -- * Derivers
  , deriveCapnProto
  , deriveToCapnProto
  , deriveFromCapnProto
  ) where

import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.Foldable (foldlM)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word8, Word16, Word32, Word64)
import Language.Haskell.TH

import qualified CapnProto.Value as CP

import Wireform.Derive.Backend
import Wireform.Derive.ModifierInfo
import Wireform.Derive.TypeInfo

-- ---------------------------------------------------------------------------
-- Classes
-- ---------------------------------------------------------------------------

-- | Haskell values that can be projected into a Cap'n Proto 'CP.Value'.
class ToCapnProto a where
  toCapnProto :: a -> CP.Value

-- | Cap'n Proto 'CP.Value' views that can be parsed back into Haskell.
class FromCapnProto a where
  fromCapnProto :: CP.Value -> Either String a

-- ---------------------------------------------------------------------------
-- Scalar instances
-- ---------------------------------------------------------------------------

instance ToCapnProto CP.Value where
  toCapnProto = id

instance FromCapnProto CP.Value where
  fromCapnProto = Right

instance ToCapnProto Bool where toCapnProto = CP.Bool
instance FromCapnProto Bool where
  fromCapnProto = \case
    CP.Bool b -> Right b
    CP.Void   -> Right False
    v -> Left ("CapnProto.Derive: expected Bool, got " <> showCtor v)

instance ToCapnProto Int8  where toCapnProto = CP.Int8
instance FromCapnProto Int8 where
  fromCapnProto = \case
    CP.Int8 n  -> Right n
    CP.Int16 n -> Right (fromIntegral n)
    CP.Int32 n -> Right (fromIntegral n)
    CP.Int64 n -> Right (fromIntegral n)
    CP.Void    -> Right 0
    v -> Left ("CapnProto.Derive: expected Int8, got " <> showCtor v)

instance ToCapnProto Int16 where toCapnProto = CP.Int16
instance FromCapnProto Int16 where
  fromCapnProto = \case
    CP.Int8 n  -> Right (fromIntegral n)
    CP.Int16 n -> Right n
    CP.Int32 n -> Right (fromIntegral n)
    CP.Int64 n -> Right (fromIntegral n)
    CP.Void    -> Right 0
    v -> Left ("CapnProto.Derive: expected Int16, got " <> showCtor v)

instance ToCapnProto Int32 where toCapnProto = CP.Int32
instance FromCapnProto Int32 where
  fromCapnProto = \case
    CP.Int8 n  -> Right (fromIntegral n)
    CP.Int16 n -> Right (fromIntegral n)
    CP.Int32 n -> Right n
    CP.Int64 n -> Right (fromIntegral n)
    CP.Void    -> Right 0
    v -> Left ("CapnProto.Derive: expected Int32, got " <> showCtor v)

instance ToCapnProto Int64 where toCapnProto = CP.Int64
instance FromCapnProto Int64 where
  fromCapnProto = \case
    CP.Int8 n  -> Right (fromIntegral n)
    CP.Int16 n -> Right (fromIntegral n)
    CP.Int32 n -> Right (fromIntegral n)
    CP.Int64 n -> Right n
    CP.Void    -> Right 0
    v -> Left ("CapnProto.Derive: expected Int64, got " <> showCtor v)

instance ToCapnProto Int where
  toCapnProto = CP.Int64 . fromIntegral
instance FromCapnProto Int where
  fromCapnProto v = fromIntegral <$> (fromCapnProto v :: Either String Int64)

instance ToCapnProto Word8  where toCapnProto = CP.UInt8
instance FromCapnProto Word8 where
  fromCapnProto = \case
    CP.UInt8 n  -> Right n
    CP.UInt16 n -> Right (fromIntegral n)
    CP.UInt32 n -> Right (fromIntegral n)
    CP.UInt64 n -> Right (fromIntegral n)
    CP.Void     -> Right 0
    v -> Left ("CapnProto.Derive: expected UInt8, got " <> showCtor v)

instance ToCapnProto Word16 where toCapnProto = CP.UInt16
instance FromCapnProto Word16 where
  fromCapnProto = \case
    CP.UInt8 n  -> Right (fromIntegral n)
    CP.UInt16 n -> Right n
    CP.UInt32 n -> Right (fromIntegral n)
    CP.UInt64 n -> Right (fromIntegral n)
    CP.Void     -> Right 0
    v -> Left ("CapnProto.Derive: expected UInt16, got " <> showCtor v)

instance ToCapnProto Word32 where toCapnProto = CP.UInt32
instance FromCapnProto Word32 where
  fromCapnProto = \case
    CP.UInt8 n  -> Right (fromIntegral n)
    CP.UInt16 n -> Right (fromIntegral n)
    CP.UInt32 n -> Right n
    CP.UInt64 n -> Right (fromIntegral n)
    CP.Void     -> Right 0
    v -> Left ("CapnProto.Derive: expected UInt32, got " <> showCtor v)

instance ToCapnProto Word64 where toCapnProto = CP.UInt64
instance FromCapnProto Word64 where
  fromCapnProto = \case
    CP.UInt8 n  -> Right (fromIntegral n)
    CP.UInt16 n -> Right (fromIntegral n)
    CP.UInt32 n -> Right (fromIntegral n)
    CP.UInt64 n -> Right n
    CP.Void     -> Right 0
    v -> Left ("CapnProto.Derive: expected UInt64, got " <> showCtor v)

instance ToCapnProto Float  where toCapnProto = CP.Float32
instance FromCapnProto Float where
  fromCapnProto = \case
    CP.Float32 f -> Right f
    CP.Float64 d -> Right (realToFrac d)
    CP.Void      -> Right 0
    v -> Left ("CapnProto.Derive: expected Float32, got " <> showCtor v)

instance ToCapnProto Double where toCapnProto = CP.Float64
instance FromCapnProto Double where
  fromCapnProto = \case
    CP.Float64 d -> Right d
    CP.Float32 f -> Right (realToFrac f)
    CP.Void      -> Right 0
    v -> Left ("CapnProto.Derive: expected Float64, got " <> showCtor v)

instance ToCapnProto Text where toCapnProto = CP.Text
instance FromCapnProto Text where
  fromCapnProto = \case
    CP.Text t -> Right t
    v -> Left ("CapnProto.Derive: expected Text, got " <> showCtor v)

instance ToCapnProto ByteString where toCapnProto = CP.Data
instance FromCapnProto ByteString where
  fromCapnProto = \case
    CP.Data bs -> Right bs
    v -> Left ("CapnProto.Derive: expected Data, got " <> showCtor v)

instance ToCapnProto a => ToCapnProto [a] where
  toCapnProto xs = CP.List (V.fromList (map toCapnProto xs))

instance FromCapnProto a => FromCapnProto [a] where
  fromCapnProto = \case
    CP.List vs -> traverse fromCapnProto (V.toList vs)
    v -> Left ("CapnProto.Derive: expected List, got " <> showCtor v)

instance ToCapnProto a => ToCapnProto (Vector a) where
  toCapnProto xs = CP.List (V.map toCapnProto xs)

instance FromCapnProto a => FromCapnProto (Vector a) where
  fromCapnProto = \case
    CP.List vs -> V.mapM fromCapnProto vs
    v -> Left ("CapnProto.Derive: expected List, got " <> showCtor v)

-- | Standalone 'Maybe' instance. Cap'n Proto has no native notion of
-- value-level optionality, so 'Nothing' is encoded as 'CP.Void' and
-- 'Just x' as @toCapnProto x@. When the field type is itself a
-- scalar, the resulting 'CP.Void' rests in the data section; when
-- it is a pointer, it rests in the pointer section.
instance ToCapnProto a => ToCapnProto (Maybe a) where
  toCapnProto = \case
    Nothing -> CP.Void
    Just x  -> toCapnProto x

instance FromCapnProto a => FromCapnProto (Maybe a) where
  fromCapnProto = \case
    CP.Void -> Right Nothing
    v       -> Just <$> fromCapnProto v

showCtor :: CP.Value -> String
showCtor = \case
  CP.Void       -> "Void"
  CP.Bool _     -> "Bool"
  CP.Int8 _     -> "Int8"
  CP.Int16 _    -> "Int16"
  CP.Int32 _    -> "Int32"
  CP.Int64 _    -> "Int64"
  CP.UInt8 _    -> "UInt8"
  CP.UInt16 _   -> "UInt16"
  CP.UInt32 _   -> "UInt32"
  CP.UInt64 _   -> "UInt64"
  CP.Float32 _  -> "Float32"
  CP.Float64 _  -> "Float64"
  CP.Text _     -> "Text"
  CP.Data _     -> "Data"
  CP.Struct _ _ -> "Struct"
  CP.List _     -> "List"
  CP.Enum _     -> "Enum"

-- ---------------------------------------------------------------------------
-- Slot classification
-- ---------------------------------------------------------------------------

-- | Where a record field lives in the encoded struct.
data SlotKind = DataSlot | PointerSlot
  deriving (Eq, Show)

-- | Classify a field's declared Haskell type. Known fixed-width
-- numerics, 'Bool', 'Float', 'Double', and the unit-like cases land in
-- the data section; everything else (including user-defined types,
-- lists, vectors, 'Text', 'ByteString') lands in the pointer section.
-- @Maybe a@ classifies the same way as @a@.
classifySlot :: Type -> SlotKind
classifySlot t = case unwrapMaybe t of
  Just inner -> classifySlot inner
  Nothing    -> case t of
    ConT n
      | n == ''Bool   -> DataSlot
      | n == ''Int8   -> DataSlot
      | n == ''Int16  -> DataSlot
      | n == ''Int32  -> DataSlot
      | n == ''Int64  -> DataSlot
      | n == ''Int    -> DataSlot
      | n == ''Word8  -> DataSlot
      | n == ''Word16 -> DataSlot
      | n == ''Word32 -> DataSlot
      | n == ''Word64 -> DataSlot
      | n == ''Float  -> DataSlot
      | n == ''Double -> DataSlot
    _ -> PointerSlot

-- | Like 'classifySlot', but consults the field's @coerced@ target
-- type when present so the slot kind matches the actual encoded
-- representation rather than the surface-level newtype.
classifySlotForField :: Type -> ModifierInfo -> SlotKind
classifySlotForField ty mi = case miCoerce mi of
  Just nm -> classifySlot (ConT nm)
  Nothing -> classifySlot ty

-- ---------------------------------------------------------------------------
-- Public deriver entry points
-- ---------------------------------------------------------------------------

deriveCapnProto :: Name -> Q [Dec]
deriveCapnProto nm =
  (++) <$> deriveToCapnProto nm <*> deriveFromCapnProto nm

deriveToCapnProto :: Name -> Q [Dec]
deriveToCapnProto nm = do
  ti   <- reifyTypeInfo nm
  body <- toCPBody ti
  let typ  = applyTypeArgs (ConT (typeInfoName ti)) (typeInfoVarTypes ti)
      decl = InstanceD Nothing []
              (AppT (ConT ''ToCapnProto) typ)
              [FunD 'toCapnProto [Clause [] (NormalB body) []]]
  pure [decl]

deriveFromCapnProto :: Name -> Q [Dec]
deriveFromCapnProto nm = do
  ti   <- reifyTypeInfo nm
  body <- fromCPBody ti
  let typ  = applyTypeArgs (ConT (typeInfoName ti)) (typeInfoVarTypes ti)
      decl = InstanceD Nothing []
              (AppT (ConT ''FromCapnProto) typ)
              [FunD 'fromCapnProto [Clause [] (NormalB body) []]]
  pure [decl]

-- ---------------------------------------------------------------------------
-- ToCapnProto: dispatch on shape
-- ---------------------------------------------------------------------------

toCPBody :: TypeInfo -> Q Exp
toCPBody ti = case typeInfoShape ti of
  TypeShapeNewtype c  -> toCPNewtype c
  TypeShapeRecord  c  -> toCPRecord  c
  TypeShapeEnum    cs -> toCPEnum    cs
  TypeShapeSum     _  -> fail capnProtoSumErr

capnProtoSumErr :: String
capnProtoSumErr =
  "CapnProto.Derive: refusing to derive instances for a multi-constructor \
  \sum type. Cap'n Proto encodes alternatives as 'union' groups inside a \
  \struct, which requires schema-side discriminant metadata not captured \
  \by CapnProto.Value. Use a different backend or hand-write the instance."

toCPNewtype :: ConInfo -> Q Exp
toCPNewtype c = case conInfoFields c of
  [FieldInfo (Just sel) _] -> do
    x <- newName "x"
    lamE [varP x] [| toCapnProto ($(varE sel) $(varE x)) |]
  [FieldInfo Nothing _] -> do
    x <- newName "x"
    lamE [conP (conInfoName c) [varP x]] [| toCapnProto $(varE x) |]
  _ -> fail "CapnProto.Derive: newtype must have exactly one field"

-- | Encode a record by walking its fields, classifying each one as a
-- data slot or pointer slot, and assembling two parallel vectors that
-- preserve declaration order /within each section/.
toCPRecord :: ConInfo -> Q Exp
toCPRecord c = do
  x <- newName "x"
  (datExps, ptrExps) <- recordToCPSlots (varE x) c
  let datList = ListE datExps
      ptrList = ListE ptrExps
  lamE [varP x]
    [| CP.Struct (V.fromList $(pure datList)) (V.fromList $(pure ptrList)) |]

recordToCPSlots :: Q Exp -> ConInfo -> Q ([Exp], [Exp])
recordToCPSlots varExp c =
  foldlM step ([], []) (conInfoFields c)
  where
    step (datAcc, ptrAcc) f@(FieldInfo mSel ty) = do
      mEnc <- toCPFieldEnc varExp f
      case mEnc of
        Nothing -> pure (datAcc, ptrAcc)
        Just e  -> do
          mi <- case mSel of
            Just n  -> reifyModifierInfoFor backendCapnProto n
            Nothing -> pure (emptyModifierInfo backendCapnProto)
          case classifySlotForField ty mi of
            DataSlot    -> pure (datAcc ++ [e], ptrAcc)
            PointerSlot -> pure (datAcc, ptrAcc ++ [e])

-- | Compute the encoded expression for a single field, or 'Nothing'
-- if the field is to be skipped entirely. When @coerced ''T@ is set
-- the field is coerced to @T@ before being passed to 'toCapnProto'
-- so the inner instance is selected unambiguously.
toCPFieldEnc :: Q Exp -> FieldInfo -> Q (Maybe Exp)
toCPFieldEnc varExp (FieldInfo mSel _) = do
  selName <- requireSelector mSel
  mi <- reifyModifierInfoFor backendCapnProto selName
  if miSkip mi
    then pure Nothing
    else do
      let getter = appE (varE selName) varExp
      e <- case miCoerce mi of
        Nothing -> [| toCapnProto $getter |]
        Just nm -> [| toCapnProto (coerce $getter :: $(conT nm)) |]
      pure (Just e)

toCPEnum :: [ConInfo] -> Q Exp
toCPEnum cs = do
  v <- newName "v"
  matches <- mapM enumMatchTo (zip [0 ..] cs)
  body <- caseE (varE v) (map pure matches)
  lamE [varP v] (pure body)
  where
    enumMatchTo :: (Word16, ConInfo) -> Q Match
    enumMatchTo (defaultIdx, c) = do
      mi <- reifyModifierInfoFor backendCapnProto (conInfoName c)
      let n = case miTag mi of
            Just t  -> fromIntegral t :: Word16
            Nothing -> defaultIdx
      bodyE <- [| CP.Enum $(litE (integerL (fromIntegral n))) |]
      pure (Match (ConP (conInfoName c) [] []) (NormalB bodyE) [])

-- ---------------------------------------------------------------------------
-- FromCapnProto: dispatch on shape
-- ---------------------------------------------------------------------------

fromCPBody :: TypeInfo -> Q Exp
fromCPBody ti = case typeInfoShape ti of
  TypeShapeNewtype c  -> fromCPNewtype c
  TypeShapeRecord  c  -> fromCPRecord  c
  TypeShapeEnum    cs -> fromCPEnum    cs
  TypeShapeSum     _  -> fail capnProtoSumErr

fromCPNewtype :: ConInfo -> Q Exp
fromCPNewtype c = case conInfoFields c of
  [FieldInfo _ _] -> [| fmap $(conE (conInfoName c)) . fromCapnProto |]
  _               -> fail "CapnProto.Derive: newtype must have exactly one field"

fromCPRecord :: ConInfo -> Q Exp
fromCPRecord c = do
  v    <- newName "v"
  dat  <- newName "dat"
  ptrs <- newName "ptrs"
  paired <- pairFieldsWithSectionIdx (conInfoFields c)
  let usesData = any isDataSpec paired
      usesPtrs = any isPtrSpec  paired
  bodyE <- recordParserSpecs dat ptrs c paired
  let datPat = if usesData then varP dat  else wildP
      ptrPat = if usesPtrs then varP ptrs else wildP
  lamE [varP v]
    (caseE (varE v)
       [ match (conP 'CP.Struct [datPat, ptrPat])
               (normalB (pure bodyE))
               []
       , match wildP
               (normalB
                  [| Left "CapnProto.Derive: expected Struct for record type" |])
               []
       ])
  where
    isDataSpec (SpecData    _ _) = True
    isDataSpec _                 = False
    isPtrSpec  (SpecPointer _ _) = True
    isPtrSpec  _                 = False

-- | A field's positional index within the section it lives in, paired
-- with the field itself. Skipped fields carry 'Nothing' and consume
-- no index in either section.
data ParseSpec
  = SpecSkipped !FieldInfo
  | SpecData    !Int !FieldInfo
  | SpecPointer !Int !FieldInfo

-- | Build the record parser. Each non-skipped field consumes one slot
-- index in either the data or pointer section; the index assignment
-- mirrors what 'recordToCPSlots' produces on the encode side.
recordParserSpecs :: Name -> Name -> ConInfo -> [ParseSpec] -> Q Exp
recordParserSpecs dat ptrs c paired =
  case paired of
    [] -> [| Right $(conE (conInfoName c)) |]
    (p0 : ps) -> do
      e0 <- fieldParserQ dat ptrs p0
      hd <- [| $(conE (conInfoName c)) <$> $(pure e0) |]
      foldlM
        (\acc fp -> do
            ef <- fieldParserQ dat ptrs fp
            [| $(pure acc) <*> $(pure ef) |])
        hd
        ps

-- | Walk fields in declaration order, assigning each non-skipped
-- field its zero-based index within its section.
pairFieldsWithSectionIdx :: [FieldInfo] -> Q [ParseSpec]
pairFieldsWithSectionIdx = go 0 0
  where
    go _ _ [] = pure []
    go d p (f@(FieldInfo mSel ty) : fs) = do
      mi <- case mSel of
        Just n  -> reifyModifierInfoFor backendCapnProto n
        Nothing -> pure (emptyModifierInfo backendCapnProto)
      if miSkip mi
        then (SpecSkipped f :) <$> go d p fs
        else case classifySlotForField ty mi of
          DataSlot    -> (SpecData    d f :) <$> go (d + 1) p       fs
          PointerSlot -> (SpecPointer p f :) <$> go d       (p + 1) fs

-- | Parser for a single field.
fieldParserQ :: Name -> Name -> ParseSpec -> Q Exp
fieldParserQ datName ptrsName spec = do
  let (mIdx, FieldInfo mSel fieldTy) = case spec of
        SpecSkipped f       -> (Nothing,                f)
        SpecData    i f     -> (Just (DataSlot,    i),  f)
        SpecPointer i f     -> (Just (PointerSlot, i),  f)
  selName <- requireSelector mSel
  mi <- reifyModifierInfoFor backendCapnProto selName
  case mIdx of
    Nothing ->
      case miDefaults mi of
        Just defNm -> [| Right $(varE defNm) |]
        Nothing    -> [| Left ("CapnProto.Derive: missing 'defaults' for skipped field "
                                ++ $(litE (stringL (nameBase selName)))) |]
    Just (kind, idx) -> do
      let idxLit  = litE (integerL (fromIntegral idx))
          nameLit = litE (stringL (nameBase selName))
          (sectionVar, sectionLabel) = case kind of
            DataSlot    -> (varE datName,  "data")
            PointerSlot -> (varE ptrsName, "pointer")
          sectionLabelLit = litE (stringL sectionLabel)
      base <-
        [| case lookupSlot $idxLit $sectionVar of
             Nothing ->
               Left ("CapnProto.Derive: missing " ++ $sectionLabelLit
                      ++ " slot " ++ show ($idxLit :: Int)
                      ++ " for field " ++ $nameLit)
             Just v -> fromCapnProto v |]
      case miCoerce mi of
        Nothing -> pure base
        Just nm -> [| fmap (coerce :: $(conT nm) -> $(pure fieldTy)) $(pure base) |]

-- | Lookup the @i@-th value in a struct slot vector.
lookupSlot :: Int -> Vector CP.Value -> Maybe CP.Value
lookupSlot i vs
  | i < 0 || i >= V.length vs = Nothing
  | otherwise                 = Just (vs V.! i)

fromCPEnum :: [ConInfo] -> Q Exp
fromCPEnum cs = do
  v <- newName "v"
  i <- newName "i"
  branches <- mapM (enumMatchFrom i) (zip [0 ..] cs)
  let multi = MultiIfE
        (map (\(g, e) -> (g, AppE (ConE 'Right) e)) branches
         ++ [(NormalG (ConE 'True),
               AppE (ConE 'Left)
                 (AppE (AppE (VarE 'mappend)
                       (LitE (StringL "CapnProto.Derive: unknown enum value ")))
                       (AppE (VarE 'show) (VarE i))))])
  lamE [varP v]
    (caseE (varE v)
       [ match (conP 'CP.Enum [varP i])
               (normalB (pure multi))
               []
       , match wildP
               (normalB [| Left "CapnProto.Derive: enum expected Enum" |])
               []
       ])
  where
    enumMatchFrom :: Name -> (Word16, ConInfo) -> Q (Guard, Exp)
    enumMatchFrom iVar (defaultIdx, c) = do
      mi <- reifyModifierInfoFor backendCapnProto (conInfoName c)
      let n = case miTag mi of
            Just t  -> fromIntegral t :: Word16
            Nothing -> defaultIdx
          guardE = InfixE (Just (VarE iVar)) (VarE '(==))
                         (Just (LitE (IntegerL (fromIntegral n))))
      pure (NormalG guardE, ConE (conInfoName c))

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Detect @Maybe a@ in a field type.
unwrapMaybe :: Type -> Maybe Type
unwrapMaybe (AppT (ConT n) t) | n == ''Maybe = Just t
unwrapMaybe _                                 = Nothing

requireSelector :: Maybe Name -> Q Name
requireSelector (Just n) = pure n
requireSelector Nothing  =
  fail "CapnProto.Derive: cannot derive CapnProto for non-record positional field"

applyTypeArgs :: Type -> [Type] -> Type
applyTypeArgs = foldl AppT
