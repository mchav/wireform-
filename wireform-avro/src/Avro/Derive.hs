{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Annotation-driven Template Haskell deriver for Avro
-- 'Avro.Class.ToAvro' \/ 'Avro.Class.FromAvro' instances, plus a
-- companion 'HasAvroSchema' class that exposes the corresponding
-- 'Avro.Schema.AvroType' for downstream tooling.
--
-- Avro is special among the wireform backends because the runtime
-- 'Avro.Value.Value' representation is /positional/ for records: the
-- field /names/ live exclusively in the schema. The deriver therefore
-- emits two parallel pieces of generated code:
--
-- 1. 'Avro.Class.toAvro' \/ 'Avro.Class.fromAvro' — produce / consume
--    an 'AV.Record' as a positional 'Vector Value'. No field names
--    appear at the value level.
-- 2. 'avroSchema' (via 'HasAvroSchema') — exposes the 'AS.AvroType'
--    that names those positional fields, with all 'rename' \/
--    'renameStyle' \/ 'renameIdiomatic' modifiers applied.
--
-- Encoding shape:
--
-- * 'TypeShapeNewtype' — pass-through to the inner field's instance,
--   schema-wise too.
-- * 'TypeShapeRecord'  — 'AV.Record' with one entry per non-skipped
--   field, in declaration order. Schema is an 'AS.AvroRecord' with
--   the same fields named after the (possibly renamed) selector base
--   names.
-- * 'TypeShapeEnum'    — 'AV.Enum' carrying the constructor's
--   declaration-order ordinal. Schema is an 'AS.AvroEnum' whose
--   symbol vector contains the (possibly renamed) constructor names
--   in the same order.
-- * 'TypeShapeSum'     — 'AV.Union' indexed by constructor ordinal.
--   Each branch's payload is encoded as 'AV.Null' (nullary), the
--   inner value (unary), or a positional 'AV.Record' (n-ary). Schema
--   is an 'AS.AvroUnion' whose branches mirror that.
--
-- Modifiers honoured:
--
-- * 'rename', 'renameStyle', 'renameWith' — affect the schema name
--   only (records are positional on the wire).
-- * 'skip' — drops the field from both the encoded vector and the
--   schema; on decode, requires a 'defaults' modifier.
-- * 'defaults' — supplies the value for skipped fields on decode.
-- * 'coerced' — wraps encode \/ decode in 'Data.Coerce.coerce'.
--
-- 'tag' is meaningless for Avro records (positional layout) and is
-- ignored.
module Avro.Derive
  ( -- * Instance derivation
    deriveAvro
  , deriveToAvro
  , deriveFromAvro

    -- * Schema reflection
  , avroSchemaFor
  , HasAvroSchema (..)
  ) where

import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.Foldable (foldlM)
import Data.Int (Int8, Int16, Int32, Int64)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Word (Word8, Word16, Word32, Word64)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (lift)

import qualified Avro.Class as A
import qualified Avro.Schema as AS
import qualified Avro.Value as AV

import Wireform.Derive.Backend
import Wireform.Derive.ModifierInfo
import Wireform.Derive.TypeInfo

-- ---------------------------------------------------------------------------
-- HasAvroSchema
-- ---------------------------------------------------------------------------

-- | Reflect a Haskell type onto its Avro schema.
--
-- The 'Proxy' argument carries no value-level information; we keep it
-- so callers can write @'avroSchema' ('Proxy' :: 'Proxy' MyType)@ at
-- a use site without 'TypeApplications'. The companion deriver
-- 'deriveAvro' emits an instance for every type it touches.
class HasAvroSchema a where
  avroSchema :: proxy a -> AS.AvroType

instance HasAvroSchema Bool   where avroSchema _ = AS.AvroPrimitive AS.AvroBool
instance HasAvroSchema Int    where avroSchema _ = AS.AvroPrimitive AS.AvroLong
instance HasAvroSchema Int8   where avroSchema _ = AS.AvroPrimitive AS.AvroInt
instance HasAvroSchema Int16  where avroSchema _ = AS.AvroPrimitive AS.AvroInt
instance HasAvroSchema Int32  where avroSchema _ = AS.AvroPrimitive AS.AvroInt
instance HasAvroSchema Int64  where avroSchema _ = AS.AvroPrimitive AS.AvroLong
instance HasAvroSchema Word   where avroSchema _ = AS.AvroPrimitive AS.AvroLong
instance HasAvroSchema Word8  where avroSchema _ = AS.AvroPrimitive AS.AvroInt
instance HasAvroSchema Word16 where avroSchema _ = AS.AvroPrimitive AS.AvroInt
instance HasAvroSchema Word32 where avroSchema _ = AS.AvroPrimitive AS.AvroLong
instance HasAvroSchema Word64 where avroSchema _ = AS.AvroPrimitive AS.AvroLong
instance HasAvroSchema Float  where avroSchema _ = AS.AvroPrimitive AS.AvroFloat
instance HasAvroSchema Double where avroSchema _ = AS.AvroPrimitive AS.AvroDouble
instance HasAvroSchema Text   where avroSchema _ = AS.AvroPrimitive AS.AvroString
instance HasAvroSchema ByteString where
  avroSchema _ = AS.AvroPrimitive AS.AvroBytes
instance HasAvroSchema () where avroSchema _ = AS.AvroPrimitive AS.AvroNull

-- | An optional value renders as a two-branch union @[null, T]@,
-- matching how @ToAvro (Maybe a)@ encodes 'Nothing' as 'AV.Null' and
-- 'Just' x as @toAvro x@.
instance HasAvroSchema a => HasAvroSchema (Maybe a) where
  avroSchema _ = AS.AvroUnion
    { AS.avroUnionBranches = V.fromList
        [ AS.AvroPrimitive AS.AvroNull
        , avroSchema (Proxy :: Proxy a)
        ]
    }

instance HasAvroSchema a => HasAvroSchema [a] where
  avroSchema _ = AS.AvroArray
    { AS.avroArrayItems = avroSchema (Proxy :: Proxy a)
    }

instance HasAvroSchema a => HasAvroSchema (V.Vector a) where
  avroSchema _ = AS.AvroArray
    { AS.avroArrayItems = avroSchema (Proxy :: Proxy a)
    }

-- ---------------------------------------------------------------------------
-- Public entry points
-- ---------------------------------------------------------------------------

-- | Derive 'A.ToAvro', 'A.FromAvro', and 'HasAvroSchema' instances
-- for a type.
deriveAvro :: Name -> Q [Dec]
deriveAvro nm = do
  to     <- deriveToAvro nm
  from   <- deriveFromAvro nm
  schema <- deriveHasAvroSchema nm
  pure (to ++ from ++ schema)

deriveToAvro :: Name -> Q [Dec]
deriveToAvro nm = do
  ti   <- reifyTypeInfo nm
  body <- toAvroBody ti
  let typ  = applyTypeArgs (ConT (typeInfoName ti)) (typeInfoVarTypes ti)
      decl = InstanceD Nothing []
              (AppT (ConT ''A.ToAvro) typ)
              [FunD 'A.toAvro [Clause [] (NormalB body) []]]
  pure [decl]

deriveFromAvro :: Name -> Q [Dec]
deriveFromAvro nm = do
  ti   <- reifyTypeInfo nm
  body <- fromAvroBody ti
  let typ  = applyTypeArgs (ConT (typeInfoName ti)) (typeInfoVarTypes ti)
      decl = InstanceD Nothing []
              (AppT (ConT ''A.FromAvro) typ)
              [FunD 'A.fromAvro [Clause [] (NormalB body) []]]
  pure [decl]

deriveHasAvroSchema :: Name -> Q [Dec]
deriveHasAvroSchema nm = do
  ti     <- reifyTypeInfo nm
  schema <- avroSchemaExp ti
  let typ  = applyTypeArgs (ConT (typeInfoName ti)) (typeInfoVarTypes ti)
      decl = InstanceD Nothing []
              (AppT (ConT ''HasAvroSchema) typ)
              [FunD 'avroSchema [Clause [WildP] (NormalB schema) []]]
  pure [decl]

-- | Splice an 'AS.AvroType' value for the given type. Equivalent to
-- @avroSchema (Proxy :: Proxy T)@ but spelled to mirror the
-- @csvHeaderFor@ \/ @bsonSchemaFor@ aesthetic in sibling packages.
--
-- The type must already have a 'HasAvroSchema' instance — typically
-- emitted by 'deriveAvro'.
avroSchemaFor :: Name -> Q Exp
avroSchemaFor nm = do
  ti <- reifyTypeInfo nm
  let typ = applyTypeArgs (ConT (typeInfoName ti)) (typeInfoVarTypes ti)
  [| avroSchema (Proxy :: Proxy $(pure typ)) |]

-- ---------------------------------------------------------------------------
-- ToAvro
-- ---------------------------------------------------------------------------

toAvroBody :: TypeInfo -> Q Exp
toAvroBody ti = case typeInfoShape ti of
  TypeShapeNewtype c   -> toAvroNewtype c
  TypeShapeRecord  c   -> toAvroRecord  c
  TypeShapeEnum    cs  -> toAvroEnum    cs
  TypeShapeSum     cs  -> toAvroSum     cs

toAvroNewtype :: ConInfo -> Q Exp
toAvroNewtype c = case conInfoFields c of
  [FieldInfo (Just sel) _] -> do
    x <- newName "x"
    lamE [varP x] [| A.toAvro ($(varE sel) $(varE x)) |]
  [FieldInfo Nothing _] -> do
    x <- newName "x"
    lamE [conP (conInfoName c) [varP x]] [| A.toAvro $(varE x) |]
  _ -> fail "Avro.Derive: newtype must have exactly one field"

toAvroRecord :: ConInfo -> Q Exp
toAvroRecord c = do
  x      <- newName "x"
  fields <- recordToAvroFields (varE x) c
  lamE [varP x] [| AV.Record (V.fromList $(pure fields)) |]

recordToAvroFields :: Q Exp -> ConInfo -> Q Exp
recordToAvroFields varExp c = do
  exps <- mapM (toAvroField varExp) (conInfoFields c)
  pure (ListE (concat exps))

toAvroField :: Q Exp -> FieldInfo -> Q [Exp]
toAvroField varExp (FieldInfo mSel _) = do
  selName <- requireSelector mSel
  mi      <- reifyModifierInfoFor backendAvro selName
  if miSkip mi
    then pure []
    else do
      let getter  = appE (varE selName) varExp
          encoded = case miCoerce mi of
            Nothing -> [| A.toAvro $getter |]
            Just _  -> [| A.toAvro (coerce $getter) |]
      e <- encoded
      pure [e]

toAvroEnum :: [ConInfo] -> Q Exp
toAvroEnum cs = do
  v       <- newName "v"
  matches <- mapM enumMatch (zip [(0 :: Int) ..] cs)
  body    <- caseE (varE v) (map pure matches)
  lamE [varP v] (pure body)
  where
    enumMatch :: (Int, ConInfo) -> Q Match
    enumMatch (i, c) = do
      bodyE <- [| AV.Enum $(litE (integerL (fromIntegral i))) |]
      pure (Match (ConP (conInfoName c) [] []) (NormalB bodyE) [])

toAvroSum :: [ConInfo] -> Q Exp
toAvroSum cs = do
  v       <- newName "v"
  matches <- mapM sumMatch (zip [(0 :: Int) ..] cs)
  body    <- caseE (varE v) (map pure matches)
  lamE [varP v] (pure body)
  where
    sumMatch :: (Int, ConInfo) -> Q Match
    sumMatch (i, c) = do
      fieldNames <- mapM (\_ -> newName "f") (conInfoFields c)
      let pat = ConP (conInfoName c) [] (map VarP fieldNames)
      contentsE <- case fieldNames of
        []  -> [| AV.Null |]
        [n] -> [| A.toAvro $(varE n) |]
        ns  -> [| AV.Record (V.fromList
                    $(pure (ListE (map (AppE (VarE 'A.toAvro) . VarE) ns)))) |]
      body <-
        [| AV.Union
             $(litE (integerL (fromIntegral i)))
             $(pure contentsE) |]
      pure (Match pat (NormalB body) [])

-- ---------------------------------------------------------------------------
-- FromAvro
-- ---------------------------------------------------------------------------

fromAvroBody :: TypeInfo -> Q Exp
fromAvroBody ti = case typeInfoShape ti of
  TypeShapeNewtype c   -> fromAvroNewtype c
  TypeShapeRecord  c   -> fromAvroRecord  c
  TypeShapeEnum    cs  -> fromAvroEnum    cs
  TypeShapeSum     cs  -> fromAvroSum     cs

fromAvroNewtype :: ConInfo -> Q Exp
fromAvroNewtype c = case conInfoFields c of
  [FieldInfo _ _] -> [| fmap $(conE (conInfoName c)) . A.fromAvro |]
  _               -> fail "Avro.Derive: newtype must have exactly one field"

fromAvroRecord :: ConInfo -> Q Exp
fromAvroRecord c = do
  v     <- newName "v"
  vs    <- newName "vs"
  bodyE <- recordParser vs c
  lamE [varP v]
    (caseE (varE v)
       [ match (conP 'AV.Record [varP vs])
               (normalB (pure bodyE))
               []
       , match wildP
               (normalB
                  [| Left "Avro.Derive: expected Record for record type" |])
               []
       ])

-- | Walk the field list building a chain of @>>= \\v -> …@. The
-- positional vector index advances only for non-skipped fields, just
-- like the CSV deriver.
recordParser :: Name -> ConInfo -> Q Exp
recordParser vs c =
  case conInfoFields c of
    []     -> [| Right $(conE (conInfoName c)) |]
    fields -> buildSequence vs (conInfoName c) fields

buildSequence :: Name -> Name -> [FieldInfo] -> Q Exp
buildSequence vs conName = go 0 []
  where
    go :: Int -> [Name] -> [FieldInfo] -> Q Exp
    go _ acc [] = do
      let assemble = foldl (\e vN -> AppE e (VarE vN))
                           (ConE conName)
                           (reverse acc)
      [| Right $(pure assemble) |]
    go pos acc (f : fs) = do
      vName <- newName "v"
      (cellExp, advance) <- fieldCell vs pos f
      restExp            <- go (pos + advance) (vName : acc) fs
      [| $(pure cellExp) >>= \ $(varP vName) -> $(pure restExp) |]

-- | Produce @(parserExp, advance)@ for a single field. 'advance' is 0
-- when the field is skipped (no position consumed in the wire vector)
-- and 1 otherwise.
fieldCell :: Name -> Int -> FieldInfo -> Q (Exp, Int)
fieldCell vs pos (FieldInfo mSel _) = do
  selName <- requireSelector mSel
  mi      <- reifyModifierInfoFor backendAvro selName
  if miSkip mi
    then case miDefaults mi of
      Just defNm -> do
        e <- [| Right $(varE defNm) |]
        pure (e, 0)
      Nothing ->
        let msg = "Avro.Derive: missing 'defaults' for skipped field "
                  ++ nameBase selName
        in (\e -> (e, 0)) <$> [| Left $(litE (stringL msg)) |]
    else do
      let posLit = litE (integerL (fromIntegral pos))
          base = [| if V.length $(varE vs) > $posLit
                      then A.fromAvro ($(varE vs) V.! $posLit)
                      else Left ("Avro.Derive: record missing field at index "
                                 ++ show ($posLit :: Int)) |]
      e <- case miCoerce mi of
        Nothing -> base
        Just _  -> [| fmap coerce $base |]
      pure (e, 1)

fromAvroEnum :: [ConInfo] -> Q Exp
fromAvroEnum cs = do
  v <- newName "v"
  i <- newName "i"
  let branches = zipWith
        (\idx c ->
          ( NormalG (InfixE (Just (VarE i))
                            (VarE '(==))
                            (Just (LitE (IntegerL (fromIntegral idx)))))
          , AppE (ConE 'Right) (ConE (conInfoName c))
          ))
        [(0 :: Int) ..]
        cs
      fallback =
        ( NormalG (ConE 'True)
        , AppE (ConE 'Left)
            (AppE (AppE (VarE 'mappend)
                      (LitE (StringL "Avro.Derive: unknown enum index ")))
                  (AppE (VarE 'show) (VarE i)))
        )
      multi = MultiIfE (branches ++ [fallback])
  lamE [varP v]
    (caseE (varE v)
       [ match (conP 'AV.Enum [varP i])
               (normalB (pure multi))
               []
       , match wildP
               (normalB [| Left "Avro.Derive: enum expected Enum" |])
               []
       ])

fromAvroSum :: [ConInfo] -> Q Exp
fromAvroSum cs = do
  v        <- newName "v"
  iVar     <- newName "i"
  payload  <- newName "p"
  branches <- mapM (sumBranchDecode iVar payload) (zip [(0 :: Int) ..] cs)
  let fallback =
        ( NormalG (ConE 'True)
        , AppE (ConE 'Left)
            (AppE (AppE (VarE 'mappend)
                      (LitE (StringL "Avro.Derive: unknown union branch ")))
                  (AppE (VarE 'show) (VarE iVar)))
        )
      multi = MultiIfE (branches ++ [fallback])
  lamE [varP v]
    (caseE (varE v)
       [ match (conP 'AV.Union [varP iVar, varP payload])
               (normalB (pure multi))
               []
       , match wildP
               (normalB [| Left "Avro.Derive: sum expected Union" |])
               []
       ])

sumBranchDecode :: Name -> Name -> (Int, ConInfo) -> Q (Guard, Exp)
sumBranchDecode iVar payloadVar (idx, c) = do
  let guardExp = InfixE (Just (VarE iVar))
                        (VarE '(==))
                        (Just (LitE (IntegerL (fromIntegral idx))))
  body <- case conInfoFields c of
    []     -> [| Right $(conE (conInfoName c)) |]
    [_one] -> [| fmap $(conE (conInfoName c)) (A.fromAvro $(varE payloadVar)) |]
    many   -> sumNAry payloadVar (conInfoName c) (length many)
  pure (NormalG guardExp, body)

sumNAry :: Name -> Name -> Int -> Q Exp
sumNAry payloadVar conName arity = do
  arr <- newName "arr"
  let parseI :: Int -> Q Exp
      parseI i =
        [| A.fromAvro ($(varE arr) V.! $(litE (integerL (fromIntegral i)))) |]
  hd <- do
    e0 <- parseI 0
    [| $(conE conName) <$> $(pure e0) |]
  body <- foldlM
    (\acc i -> do
        ei <- parseI i
        [| $(pure acc) <*> $(pure ei) |])
    hd
    [1 .. arity - 1]
  let conNameStr = nameBase conName
  [| case $(varE payloadVar) of
       AV.Record $(varP arr)
         | V.length $(varE arr) == $(litE (integerL (fromIntegral arity)))
             -> $(pure body)
         | otherwise
             -> Left ("Avro.Derive: " ++ conNameStr
                      ++ " expected " ++ show arity ++ " contents, got "
                      ++ show (V.length $(varE arr)))
       _ -> Left ("Avro.Derive: " ++ conNameStr
                  ++ " expected Record contents")
   |]

-- ---------------------------------------------------------------------------
-- HasAvroSchema body
-- ---------------------------------------------------------------------------

avroSchemaExp :: TypeInfo -> Q Exp
avroSchemaExp ti = case typeInfoShape ti of
  TypeShapeNewtype c   -> avroNewtypeSchema c
  TypeShapeRecord  c   -> avroRecordSchema (typeInfoName ti) c
  TypeShapeEnum    cs  -> avroEnumSchema   (typeInfoName ti) cs
  TypeShapeSum     cs  -> avroSumSchema cs

avroNewtypeSchema :: ConInfo -> Q Exp
avroNewtypeSchema c = case conInfoFields c of
  [FieldInfo _ ty] -> [| avroSchema (Proxy :: Proxy $(pure ty)) |]
  _ -> fail "Avro.Derive: newtype must have exactly one field"

avroRecordSchema :: Name -> ConInfo -> Q Exp
avroRecordSchema tyName c = do
  fields <- recordFieldDecls c
  [| AS.AvroRecord
       { AS.avroRecordName      = $(litE (stringL (nameBase tyName)))
       , AS.avroRecordNamespace = Nothing
       , AS.avroRecordDoc       = Nothing
       , AS.avroRecordAliases   = V.empty
       , AS.avroRecordFields    = V.fromList $(pure fields)
       , AS.avroRecordProps     = Map.empty
       } |]

recordFieldDecls :: ConInfo -> Q Exp
recordFieldDecls c = do
  exps <- mapM fieldSchemaDecl (conInfoFields c)
  pure (ListE (concat exps))

fieldSchemaDecl :: FieldInfo -> Q [Exp]
fieldSchemaDecl (FieldInfo mSel ty) = do
  selName <- requireSelector mSel
  mi      <- reifyModifierInfoFor backendAvro selName
  if miSkip mi
    then pure []
    else do
      keyExp <- renderWireKey mi (T.pack (nameBase selName))
      e <- [| AS.AvroField
                { AS.avroFieldName    = $(pure keyExp)
                , AS.avroFieldType    = avroSchema (Proxy :: Proxy $(pure ty))
                , AS.avroFieldDefault = Nothing
                , AS.avroFieldOrder   = Nothing
                , AS.avroFieldAliases = V.empty
                , AS.avroFieldDoc     = Nothing
                , AS.avroFieldProps   = Map.empty
                } |]
      pure [e]

avroEnumSchema :: Name -> [ConInfo] -> Q Exp
avroEnumSchema tyName cs = do
  symbols <- mapM enumSymbol cs
  [| AS.AvroEnum
       { AS.avroEnumName      = $(litE (stringL (nameBase tyName)))
       , AS.avroEnumNamespace = Nothing
       , AS.avroEnumDoc       = Nothing
       , AS.avroEnumAliases   = V.empty
       , AS.avroEnumSymbols   = V.fromList $(pure (ListE symbols))
       , AS.avroEnumDefault   = Nothing
       } |]

enumSymbol :: ConInfo -> Q Exp
enumSymbol c = do
  mi <- reifyModifierInfoFor backendAvro (conInfoName c)
  renderWireKey mi (T.pack (nameBase (conInfoName c)))

avroSumSchema :: [ConInfo] -> Q Exp
avroSumSchema cs = do
  branches <- mapM sumBranchSchema cs
  [| AS.AvroUnion
       { AS.avroUnionBranches = V.fromList $(pure (ListE branches))
       } |]

sumBranchSchema :: ConInfo -> Q Exp
sumBranchSchema c = case conInfoFields c of
  []                -> [| AS.AvroPrimitive AS.AvroNull |]
  [FieldInfo _ ty]  -> [| avroSchema (Proxy :: Proxy $(pure ty)) |]
  fs                -> do
    mi      <- reifyModifierInfoFor backendAvro (conInfoName c)
    nameExp <- renderWireKey mi (T.pack (nameBase (conInfoName c)))
    fieldExps <- mapM
      (\(i :: Int, FieldInfo _ ty) -> do
        let fname = T.pack ('f' : show i)
        [| AS.AvroField
             { AS.avroFieldName    = $(lift fname)
             , AS.avroFieldType    = avroSchema (Proxy :: Proxy $(pure ty))
             , AS.avroFieldDefault = Nothing
             , AS.avroFieldOrder   = Nothing
             , AS.avroFieldAliases = V.empty
             , AS.avroFieldDoc     = Nothing
             , AS.avroFieldProps   = Map.empty
             } |])
      (zip [0 ..] fs)
    [| AS.AvroRecord
         { AS.avroRecordName      = $(pure nameExp)
         , AS.avroRecordNamespace = Nothing
         , AS.avroRecordDoc       = Nothing
         , AS.avroRecordAliases   = V.empty
         , AS.avroRecordFields    = V.fromList $(pure (ListE fieldExps))
         , AS.avroRecordProps     = Map.empty
         } |]

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

requireSelector :: Maybe Name -> Q Name
requireSelector (Just n) = pure n
requireSelector Nothing  =
  fail "Avro.Derive: cannot derive Avro for non-record positional field"

applyTypeArgs :: Type -> [Type] -> Type
applyTypeArgs = foldl AppT
