{-# LANGUAGE TemplateHaskell #-}

{- | FlatBuffers code generation — generates Haskell data types from
FlatBuffers schemas. Tables get Maybe for optional fields, structs
get strict records, enums and unions become sum types.
-}
module FlatBuffers.CodeGen (
  generateFlatBuffersTypes,
  generateFlatBuffersTypesWithRegistry,
  deriveFlatBuffers,
) where

import Data.Char (toLower, toUpper)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Word (Word16, Word32, Word64, Word8)
import FlatBuffers.Registry
import FlatBuffers.Schema
import Language.Haskell.TH


-- ---------------------------------------------------------------------------
-- Text-based code generation
-- ---------------------------------------------------------------------------

generateFlatBuffersTypes :: FlatBuffersSchema -> Text
generateFlatBuffersTypes = generateFlatBuffersTypesWithRegistry defaultFlatBuffersRegistry


{- | Generate Haskell source code using a custom 'FlatBuffersRegistry'.
When a table field has metadata matching a registered handler, the handler's
type transformation and extra code generation are applied.
-}
generateFlatBuffersTypesWithRegistry :: FlatBuffersRegistry -> FlatBuffersSchema -> Text
generateFlatBuffersTypesWithRegistry reg schema =
  let decls = concatMap (genFBDeclWithRegistry reg) (V.toList (fbsDecls schema))
  in T.intercalate "\n\n" decls


genFBDeclWithRegistry :: FlatBuffersRegistry -> FBDeclaration -> [Text]
genFBDeclWithRegistry reg (FBTable t) = genTableDeclWithRegistry reg t
genFBDeclWithRegistry _reg (FBStruct s) = [genFBStructDecl s]
genFBDeclWithRegistry _reg (FBEnum e) = [genFBEnumDecl e]
genFBDeclWithRegistry _reg (FBUnion u) = [genFBUnionDecl u]


-- ---------------------------------------------------------------------------
-- Table generation (text) — optional fields get Maybe
-- ---------------------------------------------------------------------------

genTableDeclWithRegistry :: FlatBuffersRegistry -> TableDef -> [Text]
genTableDeclWithRegistry reg td =
  let name = tdName td
      fields = V.toList (tdFields td)
      extraCode =
        concatMap
          ( \fld ->
              concatMap
                ( \(k, mv) ->
                    case Map.lookup k (frMetadataHandlers reg) of
                      Just handler -> FlatBuffers.Registry.hExtraCode handler k mv
                      Nothing -> []
                )
                (V.toList (tfMetadata fld))
          )
          fields
      mainDecl =
        T.unlines $
          ["data " <> name <> " = " <> name]
            <> case fields of
              [] ->
                ["  deriving stock (Show, Eq, Generic)"]
              (f : fs) ->
                ["  { " <> genTableFieldDeclWithRegistry reg name f]
                  <> map (\fld -> "  , " <> genTableFieldDeclWithRegistry reg name fld) fs
                  <> ["  } deriving stock (Show, Eq, Generic)"]
  in [mainDecl] <> if null extraCode then [] else [T.unlines extraCode]


genTableFieldDeclWithRegistry :: FlatBuffersRegistry -> Text -> TableField -> Text
genTableFieldDeclWithRegistry reg recName fld =
  let accessor = fbFieldAccessorName recName (tfName fld)
      baseType = case tfDefault fld of
        Nothing -> "!(Maybe " <> fbInnerHsType (tfType fld) <> ")"
        Just _ -> fbStrictHsType (tfType fld)
      hsType = applyFBMetadataTransforms reg fld baseType
  in accessor <> " :: " <> hsType


applyFBMetadataTransforms :: FlatBuffersRegistry -> TableField -> Text -> Text
applyFBMetadataTransforms reg fld ty =
  foldl
    ( \acc (k, _mv) ->
        case Map.lookup k (frMetadataHandlers reg) of
          Just handler -> FlatBuffers.Registry.hTransformType handler acc
          Nothing -> acc
    )
    ty
    (V.toList (tfMetadata fld))


-- ---------------------------------------------------------------------------
-- Struct generation (text) — all strict
-- ---------------------------------------------------------------------------

genFBStructDecl :: FBStructDef -> Text
genFBStructDecl fsd =
  let name = fsdName fsd
      fields = V.toList (fsdFields fsd)
  in T.unlines $
       ["data " <> name <> " = " <> name]
         <> case fields of
           [] ->
             ["  deriving stock (Show, Eq, Generic)"]
           ((fn, ft) : rest) ->
             ["  { " <> fbFieldAccessorName name fn <> " :: " <> fbStrictHsType ft]
               <> map (\(n, t) -> "  , " <> fbFieldAccessorName name n <> " :: " <> fbStrictHsType t) rest
               <> ["  } deriving stock (Show, Eq, Generic)"]


-- ---------------------------------------------------------------------------
-- Enum generation (text)
-- ---------------------------------------------------------------------------

genFBEnumDecl :: FBEnumDef -> Text
genFBEnumDecl fed =
  let name = fedName fed
      vals = V.toList (fedValues fed)
  in T.unlines $
       ["data " <> name]
         <> case vals of
           [] -> ["  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)"]
           ((sym, _) : rest) ->
             ["  = " <> fbEnumConName name sym]
               <> map (\(s, _) -> "  | " <> fbEnumConName name s) rest
               <> ["  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)"]


fbEnumConName :: Text -> Text -> Text
fbEnumConName enumName valName =
  enumName <> upperFirst (snakeToCamel valName)


-- ---------------------------------------------------------------------------
-- Union generation (text) — sum type
-- ---------------------------------------------------------------------------

genFBUnionDecl :: FBUnionDef -> Text
genFBUnionDecl fud =
  let name = fudName fud
      members = V.toList (fudMembers fud)
  in T.unlines $
       ["data " <> name]
         <> case members of
           [] -> ["  deriving stock (Show, Eq, Generic)"]
           (m : ms) ->
             ["  = " <> name <> upperFirst m <> " !" <> m]
               <> map (\mem -> "  | " <> name <> upperFirst mem <> " !" <> mem) ms
               <> ["  deriving stock (Show, Eq, Generic)"]


-- ---------------------------------------------------------------------------
-- Type mapping
-- ---------------------------------------------------------------------------

fbFieldAccessorName :: Text -> Text -> Text
fbFieldAccessorName recName fieldName =
  lowerFirst recName <> upperFirst (snakeToCamel fieldName)


fbStrictHsType :: FBType -> Text
fbStrictHsType = \case
  FTBool -> "!Bool"
  FTByte -> "{-# UNPACK #-} !Int8"
  FTUByte -> "{-# UNPACK #-} !Word8"
  FTShort -> "{-# UNPACK #-} !Int16"
  FTUShort -> "{-# UNPACK #-} !Word16"
  FTInt -> "{-# UNPACK #-} !Int32"
  FTUInt -> "{-# UNPACK #-} !Word32"
  FTLong -> "{-# UNPACK #-} !Int64"
  FTULong -> "{-# UNPACK #-} !Word64"
  FTFloat -> "{-# UNPACK #-} !Float"
  FTDouble -> "{-# UNPACK #-} !Double"
  FTString -> "!Text"
  FTVector inner -> "!(Vector " <> fbInnerHsType inner <> ")"
  FTNamed n -> "!" <> n


fbInnerHsType :: FBType -> Text
fbInnerHsType = \case
  FTBool -> "Bool"
  FTByte -> "Int8"
  FTUByte -> "Word8"
  FTShort -> "Int16"
  FTUShort -> "Word16"
  FTInt -> "Int32"
  FTUInt -> "Word32"
  FTLong -> "Int64"
  FTULong -> "Word64"
  FTFloat -> "Float"
  FTDouble -> "Double"
  FTString -> "Text"
  FTVector inner -> "(Vector " <> fbInnerHsType inner <> ")"
  FTNamed n -> n


-- ---------------------------------------------------------------------------
-- Template Haskell
-- ---------------------------------------------------------------------------

deriveFlatBuffers :: FlatBuffersSchema -> Q [Dec]
deriveFlatBuffers schema = do
  let decls = V.toList (fbsDecls schema)
  concat <$> mapM deriveFBDecl decls


deriveFBDecl :: FBDeclaration -> Q [Dec]
deriveFBDecl (FBTable t) = deriveFBTableTH t
deriveFBDecl (FBStruct s) = deriveFBStructTH s
deriveFBDecl (FBEnum e) = deriveFBEnumTH e
deriveFBDecl (FBUnion u) = deriveFBUnionTH u


-- ---------------------------------------------------------------------------
-- TH: Table (records with Maybe for optional)
-- ---------------------------------------------------------------------------

deriveFBTableTH :: TableDef -> Q [Dec]
deriveFBTableTH td = do
  let name = tdName td
      tyName = mkName (T.unpack name)
      conName = mkName (T.unpack name)
      fields = V.toList (tdFields td)
  fieldDecs <- mapM (mkTableField name) fields
  let dataDec =
        DataD
          []
          tyName
          []
          Nothing
          [RecC conName fieldDecs]
          [DerivClause (Just StockStrategy) [ConT ''Show, ConT ''Eq]]
  pure [dataDec]


mkTableField :: Text -> TableField -> Q VarBangType
mkTableField recName fld = do
  let accessor = fbFieldAccessorName recName (tfName fld)
      accName = mkName (T.unpack accessor)
  hsTy <- case tfDefault fld of
    Nothing -> do
      inner <- fbTypeToTH (tfType fld)
      pure (AppT (ConT ''Maybe) inner)
    Just _ -> fbTypeToTH (tfType fld)
  let bangTy = Bang NoSourceUnpackedness SourceStrict
  pure (accName, bangTy, hsTy)


-- ---------------------------------------------------------------------------
-- TH: Struct (strict records)
-- ---------------------------------------------------------------------------

deriveFBStructTH :: FBStructDef -> Q [Dec]
deriveFBStructTH fsd = do
  let name = fsdName fsd
      tyName = mkName (T.unpack name)
      conName = mkName (T.unpack name)
      fields = V.toList (fsdFields fsd)
  fieldDecs <- mapM (mkStructField name) fields
  let dataDec =
        DataD
          []
          tyName
          []
          Nothing
          [RecC conName fieldDecs]
          [DerivClause (Just StockStrategy) [ConT ''Show, ConT ''Eq]]
  pure [dataDec]


mkStructField :: Text -> (Text, FBType) -> Q VarBangType
mkStructField recName (fieldName, fieldType) = do
  let accessor = fbFieldAccessorName recName fieldName
      accName = mkName (T.unpack accessor)
  hsTy <- fbTypeToTH fieldType
  let bangTy = Bang NoSourceUnpackedness SourceStrict
  pure (accName, bangTy, hsTy)


-- ---------------------------------------------------------------------------
-- TH: Enum
-- ---------------------------------------------------------------------------

deriveFBEnumTH :: FBEnumDef -> Q [Dec]
deriveFBEnumTH fed = do
  let name = fedName fed
      tyName = mkName (T.unpack name)
      vals = V.toList (fedValues fed)
      cons = map (\(sym, _) -> NormalC (mkName (T.unpack (fbEnumConName name sym))) []) vals
      dataDec =
        DataD
          []
          tyName
          []
          Nothing
          cons
          [ DerivClause
              (Just StockStrategy)
              [ConT ''Show, ConT ''Eq, ConT ''Ord, ConT ''Enum, ConT ''Bounded]
          ]
  pure [dataDec]


-- ---------------------------------------------------------------------------
-- TH: Union
-- ---------------------------------------------------------------------------

deriveFBUnionTH :: FBUnionDef -> Q [Dec]
deriveFBUnionTH fud = do
  let name = fudName fud
      tyName = mkName (T.unpack name)
      members = V.toList (fudMembers fud)
      mkBang ty = (Bang NoSourceUnpackedness SourceStrict, ty)
      cons =
        map
          ( \mem ->
              NormalC
                (mkName (T.unpack (name <> upperFirst mem)))
                [mkBang (ConT (mkName (T.unpack mem)))]
          )
          members
      dataDec =
        DataD
          []
          tyName
          []
          Nothing
          cons
          [DerivClause (Just StockStrategy) [ConT ''Show, ConT ''Eq]]
  pure [dataDec]


-- ---------------------------------------------------------------------------
-- TH type mapping
-- ---------------------------------------------------------------------------

fbTypeToTH :: FBType -> Q Type
fbTypeToTH = \case
  FTBool -> [t|Bool|]
  FTByte -> [t|Int8|]
  FTUByte -> [t|Word8|]
  FTShort -> [t|Int16|]
  FTUShort -> [t|Word16|]
  FTInt -> [t|Int32|]
  FTUInt -> [t|Word32|]
  FTLong -> [t|Int64|]
  FTULong -> [t|Word64|]
  FTFloat -> [t|Float|]
  FTDouble -> [t|Double|]
  FTString -> [t|Text|]
  FTVector inner -> do
    innerTy <- fbTypeToTH inner
    pure (AppT (ConT ''V.Vector) innerTy)
  FTNamed n -> pure (ConT (mkName (T.unpack n)))


-- ---------------------------------------------------------------------------
-- Name helpers
-- ---------------------------------------------------------------------------

lowerFirst :: Text -> Text
lowerFirst s = case T.uncons s of
  Just (c, rest) -> T.cons (toLower c) rest
  Nothing -> s


upperFirst :: Text -> Text
upperFirst s = case T.uncons s of
  Just (c, rest) -> T.cons (toUpper c) rest
  Nothing -> s


snakeToCamel :: Text -> Text
snakeToCamel t =
  let parts = T.splitOn "_" t
  in case parts of
       [] -> t
       (p : ps) -> T.concat (lowerFirst p : map titleCase ps)


titleCase :: Text -> Text
titleCase s = case T.uncons s of
  Just (c, rest) -> T.cons (toUpper c) (T.toLower rest)
  Nothing -> s
