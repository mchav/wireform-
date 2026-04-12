{-# LANGUAGE TemplateHaskell #-}
-- | Avro code generation — generates Haskell data types and
-- ToAvro\/FromAvro instances from Avro schemas.
--
-- == Text generation
--
-- 'generateAvroTypes' takes an 'AvroType' and produces Haskell source
-- as 'Text'.  The output includes @data@ declarations, strict fields,
-- @UNPACK@ pragmas, and ToAvro\/FromAvro instances.
--
-- == Template Haskell
--
-- 'deriveAvro' generates declarations at compile time from an 'AvroType'.
-- 'deriveAvroFromJSON' parses schema JSON and generates.
module Avro.CodeGen
  ( generateAvroTypes
  , generateAvroTypesWithRegistry
  , deriveAvro
  , deriveAvroFromJSON
  ) where

import Data.ByteString (ByteString)
import Data.Char (toLower, toUpper)
import Data.Int (Int32, Int64)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Language.Haskell.TH

import Avro.Schema
  ( AvroType(..)
  , AvroSchema(..)
  , AvroField(..)
  , LogicalType(..)
  )
import Avro.Schema.Parse (parseAvroSchema)
import Avro.Registry


-- ---------------------------------------------------------------------------
-- Text-based code generation
-- ---------------------------------------------------------------------------

-- | Generate Haskell source code (as 'Text') for a given 'AvroType'.
-- Produces data type declarations, ToAvro instances, and FromAvro instances
-- for records, enums, and nested types.
-- Uses 'defaultAvroRegistry' for logical type handling.
generateAvroTypes :: AvroType -> Text
generateAvroTypes = generateAvroTypesWithRegistry defaultAvroRegistry

-- | Generate Haskell source code using a custom 'AvroRegistry'.
-- When a field has @AvroLogical@ with a logical type name found in
-- the registry, the handler's Haskell type is used instead of the base type.
-- When a field has custom props matching a registered 'PropHandler',
-- extra code is emitted.
generateAvroTypesWithRegistry :: AvroRegistry -> AvroType -> Text
generateAvroTypesWithRegistry reg ty = T.intercalate "\n\n" (collectDecls reg ty)

collectDecls :: AvroRegistry -> AvroType -> [Text]
collectDecls reg = \case
  AvroRecord{avroRecordName = name, avroRecordFields = fields} ->
    let fieldList = V.toList fields
        nested = concatMap (collectNestedFieldDecls reg) fieldList
        propLines = concatMap (fieldPropLines reg) fieldList
    in nested
      <> [ genRecordDecl reg name fieldList
         , genToAvroRecord reg name fieldList
         , genFromAvroRecord reg name fieldList
         ]
      <> if null propLines then [] else [T.unlines propLines]

  AvroEnum{avroEnumName = name, avroEnumSymbols = syms} ->
    let symList = V.toList syms
    in [ genEnumDecl name symList
       , genToAvroEnum name symList
       , genFromAvroEnum name symList
       ]

  AvroArray{avroArrayItems = itemTy} -> collectDecls reg itemTy
  AvroMap{avroMapValues = valTy} -> collectDecls reg valTy
  AvroUnion{avroUnionBranches = branches} ->
    concatMap (collectDecls reg) (V.toList branches)
  _ -> []

fieldPropLines :: AvroRegistry -> AvroField -> [Text]
fieldPropLines reg fld =
  concatMap (\(k, v) ->
    case Map.lookup k (arCustomProps reg) of
      Just handler -> phCodeGen handler k v
      Nothing -> []
  ) (Map.toList (avroFieldProps fld))

collectNestedFieldDecls :: AvroRegistry -> AvroField -> [Text]
collectNestedFieldDecls reg fld = case avroFieldType fld of
  AvroRecord{} -> collectDecls reg (avroFieldType fld)
  AvroEnum{} -> collectDecls reg (avroFieldType fld)
  AvroUnion{avroUnionBranches = branches} ->
    concatMap (collectNestedInUnion reg) (V.toList branches)
  AvroArray{avroArrayItems = itemTy} -> collectNestedInner reg itemTy
  AvroMap{avroMapValues = valTy} -> collectNestedInner reg valTy
  _ -> []

collectNestedInUnion :: AvroRegistry -> AvroType -> [Text]
collectNestedInUnion reg ty = case ty of
  AvroRecord{} -> collectDecls reg ty
  AvroEnum{} -> collectDecls reg ty
  _ -> []

collectNestedInner :: AvroRegistry -> AvroType -> [Text]
collectNestedInner reg ty = case ty of
  AvroRecord{} -> collectDecls reg ty
  AvroEnum{} -> collectDecls reg ty
  _ -> []

-- ---------------------------------------------------------------------------
-- Record data declaration
-- ---------------------------------------------------------------------------

genRecordDecl :: AvroRegistry -> Text -> [AvroField] -> Text
genRecordDecl reg name fields = T.unlines $
  [ "data " <> name <> " = " <> name ]
  <> case fields of
    [] -> [ "  deriving stock (Show, Eq, Generic)"
          , "  deriving anyclass NFData"
          ]
    (f:fs) ->
      [ "  { " <> genFieldDecl reg name f ]
      <> map (\fld -> "  , " <> genFieldDecl reg name fld) fs
      <> [ "  } deriving stock (Show, Eq, Generic)"
         , "    deriving anyclass NFData"
         ]

genFieldDecl :: AvroRegistry -> Text -> AvroField -> Text
genFieldDecl reg recName fld =
  let accessor = fieldAccessorName recName (avroFieldName fld)
      hsType = avroFieldHsType reg (avroFieldType fld)
  in accessor <> " :: " <> hsType

fieldAccessorName :: Text -> Text -> Text
fieldAccessorName recName fieldName =
  lowerFirst recName <> upperFirst (snakeToCamel fieldName)

-- | Map an Avro field type to its strict Haskell type annotation.
avroFieldHsType :: AvroRegistry -> AvroType -> Text
avroFieldHsType reg = \case
  AvroPrimitive AvroNull -> "()"
  AvroPrimitive AvroBool -> "!Bool"
  AvroPrimitive AvroInt -> "{-# UNPACK #-} !Int32"
  AvroPrimitive AvroLong -> "{-# UNPACK #-} !Int64"
  AvroPrimitive AvroFloat -> "{-# UNPACK #-} !Float"
  AvroPrimitive AvroDouble -> "{-# UNPACK #-} !Double"
  AvroPrimitive AvroBytes -> "!ByteString"
  AvroPrimitive AvroString -> "!Text"
  AvroPrimitive (AvroSchemaRef ref) -> "!" <> ref

  AvroRecord{avroRecordName = n} -> "!" <> n
  AvroEnum{avroEnumName = n} -> "!" <> n

  AvroArray{avroArrayItems = itemTy} ->
    "!(Vector " <> avroInnerHsType reg itemTy <> ")"

  AvroMap{avroMapValues = valTy} ->
    "!(Map Text " <> avroInnerHsType reg valTy <> ")"

  AvroUnion{avroUnionBranches = branches} ->
    case isNullableUnion (V.toList branches) of
      Just inner -> "!(Maybe " <> avroInnerHsType reg inner <> ")"
      Nothing -> "!Avro.Value.Value"

  AvroFixed{} -> "!ByteString"
  AvroLogical{avroLogicalBase = base, avroLogicalType = lt} ->
    case logicalTypeLookup reg lt of
      Just handler -> "!" <> lthHaskellType handler
      Nothing -> avroFieldHsType reg base

-- | The "inner" type without strictness annotations (for containers).
avroInnerHsType :: AvroRegistry -> AvroType -> Text
avroInnerHsType reg = \case
  AvroPrimitive AvroNull -> "()"
  AvroPrimitive AvroBool -> "Bool"
  AvroPrimitive AvroInt -> "Int32"
  AvroPrimitive AvroLong -> "Int64"
  AvroPrimitive AvroFloat -> "Float"
  AvroPrimitive AvroDouble -> "Double"
  AvroPrimitive AvroBytes -> "ByteString"
  AvroPrimitive AvroString -> "Text"
  AvroPrimitive (AvroSchemaRef ref) -> ref
  AvroRecord{avroRecordName = n} -> n
  AvroEnum{avroEnumName = n} -> n
  AvroArray{avroArrayItems = itemTy} ->
    "(Vector " <> avroInnerHsType reg itemTy <> ")"
  AvroMap{avroMapValues = valTy} ->
    "(Map Text " <> avroInnerHsType reg valTy <> ")"
  AvroUnion{avroUnionBranches = branches} ->
    case isNullableUnion (V.toList branches) of
      Just inner -> "(Maybe " <> avroInnerHsType reg inner <> ")"
      Nothing -> "Avro.Value.Value"
  AvroFixed{} -> "ByteString"
  AvroLogical{avroLogicalType = lt, avroLogicalBase = base} ->
    case logicalTypeLookup reg lt of
      Just handler -> lthHaskellType handler
      Nothing -> avroInnerHsType reg base

logicalTypeLookup :: AvroRegistry -> LogicalType -> Maybe LogicalTypeHandler
logicalTypeLookup reg lt = Map.lookup (logicalTypeToName lt) (arLogicalTypes reg)

logicalTypeToName :: LogicalType -> Text
logicalTypeToName DateLogical             = "date"
logicalTypeToName TimeMillisLogical       = "time-millis"
logicalTypeToName TimeMicrosLogical       = "time-micros"
logicalTypeToName TimestampMillisLogical  = "timestamp-millis"
logicalTypeToName TimestampMicrosLogical  = "timestamp-micros"
logicalTypeToName DurationLogical         = "duration"
logicalTypeToName UuidLogical             = "uuid"
logicalTypeToName (DecimalLogical _ _)    = "decimal"
logicalTypeToName (CustomLogical name)    = name

-- | Check if a union is ["null", T] or [T, "null"] and return T.
isNullableUnion :: [AvroType] -> Maybe AvroType
isNullableUnion [AvroPrimitive AvroNull, other] = Just other
isNullableUnion [other, AvroPrimitive AvroNull] = Just other
isNullableUnion _ = Nothing

-- ---------------------------------------------------------------------------
-- Enum declarations
-- ---------------------------------------------------------------------------

genEnumDecl :: Text -> [Text] -> Text
genEnumDecl name syms = T.unlines $
  case syms of
    [] ->
      [ "data " <> name
      , "  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)"
      , "  deriving anyclass NFData"
      ]
    (s:ss) ->
      [ "data " <> name ]
      <> [ "  = " <> enumConName name s ]
      <> map (\sym -> "  | " <> enumConName name sym) ss
      <> [ "  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)"
         , "  deriving anyclass NFData"
         ]

enumConName :: Text -> Text -> Text
enumConName enumName sym =
  let parts = T.splitOn "_" sym
  in enumName <> T.concat (map (titleCase . T.toLower) parts)

-- ---------------------------------------------------------------------------
-- ToAvro instances
-- ---------------------------------------------------------------------------

genToAvroRecord :: AvroRegistry -> Text -> [AvroField] -> Text
genToAvroRecord reg name fields = T.unlines $
  [ "instance ToAvro " <> name <> " where"
  , "  toAvro msg = Avro.Value.Record $ V.fromList"
  ] <> case fields of
    [] -> [ "    []" ]
    _  ->
      [ "    [ " <> toAvroFieldExpr reg name (head fields) ]
      <> map (\f -> "    , " <> toAvroFieldExpr reg name f) (tail fields)
      <> [ "    ]" ]

toAvroFieldExpr :: AvroRegistry -> Text -> AvroField -> Text
toAvroFieldExpr reg recName fld =
  let accessor = "msg." <> fieldAccessorName recName (avroFieldName fld)
  in toAvroConvert reg (avroFieldType fld) accessor

toAvroConvert :: AvroRegistry -> AvroType -> Text -> Text
toAvroConvert reg ty accessor = case ty of
  AvroPrimitive AvroNull -> "Avro.Value.Null"
  AvroPrimitive AvroBool -> "Avro.Value.Bool " <> accessor
  AvroPrimitive AvroInt -> "Avro.Value.Int " <> accessor
  AvroPrimitive AvroLong -> "Avro.Value.Long " <> accessor
  AvroPrimitive AvroFloat -> "Avro.Value.Float " <> accessor
  AvroPrimitive AvroDouble -> "Avro.Value.Double " <> accessor
  AvroPrimitive AvroBytes -> "Avro.Value.Bytes " <> accessor
  AvroPrimitive AvroString -> "Avro.Value.String " <> accessor
  AvroPrimitive (AvroSchemaRef _) -> "toAvro " <> accessor
  AvroRecord{} -> "toAvro " <> accessor
  AvroEnum{avroEnumName = n} -> "Avro.Value.Enum (fromEnum " <> accessor <> ")"
  AvroArray{} -> "Avro.Value.Array (V.map toAvro " <> accessor <> ")"
  AvroMap{} -> "Avro.Value.Map (V.fromList (map (\\(k,v) -> (k, toAvro v)) (Map.toList " <> accessor <> ")))"
  AvroUnion{avroUnionBranches = branches} ->
    case isNullableUnion (V.toList branches) of
      Just _ -> "maybe (Avro.Value.Union 0 Avro.Value.Null) (\\x -> Avro.Value.Union 1 (toAvro x)) " <> accessor
      Nothing -> "toAvro " <> accessor
  AvroFixed{} -> "Avro.Value.Fixed " <> accessor
  AvroLogical{avroLogicalBase = base, avroLogicalType = lt} ->
    case logicalTypeLookup reg lt of
      Just handler -> lthEncode handler <> " " <> accessor
      Nothing -> toAvroConvert reg base accessor

genToAvroEnum :: Text -> [Text] -> Text
genToAvroEnum name syms = T.unlines $
  [ "instance ToAvro " <> name <> " where" ]
  <> zipWith (\i sym ->
      "  toAvro " <> enumConName name sym <> " = Avro.Value.Enum " <> T.pack (show (i :: Int))
    ) [0..] syms

-- ---------------------------------------------------------------------------
-- FromAvro instances
-- ---------------------------------------------------------------------------

genFromAvroRecord :: AvroRegistry -> Text -> [AvroField] -> Text
genFromAvroRecord reg name fields = T.unlines $
  [ "instance FromAvro " <> name <> " where"
  , "  fromAvro (Avro.Value.Record fields') = do"
  ] <> case fields of
    [] -> [ "    pure " <> name ]
    _  ->
      zipWith (\i fld ->
        let accessor = fieldAccessorName name (avroFieldName fld)
        in "    " <> accessor <> "' <- " <> fromAvroFieldExpr reg (avroFieldType fld) i
      ) [0 :: Int ..] fields
      <> [ "    pure " <> name
         , "      { " <> T.intercalate "\n      , "
            (map (\fld ->
              let accessor = fieldAccessorName name (avroFieldName fld)
              in accessor <> " = " <> accessor <> "'"
            ) fields)
         , "      }"
         ]
  <> [ "  fromAvro _ = Left \"FromAvro " <> name <> ": expected Record\"" ]

fromAvroFieldExpr :: AvroRegistry -> AvroType -> Int -> Text
fromAvroFieldExpr reg ty idx =
  let getExpr = "fields' V.! " <> T.pack (show idx)
  in case ty of
    AvroPrimitive AvroNull -> "pure ()"
    AvroPrimitive AvroBool ->
      "case " <> getExpr <> " of { Avro.Value.Bool v -> Right v; _ -> Left \"expected Bool\" }"
    AvroPrimitive AvroInt ->
      "case " <> getExpr <> " of { Avro.Value.Int v -> Right v; _ -> Left \"expected Int\" }"
    AvroPrimitive AvroLong ->
      "case " <> getExpr <> " of { Avro.Value.Long v -> Right v; _ -> Left \"expected Long\" }"
    AvroPrimitive AvroFloat ->
      "case " <> getExpr <> " of { Avro.Value.Float v -> Right v; _ -> Left \"expected Float\" }"
    AvroPrimitive AvroDouble ->
      "case " <> getExpr <> " of { Avro.Value.Double v -> Right v; _ -> Left \"expected Double\" }"
    AvroPrimitive AvroBytes ->
      "case " <> getExpr <> " of { Avro.Value.Bytes v -> Right v; _ -> Left \"expected Bytes\" }"
    AvroPrimitive AvroString ->
      "case " <> getExpr <> " of { Avro.Value.String v -> Right v; _ -> Left \"expected String\" }"
    AvroPrimitive (AvroSchemaRef _) ->
      "fromAvro (" <> getExpr <> ")"
    AvroRecord{} ->
      "fromAvro (" <> getExpr <> ")"
    AvroEnum{} ->
      "case " <> getExpr <> " of { Avro.Value.Enum v -> Right (toEnum v); _ -> Left \"expected Enum\" }"
    AvroArray{} ->
      "case " <> getExpr <> " of { Avro.Value.Array vs -> V.mapM fromAvro vs; _ -> Left \"expected Array\" }"
    AvroMap{} ->
      "case " <> getExpr <> " of { Avro.Value.Map entries -> Right (Map.fromList [(k, v') | (k, v) <- V.toList entries, Right v' <- [fromAvro v]]); _ -> Left \"expected Map\" }"
    AvroUnion{avroUnionBranches = branches} ->
      case isNullableUnion (V.toList branches) of
        Just _ ->
          "case " <> getExpr <> " of { Avro.Value.Union _ Avro.Value.Null -> Right Nothing; Avro.Value.Union _ v -> Just <$> fromAvro v; Avro.Value.Null -> Right Nothing; other -> Just <$> fromAvro other }"
        Nothing ->
          "fromAvro (" <> getExpr <> ")"
    AvroFixed{} ->
      "case " <> getExpr <> " of { Avro.Value.Fixed v -> Right v; Avro.Value.Bytes v -> Right v; _ -> Left \"expected Fixed\" }"
    AvroLogical{avroLogicalBase = base, avroLogicalType = lt} ->
      case logicalTypeLookup reg lt of
        Just handler -> lthDecode handler <> " (" <> getExpr <> ")"
        Nothing -> fromAvroFieldExpr reg base idx

genFromAvroEnum :: Text -> [Text] -> Text
genFromAvroEnum name syms = T.unlines $
  [ "instance FromAvro " <> name <> " where" ]
  <> zipWith (\i sym ->
      "  fromAvro (Avro.Value.Enum " <> T.pack (show (i :: Int)) <> ") = Right " <> enumConName name sym
    ) [0..] syms
  <> [ "  fromAvro _ = Left \"FromAvro " <> name <> ": expected Enum\"" ]

-- ---------------------------------------------------------------------------
-- Template Haskell
-- ---------------------------------------------------------------------------

-- | Generate Haskell declarations from an 'AvroType' at compile time.
deriveAvro :: AvroType -> Q [Dec]
deriveAvro ty = do
  let decls = collectTypes ty
  concat <$> mapM deriveOneType decls

-- | Parse Avro schema JSON and generate declarations at compile time.
deriveAvroFromJSON :: ByteString -> Q [Dec]
deriveAvroFromJSON bs = case parseAvroSchema bs of
  Left err -> fail ("deriveAvroFromJSON: " <> err)
  Right ty -> deriveAvro ty

data CollectedType
  = CTRecord Text [AvroField]
  | CTEnum Text [Text]

collectTypes :: AvroType -> [CollectedType]
collectTypes = \case
  AvroRecord{avroRecordName = name, avroRecordFields = fields} ->
    let nested = concatMap (collectFieldTypes . avroFieldType) (V.toList fields)
    in nested <> [CTRecord name (V.toList fields)]
  AvroEnum{avroEnumName = name, avroEnumSymbols = syms} ->
    [CTEnum name (V.toList syms)]
  AvroArray{avroArrayItems = itemTy} -> collectTypes itemTy
  AvroMap{avroMapValues = valTy} -> collectTypes valTy
  AvroUnion{avroUnionBranches = branches} ->
    concatMap collectTypes (V.toList branches)
  _ -> []

collectFieldTypes :: AvroType -> [CollectedType]
collectFieldTypes ty = case ty of
  AvroRecord{} -> collectTypes ty
  AvroEnum{} -> collectTypes ty
  AvroUnion{avroUnionBranches = branches} ->
    concatMap collectFieldTypes (V.toList branches)
  AvroArray{avroArrayItems = itemTy} -> collectFieldTypes itemTy
  AvroMap{avroMapValues = valTy} -> collectFieldTypes valTy
  _ -> []

deriveOneType :: CollectedType -> Q [Dec]
deriveOneType (CTRecord name fields) = deriveRecordTH name fields
deriveOneType (CTEnum name syms) = deriveEnumTH name syms

-- ---------------------------------------------------------------------------
-- TH: Record
-- ---------------------------------------------------------------------------

deriveRecordTH :: Text -> [AvroField] -> Q [Dec]
deriveRecordTH name fields = do
  let tyName = mkName (T.unpack name)
      conName = mkName (T.unpack name)
  fieldDecs <- mapM (mkRecordField name) fields
  let dataDec = DataD [] tyName [] Nothing
        [RecC conName fieldDecs]
        [ DerivClause (Just StockStrategy) [ConT ''Show, ConT ''Eq] ]
  toAvroInst <- mkToAvroInstance name tyName conName fields
  fromAvroInst <- mkFromAvroInstance name tyName conName fields
  pure (dataDec : toAvroInst <> fromAvroInst)

mkRecordField :: Text -> AvroField -> Q VarBangType
mkRecordField recName fld = do
  let accessor = fieldAccessorName recName (avroFieldName fld)
      accName = mkName (T.unpack accessor)
  hsTy <- avroFieldToTHType (avroFieldType fld)
  let bang = Bang NoSourceUnpackedness SourceStrict
  pure (accName, bang, hsTy)

avroFieldToTHType :: AvroType -> Q Type
avroFieldToTHType = \case
  AvroPrimitive AvroNull -> [t| () |]
  AvroPrimitive AvroBool -> [t| Bool |]
  AvroPrimitive AvroInt -> [t| Int32 |]
  AvroPrimitive AvroLong -> [t| Int64 |]
  AvroPrimitive AvroFloat -> [t| Float |]
  AvroPrimitive AvroDouble -> [t| Double |]
  AvroPrimitive AvroBytes -> [t| ByteString |]
  AvroPrimitive AvroString -> [t| Text |]
  AvroPrimitive (AvroSchemaRef ref) -> pure (ConT (mkName (T.unpack ref)))
  AvroRecord{avroRecordName = n} -> pure (ConT (mkName (T.unpack n)))
  AvroEnum{avroEnumName = n} -> pure (ConT (mkName (T.unpack n)))
  AvroArray{avroArrayItems = itemTy} -> do
    inner <- avroFieldToTHType itemTy
    pure (AppT (ConT ''V.Vector) inner)
  AvroMap{avroMapValues = valTy} -> do
    inner <- avroFieldToTHType valTy
    pure (AppT (AppT (ConT ''Map) (ConT ''Text)) inner)
  AvroUnion{avroUnionBranches = branches} ->
    case isNullableUnion (V.toList branches) of
      Just inner -> do
        innerTy <- avroFieldToTHType inner
        pure (AppT (ConT ''Maybe) innerTy)
      Nothing -> [t| () |]
  AvroFixed{} -> [t| ByteString |]
  AvroLogical{avroLogicalBase = base} -> avroFieldToTHType base

mkToAvroInstance :: Text -> Name -> Name -> [AvroField] -> Q [Dec]
mkToAvroInstance recName tyName conName fields = do
  msgVar <- newName "msg"
  let fieldExprs = map (mkToAvroFieldExpr recName msgVar) fields
      body = NormalB $
        AppE (VarE (mkName "Avro.Value.Record"))
          (AppE (VarE 'V.fromList) (ListE fieldExprs))
      toAvroFn = FunD (mkName "toAvro")
        [Clause [VarP msgVar] body []]
  pure [InstanceD Nothing [] (AppT (ConT (mkName "ToAvro")) (ConT tyName)) [toAvroFn]]

mkToAvroFieldExpr :: Text -> Name -> AvroField -> Exp
mkToAvroFieldExpr recName msgVar fld =
  let accessor = fieldAccessorName recName (avroFieldName fld)
      getField = AppE (VarE (mkName (T.unpack accessor))) (VarE msgVar)
  in mkToAvroConvertExpr (avroFieldType fld) getField

mkToAvroConvertExpr :: AvroType -> Exp -> Exp
mkToAvroConvertExpr ty expr = case ty of
  AvroPrimitive AvroNull -> ConE (mkName "Avro.Value.Null")
  AvroPrimitive AvroBool -> AppE (ConE (mkName "Avro.Value.Bool")) expr
  AvroPrimitive AvroInt -> AppE (ConE (mkName "Avro.Value.Int")) expr
  AvroPrimitive AvroLong -> AppE (ConE (mkName "Avro.Value.Long")) expr
  AvroPrimitive AvroFloat -> AppE (ConE (mkName "Avro.Value.Float")) expr
  AvroPrimitive AvroDouble -> AppE (ConE (mkName "Avro.Value.Double")) expr
  AvroPrimitive AvroBytes -> AppE (ConE (mkName "Avro.Value.Bytes")) expr
  AvroPrimitive AvroString -> AppE (ConE (mkName "Avro.Value.String")) expr
  _ -> AppE (VarE (mkName "toAvro")) expr

mkFromAvroInstance :: Text -> Name -> Name -> [AvroField] -> Q [Dec]
mkFromAvroInstance recName tyName conName fields = do
  fieldsVar <- newName "fields'"
  let numFields = length fields
      fieldBinds = zipWith (\i fld ->
        let accessor = fieldAccessorName recName (avroFieldName fld)
            varN = mkName (T.unpack accessor <> "'")
        in BindS (VarP varN) (mkFromAvroFieldExprTH (avroFieldType fld) fieldsVar i)
        ) [0..] fields
      recFields = map (\fld ->
        let accessor = fieldAccessorName recName (avroFieldName fld)
        in (mkName (T.unpack accessor), VarE (mkName (T.unpack accessor <> "'")))
        ) fields
      resultExpr = NoBindS $ AppE (VarE 'pure) (RecConE conName recFields)
      doBody = DoE Nothing (fieldBinds <> [resultExpr])
      matchOk = Match
        (ConP (mkName "Avro.Value.Record") [] [VarP fieldsVar])
        (NormalB doBody)
        []
      matchFail = Match WildP
        (NormalB (AppE (VarE 'Left) (LitE (StringL ("FromAvro " <> T.unpack recName <> ": expected Record")))))
        []
      fromAvroFn = FunD (mkName "fromAvro")
        [Clause [] (NormalB (LamCaseE [matchOk, matchFail])) []]
  pure [InstanceD Nothing [] (AppT (ConT (mkName "FromAvro")) (ConT tyName)) [fromAvroFn]]

mkFromAvroFieldExprTH :: AvroType -> Name -> Int -> Exp
mkFromAvroFieldExprTH ty fieldsVar idx =
  let getExpr = AppE (AppE (VarE '(V.!)) (VarE fieldsVar)) (LitE (IntegerL (fromIntegral idx)))
  in case ty of
    AvroPrimitive AvroBool -> AppE (VarE (mkName "fromAvro")) getExpr
    AvroPrimitive AvroInt -> AppE (VarE (mkName "fromAvro")) getExpr
    AvroPrimitive AvroLong -> AppE (VarE (mkName "fromAvro")) getExpr
    AvroPrimitive AvroFloat -> AppE (VarE (mkName "fromAvro")) getExpr
    AvroPrimitive AvroDouble -> AppE (VarE (mkName "fromAvro")) getExpr
    AvroPrimitive AvroBytes -> AppE (VarE (mkName "fromAvro")) getExpr
    AvroPrimitive AvroString -> AppE (VarE (mkName "fromAvro")) getExpr
    _ -> AppE (VarE (mkName "fromAvro")) getExpr

-- ---------------------------------------------------------------------------
-- TH: Enum
-- ---------------------------------------------------------------------------

deriveEnumTH :: Text -> [Text] -> Q [Dec]
deriveEnumTH name syms = do
  let tyName = mkName (T.unpack name)
      cons = map (\sym -> NormalC (mkName (T.unpack (enumConName name sym))) []) syms
      dataDec = DataD [] tyName [] Nothing cons
        [ DerivClause (Just StockStrategy)
            [ConT ''Show, ConT ''Eq, ConT ''Ord, ConT ''Enum, ConT ''Bounded]
        ]
  toAvroInst <- mkEnumToAvroInstance name tyName syms
  fromAvroInst <- mkEnumFromAvroInstance name tyName syms
  pure (dataDec : toAvroInst <> fromAvroInst)

mkEnumToAvroInstance :: Text -> Name -> [Text] -> Q [Dec]
mkEnumToAvroInstance name tyName syms = do
  let clauses = zipWith (\i sym ->
        Clause
          [ConP (mkName (T.unpack (enumConName name sym))) [] []]
          (NormalB (AppE (ConE (mkName "Avro.Value.Enum")) (LitE (IntegerL (fromIntegral (i :: Int))))))
          []
        ) [0..] syms
      toAvroFn = FunD (mkName "toAvro") clauses
  pure [InstanceD Nothing [] (AppT (ConT (mkName "ToAvro")) (ConT tyName)) [toAvroFn]]

mkEnumFromAvroInstance :: Text -> Name -> [Text] -> Q [Dec]
mkEnumFromAvroInstance name tyName syms = do
  let matchClauses = zipWith (\i sym ->
        Match
          (ConP (mkName "Avro.Value.Enum") [] [LitP (IntegerL (fromIntegral (i :: Int)))])
          (NormalB (AppE (VarE 'Right) (ConE (mkName (T.unpack (enumConName name sym))))))
          []
        ) [0..] syms
      catchAll = Match WildP
        (NormalB (AppE (VarE 'Left) (LitE (StringL ("FromAvro " <> T.unpack name <> ": expected Enum")))))
        []
      fromAvroFn = FunD (mkName "fromAvro")
        [Clause [] (NormalB (LamCaseE (matchClauses <> [catchAll]))) []]
  pure [InstanceD Nothing [] (AppT (ConT (mkName "FromAvro")) (ConT tyName)) [fromAvroFn]]

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
    (p:ps) -> T.concat (lowerFirst p : map titleCase ps)

titleCase :: Text -> Text
titleCase s = case T.uncons s of
  Just (c, rest) -> T.cons (toUpper c) (T.toLower rest)
  Nothing -> s
