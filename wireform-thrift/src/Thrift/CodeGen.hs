{-# LANGUAGE TemplateHaskell #-}

{- | Thrift code generation — generates Haskell data types and
ToThrift\/FromThrift instances from Thrift schemas.

== Text generation

'generateThriftTypes' takes a 'ThriftSchema' and produces Haskell source
as 'Text'.  The output includes @data@ declarations for structs, enums,
and service method descriptors.

== Template Haskell

'deriveThrift' generates declarations at compile time from a 'ThriftSchema'.
-}
module Thrift.CodeGen (
  generateThriftTypes,
  generateThriftTypesWithRegistry,
  deriveThrift,
) where

import Data.ByteString (ByteString)
import Data.Char (toLower, toUpper)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Language.Haskell.TH
import Thrift.Registry
import Thrift.Schema


-- ---------------------------------------------------------------------------
-- Text-based code generation
-- ---------------------------------------------------------------------------

{- | Generate Haskell source code (as 'Text') for all types in a 'ThriftSchema'.
Uses 'defaultThriftRegistry'.
-}
generateThriftTypes :: ThriftSchema -> Text
generateThriftTypes = generateThriftTypesWithRegistry defaultThriftRegistry


{- | Generate Haskell source code using a custom 'ThriftRegistry'.
When a field has an annotation matching a handler, the type transformation
is applied and extra code is emitted.
-}
generateThriftTypesWithRegistry :: ThriftRegistry -> ThriftSchema -> Text
generateThriftTypesWithRegistry reg schema =
  let typedefDecls = map genThriftTypedef (tsTypedefs schema)
      constDecls = map genThriftConst (tsConsts schema)
      enumDecls = map genThriftEnum (tsEnums schema)
      structDecls = map (genThriftStructWithRegistry reg) (tsStructs schema)
      serviceDecls = map genThriftService (tsServices schema)
  in T.intercalate "\n\n" (typedefDecls <> constDecls <> enumDecls <> structDecls <> serviceDecls)


-- ---------------------------------------------------------------------------
-- Typedef generation (text)
-- ---------------------------------------------------------------------------

genThriftTypedef :: ThriftTypedef -> Text
genThriftTypedef td =
  "type " <> ttName td <> " = " <> thriftInnerHsType (ttType td)


-- ---------------------------------------------------------------------------
-- Const generation (text)
-- ---------------------------------------------------------------------------

genThriftConst :: ThriftConst -> Text
genThriftConst tc =
  let name = lowerFirst (snakeToCamel (T.toLower (tcName tc)))
      hsType = thriftInnerHsType (tcType tc)
      hsVal = constValueToHs (tcType tc) (tcValue tc)
  in name <> " :: " <> hsType <> "\n" <> name <> " = " <> hsVal


constValueToHs :: ThriftType -> ThriftConstValue -> Text
constValueToHs _ (TCVInt n) = T.pack (show n)
constValueToHs _ (TCVDouble d) = T.pack (show d)
constValueToHs _ (TCVString s) = "\"" <> s <> "\""
constValueToHs _ (TCVBool True) = "True"
constValueToHs _ (TCVBool False) = "False"
constValueToHs _ (TCVIdent i) = i
constValueToHs _ (TCVList vs) =
  "[" <> T.intercalate ", " (map (constValueToHs TString) vs) <> "]"
constValueToHs _ (TCVMap _) = "mempty"


-- ---------------------------------------------------------------------------
-- Enum generation (text)
-- ---------------------------------------------------------------------------

genThriftEnum :: ThriftEnum -> Text
genThriftEnum te =
  let name = teName te
      vals = teValues te
  in T.unlines $
       case vals of
         [] ->
           [ "data " <> name
           , "  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)"
           , "  deriving anyclass NFData"
           ]
         ((sym, _) : rest) ->
           ["data " <> name]
             <> ["  = " <> thriftEnumConName name sym]
             <> map (\(s, _) -> "  | " <> thriftEnumConName name s) rest
             <> [ "  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)"
                , "  deriving anyclass NFData"
                ]
         <> genThriftEnumToThrift name vals
         <> genThriftEnumFromThrift name vals


thriftEnumConName :: Text -> Text -> Text
thriftEnumConName enumName valName =
  enumName <> upperFirst (snakeToCamel (T.toLower valName))


genThriftEnumToThrift :: Text -> [(Text, Int32)] -> [Text]
genThriftEnumToThrift name vals =
  [ ""
  , "instance ToThrift " <> name <> " where"
  ]
    <> zipWith
      ( \i (sym, _) ->
          "  toThrift " <> thriftEnumConName name sym <> " = Thrift.Value.I32 " <> T.pack (show i)
      )
      [0 :: Int ..]
      vals


genThriftEnumFromThrift :: Text -> [(Text, Int32)] -> [Text]
genThriftEnumFromThrift name vals =
  [ ""
  , "instance FromThrift " <> name <> " where"
  ]
    <> zipWith
      ( \i (sym, _) ->
          "  fromThrift (Thrift.Value.I32 " <> T.pack (show i) <> ") = Right " <> thriftEnumConName name sym
      )
      [0 :: Int ..]
      vals
    <> ["  fromThrift _ = Left \"FromThrift " <> name <> ": expected I32 enum value\""]


-- ---------------------------------------------------------------------------
-- Struct generation (text)
-- ---------------------------------------------------------------------------

genThriftStructWithRegistry :: ThriftRegistry -> ThriftStruct -> Text
genThriftStructWithRegistry reg ts =
  let name = tsName ts
      fields = tsFields ts
      structAnns = V.toList (tsAnnotations ts)
      extraStructCode =
        concatMap
          ( \(k, v) ->
              case Map.lookup k (trStructAnnotations reg) of
                Just handler -> sahExtraCode handler k v
                Nothing -> []
          )
          structAnns
      extraDerivs =
        concatMap
          ( \(k, v) ->
              case Map.lookup k (trStructAnnotations reg) of
                Just handler -> sahExtraDerivations handler v
                Nothing -> []
          )
          structAnns
      fieldExtraCode =
        concatMap
          ( \fld ->
              concatMap
                ( \(k, v) ->
                    case Map.lookup k (trFieldAnnotations reg) of
                      Just handler -> fahExtraCode handler (tfFieldName fld) v
                      Nothing -> []
                )
                (V.toList (tfAnnotations fld))
          )
          fields
  in T.unlines $
       genStructDataDeclWithRegistry reg name fields
         <> (if null extraDerivs then [] else map ("  deriving " <>) extraDerivs)
         <> genStructToThrift name fields
         <> genStructFromThrift name fields
         <> extraStructCode
         <> fieldExtraCode


genStructDataDeclWithRegistry :: ThriftRegistry -> Text -> [ThriftField] -> [Text]
genStructDataDeclWithRegistry reg name fields =
  ["data " <> name <> " = " <> name]
    <> case fields of
      [] ->
        [ "  deriving stock (Show, Eq, Generic)"
        , "  deriving anyclass NFData"
        ]
      (f : fs) ->
        ["  { " <> genThriftFieldDeclWithRegistry reg name f]
          <> map (\fld -> "  , " <> genThriftFieldDeclWithRegistry reg name fld) fs
          <> [ "  } deriving stock (Show, Eq, Generic)"
             , "    deriving anyclass NFData"
             ]


genThriftFieldDeclWithRegistry :: ThriftRegistry -> Text -> ThriftField -> Text
genThriftFieldDeclWithRegistry reg recName fld =
  let accessor = thriftFieldAccessorName recName (tfFieldName fld)
      baseType = thriftFieldHsType (tfFieldType fld) (tfRequiredness fld)
      hsType = applyFieldAnnotationTransforms reg fld baseType
  in accessor <> " :: " <> hsType


applyFieldAnnotationTransforms :: ThriftRegistry -> ThriftField -> Text -> Text
applyFieldAnnotationTransforms reg fld ty =
  foldl
    ( \acc (k, _v) ->
        case Map.lookup k (trFieldAnnotations reg) of
          Just handler -> fahTransformType handler acc
          Nothing -> acc
    )
    ty
    (V.toList (tfAnnotations fld))


thriftFieldAccessorName :: Text -> Text -> Text
thriftFieldAccessorName recName fieldName =
  lowerFirst recName <> upperFirst (snakeToCamel fieldName)


thriftFieldHsType :: ThriftType -> Requiredness -> Text
thriftFieldHsType ty req = case req of
  Optional -> "!(Maybe " <> thriftInnerHsType ty <> ")"
  Required -> thriftStrictHsType ty
  Default -> thriftStrictHsType ty


thriftStrictHsType :: ThriftType -> Text
thriftStrictHsType = \case
  TBool -> "!Bool"
  TByte -> "{-# UNPACK #-} !Int8"
  TI16 -> "{-# UNPACK #-} !Int16"
  TI32 -> "{-# UNPACK #-} !Int32"
  TI64 -> "{-# UNPACK #-} !Int64"
  TDouble -> "{-# UNPACK #-} !Double"
  TString -> "!Text"
  TBinary -> "!ByteString"
  TUUID -> "!ByteString"
  TStruct n -> "!" <> n
  TEnum n -> "!" <> n
  TTypedef n -> "!" <> n
  TList elemTy -> "!(Vector " <> thriftInnerHsType elemTy <> ")"
  TSet elemTy -> "!(Vector " <> thriftInnerHsType elemTy <> ")"
  TMap keyTy valTy -> "!(Map " <> thriftInnerHsType keyTy <> " " <> thriftInnerHsType valTy <> ")"


thriftInnerHsType :: ThriftType -> Text
thriftInnerHsType = \case
  TBool -> "Bool"
  TByte -> "Int8"
  TI16 -> "Int16"
  TI32 -> "Int32"
  TI64 -> "Int64"
  TDouble -> "Double"
  TString -> "Text"
  TBinary -> "ByteString"
  TUUID -> "ByteString"
  TStruct n -> n
  TEnum n -> n
  TTypedef n -> n
  TList elemTy -> "(Vector " <> thriftInnerHsType elemTy <> ")"
  TSet elemTy -> "(Vector " <> thriftInnerHsType elemTy <> ")"
  TMap keyTy valTy -> "(Map " <> thriftInnerHsType keyTy <> " " <> thriftInnerHsType valTy <> ")"


-- ---------------------------------------------------------------------------
-- Struct ToThrift instance (text)
-- ---------------------------------------------------------------------------

genStructToThrift :: Text -> [ThriftField] -> [Text]
genStructToThrift name fields =
  [ ""
  , "instance ToThrift " <> name <> " where"
  , "  toThrift msg = Thrift.Value.Struct $ V.fromList"
  ]
    <> case fields of
      [] -> ["    []"]
      _ ->
        ["    [ " <> toThriftFieldExpr name (head fields)]
          <> map (\f -> "    , " <> toThriftFieldExpr name f) (tail fields)
          <> ["    ]"]


toThriftFieldExpr :: Text -> ThriftField -> Text
toThriftFieldExpr recName fld =
  let accessor = "msg." <> thriftFieldAccessorName recName (tfFieldName fld)
      fid = T.pack (show (tfFieldId fld))
  in case tfRequiredness fld of
       Optional ->
         "maybe (0, Thrift.Value.Bool False) (\\v -> (" <> fid <> ", " <> toThriftConvert (tfFieldType fld) "v" <> ")) " <> accessor
       _ ->
         "(" <> fid <> ", " <> toThriftConvert (tfFieldType fld) accessor <> ")"


toThriftConvert :: ThriftType -> Text -> Text
toThriftConvert ty accessor = case ty of
  TBool -> "Thrift.Value.Bool " <> accessor
  TByte -> "Thrift.Value.Byte " <> accessor
  TI16 -> "Thrift.Value.I16 " <> accessor
  TI32 -> "Thrift.Value.I32 " <> accessor
  TI64 -> "Thrift.Value.I64 " <> accessor
  TDouble -> "Thrift.Value.Double " <> accessor
  TString -> "Thrift.Value.String " <> accessor
  TBinary -> "Thrift.Value.Binary " <> accessor
  TUUID -> "Thrift.Value.UUID " <> accessor
  TStruct _ -> "toThrift " <> accessor
  TEnum _ -> "toThrift " <> accessor
  TTypedef _ -> "toThrift " <> accessor
  TList _ -> "toThrift " <> accessor
  TSet _ -> "toThrift " <> accessor
  TMap _ _ -> "toThrift " <> accessor


-- ---------------------------------------------------------------------------
-- Struct FromThrift instance (text)
-- ---------------------------------------------------------------------------

genStructFromThrift :: Text -> [ThriftField] -> [Text]
genStructFromThrift name fields =
  [ ""
  , "instance FromThrift " <> name <> " where"
  , "  fromThrift (Thrift.Value.Struct fieldVec) = do"
  , "    let fieldMap = Map.fromList (V.toList fieldVec)"
  ]
    <> map (fromThriftFieldBind name) fields
    <> [ "    pure " <> name
       , "      { "
           <> T.intercalate
             "\n      , "
             ( map
                 ( \fld ->
                     let accessor = thriftFieldAccessorName name (tfFieldName fld)
                     in accessor <> " = " <> accessor <> "'"
                 )
                 fields
             )
       , "      }"
       , "  fromThrift _ = Left \"FromThrift " <> name <> ": expected Struct\""
       ]


fromThriftFieldBind :: Text -> ThriftField -> Text
fromThriftFieldBind recName fld =
  let accessor = thriftFieldAccessorName recName (tfFieldName fld)
      fid = T.pack (show (tfFieldId fld))
  in case tfRequiredness fld of
       Optional ->
         "    let " <> accessor <> "' = case Map.lookup " <> fid <> " fieldMap of { Just v -> case fromThrift v of { Right x -> Just x; _ -> Nothing }; Nothing -> Nothing }"
       _ ->
         "    " <> accessor <> "' <- case Map.lookup " <> fid <> " fieldMap of { Just v -> fromThrift v; Nothing -> Left \"missing field " <> tfFieldName fld <> "\" }"


-- ---------------------------------------------------------------------------
-- Service generation (text) — type-level method descriptors
-- ---------------------------------------------------------------------------

genThriftService :: ThriftService -> Text
genThriftService svc =
  let name = tsvName svc
      methods = tsvMethods svc
  in T.unlines $
       [ "-- | Service @" <> name <> "@ method descriptors."
       , "data " <> name <> "Method"
       ]
         <> case methods of
           [] ->
             ["  deriving stock (Show, Eq, Ord, Enum, Bounded)"]
           (m : ms) ->
             ["  = " <> name <> upperFirst (snakeToCamel (tmName m)) <> "Method"]
               <> map
                 ( \method ->
                     "  | " <> name <> upperFirst (snakeToCamel (tmName method)) <> "Method"
                 )
                 ms
               <> ["  deriving stock (Show, Eq, Ord, Enum, Bounded)"]
         <> [ ""
            , name <> "ServiceName :: Text"
            , name <> "ServiceName = \"" <> name <> "\""
            , ""
            , name <> "MethodName :: " <> name <> "Method -> Text"
            ]
         <> map
           ( \method ->
               name <> "MethodName " <> name <> upperFirst (snakeToCamel (tmName method)) <> "Method = \"" <> tmName method <> "\""
           )
           methods


-- ---------------------------------------------------------------------------
-- Template Haskell
-- ---------------------------------------------------------------------------

-- | Generate Haskell declarations from a 'ThriftSchema' at compile time.
deriveThrift :: ThriftSchema -> Q [Dec]
deriveThrift schema = do
  enumDecs <- concat <$> mapM deriveThriftEnumTH (tsEnums schema)
  structDecs <- concat <$> mapM deriveThriftStructTH (tsStructs schema)
  pure (enumDecs <> structDecs)


-- ---------------------------------------------------------------------------
-- TH: Enum
-- ---------------------------------------------------------------------------

deriveThriftEnumTH :: ThriftEnum -> Q [Dec]
deriveThriftEnumTH te = do
  let name = teName te
      tyName = mkName (T.unpack name)
      vals = teValues te
      cons = map (\(sym, _) -> NormalC (mkName (T.unpack (thriftEnumConName name sym))) []) vals
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
-- TH: Struct
-- ---------------------------------------------------------------------------

deriveThriftStructTH :: ThriftStruct -> Q [Dec]
deriveThriftStructTH ts = do
  let name = tsName ts
      tyName = mkName (T.unpack name)
      conName = mkName (T.unpack name)
      fields = tsFields ts
  fieldDecs <- mapM (mkThriftRecordField name) fields
  let dataDec =
        DataD
          []
          tyName
          []
          Nothing
          [RecC conName fieldDecs]
          [DerivClause (Just StockStrategy) [ConT ''Show, ConT ''Eq]]
  pure [dataDec]


mkThriftRecordField :: Text -> ThriftField -> Q VarBangType
mkThriftRecordField recName fld = do
  let accessor = thriftFieldAccessorName recName (tfFieldName fld)
      accName = mkName (T.unpack accessor)
  hsTy <- thriftFieldToTHType (tfFieldType fld) (tfRequiredness fld)
  let bang = Bang NoSourceUnpackedness SourceStrict
  pure (accName, bang, hsTy)


thriftFieldToTHType :: ThriftType -> Requiredness -> Q Type
thriftFieldToTHType ty req = case req of
  Optional -> do
    inner <- thriftTypeToTH ty
    pure (AppT (ConT ''Maybe) inner)
  _ -> thriftTypeToTH ty


thriftTypeToTH :: ThriftType -> Q Type
thriftTypeToTH = \case
  TBool -> [t|Bool|]
  TByte -> [t|Int8|]
  TI16 -> [t|Int16|]
  TI32 -> [t|Int32|]
  TI64 -> [t|Int64|]
  TDouble -> [t|Double|]
  TString -> [t|Text|]
  TBinary -> [t|ByteString|]
  TUUID -> [t|ByteString|]
  TStruct n -> pure (ConT (mkName (T.unpack n)))
  TEnum n -> pure (ConT (mkName (T.unpack n)))
  TTypedef n -> pure (ConT (mkName (T.unpack n)))
  TList elemTy -> do
    inner <- thriftTypeToTH elemTy
    pure (AppT (ConT ''V.Vector) inner)
  TSet elemTy -> do
    inner <- thriftTypeToTH elemTy
    pure (AppT (ConT ''V.Vector) inner)
  TMap keyTy valTy -> do
    k <- thriftTypeToTH keyTy
    v <- thriftTypeToTH valTy
    pure (AppT (AppT (ConT ''Map) k) v)


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
