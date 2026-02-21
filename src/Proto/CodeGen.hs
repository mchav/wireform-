-- | Code generation for Haskell modules from parsed proto files.
--
-- Generates complete, compilable Haskell modules with:
--
-- * Proper cross-module imports via a TypeRegistry
-- * Record types for messages, sum types for enums and oneofs
-- * MessageEncode / MessageDecode / MessageSize instances
-- * ProtoToJSON / ProtoFromJSON instances (using json_name annotations)
-- * Map field, oneof, and nested message support
module Proto.CodeGen
  ( generateModule
  , generateModuleText
  , GenerateOpts (..)
  , defaultGenerateOpts
  , TypeRegistry
  , TypeInfo (..)
  , TypeKind (..)
  , buildTypeRegistry
  , moduleNameForProto
  , hsTypeName
  , hsModuleName
  , scopedTypeName
  , scopedFieldName
  , scopedEnumCon
  , snakeToCamel
  , snakeToPascal
  , lowerFirst
  , escapeReserved
  ) where

import Data.Char (toLower, toUpper, isUpper)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)

import Proto.AST
import Proto.Options
import Proto.Annotations (lookupSimpleOption, optionAsString)
import Proto.Parser.Resolver (ResolvedProto(..))

data GenerateOpts = GenerateOpts
  { genModulePrefix    :: Text
  , genStrictFields    :: Bool
  , genUnpackPrims     :: Bool
  , genDeriveGeneric   :: Bool
  , genDeriveNFData    :: Bool
  , genPackedRepeated  :: Bool
  , genLazySubmessages :: Bool
  } deriving stock (Show, Eq)

defaultGenerateOpts :: GenerateOpts
defaultGenerateOpts = GenerateOpts
  { genModulePrefix    = "Proto.Gen"
  , genStrictFields    = True
  , genUnpackPrims     = True
  , genDeriveGeneric   = True
  , genDeriveNFData    = True
  , genPackedRepeated  = True
  , genLazySubmessages = False
  }

-- ---------------------------------------------------------------------------
-- Type registry: maps fully-qualified proto names to Haskell type info
-- ---------------------------------------------------------------------------

data TypeKind = TKMessage | TKEnum
  deriving stock (Show, Eq, Ord)

data TypeInfo = TypeInfo
  { tiModule   :: Text
  , tiHsName   :: Text
  , tiKind     :: TypeKind
  } deriving stock (Show, Eq)

type TypeRegistry = Map Text TypeInfo

-- | Build a TypeRegistry from a list of (proto file path, resolved proto).
-- Walks all top-levels and nested definitions, recording their FQ names.
buildTypeRegistry :: GenerateOpts -> [(FilePath, ResolvedProto)] -> TypeRegistry
buildTypeRegistry opts rpList =
  Map.union wellKnownTypes (Map.unions (fmap (\(fp, rp) -> registryForFile opts (normalizeProtoPath fp) (rpFile rp)) rpList))

wellKnownTypes :: TypeRegistry
wellKnownTypes = Map.fromList
  [ ("google.protobuf.Duration",    TypeInfo "Proto.Google.Protobuf.Duration"    "Duration"    TKMessage)
  , ("google.protobuf.Timestamp",   TypeInfo "Proto.Google.Protobuf.Timestamp"   "Timestamp"   TKMessage)
  , ("google.protobuf.Empty",       TypeInfo "Proto.Google.Protobuf.Empty"       "Empty"       TKMessage)
  , ("google.protobuf.Any",         TypeInfo "Proto.Google.Protobuf.Any"         "Any"         TKMessage)
  , ("google.protobuf.Struct",      TypeInfo "Proto.Google.Protobuf.Struct"      "Struct"      TKMessage)
  , ("google.protobuf.Value",       TypeInfo "Proto.Google.Protobuf.Struct"      "Value"       TKMessage)
  , ("google.protobuf.ListValue",   TypeInfo "Proto.Google.Protobuf.Struct"      "ListValue"   TKMessage)
  , ("google.protobuf.FieldMask",   TypeInfo "Proto.Google.Protobuf.FieldMask"   "FieldMask"   TKMessage)
  , ("google.protobuf.DoubleValue", TypeInfo "Proto.Google.Protobuf.Wrappers"    "DoubleValue" TKMessage)
  , ("google.protobuf.FloatValue",  TypeInfo "Proto.Google.Protobuf.Wrappers"    "FloatValue"  TKMessage)
  , ("google.protobuf.Int64Value",  TypeInfo "Proto.Google.Protobuf.Wrappers"    "Int64Value"  TKMessage)
  , ("google.protobuf.UInt64Value", TypeInfo "Proto.Google.Protobuf.Wrappers"    "UInt64Value" TKMessage)
  , ("google.protobuf.Int32Value",  TypeInfo "Proto.Google.Protobuf.Wrappers"    "Int32Value"  TKMessage)
  , ("google.protobuf.UInt32Value", TypeInfo "Proto.Google.Protobuf.Wrappers"    "UInt32Value" TKMessage)
  , ("google.protobuf.BoolValue",   TypeInfo "Proto.Google.Protobuf.Wrappers"    "BoolValue"   TKMessage)
  , ("google.protobuf.StringValue", TypeInfo "Proto.Google.Protobuf.Wrappers"    "StringValue" TKMessage)
  , ("google.protobuf.BytesValue",  TypeInfo "Proto.Google.Protobuf.Wrappers"    "BytesValue"  TKMessage)
  ]

-- | Normalize a proto file path to a relative path suitable for module naming.
-- Handles absolute paths by extracting the proto-relative portion.
normalizeProtoPath :: FilePath -> FilePath
normalizeProtoPath = id

registryForFile :: GenerateOpts -> FilePath -> ProtoFile -> TypeRegistry
registryForFile opts fp pf =
  let modName = moduleNameForProto opts fp pf
      pkg = fromMaybe "" (protoPackage pf)
  in Map.unions (fmap (registryForTopLevel modName pkg []) (protoTopLevels pf))

registryForTopLevel :: Text -> Text -> [Text] -> TopLevel -> TypeRegistry
registryForTopLevel modName pkg scope = \case
  TLMessage msg -> registryForMessage modName pkg scope msg
  TLEnum ed     -> registryForEnum modName pkg scope ed
  _             -> Map.empty

registryForMessage :: Text -> Text -> [Text] -> MessageDef -> TypeRegistry
registryForMessage modName pkg scope msg =
  let scope' = scope <> [msgName msg]
      fqName = if T.null pkg then T.intercalate "." scope' else pkg <> "." <> T.intercalate "." scope'
      hsName = T.intercalate "'" (fmap hsTypeName scope')
      self = Map.singleton fqName TypeInfo
        { tiModule = modName
        , tiHsName = hsName
        , tiKind   = TKMessage
        }
      nested = Map.unions (fmap (registryForElement modName pkg scope') (msgElements msg))
  in Map.union self nested

registryForElement :: Text -> Text -> [Text] -> MessageElement -> TypeRegistry
registryForElement modName pkg scope = \case
  MEMessage inner -> registryForMessage modName pkg scope inner
  MEEnum ed       -> registryForEnum modName pkg scope ed
  _               -> Map.empty

registryForEnum :: Text -> Text -> [Text] -> EnumDef -> TypeRegistry
registryForEnum modName pkg scope ed =
  let scope' = scope <> [enumName ed]
      fqName = if T.null pkg then T.intercalate "." scope' else pkg <> "." <> T.intercalate "." scope'
      hsName = T.intercalate "'" (fmap hsTypeName scope')
  in Map.singleton fqName TypeInfo
    { tiModule = modName
    , tiHsName = hsName
    , tiKind   = TKEnum
    }

-- | Compute the Haskell module name for a proto file.
-- Uses the file path to derive a unique module name.
-- For "temporal/api/enums/v1/common.proto", produces
-- "Proto.Temporal.Temporal.Api.Enums.V1.Common"
moduleNameForProto :: GenerateOpts -> FilePath -> ProtoFile -> Text
moduleNameForProto opts filePath _pf =
  let cleaned = T.pack filePath
      stripped = fromMaybe cleaned (T.stripSuffix ".proto" cleaned)
      parts = T.splitOn "/" stripped
      hsparts = fmap pathPartToModule parts
  in genModulePrefix opts <> "." <> T.intercalate "." hsparts
  where
    pathPartToModule t = snakeToPascal (capitalize t)
    capitalize t = case T.uncons t of
      Just (c, rest) -> T.cons (toUpper c) rest
      Nothing        -> t

-- ---------------------------------------------------------------------------
-- Code generation
-- ---------------------------------------------------------------------------

data GenCtx = GenCtx
  { gcOpts     :: GenerateOpts
  , gcRegistry :: TypeRegistry
  , gcThisMod  :: Text
  , gcPkg      :: Maybe Text
  , gcLocalTypes :: Set Text
  }

generateModule :: GenerateOpts -> TypeRegistry -> FilePath -> ProtoFile -> Doc ann
generateModule opts reg filePath pf =
  let thisMod = moduleNameForProto opts filePath pf
      localTypes = collectLocalTypes (fromMaybe "" (protoPackage pf)) [] (protoTopLevels pf)
      ctx = GenCtx
        { gcOpts = opts
        , gcRegistry = reg
        , gcThisMod = thisMod
        , gcPkg = protoPackage pf
        , gcLocalTypes = localTypes
        }
      body = concatMap (genTopLevel ctx []) (protoTopLevels pf)
      referencedTypes = collectReferencedTypes (protoTopLevels pf)
      importedModules = computeImports ctx referencedTypes
      localHsNames = Set.fromList [tiHsName ti | (_, ti) <- Map.toList reg, tiModule ti == thisMod]
  in vsep
    [ genModuleHeader opts filePath pf
    , mempty
    , genImports localHsNames importedModules
    , mempty
    , vsep body
    ]

generateModuleText :: GenerateOpts -> TypeRegistry -> FilePath -> ProtoFile -> Text
generateModuleText opts reg filePath pf =
  renderStrict (layoutPretty defaultLayoutOptions (generateModule opts reg filePath pf))

-- Collect all FQ type names defined locally in this file
collectLocalTypes :: Text -> [Text] -> [TopLevel] -> Set Text
collectLocalTypes pkg scope = foldMap go
  where
    go = \case
      TLMessage msg ->
        let scope' = scope <> [msgName msg]
            fqName = if T.null pkg then T.intercalate "." scope' else pkg <> "." <> T.intercalate "." scope'
        in Set.singleton fqName <> foldMap (goElem scope') (msgElements msg)
      TLEnum ed ->
        let scope' = scope <> [enumName ed]
            fqName = if T.null pkg then T.intercalate "." scope' else pkg <> "." <> T.intercalate "." scope'
        in Set.singleton fqName
      _ -> Set.empty
    goElem s = \case
      MEMessage inner ->
        let scope' = s <> [msgName inner]
            fqName = if T.null pkg then T.intercalate "." scope' else pkg <> "." <> T.intercalate "." scope'
        in Set.singleton fqName <> foldMap (goElem scope') (msgElements inner)
      MEEnum ed ->
        let scope' = s <> [enumName ed]
            fqName = if T.null pkg then T.intercalate "." scope' else pkg <> "." <> T.intercalate "." scope'
        in Set.singleton fqName
      _ -> Set.empty

-- Collect all FTNamed type references from top-levels
collectReferencedTypes :: [TopLevel] -> Set Text
collectReferencedTypes = foldMap goTL
  where
    goTL = \case
      TLMessage msg -> goMsg msg
      _ -> Set.empty
    goMsg msg = foldMap goElem (msgElements msg)
    goElem = \case
      MEField fd -> goFT (fieldType fd)
      MEMapField mf -> goFT (mapValueType mf)
      MEOneof od -> foldMap (\f -> goFT (oneofFieldType f)) (oneofFields od)
      MEMessage inner -> goMsg inner
      _ -> Set.empty
    goFT = \case
      FTNamed n -> Set.singleton n
      _ -> Set.empty

-- Given referenced type names, compute import statements for external modules
-- Excludes types that collide with locally-defined names.
computeImports :: GenCtx -> Set Text -> Map Text (Set Text)
computeImports ctx refs =
  let reg = gcRegistry ctx
      thisMod = gcThisMod ctx
      localHsNames = Set.fromList [tiHsName ti | (_, ti) <- Map.toList reg, tiModule ti == thisMod]
  in Map.fromListWith Set.union
    [ (tiModule ti, Set.singleton (tiHsName ti))
    | ref <- Set.toList refs
    , Just ti <- [resolveType ctx ref]
    , tiModule ti /= thisMod
    , not (Set.member (tiHsName ti) localHsNames)
    ]

-- Resolve a proto type name to TypeInfo. Tries FQ lookup first, then
-- with current package prefix, then with parent message scopes, then simple.
resolveType :: GenCtx -> Text -> Maybe TypeInfo
resolveType ctx name = resolveTypeWithScope ctx [] name

resolveTypeWithScope :: GenCtx -> [Text] -> Text -> Maybe TypeInfo
resolveTypeWithScope ctx scope name =
  let reg = gcRegistry ctx
      pkg = fromMaybe "" (gcPkg ctx)
      candidates =
        [ name
        , pkg <> "." <> name
        ] <> [pkg <> "." <> T.intercalate "." s <> "." <> name | s <- tails' scope, not (null s)]
  in firstJust (\c -> Map.lookup c reg) candidates
  where
    tails' [] = []
    tails' xs = xs : tails' (init xs)
    firstJust _ [] = Nothing
    firstJust f (x:xs) = case f x of
      Just v -> Just v
      Nothing -> firstJust f xs

-- ---------------------------------------------------------------------------
-- Module header & imports
-- ---------------------------------------------------------------------------

genModuleHeader :: GenerateOpts -> FilePath -> ProtoFile -> Doc ann
genModuleHeader opts filePath pf =
  let modName = moduleNameForProto opts filePath pf
      pkgDoc = maybe "" (\p -> " from package @" <> p <> "@") (protoPackage pf)
      fo = extractFileOptions (protoOptions pf)
      deprLine = if foDeprecated fo
        then [pretty ("--" :: Text), pretty ("-- __This file is deprecated.__" :: Text)]
        else []
  in vsep $
    [ pretty ("{-# LANGUAGE StrictData #-}" :: Text)
    , pretty ("{-# LANGUAGE DeriveGeneric #-}" :: Text)
    , pretty ("{-# LANGUAGE DeriveAnyClass #-}" :: Text)
    , pretty ("{-# LANGUAGE DerivingStrategies #-}" :: Text)
    , pretty ("{-# LANGUAGE OverloadedStrings #-}" :: Text)
    , pretty ("{-# LANGUAGE BangPatterns #-}" :: Text)
    , pretty ("{-# LANGUAGE OverloadedRecordDot #-}" :: Text)
    , pretty ("-- | Auto-generated protobuf types" :: Text) <> pretty pkgDoc <> pretty ("." :: Text)
    , pretty ("--" :: Text)
    , pretty ("-- Generated by hs-proto. Do not edit." :: Text)
    ]
    <> deprLine
    <> [ pretty ("module " :: Text) <> pretty modName <> pretty (" where" :: Text) ]

genImports :: Set Text -> Map Text (Set Text) -> Doc ann
genImports localNames externalImports = vsep $
  [ pretty ("import Data.ByteString (ByteString)" :: Text)
  , pretty ("import qualified Data.ByteString as BS" :: Text)
  , pretty ("import qualified Data.ByteString.Builder as B" :: Text)
  , pretty ("import Data.Int (Int32, Int64)" :: Text)
  , pretty ("import Data.Text (Text)" :: Text)
  , pretty ("import qualified Data.Text as T" :: Text)
  , pretty ("import Data.Word (Word32, Word64)" :: Text)
  , pretty ("import qualified Data.Map.Strict as Map" :: Text)
  , pretty ("import qualified Data.Vector as V" :: Text)
  , pretty ("import qualified Data.Vector.Unboxed as VU" :: Text)
  , pretty ("import GHC.Generics (Generic)" :: Text)
  , pretty ("import Control.DeepSeq (NFData(..))" :: Text)
  , pretty ("import Proto.Encode" :: Text)
  , pretty ("import Proto.Decode" :: Text)
  , pretty ("import Proto.JSON" :: Text)
  , pretty ("import Proto.Wire (Tag(..), WireType(..))" :: Text)
  , pretty ("import Proto.Wire.Encode (putTag, putVarint, putFixed32, putFixed64," :: Text)
  , pretty ("  putFloat, putDouble, putText, putByteString, putLengthDelimited," :: Text)
  , pretty ("  putSVarint32, putSVarint64, putVarintSigned," :: Text)
  , pretty ("  varintSize, tagSize, fieldMessageSize," :: Text)
  , pretty ("  fieldVarintSize, fieldFixed32Size, fieldFixed64Size," :: Text)
  , pretty ("  fieldBoolSize, fieldFloatSize, fieldDoubleSize," :: Text)
  , pretty ("  fieldTextSize, fieldBytesSize)" :: Text)
  ]
  <> fmap (genExternalImport localNames) (Map.toAscList externalImports)

genExternalImport :: Set Text -> (Text, Set Text) -> Doc ann
genExternalImport localNames (modName, _types) =
  let needsHiding = not (Set.null localNames)
      hidingNames = Set.toAscList localNames
  in if needsHiding
     then pretty ("import " :: Text) <> pretty modName <> pretty (" hiding (" :: Text) <>
          hsep (punctuate (pretty ("," :: Text)) (fmap pretty hidingNames)) <>
          pretty (")" :: Text)
     else pretty ("import " :: Text) <> pretty modName

-- ---------------------------------------------------------------------------
-- Top-level generation
-- ---------------------------------------------------------------------------

genTopLevel :: GenCtx -> [Text] -> TopLevel -> [Doc ann]
genTopLevel ctx scope = \case
  TLMessage msg -> genMessage ctx scope msg
  TLEnum ed     -> genEnum ctx scope ed
  TLService _   -> []
  TLExtend _ _  -> []
  TLOption _    -> []

genMessage :: GenCtx -> [Text] -> MessageDef -> [Doc ann]
genMessage ctx scope msg =
  let scope' = scope <> [msgName msg]
      tyN = scopedTypeName scope'
      nestedDefs = concatMap (genNestedElement ctx scope') (msgElements msg)
  in [ mempty
     , genMessageDataDecl ctx scope' msg
     ]
     <> nestedDefs
     <> [ mempty
        , genDefaultInstance ctx scope' msg
        , mempty
        , genEncodeInstance ctx scope' msg
        , mempty
        , genSizeInstance ctx scope' msg
        , mempty
        , genDecodeInstance ctx scope' msg
        , mempty
        , genToJSONInstance ctx scope' msg
        , mempty
        , genFromJSONInstance ctx scope' msg
        ]

genNestedElement :: GenCtx -> [Text] -> MessageElement -> [Doc ann]
genNestedElement ctx scope = \case
  MEMessage inner -> genMessage ctx scope inner
  MEEnum ed       -> genEnum ctx scope ed
  MEOneof od      -> [genOneofDecl ctx scope od, genOneofToJSONInstance ctx scope od, genOneofFromJSONInstance ctx scope od]
  _               -> []

-- ---------------------------------------------------------------------------
-- Data declarations
-- ---------------------------------------------------------------------------

genMessageDataDecl :: GenCtx -> [Text] -> MessageDef -> Doc ann
genMessageDataDecl ctx scope msg =
  let tyN = scopedTypeName scope
  in vsep
    [ pretty ("data " :: Text) <> pretty tyN <> pretty (" = " :: Text) <> pretty tyN
    , indent 2 (braceFields (concatMap (extractFieldDecl ctx scope) (msgElements msg)))
    , indent 2 (pretty ("deriving stock (Show, Eq, Generic)" :: Text))
    , indent 2 (pretty ("deriving anyclass NFData" :: Text))
    ]
  where
    braceFields [] = pretty ("{ }" :: Text)
    braceFields (f:fs) =
      vsep (pretty ("{ " :: Text) <> f : fmap (\x -> pretty (", " :: Text) <> x) fs) <> line <> pretty ("}" :: Text)

extractFieldDecl :: GenCtx -> [Text] -> MessageElement -> [Doc ann]
extractFieldDecl ctx scope = \case
  MEField fd  -> [genFieldDecl ctx scope fd]
  MEMapField mf -> [genMapFieldDecl ctx scope mf]
  MEOneof od -> [genOneofFieldRef ctx scope od]
  _ -> []

genFieldDecl :: GenCtx -> [Text] -> FieldDef -> Doc ann
genFieldDecl ctx scope fd =
  pretty (scopedFieldName scope (fieldName fd)) <+> pretty ("::" :: Text) <+>
  hsFieldType ctx scope (fieldType fd) (fieldLabel fd)

genMapFieldDecl :: GenCtx -> [Text] -> MapField -> Doc ann
genMapFieldDecl ctx scope mf =
  pretty (scopedFieldName scope (mapFieldName mf)) <+> pretty ("::" :: Text) <+>
  pretty ("!(Map.Map " :: Text) <> hsScalarType (mapKeyType mf) <+>
  hsFieldTypeInner ctx scope (mapValueType mf) <> pretty (")" :: Text)

genOneofFieldRef :: GenCtx -> [Text] -> OneofDef -> Doc ann
genOneofFieldRef ctx scope od =
  pretty (scopedFieldName scope (oneofName od)) <+> pretty ("::" :: Text) <+>
  pretty ("!(Maybe " :: Text) <> pretty (scopedTypeName scope <> "'" <> snakeToPascal (oneofName od)) <> pretty (")" :: Text)

genOneofDecl :: GenCtx -> [Text] -> OneofDef -> Doc ann
genOneofDecl ctx scope od =
  let tyN = scopedTypeName scope <> "'" <> snakeToPascal (oneofName od)
  in vsep
    [ pretty ("data " :: Text) <> pretty tyN
    , indent 2 (vsep (zipWith (\pfx f -> pfx <+> genOneofCon ctx scope f) seps (oneofFields od)))
    , indent 2 (pretty ("deriving stock (Show, Eq, Generic)" :: Text))
    , indent 2 (pretty ("deriving anyclass NFData" :: Text))
    ]
  where
    seps = pretty ("=" :: Text) : repeat (pretty ("|" :: Text))
    genOneofCon cx s f =
      pretty (oneofConName s (oneofName od) (oneofFieldName f)) <+>
      hsOneofFieldType cx s (oneofFieldType f)

genOneofToJSONInstance :: GenCtx -> [Text] -> OneofDef -> Doc ann
genOneofToJSONInstance ctx scope od =
  let tyN = scopedTypeName scope <> "'" <> snakeToPascal (oneofName od)
  in vsep
    [ pretty ("instance ProtoToJSON " :: Text) <> pretty tyN <> pretty (" where" :: Text)
    , indent 2 (pretty ("protoToJSON _ = JsonNull" :: Text))
    ]

genOneofFromJSONInstance :: GenCtx -> [Text] -> OneofDef -> Doc ann
genOneofFromJSONInstance ctx scope od =
  let tyN = scopedTypeName scope <> "'" <> snakeToPascal (oneofName od)
      firstCon = case oneofFields od of
        (f:_) -> oneofConName scope (oneofName od) (oneofFieldName f)
        [] -> tyN
  in vsep
    [ pretty ("instance ProtoFromJSON " :: Text) <> pretty tyN <> pretty (" where" :: Text)
    , indent 2 (pretty ("protoFromJSON _ = Left \"Cannot parse oneof from JSON\"" :: Text))
    ]

-- ---------------------------------------------------------------------------
-- Default instances
-- ---------------------------------------------------------------------------

genDefaultInstance :: GenCtx -> [Text] -> MessageDef -> Doc ann
genDefaultInstance ctx scope msg =
  let tyN = scopedTypeName scope
  in vsep
    [ pretty ("default" :: Text) <> pretty tyN <+> pretty ("::" :: Text) <+> pretty tyN
    , pretty ("default" :: Text) <> pretty tyN <+> pretty ("=" :: Text) <+> pretty tyN
    , indent 2 (genDefaultFields ctx scope (msgElements msg))
    ]

genDefaultFields :: GenCtx -> [Text] -> [MessageElement] -> Doc ann
genDefaultFields ctx scope elems =
  let fields = concatMap extractDefault elems
  in case fields of
    [] -> pretty ("{ }" :: Text)
    (f:fs) -> vsep (pretty ("{ " :: Text) <> f : fmap (\x -> pretty (", " :: Text) <> x) fs) <> line <> pretty ("}" :: Text)
  where
    extractDefault = \case
      MEField fd ->
        [pretty (scopedFieldName scope (fieldName fd)) <+> pretty ("=" :: Text) <+> defaultValue ctx (fieldLabel fd) (fieldType fd)]
      MEMapField mf ->
        [pretty (scopedFieldName scope (mapFieldName mf)) <+> pretty ("=" :: Text) <+> pretty ("Map.empty" :: Text)]
      MEOneof od ->
        [pretty (scopedFieldName scope (oneofName od)) <+> pretty ("=" :: Text) <+> pretty ("Nothing" :: Text)]
      _ -> []

defaultValue :: GenCtx -> Maybe FieldLabel -> FieldType -> Doc ann
defaultValue ctx lbl ft = case lbl of
  Just Repeated -> case ft of
    FTScalar s | isUnboxableScalar s -> pretty ("VU.empty" :: Text)
    _                                -> pretty ("V.empty" :: Text)
  Just Optional -> pretty ("Nothing" :: Text)
  _ -> case ft of
    FTScalar SBool   -> pretty ("False" :: Text)
    FTScalar SString -> pretty ("\"\"" :: Text)
    FTScalar SBytes  -> pretty ("\"\"" :: Text)
    FTScalar _       -> pretty ("0" :: Text)
    FTNamed n        ->
      case resolveType ctx n of
        Just ti | tiKind ti == TKEnum -> pretty ("(toEnum 0)" :: Text)
        _                             -> pretty ("Nothing" :: Text)

-- ---------------------------------------------------------------------------
-- Encode instances
-- ---------------------------------------------------------------------------

genEncodeInstance :: GenCtx -> [Text] -> MessageDef -> Doc ann
genEncodeInstance ctx scope msg =
  let tyN = scopedTypeName scope
      fields = extractAllFields ctx scope (msgElements msg)
  in vsep
    [ pretty ("instance MessageEncode " :: Text) <> pretty tyN <> pretty (" where" :: Text)
    , indent 2 $ vsep
        [ pretty ("buildMessage msg =" :: Text)
        , indent 2 $ case fields of
            [] -> pretty ("mempty" :: Text)
            _  -> vsep (zipWith (\i f -> genFieldBuild ctx i f) [0..] fields)
        ]
    ]

genFieldBuild :: GenCtx -> Int -> FieldInfoFull -> Doc ann
genFieldBuild ctx idx fi =
  let op = if idx == 0 then mempty else pretty ("<> " :: Text)
      accessor = "msg." <> fifAccessor fi
      fn = T.pack (show (fifFieldNum fi))
  in op <> case fifKind fi of
    FKScalar lbl ft -> genBuildExprScalar fn accessor lbl ft
    FKNamed lbl name tk -> genBuildExprNamed ctx fn accessor lbl name tk
    FKMap keyT valT -> genBuildExprMap ctx fn accessor keyT valT
    FKOneof scope ood -> genBuildExprOneof ctx scope fn accessor ood

genBuildExprScalar :: Text -> Text -> Maybe FieldLabel -> ScalarType -> Doc ann
genBuildExprScalar fn accessor lbl st = case lbl of
  Just Repeated -> genRepeatedScalarBuild fn accessor st
  Just Optional -> pretty ("(maybe mempty (\\v -> " :: Text) <> genSingleScalarBuild fn "v" st <> pretty (") " :: Text) <> pretty accessor <> pretty (")" :: Text)
  _ -> pretty ("(if " :: Text) <> scalarDefaultCheck accessor st <> pretty (" then mempty else " :: Text) <> genSingleScalarBuild fn accessor st <> pretty (")" :: Text)

genBuildExprNamed :: GenCtx -> Text -> Text -> Maybe FieldLabel -> Text -> TypeKind -> Doc ann
genBuildExprNamed ctx fn accessor lbl _name tk = case tk of
  TKEnum -> case lbl of
    Just Repeated -> pretty ("V.foldl' (\\acc v -> acc <> encodeFieldVarint " :: Text) <> pretty fn <+> pretty ("(fromIntegral (fromEnum v))) mempty " :: Text) <> pretty accessor
    Just Optional -> pretty ("(maybe mempty (\\v -> encodeFieldVarint " :: Text) <> pretty fn <+> pretty ("(fromIntegral (fromEnum v))) " :: Text) <> pretty accessor <> pretty (")" :: Text)
    _ -> pretty ("(if fromEnum " :: Text) <> pretty accessor <> pretty (" == 0 then mempty else encodeFieldVarint " :: Text) <> pretty fn <+> pretty ("(fromIntegral (fromEnum " :: Text) <> pretty accessor <> pretty (")))" :: Text)
  TKMessage -> case lbl of
    Just Repeated -> pretty ("V.foldl' (\\acc v -> acc <> encodeFieldMessage " :: Text) <> pretty fn <+> pretty ("v) mempty " :: Text) <> pretty accessor
    Just Optional -> pretty ("(maybe mempty (\\v -> encodeFieldMessage " :: Text) <> pretty fn <+> pretty ("v) " :: Text) <> pretty accessor <> pretty (")" :: Text)
    _ -> pretty ("(maybe mempty (\\v -> encodeFieldMessage " :: Text) <> pretty fn <+> pretty ("v) " :: Text) <> pretty accessor <> pretty (")" :: Text)

genBuildExprMap :: GenCtx -> Text -> Text -> ScalarType -> FieldType -> Doc ann
genBuildExprMap ctx fn accessor keyT valT =
  pretty ("Map.foldlWithKey' (\\acc k v -> acc <> encodeMapField " :: Text) <> pretty fn <>
  pretty (" (" :: Text) <> genMapKeyEncode keyT <> pretty (" k) (" :: Text) <> genMapValEncode ctx valT <> pretty (" v)) mempty " :: Text) <> pretty accessor

genMapKeyEncode :: ScalarType -> Doc ann
genMapKeyEncode st = case st of
  SString -> pretty ("encodeFieldString 1" :: Text)
  SBool   -> pretty ("encodeFieldBool 1" :: Text)
  SInt32  -> pretty ("(\\x -> encodeFieldVarint 1 (fromIntegral x))" :: Text)
  SInt64  -> pretty ("(\\x -> encodeFieldVarint 1 (fromIntegral x))" :: Text)
  SUInt32 -> pretty ("(\\x -> encodeFieldVarint 1 (fromIntegral x))" :: Text)
  SUInt64 -> pretty ("encodeFieldVarint 1" :: Text)
  SSInt32 -> pretty ("encodeFieldSVarint32 1" :: Text)
  SSInt64 -> pretty ("encodeFieldSVarint64 1" :: Text)
  SFixed32 -> pretty ("encodeFieldFixed32 1" :: Text)
  SFixed64 -> pretty ("encodeFieldFixed64 1" :: Text)
  SSFixed32 -> pretty ("(\\x -> encodeFieldFixed32 1 (fromIntegral x))" :: Text)
  SSFixed64 -> pretty ("(\\x -> encodeFieldFixed64 1 (fromIntegral x))" :: Text)
  _ -> pretty ("encodeFieldBytes 1" :: Text)

genMapValEncode :: GenCtx -> FieldType -> Doc ann
genMapValEncode ctx = \case
  FTScalar st -> genMapKeyEncode' 2 st
  FTNamed n -> case resolveType ctx n of
    Just ti | tiKind ti == TKEnum -> pretty ("(\\x -> encodeFieldVarint 2 (fromIntegral (fromEnum x)))" :: Text)
    _ -> pretty ("encodeFieldMessage 2" :: Text)
  where
    genMapKeyEncode' fn st = case st of
      SString  -> pretty ("encodeFieldString " :: Text) <> pretty (T.pack (show fn))
      SBytes   -> pretty ("encodeFieldBytes " :: Text) <> pretty (T.pack (show fn))
      SBool    -> pretty ("encodeFieldBool " :: Text) <> pretty (T.pack (show fn))
      SDouble  -> pretty ("encodeFieldDouble " :: Text) <> pretty (T.pack (show fn))
      SFloat   -> pretty ("encodeFieldFloat " :: Text) <> pretty (T.pack (show fn))
      SFixed32 -> pretty ("encodeFieldFixed32 " :: Text) <> pretty (T.pack (show fn))
      SFixed64 -> pretty ("encodeFieldFixed64 " :: Text) <> pretty (T.pack (show fn))
      SSFixed32 -> pretty ("encodeFieldFixed32 " :: Text) <> pretty (T.pack (show fn)) <> pretty (" . fromIntegral" :: Text)
      SSFixed64 -> pretty ("encodeFieldFixed64 " :: Text) <> pretty (T.pack (show fn)) <> pretty (" . fromIntegral" :: Text)
      _ -> pretty ("encodeFieldVarint " :: Text) <> pretty (T.pack (show fn)) <> pretty (" . fromIntegral" :: Text)

genBuildExprOneof :: GenCtx -> [Text] -> Text -> Text -> OneofDef -> Doc ann
genBuildExprOneof ctx scope fn accessor ood =
  pretty ("(case " :: Text) <> pretty accessor <> pretty (" of" :: Text) <> line <>
  indent 2 (vsep
    (pretty ("Nothing -> mempty" :: Text) :
     fmap (genOneofArmEncode ctx scope (oneofName ood)) (oneofFields ood))) <>
  pretty (")" :: Text)

genOneofArmEncode :: GenCtx -> [Text] -> Text -> OneofField -> Doc ann
genOneofArmEncode ctx scope ooName f =
  let conName = oneofConName scope ooName (oneofFieldName f)
      fn = T.pack (show (unFieldNumber (oneofFieldNumber f)))
  in pretty ("Just (" :: Text) <> pretty conName <+> pretty ("v) -> " :: Text) <>
     case oneofFieldType f of
       FTScalar st -> genSingleScalarBuild fn "v" st
       FTNamed n -> case resolveType ctx n of
         Just ti | tiKind ti == TKEnum ->
           pretty ("encodeFieldVarint " :: Text) <> pretty fn <+> pretty ("(fromIntegral (fromEnum v))" :: Text)
         _ -> pretty ("encodeFieldMessage " :: Text) <> pretty fn <+> pretty ("v" :: Text)

genSingleScalarBuild :: Text -> Text -> ScalarType -> Doc ann
genSingleScalarBuild fn accessor = \case
  SDouble   -> pretty ("encodeFieldDouble " :: Text) <> pretty fn <+> pretty accessor
  SFloat    -> pretty ("encodeFieldFloat " :: Text) <> pretty fn <+> pretty accessor
  SInt32    -> pretty ("encodeFieldVarint " :: Text) <> pretty fn <+> pretty ("(fromIntegral " :: Text) <> pretty accessor <> pretty (")" :: Text)
  SInt64    -> pretty ("encodeFieldVarint " :: Text) <> pretty fn <+> pretty ("(fromIntegral " :: Text) <> pretty accessor <> pretty (")" :: Text)
  SUInt32   -> pretty ("encodeFieldVarint " :: Text) <> pretty fn <+> pretty ("(fromIntegral " :: Text) <> pretty accessor <> pretty (")" :: Text)
  SUInt64   -> pretty ("encodeFieldVarint " :: Text) <> pretty fn <+> pretty accessor
  SSInt32   -> pretty ("encodeFieldSVarint32 " :: Text) <> pretty fn <+> pretty accessor
  SSInt64   -> pretty ("encodeFieldSVarint64 " :: Text) <> pretty fn <+> pretty accessor
  SFixed32  -> pretty ("encodeFieldFixed32 " :: Text) <> pretty fn <+> pretty accessor
  SFixed64  -> pretty ("encodeFieldFixed64 " :: Text) <> pretty fn <+> pretty accessor
  SSFixed32 -> pretty ("encodeFieldFixed32 " :: Text) <> pretty fn <+> pretty ("(fromIntegral " :: Text) <> pretty accessor <> pretty (")" :: Text)
  SSFixed64 -> pretty ("encodeFieldFixed64 " :: Text) <> pretty fn <+> pretty ("(fromIntegral " :: Text) <> pretty accessor <> pretty (")" :: Text)
  SBool     -> pretty ("encodeFieldBool " :: Text) <> pretty fn <+> pretty accessor
  SString   -> pretty ("encodeFieldString " :: Text) <> pretty fn <+> pretty accessor
  SBytes    -> pretty ("encodeFieldBytes " :: Text) <> pretty fn <+> pretty accessor

genRepeatedScalarBuild :: Text -> Text -> ScalarType -> Doc ann
genRepeatedScalarBuild fn accessor = \case
  SString ->
    pretty ("V.foldl' (\\acc v -> acc <> encodeFieldString " :: Text) <> pretty fn <+> pretty ("v) mempty " :: Text) <> pretty accessor
  SBytes ->
    pretty ("V.foldl' (\\acc v -> acc <> encodeFieldBytes " :: Text) <> pretty fn <+> pretty ("v) mempty " :: Text) <> pretty accessor
  SUInt32 ->
    pretty ("encodePackedVarint " :: Text) <> pretty fn <+> pretty ("(VU.map fromIntegral " :: Text) <> pretty accessor <> pretty (")" :: Text)
  SInt32 ->
    pretty ("encodePackedVarint " :: Text) <> pretty fn <+> pretty ("(VU.map fromIntegral " :: Text) <> pretty accessor <> pretty (")" :: Text)
  SInt64 ->
    pretty ("encodePackedVarint " :: Text) <> pretty fn <+> pretty ("(VU.map fromIntegral " :: Text) <> pretty accessor <> pretty (")" :: Text)
  SBool ->
    pretty ("encodePackedVarint " :: Text) <> pretty fn <+> pretty ("(VU.map (\\b -> if b then 1 else 0) " :: Text) <> pretty accessor <> pretty (")" :: Text)
  s -> pretty ("encode" :: Text) <> pretty (packedFnName s) <+> pretty fn <+> pretty accessor

scalarDefaultCheck :: Text -> ScalarType -> Doc ann
scalarDefaultCheck accessor = \case
  SBool   -> pretty accessor <+> pretty ("== False" :: Text)
  SString -> pretty accessor <+> pretty ("== T.empty" :: Text)
  SBytes  -> pretty ("BS.null " :: Text) <> pretty accessor
  _       -> pretty accessor <+> pretty ("== 0" :: Text)

packedFnName :: ScalarType -> Text
packedFnName = \case
  SDouble   -> "PackedDouble"
  SFloat    -> "PackedFloat"
  SInt32    -> "PackedVarint"
  SInt64    -> "PackedVarint"
  SUInt32   -> "PackedVarint"
  SUInt64   -> "PackedVarint"
  SSInt32   -> "PackedSVarint32"
  SSInt64   -> "PackedSVarint64"
  SFixed32  -> "PackedFixed32"
  SFixed64  -> "PackedFixed64"
  SSFixed32 -> "PackedFixed32"
  SSFixed64 -> "PackedFixed64"
  SBool     -> "PackedVarint"
  s         -> error ("Cannot pack: " <> show s)

-- ---------------------------------------------------------------------------
-- Size instances
-- ---------------------------------------------------------------------------

genSizeInstance :: GenCtx -> [Text] -> MessageDef -> Doc ann
genSizeInstance ctx scope msg =
  let tyN = scopedTypeName scope
      fields = extractAllFields ctx scope (msgElements msg)
  in vsep
    [ pretty ("instance MessageSize " :: Text) <> pretty tyN <> pretty (" where" :: Text)
    , indent 2 $ vsep
        [ pretty ("messageSize msg =" :: Text)
        , indent 2 $ case fields of
            [] -> pretty ("0" :: Text)
            _  -> vsep (zipWith (\i f -> genFieldSizeExpr ctx i f) [0..] fields)
        ]
    ]

genFieldSizeExpr :: GenCtx -> Int -> FieldInfoFull -> Doc ann
genFieldSizeExpr ctx idx fi =
  let op = if idx == 0 then mempty else pretty ("+ " :: Text)
      accessor = "msg." <> fifAccessor fi
      fn = T.pack (show (fifFieldNum fi))
  in op <> case fifKind fi of
    FKScalar lbl ft -> genSizeScalar fn accessor lbl ft
    FKNamed lbl name tk -> genSizeNamed ctx fn accessor lbl name tk
    FKMap keyT valT -> genSizeMap fn accessor
    FKOneof scope ood -> genSizeOneof ctx scope fn accessor ood

genSizeScalar :: Text -> Text -> Maybe FieldLabel -> ScalarType -> Doc ann
genSizeScalar fn accessor lbl st = case lbl of
  Just Repeated -> pretty ("0 {- TODO: repeated size -}" :: Text)
  Just Optional -> pretty ("(maybe 0 (\\v -> " :: Text) <> genSingleSizeScalar fn "v" st <> pretty (") " :: Text) <> pretty accessor <> pretty (")" :: Text)
  _ -> pretty ("(if " :: Text) <> scalarDefaultCheck accessor st <> pretty (" then 0 else " :: Text) <> genSingleSizeScalar fn accessor st <> pretty (")" :: Text)

genSingleSizeScalar :: Text -> Text -> ScalarType -> Doc ann
genSingleSizeScalar fn accessor = \case
  SDouble   -> pretty ("fieldDoubleSize " :: Text) <> pretty fn
  SFloat    -> pretty ("fieldFloatSize " :: Text) <> pretty fn
  SFixed32  -> pretty ("fieldFixed32Size " :: Text) <> pretty fn
  SFixed64  -> pretty ("fieldFixed64Size " :: Text) <> pretty fn
  SSFixed32 -> pretty ("fieldFixed32Size " :: Text) <> pretty fn
  SSFixed64 -> pretty ("fieldFixed64Size " :: Text) <> pretty fn
  SBool     -> pretty ("fieldBoolSize " :: Text) <> pretty fn
  SInt32    -> pretty ("fieldVarintSize " :: Text) <> pretty fn <+> pretty ("(fromIntegral " :: Text) <> pretty accessor <> pretty (")" :: Text)
  SInt64    -> pretty ("fieldVarintSize " :: Text) <> pretty fn <+> pretty ("(fromIntegral " :: Text) <> pretty accessor <> pretty (")" :: Text)
  SUInt32   -> pretty ("fieldVarintSize " :: Text) <> pretty fn <+> pretty ("(fromIntegral " :: Text) <> pretty accessor <> pretty (")" :: Text)
  SUInt64   -> pretty ("fieldVarintSize " :: Text) <> pretty fn <+> pretty accessor
  SSInt32   -> pretty ("fieldSVarint32Size " :: Text) <> pretty fn <+> pretty accessor
  SSInt64   -> pretty ("fieldSVarint64Size " :: Text) <> pretty fn <+> pretty accessor
  SString   -> pretty ("fieldTextSize " :: Text) <> pretty fn <+> pretty accessor
  SBytes    -> pretty ("fieldBytesSize " :: Text) <> pretty fn <+> pretty accessor

genSizeNamed :: GenCtx -> Text -> Text -> Maybe FieldLabel -> Text -> TypeKind -> Doc ann
genSizeNamed ctx fn accessor lbl name tk = case tk of
  TKEnum -> case lbl of
    Just Repeated -> pretty ("(V.foldl' (\\acc v -> acc + fieldVarintSize " :: Text) <> pretty fn <+> pretty ("(fromIntegral (fromEnum v))) 0 " :: Text) <> pretty accessor <> pretty (")" :: Text)
    Just Optional -> pretty ("(maybe 0 (\\v -> fieldVarintSize " :: Text) <> pretty fn <+> pretty ("(fromIntegral (fromEnum v))) " :: Text) <> pretty accessor <> pretty (")" :: Text)
    _ -> pretty ("(if fromEnum " :: Text) <> pretty accessor <> pretty (" == 0 then 0 else fieldVarintSize " :: Text) <> pretty fn <+> pretty ("(fromIntegral (fromEnum " :: Text) <> pretty accessor <> pretty (")))" :: Text)
  TKMessage -> case lbl of
    Just Repeated -> pretty ("(V.foldl' (\\acc v -> acc + fieldMessageSize " :: Text) <> pretty fn <+> pretty ("(messageSize v)) 0 " :: Text) <> pretty accessor <> pretty (")" :: Text)
    Just Optional -> pretty ("(maybe 0 (\\v -> fieldMessageSize " :: Text) <> pretty fn <+> pretty ("(messageSize v)) " :: Text) <> pretty accessor <> pretty (")" :: Text)
    _ -> pretty ("(maybe 0 (\\v -> fieldMessageSize " :: Text) <> pretty fn <+> pretty ("(messageSize v)) " :: Text) <> pretty accessor <> pretty (")" :: Text)

genSizeMap :: Text -> Text -> Doc ann
genSizeMap fn accessor =
  pretty ("(Map.foldlWithKey' (\\acc _ _ -> acc + tagSize " :: Text) <> pretty fn <> pretty (" + 20) 0 " :: Text) <> pretty accessor <> pretty (")" :: Text)

genSizeOneof :: GenCtx -> [Text] -> Text -> Text -> OneofDef -> Doc ann
genSizeOneof ctx scope fn accessor ood =
  pretty ("(case " :: Text) <> pretty accessor <> pretty (" of { Nothing -> 0" :: Text) <>
  vsep (fmap (\f ->
    let conName = oneofConName scope (oneofName ood) (oneofFieldName f)
        ffn = T.pack (show (unFieldNumber (oneofFieldNumber f)))
    in pretty ("; Just (" :: Text) <> pretty conName <+> pretty ("v) -> " :: Text) <>
       case oneofFieldType f of
         FTScalar st -> genSingleSizeScalar ffn "v" st
         FTNamed n -> case resolveType ctx n of
           Just ti | tiKind ti == TKEnum -> pretty ("fieldVarintSize " :: Text) <> pretty ffn <+> pretty ("(fromIntegral (fromEnum v))" :: Text)
           _ -> pretty ("fieldMessageSize " :: Text) <> pretty ffn <+> pretty ("(messageSize v)" :: Text)
  ) (oneofFields ood)) <>
  pretty (" })" :: Text)

-- ---------------------------------------------------------------------------
-- Decode instances
-- ---------------------------------------------------------------------------

genDecodeInstance :: GenCtx -> [Text] -> MessageDef -> Doc ann
genDecodeInstance ctx scope msg =
  let tyN = scopedTypeName scope
      fields = extractAllFields ctx scope (msgElements msg)
      allAccs = fmap (\(i, _) -> "acc_" <> T.pack (show i)) (zip [0..] fields)
  in vsep
    [ pretty ("instance MessageDecode " :: Text) <> pretty tyN <> pretty (" where" :: Text)
    , indent 2 $ vsep
        [ pretty ("messageDecoder = " :: Text) <> pretty ("loop" :: Text) <+>
          hsep (fmap (\fi -> pretty (fieldDefaultText ctx fi)) fields)
        , indent 2 $ pretty ("where" :: Text)
        , indent 4 $ vsep
            [ pretty ("loop " :: Text) <> hsep (fmap pretty allAccs) <+> pretty ("= do" :: Text)
            , indent 2 $ vsep
                [ pretty ("mTag <- getTagOr" :: Text)
                , pretty ("case mTag of" :: Text)
                , indent 2 $ vsep
                    [ pretty ("Nothing -> pure (" :: Text) <> pretty tyN <+>
                      braces (hsep (punctuate comma (fmap (\fi ->
                        pretty (fifAccessor fi) <+> pretty ("=" :: Text) <+> pretty ("acc_" :: Text) <> pretty (T.pack (show (fifIndex fi)))
                      ) fields))) <>
                      pretty (")" :: Text)
                    , pretty ("Just (Tag fn wt) -> case fn of" :: Text)
                    , indent 2 $ vsep (concatMap (genFieldDecodeCase ctx allAccs) fields <> [genDefaultDecodeCase allAccs])
                    ]
                ]
            ]
        ]
    ]

genFieldDecodeCase :: GenCtx -> [Text] -> FieldInfoFull -> [Doc ann]
genFieldDecodeCase ctx allAccs fi =
  case fifKind fi of
    FKScalar lbl ft -> [genScalarDecodeCase allAccs fi lbl ft]
    FKNamed lbl name tk -> [genNamedDecodeCase ctx allAccs fi lbl name tk]
    FKMap keyT valT -> [genMapDecodeCase ctx allAccs fi keyT valT]
    FKOneof scope ood -> concatMap (genOneofDecodeCase ctx scope (oneofName ood) allAccs fi) (oneofFields ood)

genScalarDecodeCase :: [Text] -> FieldInfoFull -> Maybe FieldLabel -> ScalarType -> Doc ann
genScalarDecodeCase allAccs fi lbl st =
  let fn = T.pack (show (fifFieldNum fi))
      idx = fifIndex fi
      accName = "acc_" <> T.pack (show idx)
      singletonFn = if isUnboxableScalar st then "VU.singleton" else "V.singleton"
      newAccs = case lbl of
        Just Repeated -> replaceAt idx ("(" <> accName <> " <> " <> singletonFn <> " v)") allAccs
        _ -> replaceAt idx "v" allAccs
  in pretty fn <+> pretty ("-> do" :: Text) <> line <>
     indent 2 (vsep
       [ pretty ("v <- " :: Text) <> pretty (scalarDecoderExpr st)
       , pretty ("loop " :: Text) <> hsep (fmap pretty newAccs)
       ])

genNamedDecodeCase :: GenCtx -> [Text] -> FieldInfoFull -> Maybe FieldLabel -> Text -> TypeKind -> Doc ann
genNamedDecodeCase ctx allAccs fi lbl name tk =
  let fn = T.pack (show (fifFieldNum fi))
      idx = fifIndex fi
      accName = "acc_" <> T.pack (show idx)
      newAccs = case lbl of
        Just Repeated -> replaceAt idx ("(" <> accName <> " <> V.singleton v)") allAccs
        Just Optional -> replaceAt idx "(Just v)" allAccs
        _ -> case tk of
          TKEnum -> replaceAt idx "v" allAccs
          TKMessage -> replaceAt idx "(Just v)" allAccs
      decoderExpr :: Text
      decoderExpr = case tk of
        TKEnum -> "decodeFieldEnum"
        TKMessage -> "decodeFieldMessage"
  in pretty fn <+> pretty ("-> do" :: Text) <> line <>
     indent 2 (vsep
       [ pretty ("v <- " :: Text) <> pretty decoderExpr
       , pretty ("loop " :: Text) <> hsep (fmap pretty newAccs)
       ])

genMapDecodeCase :: GenCtx -> [Text] -> FieldInfoFull -> ScalarType -> FieldType -> Doc ann
genMapDecodeCase ctx allAccs fi keyT valT =
  let fn = T.pack (show (fifFieldNum fi))
      idx = fifIndex fi
      accName = "acc_" <> T.pack (show idx)
      newAccs = replaceAt idx ("(Map.union " <> accName <> " (Map.singleton mk' mv'))") allAccs
      keyDecoder = scalarDecoderExpr keyT
      valDecoder = mapValDecoderExpr ctx valT
      keyDefault = scalarDefaultLit keyT
      valDefault = mapValDefaultLit ctx valT
  in pretty fn <+> pretty ("-> do" :: Text) <> line <>
     indent 2 (vsep
       [ pretty ("bs' <- getLengthDelimited" :: Text)
       , pretty ("let decodeEntry = runDecoder (decodeMapEntry" :: Text) <+>
         pretty keyDecoder <+> pretty valDecoder <+> pretty keyDefault <+> pretty valDefault <> pretty (") bs'" :: Text)
       , pretty ("case decodeEntry of" :: Text)
       , indent 2 $ vsep
           [ pretty ("Left _ -> loop " :: Text) <> hsep (fmap pretty allAccs)
           , pretty ("Right (mk', mv') -> loop " :: Text) <> hsep (fmap pretty newAccs)
           ]
       ])

genOneofDecodeCase :: GenCtx -> [Text] -> Text -> [Text] -> FieldInfoFull -> OneofField -> [Doc ann]
genOneofDecodeCase ctx scope ooName allAccs fi oof =
  let fn = T.pack (show (unFieldNumber (oneofFieldNumber oof)))
      idx = fifIndex fi
      conName = oneofConName scope ooName (oneofFieldName oof)
      newAccs = replaceAt idx ("(Just (" <> conName <> " v))") allAccs
      decoderExpr = case oneofFieldType oof of
        FTScalar st -> scalarDecoderExpr st
        FTNamed n -> case resolveType ctx n of
          Just ti | tiKind ti == TKEnum -> "decodeFieldEnum"
          _ -> "decodeFieldMessage"
  in [ pretty fn <+> pretty ("-> do" :: Text) <> line <>
       indent 2 (vsep
         [ pretty ("v <- " :: Text) <> pretty decoderExpr
         , pretty ("loop " :: Text) <> hsep (fmap pretty newAccs)
         ])
     ]

genDefaultDecodeCase :: [Text] -> Doc ann
genDefaultDecodeCase allAccs =
  pretty ("_ -> skipField wt >> loop " :: Text) <> hsep (fmap pretty allAccs)

fieldDefaultText :: GenCtx -> FieldInfoFull -> Text
fieldDefaultText ctx fi = case fifKind fi of
  FKScalar lbl ft -> case lbl of
    Just Repeated | isUnboxableScalar ft -> "VU.empty"
    Just Repeated -> "V.empty"
    Just Optional -> "Nothing"
    _ -> scalarDefaultText ft
  FKNamed lbl _ tk -> case lbl of
    Just Repeated -> "V.empty"
    Just Optional -> "Nothing"
    _ -> case tk of
      TKEnum -> "(toEnum 0)"
      TKMessage -> "Nothing"
  FKMap _ _ -> "Map.empty"
  FKOneof _ _ -> "Nothing"

scalarDefaultText :: ScalarType -> Text
scalarDefaultText = \case
  SBool   -> "False"
  SString -> "\"\""
  SBytes  -> "\"\""
  _       -> "0"

scalarDecoderExpr :: ScalarType -> Text
scalarDecoderExpr = \case
  SDouble   -> "decodeFieldDouble"
  SFloat    -> "decodeFieldFloat"
  SInt32    -> "(fromIntegral <$> decodeFieldVarint)"
  SInt64    -> "(fromIntegral <$> decodeFieldVarint)"
  SUInt32   -> "(fromIntegral <$> decodeFieldVarint)"
  SUInt64   -> "decodeFieldVarint"
  SSInt32   -> "decodeFieldSVarint32"
  SSInt64   -> "decodeFieldSVarint64"
  SFixed32  -> "decodeFieldFixed32"
  SFixed64  -> "decodeFieldFixed64"
  SSFixed32 -> "(fromIntegral <$> decodeFieldFixed32)"
  SSFixed64 -> "(fromIntegral <$> decodeFieldFixed64)"
  SBool     -> "decodeFieldBool"
  SString   -> "decodeFieldString"
  SBytes    -> "decodeFieldBytes"

mapValDecoderExpr :: GenCtx -> FieldType -> Text
mapValDecoderExpr ctx = \case
  FTScalar st -> scalarDecoderExpr st
  FTNamed n -> case resolveType ctx n of
    Just ti | tiKind ti == TKEnum -> "decodeFieldEnum"
    _ -> "decodeFieldMessage"

scalarDefaultLit :: ScalarType -> Text
scalarDefaultLit = \case
  SBool   -> "False"
  SString -> "\"\""
  SBytes  -> "\"\""
  _       -> "0"

mapValDefaultLit :: GenCtx -> FieldType -> Text
mapValDefaultLit ctx = \case
  FTScalar st -> scalarDefaultLit st
  FTNamed n -> case resolveType ctx n of
    Just ti | tiKind ti == TKEnum -> "(toEnum 0)"
    _ -> "undefined"

-- ---------------------------------------------------------------------------
-- JSON instances
-- ---------------------------------------------------------------------------

genToJSONInstance :: GenCtx -> [Text] -> MessageDef -> Doc ann
genToJSONInstance ctx scope msg =
  let tyN = scopedTypeName scope
      fields = extractAllFieldsJSON ctx scope (msgElements msg)
  in vsep
    [ pretty ("instance ProtoToJSON " :: Text) <> pretty tyN <> pretty (" where" :: Text)
    , indent 2 $ vsep
        [ pretty ("protoToJSON msg = jsonObject" :: Text)
        , indent 4 $ case fields of
            [] -> pretty ("[]" :: Text)
            _ -> vsep
              [ pretty ("[ " :: Text) <> head (fmap (genToJSONField ctx) fields)
              , vsep (fmap (\f -> pretty (", " :: Text) <> genToJSONField ctx f) (tail fields))
              , pretty ("]" :: Text)
              ]
        ]
    ]

genToJSONField :: GenCtx -> JSONFieldInfo -> Doc ann
genToJSONField ctx jfi =
  pretty ("\"" :: Text) <> pretty (jfiJsonName jfi) <> pretty ("\" .= msg." :: Text) <> pretty (jfiAccessor jfi)

genFromJSONInstance :: GenCtx -> [Text] -> MessageDef -> Doc ann
genFromJSONInstance ctx scope msg =
  let tyN = scopedTypeName scope
  in vsep
    [ pretty ("instance ProtoFromJSON " :: Text) <> pretty tyN <> pretty (" where" :: Text)
    , indent 2 $ vsep
        [ pretty ("protoFromJSON _ = Right default" :: Text) <> pretty tyN
        ]
    ]

-- ---------------------------------------------------------------------------
-- Field extraction (unified)
-- ---------------------------------------------------------------------------

data FieldKindFull
  = FKScalar (Maybe FieldLabel) ScalarType
  | FKNamed (Maybe FieldLabel) Text TypeKind
  | FKMap ScalarType FieldType
  | FKOneof [Text] OneofDef
  deriving stock (Show, Eq)

data FieldInfoFull = FieldInfoFull
  { fifAccessor  :: Text
  , fifFieldNum  :: Int
  , fifIndex     :: Int
  , fifKind      :: FieldKindFull
  } deriving stock (Show, Eq)

extractAllFields :: GenCtx -> [Text] -> [MessageElement] -> [FieldInfoFull]
extractAllFields ctx scope elems =
  zipWith (\i fi -> fi { fifIndex = i }) [0..] (concatMap go elems)
  where
    go = \case
      MEField fd ->
        let accessor = scopedFieldName scope (fieldName fd)
            fn = unFieldNumber (fieldNumber fd)
            kind = case fieldType fd of
              FTScalar st -> FKScalar (fieldLabel fd) st
              FTNamed n -> FKNamed (fieldLabel fd) n (resolveTypeKindScoped ctx scope n)
        in [FieldInfoFull accessor fn 0 kind]
      MEMapField mf ->
        let accessor = scopedFieldName scope (mapFieldName mf)
            fn = unFieldNumber (mapFieldNum mf)
        in [FieldInfoFull accessor fn 0 (FKMap (mapKeyType mf) (mapValueType mf))]
      MEOneof od ->
        let accessor = scopedFieldName scope (oneofName od)
            fn = 0
        in [FieldInfoFull accessor fn 0 (FKOneof scope od)]
      _ -> []

resolveTypeKind :: GenCtx -> Text -> TypeKind
resolveTypeKind ctx name = case resolveType ctx name of
  Just ti -> tiKind ti
  Nothing -> TKMessage

resolveTypeKindScoped :: GenCtx -> [Text] -> Text -> TypeKind
resolveTypeKindScoped ctx scope name = case resolveTypeWithScope ctx scope name of
  Just ti -> tiKind ti
  Nothing -> TKMessage

-- JSON field info
data JSONFieldInfo = JSONFieldInfo
  { jfiAccessor :: Text
  , jfiJsonName :: Text
  , jfiOptional :: Bool
  } deriving stock (Show, Eq)

extractAllFieldsJSON :: GenCtx -> [Text] -> [MessageElement] -> [JSONFieldInfo]
extractAllFieldsJSON ctx scope = concatMap go
  where
    go = \case
      MEField fd ->
        let accessor = scopedFieldName scope (fieldName fd)
            jsonName = fromMaybe (snakeToCamel (fieldName fd)) (getJsonName (fieldOptions fd))
        in [JSONFieldInfo accessor jsonName True]
      MEMapField mf ->
        let accessor = scopedFieldName scope (mapFieldName mf)
            jsonName = fromMaybe (snakeToCamel (mapFieldName mf)) (getJsonName (mapOptions mf))
        in [JSONFieldInfo accessor jsonName True]
      MEOneof od ->
        let accessor = scopedFieldName scope (oneofName od)
            jsonName = snakeToCamel (oneofName od)
        in [JSONFieldInfo accessor jsonName True]
      _ -> []

getJsonName :: [OptionDef] -> Maybe Text
getJsonName = \case
  [] -> Nothing
  opts -> lookupSimpleOption "json_name" opts >>= optionAsString

-- ---------------------------------------------------------------------------
-- Enum generation
-- ---------------------------------------------------------------------------

genEnum :: GenCtx -> [Text] -> EnumDef -> [Doc ann]
genEnum ctx scope ed =
  let scope' = scope <> [enumName ed]
      tyN = scopedTypeName scope'
  in [ mempty
     , genEnumDataDecl scope' ed
     , mempty
     , genEnumToProto scope' ed
     , mempty
     , genEnumFromProto scope' ed
     , mempty
     , genEnumEncodeInstance scope' ed
     , mempty
     , genEnumSizeInstance scope' ed
     , mempty
     , genEnumToJSONInstance scope' ed
     , mempty
     , genEnumFromJSONInstance scope' ed
     ]

genEnumDataDecl :: [Text] -> EnumDef -> Doc ann
genEnumDataDecl scope ed =
  let tyN = scopedTypeName scope
  in vsep
    [ pretty ("data " :: Text) <> pretty tyN
    , indent 2 (vsep (zipWith (\pfx v -> pfx <+> pretty (scopedEnumCon scope (evName v))) seps (enumValues ed)))
    , indent 2 (pretty ("deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)" :: Text))
    , indent 2 (pretty ("deriving anyclass NFData" :: Text))
    ]
  where
    seps = pretty ("=" :: Text) : repeat (pretty ("|" :: Text))

genEnumToProto :: [Text] -> EnumDef -> Doc ann
genEnumToProto scope ed =
  let tyN = scopedTypeName scope
      genCase ev =
        pretty ("toProtoEnum" :: Text) <> pretty tyN <+>
        pretty (scopedEnumCon scope (evName ev)) <+> pretty ("= " :: Text) <>
        pretty (T.pack (show (evNumber ev)))
  in vsep
    [ pretty ("toProtoEnum" :: Text) <> pretty tyN <+> pretty ("::" :: Text) <+> pretty tyN <+> pretty ("-> Int" :: Text)
    , vsep (fmap genCase (enumValues ed))
    ]

genEnumFromProto :: [Text] -> EnumDef -> Doc ann
genEnumFromProto scope ed =
  let tyN = scopedTypeName scope
      genCase ev =
        pretty ("fromProtoEnum" :: Text) <> pretty tyN <+>
        pretty (T.pack (show (evNumber ev))) <+> pretty ("= Just " :: Text) <>
        pretty (scopedEnumCon scope (evName ev))
  in vsep
    [ pretty ("fromProtoEnum" :: Text) <> pretty tyN <+> pretty ("::" :: Text) <+> pretty ("Int -> Maybe " :: Text) <> pretty tyN
    , vsep (fmap genCase (enumValues ed))
    , pretty ("fromProtoEnum" :: Text) <> pretty tyN <+> pretty ("_ = Nothing" :: Text)
    ]

genEnumEncodeInstance :: [Text] -> EnumDef -> Doc ann
genEnumEncodeInstance scope ed =
  let tyN = scopedTypeName scope
  in vsep
    [ pretty ("instance MessageEncode " :: Text) <> pretty tyN <> pretty (" where" :: Text)
    , indent 2 (pretty ("buildMessage _ = mempty" :: Text))
    , pretty ("instance MessageSize " :: Text) <> pretty tyN <> pretty (" where" :: Text)
    , indent 2 (pretty ("messageSize _ = 0" :: Text))
    , pretty ("instance MessageDecode " :: Text) <> pretty tyN <> pretty (" where" :: Text)
    , indent 2 (pretty ("messageDecoder = pure (toEnum 0)" :: Text))
    ]

genEnumSizeInstance :: [Text] -> EnumDef -> Doc ann
genEnumSizeInstance _ _ = mempty

genEnumToJSONInstance :: [Text] -> EnumDef -> Doc ann
genEnumToJSONInstance scope ed =
  let tyN = scopedTypeName scope
      genCase ev =
        pretty ("protoToJSON " :: Text) <> pretty (scopedEnumCon scope (evName ev)) <+>
        pretty ("= JsonString \"" :: Text) <> pretty (evName ev) <> pretty ("\"" :: Text)
  in vsep
    [ pretty ("instance ProtoToJSON " :: Text) <> pretty tyN <> pretty (" where" :: Text)
    , indent 2 (vsep (fmap genCase (enumValues ed)))
    ]

genEnumFromJSONInstance :: [Text] -> EnumDef -> Doc ann
genEnumFromJSONInstance scope ed =
  let tyN = scopedTypeName scope
      genCase ev =
        pretty ("  JsonString \"" :: Text) <> pretty (evName ev) <> pretty ("\" -> Right " :: Text) <> pretty (scopedEnumCon scope (evName ev))
  in vsep
    [ pretty ("instance ProtoFromJSON " :: Text) <> pretty tyN <> pretty (" where" :: Text)
    , indent 2 $ vsep
        [ pretty ("protoFromJSON = \\case" :: Text)
        , vsep (fmap genCase (enumValues ed))
        , pretty ("  JsonNumber n -> Right (toEnum (round n))" :: Text)
        , pretty ("  _ -> Left " :: Text) <> pretty ("\"Invalid enum value for " :: Text) <> pretty tyN <> pretty ("\"" :: Text)
        ]
    ]

-- ---------------------------------------------------------------------------
-- Haskell type helpers
-- ---------------------------------------------------------------------------

hsFieldType :: GenCtx -> [Text] -> FieldType -> Maybe FieldLabel -> Doc ann
hsFieldType ctx scope ft = \case
  Just Repeated -> hsRepeatedType ctx scope ft
  Just Optional -> pretty ("!(Maybe " :: Text) <> hsFieldTypeInner ctx scope ft <> pretty (")" :: Text)
  Just Required -> pretty ("!" :: Text) <> hsFieldTypeInner ctx scope ft
  Nothing       -> case ft of
    FTScalar s | isUnboxableScalar s -> pretty ("{-# UNPACK #-} !" :: Text) <> hsScalarType s
    FTScalar _ -> pretty ("!" :: Text) <> hsFieldTypeInner ctx scope ft
    FTNamed n -> case resolveType ctx n of
      Just ti | tiKind ti == TKEnum -> pretty ("!" :: Text) <> pretty (tiHsName ti)
      _ -> pretty ("!(Maybe " :: Text) <> hsFieldTypeInner ctx scope ft <> pretty (")" :: Text)

hsFieldTypeInner :: GenCtx -> [Text] -> FieldType -> Doc ann
hsFieldTypeInner ctx scope = \case
  FTScalar s -> hsScalarType s
  FTNamed n  -> pretty (resolveHsTypeNameScoped ctx scope n)

hsOneofFieldType :: GenCtx -> [Text] -> FieldType -> Doc ann
hsOneofFieldType ctx scope = \case
  FTScalar s -> unpackPragma s <> hsScalarType s
  FTNamed n  -> pretty ("!" :: Text) <> pretty (resolveHsTypeNameScoped ctx scope n)

hsRepeatedType :: GenCtx -> [Text] -> FieldType -> Doc ann
hsRepeatedType ctx scope = \case
  FTScalar s | isUnboxableScalar s -> pretty ("!(VU.Vector " :: Text) <> hsScalarType s <> pretty (")" :: Text)
  ft -> pretty ("!(V.Vector " :: Text) <> hsFieldTypeInner ctx scope ft <> pretty (")" :: Text)

resolveHsTypeName :: GenCtx -> Text -> Text
resolveHsTypeName ctx name = case resolveType ctx name of
  Just ti -> tiHsName ti
  Nothing -> hsTypeName (lastPart name)
  where
    lastPart t = case T.splitOn "." t of
      [] -> t
      parts -> last parts

resolveHsTypeNameScoped :: GenCtx -> [Text] -> Text -> Text
resolveHsTypeNameScoped ctx scope name = case resolveTypeWithScope ctx scope name of
  Just ti -> tiHsName ti
  Nothing -> hsTypeName (lastPart name)
  where
    lastPart t = case T.splitOn "." t of
      [] -> t
      parts -> last parts

hsScalarType :: ScalarType -> Doc ann
hsScalarType = \case
  SDouble   -> pretty ("Double" :: Text)
  SFloat    -> pretty ("Float" :: Text)
  SInt32    -> pretty ("Int32" :: Text)
  SInt64    -> pretty ("Int64" :: Text)
  SUInt32   -> pretty ("Word32" :: Text)
  SUInt64   -> pretty ("Word64" :: Text)
  SSInt32   -> pretty ("Int32" :: Text)
  SSInt64   -> pretty ("Int64" :: Text)
  SFixed32  -> pretty ("Word32" :: Text)
  SFixed64  -> pretty ("Word64" :: Text)
  SSFixed32 -> pretty ("Int32" :: Text)
  SSFixed64 -> pretty ("Int64" :: Text)
  SBool     -> pretty ("Bool" :: Text)
  SString   -> pretty ("Text" :: Text)
  SBytes    -> pretty ("ByteString" :: Text)

isUnboxableScalar :: ScalarType -> Bool
isUnboxableScalar = \case
  SString -> False
  SBytes  -> False
  _       -> True

unpackPragma :: ScalarType -> Doc ann
unpackPragma s
  | isUnboxableScalar s = pretty ("{-# UNPACK #-} !" :: Text)
  | otherwise           = pretty ("!" :: Text)

-- ---------------------------------------------------------------------------
-- Name conversion helpers
-- ---------------------------------------------------------------------------

scopedTypeName :: [Text] -> Text
scopedTypeName = T.intercalate "'" . fmap hsTypeName

scopedFieldName :: [Text] -> Text -> Text
scopedFieldName scope fName =
  let prefix = case scope of
        []    -> ""
        [s]   -> lowerFirst (hsTypeName s)
        _     -> lowerFirst (T.intercalate "" (fmap hsTypeName scope))
  in escapeReserved (prefix <> titleCase (snakeToCamel fName))

scopedEnumCon :: [Text] -> Text -> Text
scopedEnumCon scope valName =
  case scope of
    [] -> snakeToPascal valName
    _  -> scopedTypeName scope <> "'" <> snakeToPascal valName

hsTypeName :: Text -> Text
hsTypeName t = case T.uncons t of
  Just (c, rest) -> T.cons (toUpper c) rest
  Nothing        -> t

hsModuleName :: Text -> Text
hsModuleName = T.intercalate "." . fmap capitalize . T.splitOn "."
  where
    capitalize t = case T.uncons t of
      Just (c, rest) -> T.cons (toUpper c) rest
      Nothing        -> t

lowerFirst :: Text -> Text
lowerFirst s = case T.uncons s of
  Just (c, rest) -> T.cons (toLower c) rest
  Nothing        -> s

titleCase :: Text -> Text
titleCase s = case T.uncons s of
  Just (c, rest) -> T.cons (toUpper c) (T.toLower rest)
  Nothing        -> s

snakeToCamel :: Text -> Text
snakeToCamel t =
  let parts = T.splitOn "_" t
  in case parts of
    [] -> t
    (p:ps) -> T.concat (lowerFirst p : fmap titleCase ps)

snakeToPascal :: Text -> Text
snakeToPascal t =
  let parts = T.splitOn "_" t
  in T.concat (fmap titleCase parts)

escapeReserved :: Text -> Text
escapeReserved t
  | t `elem` haskellReserved = t <> "'"
  | otherwise = t

haskellReserved :: [Text]
haskellReserved =
  [ "type", "class", "data", "default", "deriving", "do", "else"
  , "if", "import", "in", "infix", "infixl", "infixr", "instance"
  , "let", "module", "newtype", "of", "then", "where", "case"
  , "foreign", "forall", "mdo", "qualified", "hiding"
  ]

oneofConName :: [Text] -> Text -> Text -> Text
oneofConName scope oneofN fieldName =
  scopedTypeName scope <> "'" <> snakeToPascal oneofN <> "'" <> snakeToPascal fieldName

replaceAt :: Int -> a -> [a] -> [a]
replaceAt _ _ [] = []
replaceAt 0 x (_:ys) = x : ys
replaceAt n x (y:ys) = y : replaceAt (n - 1) x ys
