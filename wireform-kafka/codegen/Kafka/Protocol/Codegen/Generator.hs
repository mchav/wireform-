{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Kafka.Protocol.Codegen.Generator
Description : Generate Haskell code from Kafka protocol definitions
Copyright   : (c) 2025
License     : BSD-3-Clause

This module generates Haskell source code from parsed Kafka protocol schemas.
It uses the prettyprinter library to produce human-readable, well-formatted code
with proper Haddock documentation.

Generated code includes:

* Data type definitions with strict fields
* Version-aware encode/decode functions with range-based dispatch
* Support for flexible message formats and tagged fields
* Comprehensive Haddock documentation extracted from protocol comments
* Proper handling of nullable fields and defaults
-}
module Kafka.Protocol.Codegen.Generator
  ( -- * Code Generation
    generateMessageModule
  , generateMessage
  , generateModuleHeader
    -- * Field Generation
  , generateDataType
    -- * Naming
  , toHaskellTypeName
  , toHaskellFieldName
  , toHaskellModuleName
    -- * Inventory Generation
  , generateMessageInventory
  ) where

import Data.Aeson (Value)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Char (isUpper, toLower, toUpper)
import Data.Int
import Data.List (intercalate, nub, sort, groupBy, sortBy)
import Data.Maybe (fromMaybe, isJust, catMaybes)
import Data.Ord (comparing)
import Data.Scientific (Scientific)
import qualified Data.Scientific as Scientific
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Kafka.Protocol.Codegen.Types
import qualified Kafka.Protocol.Codegen.WireGenerator as WG
import Prettyprinter
import Prettyprinter.Render.Text

-- | Generate a complete Haskell module for a protocol message.
generateMessageModule :: ProtocolSchema -> Doc ann
generateMessageModule schema =
  let 
    -- Collect all type names that need to be exported (recursively)
    nestedTypeNames = concatMap (getNestedTypeNames (schemaName schema)) (schemaFields schema)
    -- Also recursively collect types from common structs
    commonTypeNames = concatMap (\field -> 
      case fieldType field of
        StructType name -> name : case fieldFields field of
          Just nestedFields -> concatMap (getNestedTypeNames name) nestedFields
          Nothing -> []
        _ -> []) (schemaCommonStructs schema)
    allTypes = nub $ filter (not . T.null) $ [schemaName schema] ++ nestedTypeNames ++ commonTypeNames
    typeExports = map (\t -> pretty t <> "(..)") allTypes
    -- Wire codec is exposed via the 'WireCodec' instance only;
    -- the legacy Serial-shape @encode<Foo>@ / @decode<Foo>@ pair is
    -- no longer emitted, so it doesn't appear in the export list.
    funcExports = []
    -- Add max version export
    maxVersionExport = "max" <> pretty (schemaName schema) <> "Version"
    allExports = typeExports ++ funcExports ++ [maxVersionExport]
    exportList = vsep $ punctuate "," allExports
  in
  vsep
    [ generateModuleHeader schema
    , ""
    , "module" <+> pretty (toHaskellModuleName $ schemaName schema)
    , indent 2 "("
    , indent 4 exportList
    , indent 2 ") where"
    , ""
    , generateImports
    , ""
    , generateMessage schema
    ]

-- | Get nested type names from a field spec (recursively)
getNestedTypeNames :: Text -> FieldSpec -> [Text]
getNestedTypeNames _ field =
  case fieldType field of
    ArrayType (StructType name) -> 
      -- Include this type and recursively get nested types from its fields
      name : case fieldFields field of
        Just nestedFields -> concatMap (getNestedTypeNames name) nestedFields
        Nothing -> []
    StructType name -> 
      -- Include this type and recursively get nested types from its fields
      name : case fieldFields field of
        Just nestedFields -> concatMap (getNestedTypeNames name) nestedFields
        Nothing -> []
    _ -> []

-- | Generate the module header with language extensions and documentation.
generateModuleHeader :: ProtocolSchema -> Doc ann
generateModuleHeader schema =
  vsep
    [ "{-# LANGUAGE DeriveGeneric #-}"
    , "{-# LANGUAGE StrictData #-}"
    , "{-# OPTIONS_GHC -Wno-unused-imports #-}"
    , ""
    , "{-|"
    , "Module      :" <+> pretty (toHaskellModuleName $ schemaName schema)
    , "Description : Kafka" <+> pretty (schemaName schema) <+> "message"
    , "Copyright   : (c) 2025"
    , "License     : BSD-3-Clause"
    , ""
    , case schemaApiKey schema of
        Just apiKey -> "Kafka" <+> pretty (schemaType schema) <+> "for API key" <+> pretty apiKey <> "."
        Nothing -> "Kafka" <+> pretty (schemaType schema) <+> "(no API key)."
    , ""
    , case schemaAbout schema of
        Just about -> vsep (map (("  " <>) . pretty) (T.lines about))
        Nothing -> ""
    , ""
    , "Valid versions:" <+> pretty (schemaValidVersions schema)
    , "Flexible versions:" <+> pretty (schemaFlexibleVersions schema)
    , ""
    , "This code is auto-generated from Kafka protocol definitions."
    , "-}"
    ]

-- | Generate import statements needed for the generated code.
-- Uses qualified imports to avoid naming conflicts.
generateImports :: Doc ann
generateImports =
  vsep
    [ "import Data.Int (Int8, Int16, Int32, Int64)"
    , "import Data.Word (Word16, Word32)"
    , "import GHC.Generics (Generic)"
    , "import qualified Data.Vector as V"
    , "import qualified Data.ByteString as BS"
    , "import qualified Kafka.Protocol.Primitives as P"
    , "import Kafka.Protocol.Primitives"
    , "  ( KafkaString, KafkaBytes, KafkaArray, KafkaUuid"
    , "  , Nullable(..)"
    , "  )"
    , "import Kafka.Protocol.Message (KafkaMessage(..))"
    , "import qualified Kafka.Protocol.Wire.Codec as WC"
    , WG.generateWireImports
    ]

-- | Generate code for a complete message (data type + encode/decode functions).
generateMessage :: ProtocolSchema -> Doc ann
generateMessage schema =
  let 
    flexibleVersion = case parseVersionSpec (schemaFlexibleVersions schema) of
      Right (VersionFrom v) -> Just v
      Right (VersionRange minV _) -> Just minV  -- Use minimum version as threshold
      Right (ExactVersion v) -> Just v
      _ -> Nothing
    
    validVersions = parseVersionSpec (schemaValidVersions schema)
    
    -- Calculate max version
    maxVersion = case validVersions of
      Right (VersionFrom v) -> Just 20  -- Use reasonable upper bound for open-ended
      Right (VersionRange _ v) -> Just v
      Right (ExactVersion v) -> Just v
      _ -> Nothing
    
    -- Generate nested types from fields
    nestedTypes = concatMap (generateNestedTypes (schemaName schema) flexibleVersion) (schemaFields schema)
    
    -- Generate common struct types
    commonTypes = concatMap (generateCommonStruct flexibleVersion) (schemaCommonStructs schema)
  in
  let
      -- Collect every (structName, fields) pair the schema introduces
      -- (common structs + nested ones, in declaration order — children
      -- before parents, so 'wirePoke' calls always have their target
      -- in scope). Used to emit per-struct Wire pokes/peeks alongside
      -- the message-level codec.
      allStructs = collectStructs schema
      perStructWire =
        [ vsep fns
        | (structName, structFields) <- allStructs
        , Just fns <- [WG.generateNestedWireFunctions
                         flexibleVersion structName structFields]
        ]
  in vsep
    [ -- Generate common struct types (data declarations only — no
      -- Serial-shape encode / decode functions; the per-struct
      -- Wire pokes/peeks below subsume them).
      vsep (map (<> line) commonTypes)
    , -- Generate nested structure types (same: data only).
      vsep (map (<> line) nestedTypes)
    , generateDataType (schemaName schema) (schemaFields schema) (schemaAbout schema)
    , ""
    , generateMaxVersionConstant (schemaName schema) maxVersion
    , ""
    , generateKafkaMessageInstance schema
    , ""
    , -- Per-struct Wire pokes / peeks (children first so the
      -- message-level codec can call them transparently).
      vsep perStructWire
    , -- Native Wire codec block: per-message 'wireMaxSize' /
      -- 'wirePoke' / 'wirePeek' functions + the 'WireCodec'
      -- instance pointing at them. There is no Serial fallback —
      -- the WireGenerator handles every schema the parser accepts.
      case WG.generateWireFunctions schema of
        Just fns -> vsep (fns ++ ["", WG.generateWireCodecOverride schema])
        Nothing  -> WG.generateWireCodecOverride schema
    ]

-- | Walk the schema's common structs + nested struct fields and
-- collect every named struct type along with its fields, in
-- declaration order (children before parents).
collectStructs :: ProtocolSchema -> [(Text, [FieldSpec])]
collectStructs schema =
  concatMap (collectStructsField "")  (schemaCommonStructs schema)
    ++ concatMap (collectStructsField (schemaName schema)) (schemaFields schema)

collectStructsField :: Text -> FieldSpec -> [(Text, [FieldSpec])]
collectStructsField _parent f = case fieldType f of
  StructType structName -> case fieldFields f of
    Just fs ->
         concatMap (collectStructsField structName) fs
      ++ [(structName, fs)]
    Nothing -> []
  ArrayType (StructType structName) -> case fieldFields f of
    Just fs ->
         concatMap (collectStructsField structName) fs
      ++ [(structName, fs)]
    Nothing -> []
  _ -> []

-- | Check if a structure has version-dependent fields (fields not present in all versions).
hasVersionDependentFields :: [FieldSpec] -> Bool
hasVersionDependentFields fields = any (not . isAlwaysPresent) fields
  where
    isAlwaysPresent :: FieldSpec -> Bool
    isAlwaysPresent field = 
      case parseVersionSpec (fieldVersions field) of
        Right (VersionFrom 0) -> True  -- "0+" means always present
        _ -> False  -- Any other version spec means version-dependent

-- | Check if a field is a tagged field (appears in tagged fields section for given version)
isTaggedField :: Int16 -> FieldSpec -> Bool
isTaggedField version field =
  case (fieldTag field, fieldTaggedVersions field) of
    (Just _, Just taggedVers) ->
      -- This field has a tag and tagged versions - check if this version is in range
      case parseVersionSpec taggedVers of
        Right spec -> inVersionRange version spec
        Left _ -> False
    _ -> False

-- | Compile-time check: does this field have a tag number assigned?
-- Used to decide whether the codegen needs to emit per-tag encode /
-- decode dispatch in the nested-struct generators. (A field with
-- 'fieldTag = Just _' is treated as tagged on the wire from
-- 'fieldTaggedVersions' onwards; before that it's absent entirely.)
isPotentiallyTaggedField :: FieldSpec -> Bool
isPotentiallyTaggedField f = isJust (fieldTag f)

-- | Sub-list of tagged fields. Used by the nested-struct generators
-- to skip them in the regular-field loop and emit them inside the
-- TaggedFields envelope instead.
nestedTaggedFields :: [FieldSpec] -> [FieldSpec]
nestedTaggedFields = filter isPotentiallyTaggedField

-- | Generate a common struct type definition. Just the @data@
-- declaration; the per-struct Wire pokes / peeks are emitted
-- separately by 'WG.generateNestedWireFunctions' over the same
-- struct list (see 'collectStructs'). No Serial-shape encode /
-- decode functions are emitted any more.
generateCommonStruct :: Maybe Int16 -> FieldSpec -> [Doc ann]
generateCommonStruct flexibleVersion field =
  case (fieldType field, fieldFields field) of
    (StructType structName, Just nestedFields) ->
      let structDoc    = generateDataType structName nestedFields (fieldAbout field)
          deeperNested = concatMap (generateNestedTypes structName flexibleVersion) nestedFields
      in deeperNested ++ [structDoc]
    _ -> []

-- | Generate nested struct @data@ declarations (only). The Wire
-- pokes / peeks for each struct are emitted separately by the
-- per-struct loop in 'generateMessage'.
generateNestedTypes :: Text -> Maybe Int16 -> FieldSpec -> [Doc ann]
generateNestedTypes _parentName flexibleVersion field =
  case fieldType field of
    ArrayType (StructType structName) ->
      case fieldFields field of
        Just nestedFields ->
          let structDoc    = generateDataType structName nestedFields (fieldAbout field)
              deeperNested = concatMap (generateNestedTypes structName flexibleVersion) nestedFields
          in deeperNested ++ [structDoc]
        Nothing -> []
    StructType structName ->
      case fieldFields field of
        Just nestedFields ->
          let structDoc    = generateDataType structName nestedFields (fieldAbout field)
              deeperNested = concatMap (generateNestedTypes structName flexibleVersion) nestedFields
          in deeperNested ++ [structDoc]
        Nothing -> []
    _ -> []

-- | Generate a constant for the maximum supported version.
generateMaxVersionConstant :: Text -> Maybe Int16 -> Doc ann
generateMaxVersionConstant typeName maxVer =
  let constantName = "max" <> pretty typeName <> "Version"
      versionValue = case maxVer of
        Just v -> pretty v
        Nothing -> "-1 -- No valid versions"
  in vsep
    [ "-- | Maximum supported version for" <+> pretty typeName <> "."
    , constantName <+> ":: Int16"
    , constantName <+> "=" <+> versionValue
    ]

-- | Generate a KafkaMessage instance for messages with API keys.
-- Messages without an API key (e.g., headers, internal types) won't have an instance.
generateKafkaMessageInstance :: ProtocolSchema -> Doc ann
generateKafkaMessageInstance schema =
  case schemaApiKey schema of
    Nothing -> mempty  -- No instance for messages without API keys
    Just apiKey ->
      let typeName = schemaName schema
          validVersions = parseVersionSpec (schemaValidVersions schema)
          flexibleVersions = parseVersionSpec (schemaFlexibleVersions schema)
          
          -- Calculate min version
          minVer = case validVersions of
            Right (ExactVersion v) -> v
            Right (VersionRange v _) -> v
            Right (VersionFrom v) -> v
            _ -> 0
          
          -- Calculate max version  
          maxVer = case validVersions of
            Right (ExactVersion v) -> v
            Right (VersionRange _ v) -> v
            Right (VersionFrom _) -> 20  -- Use reasonable upper bound for open-ended
            _ -> 0
          
          -- Calculate first flexible version
          flexVer = case flexibleVersions of
            Right (ExactVersion v) -> "Just" <+> pretty v
            Right (VersionRange v _) -> "Just" <+> pretty v
            Right (VersionFrom v) -> "Just" <+> pretty v
            _ -> "Nothing"
          
      in vsep
        [ "-- | KafkaMessage instance for" <+> pretty typeName <> "."
        , "instance KafkaMessage" <+> pretty typeName <+> "where"
        , indent 2 $ "messageApiKey = " <> pretty apiKey
        , indent 2 $ "messageMinVersion = " <> pretty minVer
        , indent 2 $ "messageMaxVersion = " <> pretty maxVer
        , indent 2 $ "messageFlexibleVersion = " <> flexVer
        ]

-- | Generate a Haskell data type from field specifications.
generateDataType :: Text -> [FieldSpec] -> Maybe Text -> Doc ann
generateDataType typeName fields maybeDoc =
  vsep $
    [ case maybeDoc of
        Just doc -> vsep
          [ "-- |" <+> pretty (T.take 200 $ T.replace "\n" " " doc)
          ]
        Nothing -> ""
    , "data" <+> pretty typeName <+> "=" <+> pretty typeName
    , indent 2 "{"
    ]
    ++ punctuate (line <> ",") (map generateField fields)
    ++
    [ line <> indent 2 "}"
    , indent 2 "deriving (Eq, Show, Generic)"
    ]
  where
    generateField :: FieldSpec -> Doc ann
    generateField field =
      let fieldDoc = case fieldAbout field of
            Just doc -> line <> indent 2 ("-- |" <+> pretty (T.take 100 $ T.replace "\n" " " doc))
            Nothing -> mempty
          fieldVersionInfo = line <> indent 2 ("-- Versions:" <+> pretty (fieldVersions field))
      in vsep
        [ fieldDoc
        , fieldVersionInfo
        , indent 2 $ pretty (toHaskellFieldName typeName (fieldName field))
            <+> ":: !" <> parens (pretty (fieldToHaskellType field))
        ]

-- -----------------------------------------------------------------------------
-- Field type rendering
--
-- The bulk of the Serial-shape encode / decode generators that
-- used to live below this point are gone — the codegen now emits
-- only the data type + 'WireCodec' instance + 'wirePoke*' /
-- 'wirePeek*' / 'wireMaxSize*' functions, all driven from
-- "Kafka.Protocol.Codegen.WireGenerator". The legacy helpers were
-- dead after that migration and are removed here.
--
-- The two surviving helpers below ('fieldToHaskellType' /
-- 'typeSpecToHaskellType') are still called by 'generateField'
-- when emitting the @data Foo = Foo { ... }@ declaration.
-- -----------------------------------------------------------------------------

-- | Convert a field spec to a Haskell type, considering nullable versions.
fieldToHaskellType :: FieldSpec -> Text
fieldToHaskellType field =
  let baseType = typeSpecToHaskellType (fieldType field) (fieldFields field)
      isNullable = isJust (fieldNullableVersions field)
      -- Don't wrap already-nullable types (KafkaString, KafkaBytes, KafkaArray)
      -- These types already handle null encoding internally.
      alreadyNullable = case fieldType field of
        PrimitiveType "string" -> True
        PrimitiveType "bytes" -> True
        ArrayType _ -> True
        _ -> False
  in if isNullable && not alreadyNullable
       then "Nullable (" <> baseType <> ")"
       else baseType

-- | Convert a TypeSpec to the base Haskell type.
typeSpecToHaskellType :: TypeSpec -> Maybe [FieldSpec] -> Text
typeSpecToHaskellType (PrimitiveType "bool") _ = "Bool"
typeSpecToHaskellType (PrimitiveType "int8") _ = "Int8"
typeSpecToHaskellType (PrimitiveType "int16") _ = "Int16"
typeSpecToHaskellType (PrimitiveType "int32") _ = "Int32"
typeSpecToHaskellType (PrimitiveType "int64") _ = "Int64"
typeSpecToHaskellType (PrimitiveType "uint16") _ = "Word16"
typeSpecToHaskellType (PrimitiveType "uint32") _ = "Word32"
typeSpecToHaskellType (PrimitiveType "string") _ = "KafkaString"
typeSpecToHaskellType (PrimitiveType "bytes") _ = "KafkaBytes"
typeSpecToHaskellType (PrimitiveType "uuid") _ = "KafkaUuid"
typeSpecToHaskellType (PrimitiveType "float64") _ = "Double"
typeSpecToHaskellType (PrimitiveType _) _ = "Int32"  -- Default for unknown
typeSpecToHaskellType (ArrayType inner) mFields =
  let innerHaskellType = typeSpecToHaskellType inner mFields
  in "KafkaArray (" <> innerHaskellType <> ")"
typeSpecToHaskellType (StructType name) _ = name

-- -----------------------------------------------------------------------------
-- Naming + module-name helpers
-- -----------------------------------------------------------------------------

toHaskellTypeName = T.pack . ensureUpper . T.unpack
  where
    ensureUpper [] = []
    ensureUpper (x:xs) = toUpper x : xs

-- | Convert a protocol field name to a Haskell record field name.
-- Prepends the type name in camelCase to avoid field name conflicts.
toHaskellFieldName :: Text -> Text -> Text
toHaskellFieldName typeName fieldName =
  let typePrefix = if T.null typeName 
                     then ""
                     else T.pack $ toCamelCase $ T.unpack typeName
      fieldSuffix = T.pack $ T.unpack fieldName
  in typePrefix <> fieldSuffix
  where
    toCamelCase [] = []
    toCamelCase (x:xs) = toLower x : xs

-- | Convert a protocol name to a Haskell module name.
toHaskellModuleName :: Text -> Text
toHaskellModuleName name = "Kafka.Protocol.Generated." <> name

-- | Render a Doc to Text.
renderDoc :: Doc ann -> Text
renderDoc = renderStrict . layoutPretty defaultLayoutOptions

-- | Generate a JSON inventory of all messages for test generation.
-- This produces a JSON array with metadata about each message type.
generateMessageInventory :: [ProtocolSchema] -> Text
generateMessageInventory schemas =
  let inventoryItems = map schemaToInventoryItem schemas
      jsonValue = Aeson.toJSON inventoryItems
  in TL.toStrict $ TL.decodeUtf8 $ Aeson.encode jsonValue

-- | Convert a ProtocolSchema to an inventory item.
schemaToInventoryItem :: ProtocolSchema -> Aeson.Value
schemaToInventoryItem schema =
  let validVersions = parseVersionSpec (schemaValidVersions schema)
      flexibleVersions = parseVersionSpec (schemaFlexibleVersions schema)
      
      -- Determine versions to test
      allVersions = case validVersions of
        Right spec -> expandVersionSpec spec
        _ -> []
      
      firstFlexibleVersion = case flexibleVersions of
        Right (VersionFrom v) -> Just v
        Right (VersionRange v _) -> Just v
        Right (ExactVersion v) -> Just v
        _ -> Nothing
      
      -- Select key versions: v0, first flexible, max, and a few intermediate
      versionsToTest = selectTestVersions allVersions firstFlexibleVersion
      
  in Aeson.object
    [ "name" Aeson..= schemaName schema
    , "type" Aeson..= schemaType schema
    , "apiKey" Aeson..= schemaApiKey schema
    , "validVersions" Aeson..= schemaValidVersions schema
    , "flexibleVersions" Aeson..= schemaFlexibleVersions schema
    , "allVersions" Aeson..= allVersions
    , "versionsToTest" Aeson..= versionsToTest
    , "about" Aeson..= schemaAbout schema
    ]

-- | Select a representative set of versions to test.
-- Includes: v0, first flexible version, max version, and some intermediate versions.
selectTestVersions :: [Int16] -> Maybe Int16 -> [Int16]
selectTestVersions [] _ = []
selectTestVersions versions maybeFlexible =
  let v0 = if 0 `elem` versions then [0] else []
      maxV = [maximum versions]
      flexV = case maybeFlexible of
        Just fv | fv `elem` versions && fv /= 0 && fv /= maximum versions -> [fv]
        _ -> []
      
      -- Add a few intermediate versions (roughly every 3-4 versions)
      intermediate = if length versions > 4
                     then let step = max 1 (length versions `div` 4)
                              indices = [step, step * 2, step * 3]
                              vs = [versions !! i | i <- indices, i < length versions]
                          in filter (\v -> v `notElem` (v0 ++ flexV ++ maxV)) vs
                     else []
      
  in nub $ sort $ v0 ++ flexV ++ intermediate ++ maxV
