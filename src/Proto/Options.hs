-- | Standard protobuf option extraction and language-specific attribute support.
--
-- Protobuf defines a set of standard options recognized by all conformant
-- implementations. This module provides structured access to these options,
-- plus Haskell-specific mappings.
--
-- Standard file-level options:
--
-- * @java_package@ — Java package name
-- * @java_outer_classname@ — Java outer class
-- * @java_multiple_files@ — Generate one .java per message
-- * @go_package@ — Go import path
-- * @csharp_namespace@ — C# namespace
-- * @objc_class_prefix@ — Objective-C class prefix
-- * @php_namespace@ — PHP namespace
-- * @ruby_package@ — Ruby module name
-- * @swift_prefix@ — Swift type prefix
-- * @optimize_for@ — SPEED, CODE_SIZE, or LITE_RUNTIME
-- * @cc_enable_arenas@ — C++ arena allocation
-- * @deprecated@ — Mark entire file deprecated
--
-- Standard message/field/enum options:
--
-- * @deprecated@ — Generate deprecation warnings
-- * @packed@ — Use packed wire encoding for repeated scalars
-- * @json_name@ — Override the JSON field name
-- * @map_entry@ — Mark a message as a synthetic map entry
-- * @allow_alias@ — Allow multiple enum values with the same number
module Proto.Options
  ( -- * File-level options
    FileOptions (..)
  , extractFileOptions

    -- * Message-level options
  , MessageOptions (..)
  , extractMessageOptions

    -- * Field-level options
  , FieldOptions (..)
  , extractFieldOptions

    -- * Enum-level options
  , EnumOptions (..)
  , extractEnumOptions

    -- * Enum value options
  , EnumValueOptions (..)
  , extractEnumValueOptions

    -- * Service/RPC options
  , ServiceOptions (..)
  , extractServiceOptions
  , RpcOptions (..)
  , extractRpcOptions

    -- * Optimization level
  , OptimizeMode (..)

    -- * Cross-language package mapping
  , LanguagePackages (..)
  , extractLanguagePackages

    -- * Deprecation
  , isDeprecated
  , deprecatedFields
  , deprecatedEnumValues
  ) where

import Data.Char (isAsciiLower)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Proto.AST
import Proto.Annotations

-- | Standard file-level options.
data FileOptions = FileOptions
  { foJavaPackage         :: !(Maybe Text)
  , foJavaOuterClassname  :: !(Maybe Text)
  , foJavaMultipleFiles   :: !Bool
  , foGoPackage           :: !(Maybe Text)
  , foCsharpNamespace     :: !(Maybe Text)
  , foObjcClassPrefix     :: !(Maybe Text)
  , foPhpNamespace        :: !(Maybe Text)
  , foRubyPackage         :: !(Maybe Text)
  , foSwiftPrefix         :: !(Maybe Text)
  , foOptimizeFor         :: !OptimizeMode
  , foCcEnableArenas      :: !Bool
  , foDeprecated          :: !Bool
  } deriving stock (Show, Eq)

data OptimizeMode = Speed | CodeSize | LiteRuntime
  deriving stock (Show, Eq, Ord)

extractFileOptions :: [OptionDef] -> FileOptions
extractFileOptions opts = FileOptions
  { foJavaPackage         = lookupSimpleOption "java_package" opts >>= optionAsString
  , foJavaOuterClassname  = lookupSimpleOption "java_outer_classname" opts >>= optionAsString
  , foJavaMultipleFiles   = fromMaybe False (lookupSimpleOption "java_multiple_files" opts >>= optionAsBool)
  , foGoPackage           = lookupSimpleOption "go_package" opts >>= optionAsString
  , foCsharpNamespace     = lookupSimpleOption "csharp_namespace" opts >>= optionAsString
  , foObjcClassPrefix     = lookupSimpleOption "objc_class_prefix" opts >>= optionAsString
  , foPhpNamespace        = lookupSimpleOption "php_namespace" opts >>= optionAsString
  , foRubyPackage         = lookupSimpleOption "ruby_package" opts >>= optionAsString
  , foSwiftPrefix         = lookupSimpleOption "swift_prefix" opts >>= optionAsString
  , foOptimizeFor         = parseOptimizeMode (lookupSimpleOption "optimize_for" opts >>= optionAsIdent)
  , foCcEnableArenas      = fromMaybe False (lookupSimpleOption "cc_enable_arenas" opts >>= optionAsBool)
  , foDeprecated          = fromMaybe False (lookupSimpleOption "deprecated" opts >>= optionAsBool)
  }

parseOptimizeMode :: Maybe Text -> OptimizeMode
parseOptimizeMode (Just "CODE_SIZE")    = CodeSize
parseOptimizeMode (Just "LITE_RUNTIME") = LiteRuntime
parseOptimizeMode _                     = Speed

-- | Standard message-level options.
data MessageOptions = MessageOptions
  { moDeprecated :: !Bool
  , moMapEntry   :: !Bool
  } deriving stock (Show, Eq)

extractMessageOptions :: [OptionDef] -> MessageOptions
extractMessageOptions opts = MessageOptions
  { moDeprecated = fromMaybe False (lookupSimpleOption "deprecated" opts >>= optionAsBool)
  , moMapEntry   = fromMaybe False (lookupSimpleOption "map_entry" opts >>= optionAsBool)
  }

-- | Standard field-level options.
data FieldOptions = FieldOptions
  { fldDeprecated :: !Bool
  , fldPacked     :: !(Maybe Bool)
  , fldJsonName   :: !(Maybe Text)
  } deriving stock (Show, Eq)

extractFieldOptions :: [OptionDef] -> FieldOptions
extractFieldOptions opts = FieldOptions
  { fldDeprecated = fromMaybe False (lookupSimpleOption "deprecated" opts >>= optionAsBool)
  , fldPacked     = lookupSimpleOption "packed" opts >>= optionAsBool
  , fldJsonName   = lookupSimpleOption "json_name" opts >>= optionAsString
  }

-- | Standard enum-level options.
data EnumOptions = EnumOptions
  { eoAllowAlias :: !Bool
  , eoDeprecated :: !Bool
  } deriving stock (Show, Eq)

extractEnumOptions :: [OptionDef] -> EnumOptions
extractEnumOptions opts = EnumOptions
  { eoAllowAlias = fromMaybe False (lookupSimpleOption "allow_alias" opts >>= optionAsBool)
  , eoDeprecated = fromMaybe False (lookupSimpleOption "deprecated" opts >>= optionAsBool)
  }

-- | Standard enum value options.
newtype EnumValueOptions = EnumValueOptions
  { evoDeprecated :: Bool
  } deriving stock (Show, Eq)

extractEnumValueOptions :: [OptionDef] -> EnumValueOptions
extractEnumValueOptions opts = EnumValueOptions
  { evoDeprecated = fromMaybe False (lookupSimpleOption "deprecated" opts >>= optionAsBool)
  }

-- | Standard service-level options.
newtype ServiceOptions = ServiceOptions
  { svoDeprecated :: Bool
  } deriving stock (Show, Eq)

extractServiceOptions :: [OptionDef] -> ServiceOptions
extractServiceOptions opts = ServiceOptions
  { svoDeprecated = fromMaybe False (lookupSimpleOption "deprecated" opts >>= optionAsBool)
  }

-- | Standard RPC-level options.
newtype RpcOptions = RpcOptions
  { roDeprecated :: Bool
  } deriving stock (Show, Eq)

extractRpcOptions :: [OptionDef] -> RpcOptions
extractRpcOptions opts = RpcOptions
  { roDeprecated = fromMaybe False (lookupSimpleOption "deprecated" opts >>= optionAsBool)
  }

-- | Cross-language package mapping extracted from a proto file's options.
data LanguagePackages = LanguagePackages
  { lpProtoPackage    :: !(Maybe Text)
  , lpJavaPackage     :: !(Maybe Text)
  , lpGoPackage       :: !(Maybe Text)
  , lpCsharpNamespace :: !(Maybe Text)
  , lpPhpNamespace    :: !(Maybe Text)
  , lpRubyPackage     :: !(Maybe Text)
  , lpSwiftPrefix     :: !(Maybe Text)
  , lpObjcPrefix      :: !(Maybe Text)
  , lpHaskellModule   :: !Text
  } deriving stock (Show, Eq)

-- | Extract the cross-language package mapping from a proto file.
-- The Haskell module name is derived from the proto package.
extractLanguagePackages :: ProtoFile -> LanguagePackages
extractLanguagePackages pf =
  let fo = extractFileOptions (protoOptions pf)
  in LanguagePackages
    { lpProtoPackage    = protoPackage pf
    , lpJavaPackage     = foJavaPackage fo
    , lpGoPackage       = foGoPackage fo
    , lpCsharpNamespace = foCsharpNamespace fo
    , lpPhpNamespace    = foPhpNamespace fo
    , lpRubyPackage     = foRubyPackage fo
    , lpSwiftPrefix     = foSwiftPrefix fo
    , lpObjcPrefix      = foObjcClassPrefix fo
    , lpHaskellModule   = deriveHaskellModule (protoPackage pf)
    }

deriveHaskellModule :: Maybe Text -> Text
deriveHaskellModule Nothing    = "Generated"
deriveHaskellModule (Just pkg) =
  T.intercalate "." (fmap capitalize (T.splitOn "." pkg))
  where
    capitalize t = case T.uncons t of
      Just (c, rest) -> T.cons (toUpper c) rest
      Nothing        -> t
    toUpper c
      | isAsciiLower c = toEnum (fromEnum c - 32)
      | otherwise = c

-- | Check if something is marked deprecated.
isDeprecated :: [OptionDef] -> Bool
isDeprecated opts = fromMaybe False (lookupSimpleOption "deprecated" opts >>= optionAsBool)

-- | All deprecated fields in a message.
deprecatedFields :: MessageDef -> [FieldDef]
deprecatedFields msg =
  concatMap (filter (isDeprecated . fieldOptions) . extractF) (msgElements msg)
  where
    extractF (MEField fd) = [fd]
    extractF _            = []

-- | All deprecated enum values.
deprecatedEnumValues :: EnumDef -> [EnumValue]
deprecatedEnumValues ed = filter (isDeprecated . evOptions) (enumValues ed)
