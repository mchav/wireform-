{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- | Proto file abstract syntax tree.

AST types use the /trees that grow/ pattern: each node is
parameterised by a phase @p@ that determines the annotation
carried at each node via the 'XNode' type family.

Two phases are provided:

* 'Semantic' — no source location info; used by codegen, analysis,
  and all existing consumer code via backward-compatible type aliases
  (@MessageDef = MessageDef' Semantic@, etc.).

* 'Parsed' — carries 'Span' for byte-accurate source reconstruction
  via 'Proto.ExactPrint'.

Supports proto2, proto3, and Editions (2023+) syntax with full coverage of
messages, enums, services, oneofs, maps, extensions, and custom options.
-}
module Proto.IDL.AST (
  -- * Phase types and spans (re-exported from "Proto.IDL.AST.Span")
  Parsed,
  Semantic,
  Span (..),
  SrcSpan (..),
  XNode,
  noSpan,
  mkSpan,

  -- * Top-level
  ProtoFile' (..),
  ProtoFile,
  Syntax (..),
  Edition (..),
  TopLevel' (..),
  TopLevel,
  ImportDef' (..),
  ImportDef,
  ImportModifier (..),

  -- * Messages
  MessageDef' (..),
  MessageDef,
  MessageElement' (..),
  MessageElement,
  FieldDef' (..),
  FieldDef,
  FieldLabel (..),
  FieldType (..),
  ScalarType (..),
  MapField' (..),
  MapField,
  OneofDef' (..),
  OneofDef,
  OneofField' (..),
  OneofField,
  ReservedDef (..),
  ReservedRange (..),

  -- * Enums
  EnumDef' (..),
  EnumDef,
  EnumValue' (..),
  EnumValue,

  -- * Services
  ServiceDef' (..),
  ServiceDef,
  RpcDef' (..),
  RpcDef,
  StreamQualifier (..),

  -- * Options and annotations
  OptionDef' (..),
  OptionDef,
  OptionName (..),
  OptionNamePart (..),
  Constant (..),

  -- * Comments
  Comment (..),

  -- * Extensions
  ExtensionRange (..),
  ExtensionRangeBound (..),

  -- * Field numbers
  FieldNumber (..),

  -- * Edition features
  FeatureSet (..),
  FieldPresenceFeature (..),
  EnumTypeFeature (..),
  RepeatedFieldEncodingFeature (..),
  Utf8ValidationFeature (..),
  MessageEncodingFeature (..),
  JsonFormatFeature (..),
  defaultFeatureSet,
  featuresForEdition,

  -- * Phase conversion
  stripSpans,
  stripTopLevel,
  stripMessage,
  stripEnum,
  stripService,
  stripField,
  stripOption,
) where

import Control.DeepSeq (NFData)
import Data.Text (Text)
import GHC.Generics (Generic)
import Proto.IDL.AST.Span


-- -----------------------------------------------------------------------
-- Backward-compatible type aliases (Semantic phase)
-- -----------------------------------------------------------------------

type ProtoFile = ProtoFile' Semantic


type TopLevel = TopLevel' Semantic


type ImportDef = ImportDef' Semantic


type MessageDef = MessageDef' Semantic


type MessageElement = MessageElement' Semantic


type FieldDef = FieldDef' Semantic


type MapField = MapField' Semantic


type OneofDef = OneofDef' Semantic


type OneofField = OneofField' Semantic


type EnumDef = EnumDef' Semantic


type EnumValue = EnumValue' Semantic


type ServiceDef = ServiceDef' Semantic


type RpcDef = RpcDef' Semantic


type OptionDef = OptionDef' Semantic


-- -----------------------------------------------------------------------
-- Top-level
-- -----------------------------------------------------------------------

-- | A complete .proto file.
data ProtoFile' p = ProtoFile
  { protoSyntax :: !Syntax
  , protoPackage :: !(Maybe Text)
  , protoImports :: ![ImportDef' p]
  , protoOptions :: ![OptionDef' p]
  , protoTopLevels :: ![TopLevel' p]
  , protoSource :: !(Maybe Text)
  }
  deriving stock (Generic)


deriving stock instance Show (XNode p) => Show (ProtoFile' p)


instance NFData (XNode p) => NFData (ProtoFile' p)


-- | Equality ignores 'protoSource' (it's metadata, not semantics).
instance Eq (XNode p) => Eq (ProtoFile' p) where
  a == b =
    protoSyntax a == protoSyntax b
      && protoPackage a == protoPackage b
      && protoImports a == protoImports b
      && protoOptions a == protoOptions b
      && protoTopLevels a == protoTopLevels b


data Syntax = Proto2 | Proto3 | Editions !Edition
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)


-- | Protobuf edition identifier (e.g. "2023", "2024").
newtype Edition = Edition {editionName :: Text}
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)


data ImportDef' p = ImportDef
  { importExt :: !(XNode p)
  , importModifier :: !(Maybe ImportModifier)
  , importPath :: !Text
  }
  deriving stock (Generic)


deriving stock instance Show (XNode p) => Show (ImportDef' p)


deriving stock instance Eq (XNode p) => Eq (ImportDef' p)


instance NFData (XNode p) => NFData (ImportDef' p)


data ImportModifier = ImportPublic | ImportWeak
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


-- | A comment in proto source.
data Comment
  = -- | @// content@ (without the @//@ prefix)
    LineComment !Text
  | -- | @/* content */@ (without delimiters)
    BlockComment !Text
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


data TopLevel' p
  = TLMessage !(MessageDef' p)
  | TLEnum !(EnumDef' p)
  | TLService !(ServiceDef' p)
  | TLExtend !Text ![FieldDef' p]
  | TLOption !(OptionDef' p)
  | -- | Standalone comment block between definitions
    TLComment ![Comment]
  deriving stock (Generic)


deriving stock instance Show (XNode p) => Show (TopLevel' p)


deriving stock instance Eq (XNode p) => Eq (TopLevel' p)


instance NFData (XNode p) => NFData (TopLevel' p)


-- -----------------------------------------------------------------------
-- Messages
-- -----------------------------------------------------------------------

-- | A protobuf message definition.
data MessageDef' p = MessageDef
  { msgExt :: !(XNode p)
  , msgDoc :: !(Maybe Text)
  , msgName :: !Text
  , msgElements :: ![MessageElement' p]
  }
  deriving stock (Generic)


deriving stock instance Show (XNode p) => Show (MessageDef' p)


deriving stock instance Eq (XNode p) => Eq (MessageDef' p)


instance NFData (XNode p) => NFData (MessageDef' p)


data MessageElement' p
  = MEField !(FieldDef' p)
  | MEEnum !(EnumDef' p)
  | MEMessage !(MessageDef' p)
  | MEOneof !(OneofDef' p)
  | MEMapField !(MapField' p)
  | MEReserved !ReservedDef
  | MEExtensions ![ExtensionRange]
  | MEOption !(OptionDef' p)
  | -- | Standalone comment inside a message body
    MEComment ![Comment]
  deriving stock (Generic)


deriving stock instance Show (XNode p) => Show (MessageElement' p)


deriving stock instance Eq (XNode p) => Eq (MessageElement' p)


instance NFData (XNode p) => NFData (MessageElement' p)


data FieldDef' p = FieldDef
  { fieldExt :: !(XNode p)
  , fieldDoc :: !(Maybe Text)
  , fieldLabel :: !(Maybe FieldLabel)
  , fieldType :: !FieldType
  , fieldName :: !Text
  , fieldNumber :: !FieldNumber
  , fieldOptions :: ![OptionDef' p]
  }
  deriving stock (Generic)


deriving stock instance Show (XNode p) => Show (FieldDef' p)


deriving stock instance Eq (XNode p) => Eq (FieldDef' p)


instance NFData (XNode p) => NFData (FieldDef' p)


newtype FieldNumber = FieldNumber {unFieldNumber :: Int}
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)


data FieldLabel = Optional | Required | Repeated
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)


data FieldType
  = FTScalar !ScalarType
  | FTNamed !Text
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


data ScalarType
  = SDouble
  | SFloat
  | SInt32
  | SInt64
  | SUInt32
  | SUInt64
  | SSInt32
  | SSInt64
  | SFixed32
  | SFixed64
  | SSFixed32
  | SSFixed64
  | SBool
  | SString
  | SBytes
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (NFData)


data MapField' p = MapField
  { mapExt :: !(XNode p)
  , mapDoc :: !(Maybe Text)
  , mapKeyType :: !ScalarType
  , mapValueType :: !FieldType
  , mapFieldName :: !Text
  , mapFieldNum :: !FieldNumber
  , mapOptions :: ![OptionDef' p]
  }
  deriving stock (Generic)


deriving stock instance Show (XNode p) => Show (MapField' p)


deriving stock instance Eq (XNode p) => Eq (MapField' p)


instance NFData (XNode p) => NFData (MapField' p)


data OneofDef' p = OneofDef
  { oneofExt :: !(XNode p)
  , oneofDoc :: !(Maybe Text)
  , oneofName :: !Text
  , oneofFields :: ![OneofField' p]
  , oneofOptions :: ![OptionDef' p]
  }
  deriving stock (Generic)


deriving stock instance Show (XNode p) => Show (OneofDef' p)


deriving stock instance Eq (XNode p) => Eq (OneofDef' p)


instance NFData (XNode p) => NFData (OneofDef' p)


data OneofField' p = OneofField
  { oneofFieldExt :: !(XNode p)
  , oneofFieldDoc :: !(Maybe Text)
  , oneofFieldType :: !FieldType
  , oneofFieldName :: !Text
  , oneofFieldNumber :: !FieldNumber
  , oneofFieldOptions :: ![OptionDef' p]
  }
  deriving stock (Generic)


deriving stock instance Show (XNode p) => Show (OneofField' p)


deriving stock instance Eq (XNode p) => Eq (OneofField' p)


instance NFData (XNode p) => NFData (OneofField' p)


data ReservedDef
  = ReservedNumbers ![ReservedRange]
  | ReservedNames ![Text]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


data ReservedRange
  = ReservedSingle !Int
  | ReservedRange !Int !Int
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


-- -----------------------------------------------------------------------
-- Enums
-- -----------------------------------------------------------------------

data EnumDef' p = EnumDef
  { enumExt :: !(XNode p)
  , enumDoc :: !(Maybe Text)
  , enumName :: !Text
  , enumValues :: ![EnumValue' p]
  , enumOptions :: ![OptionDef' p]
  }
  deriving stock (Generic)


deriving stock instance Show (XNode p) => Show (EnumDef' p)


deriving stock instance Eq (XNode p) => Eq (EnumDef' p)


instance NFData (XNode p) => NFData (EnumDef' p)


data EnumValue' p = EnumValue
  { evExt :: !(XNode p)
  , evDoc :: !(Maybe Text)
  , evName :: !Text
  , evNumber :: !Int
  , evOptions :: ![OptionDef' p]
  }
  deriving stock (Generic)


deriving stock instance Show (XNode p) => Show (EnumValue' p)


deriving stock instance Eq (XNode p) => Eq (EnumValue' p)


instance NFData (XNode p) => NFData (EnumValue' p)


-- -----------------------------------------------------------------------
-- Services
-- -----------------------------------------------------------------------

data ServiceDef' p = ServiceDef
  { svcExt :: !(XNode p)
  , svcDoc :: !(Maybe Text)
  , svcName :: !Text
  , svcRpcs :: ![RpcDef' p]
  , svcOptions :: ![OptionDef' p]
  }
  deriving stock (Generic)


deriving stock instance Show (XNode p) => Show (ServiceDef' p)


deriving stock instance Eq (XNode p) => Eq (ServiceDef' p)


instance NFData (XNode p) => NFData (ServiceDef' p)


data RpcDef' p = RpcDef
  { rpcExt :: !(XNode p)
  , rpcDoc :: !(Maybe Text)
  , rpcName :: !Text
  , rpcInput :: !Text
  , rpcInputStr :: !StreamQualifier
  , rpcOutput :: !Text
  , rpcOutputStr :: !StreamQualifier
  , rpcOptions :: ![OptionDef' p]
  }
  deriving stock (Generic)


deriving stock instance Show (XNode p) => Show (RpcDef' p)


deriving stock instance Eq (XNode p) => Eq (RpcDef' p)


instance NFData (XNode p) => NFData (RpcDef' p)


data StreamQualifier = NoStream | Streaming
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


-- -----------------------------------------------------------------------
-- Options
-- -----------------------------------------------------------------------

-- | An option (including custom options with extension names).
data OptionDef' p = OptionDef
  { optExt :: !(XNode p)
  , optName :: !OptionName
  , optValue :: !Constant
  }
  deriving stock (Generic)


deriving stock instance Show (XNode p) => Show (OptionDef' p)


deriving stock instance Eq (XNode p) => Eq (OptionDef' p)


instance NFData (XNode p) => NFData (OptionDef' p)


{- | An option name can be a simple identifier or a parenthesized extension name,
optionally followed by dotted sub-field access.
-}
newtype OptionName = OptionName
  { optNameParts :: [OptionNamePart]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


data OptionNamePart
  = SimpleOption !Text
  | ExtensionOption !Text
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


data Constant
  = CIdent !Text
  | CInt !Integer
  | CFloat !Double
  | CString !Text
  | CBool !Bool
  | CAggregate ![(Text, Constant)]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


-- -----------------------------------------------------------------------
-- Extensions
-- -----------------------------------------------------------------------

data ExtensionRange = ExtensionRange
  { extStart :: !Int
  , extEnd :: !ExtensionRangeBound
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


data ExtensionRangeBound
  = ExtBoundNum !Int
  | ExtBoundMax
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


-- -----------------------------------------------------------------------
-- Edition features
-- -----------------------------------------------------------------------

data FeatureSet = FeatureSet
  { featureFieldPresence :: !FieldPresenceFeature
  , featureEnumType :: !EnumTypeFeature
  , featureRepeatedFieldEncoding :: !RepeatedFieldEncodingFeature
  , featureUtf8Validation :: !Utf8ValidationFeature
  , featureMessageEncoding :: !MessageEncodingFeature
  , featureJsonFormat :: !JsonFormatFeature
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)


data FieldPresenceFeature = ExplicitPresence | ImplicitPresence | LegacyRequired
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)


data EnumTypeFeature = OpenEnum | ClosedEnum
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)


data RepeatedFieldEncodingFeature = PackedEncoding | ExpandedEncoding
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)


data Utf8ValidationFeature = Utf8Verify | Utf8None
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)


data MessageEncodingFeature = LengthPrefixedEncoding | DelimitedEncoding
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)


data JsonFormatFeature = JsonAllow | JsonLegacyBestEffort
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)


-- | Default feature set (matches proto3 defaults, which is edition 2023 default).
defaultFeatureSet :: FeatureSet
defaultFeatureSet =
  FeatureSet
    { featureFieldPresence = ExplicitPresence
    , featureEnumType = OpenEnum
    , featureRepeatedFieldEncoding = PackedEncoding
    , featureUtf8Validation = Utf8Verify
    , featureMessageEncoding = LengthPrefixedEncoding
    , featureJsonFormat = JsonAllow
    }


-- | Get the default feature set for a given edition.
featuresForEdition :: Edition -> FeatureSet
featuresForEdition (Edition "2023") = defaultFeatureSet
featuresForEdition (Edition "2024") =
  defaultFeatureSet
    { featureFieldPresence = ExplicitPresence
    }
featuresForEdition _ = defaultFeatureSet


-- -----------------------------------------------------------------------
-- Phase conversion
-- -----------------------------------------------------------------------

-- | Strip all span annotations, converting any phase to 'Semantic'.
stripSpans :: ProtoFile' p -> ProtoFile
stripSpans pf =
  ProtoFile
    { protoSyntax = protoSyntax pf
    , protoPackage = protoPackage pf
    , protoImports = fmap stripImport (protoImports pf)
    , protoOptions = fmap stripOption (protoOptions pf)
    , protoTopLevels = fmap stripTopLevel (protoTopLevels pf)
    , protoSource = protoSource pf
    }


stripTopLevel :: TopLevel' p -> TopLevel
stripTopLevel = \case
  TLMessage m -> TLMessage (stripMessage m)
  TLEnum e -> TLEnum (stripEnum e)
  TLService s -> TLService (stripService s)
  TLExtend n fs -> TLExtend n (fmap stripField fs)
  TLOption o -> TLOption (stripOption o)
  TLComment cs -> TLComment cs


stripImport :: ImportDef' p -> ImportDef
stripImport i = ImportDef {importExt = (), importModifier = importModifier i, importPath = importPath i}


stripMessage :: MessageDef' p -> MessageDef
stripMessage m =
  MessageDef
    { msgExt = ()
    , msgDoc = msgDoc m
    , msgName = msgName m
    , msgElements = fmap stripMsgElem (msgElements m)
    }


stripMsgElem :: MessageElement' p -> MessageElement
stripMsgElem = \case
  MEField f -> MEField (stripField f)
  MEEnum e -> MEEnum (stripEnum e)
  MEMessage m -> MEMessage (stripMessage m)
  MEOneof o -> MEOneof (stripOneof o)
  MEMapField mf -> MEMapField (stripMapField mf)
  MEReserved r -> MEReserved r
  MEExtensions e -> MEExtensions e
  MEOption o -> MEOption (stripOption o)
  MEComment cs -> MEComment cs


stripField :: FieldDef' p -> FieldDef
stripField f =
  FieldDef
    { fieldExt = ()
    , fieldDoc = fieldDoc f
    , fieldLabel = fieldLabel f
    , fieldType = fieldType f
    , fieldName = fieldName f
    , fieldNumber = fieldNumber f
    , fieldOptions = fmap stripOption (fieldOptions f)
    }


stripEnum :: EnumDef' p -> EnumDef
stripEnum e =
  EnumDef
    { enumExt = ()
    , enumDoc = enumDoc e
    , enumName = enumName e
    , enumValues = fmap stripEnumValue (enumValues e)
    , enumOptions = fmap stripOption (enumOptions e)
    }


stripEnumValue :: EnumValue' p -> EnumValue
stripEnumValue v =
  EnumValue
    { evExt = ()
    , evDoc = evDoc v
    , evName = evName v
    , evNumber = evNumber v
    , evOptions = fmap stripOption (evOptions v)
    }


stripService :: ServiceDef' p -> ServiceDef
stripService s =
  ServiceDef
    { svcExt = ()
    , svcDoc = svcDoc s
    , svcName = svcName s
    , svcRpcs = fmap stripRpc (svcRpcs s)
    , svcOptions = fmap stripOption (svcOptions s)
    }


stripRpc :: RpcDef' p -> RpcDef
stripRpc r =
  RpcDef
    { rpcExt = ()
    , rpcDoc = rpcDoc r
    , rpcName = rpcName r
    , rpcInput = rpcInput r
    , rpcInputStr = rpcInputStr r
    , rpcOutput = rpcOutput r
    , rpcOutputStr = rpcOutputStr r
    , rpcOptions = fmap stripOption (rpcOptions r)
    }


stripOneof :: OneofDef' p -> OneofDef
stripOneof o =
  OneofDef
    { oneofExt = ()
    , oneofDoc = oneofDoc o
    , oneofName = oneofName o
    , oneofFields = fmap stripOneofField (oneofFields o)
    , oneofOptions = fmap stripOption (oneofOptions o)
    }


stripOneofField :: OneofField' p -> OneofField
stripOneofField f =
  OneofField
    { oneofFieldExt = ()
    , oneofFieldDoc = oneofFieldDoc f
    , oneofFieldType = oneofFieldType f
    , oneofFieldName = oneofFieldName f
    , oneofFieldNumber = oneofFieldNumber f
    , oneofFieldOptions = fmap stripOption (oneofFieldOptions f)
    }


stripMapField :: MapField' p -> MapField
stripMapField m =
  MapField
    { mapExt = ()
    , mapDoc = mapDoc m
    , mapKeyType = mapKeyType m
    , mapValueType = mapValueType m
    , mapFieldName = mapFieldName m
    , mapFieldNum = mapFieldNum m
    , mapOptions = fmap stripOption (mapOptions m)
    }


stripOption :: OptionDef' p -> OptionDef
stripOption o = OptionDef {optExt = (), optName = optName o, optValue = optValue o}
