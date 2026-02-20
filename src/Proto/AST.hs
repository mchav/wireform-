-- | Proto file abstract syntax tree.
--
-- Supports both proto2 and proto3 syntax with full coverage of
-- messages, enums, services, oneofs, maps, extensions, and custom options.
module Proto.AST
  ( -- * Top-level
    ProtoFile (..)
  , Syntax (..)
  , TopLevel (..)
  , ImportDef (..)
  , ImportModifier (..)

    -- * Messages
  , MessageDef (..)
  , MessageElement (..)
  , FieldDef (..)
  , FieldLabel (..)
  , FieldType (..)
  , ScalarType (..)
  , MapField (..)
  , OneofDef (..)
  , OneofField (..)
  , ReservedDef (..)
  , ReservedRange (..)

    -- * Enums
  , EnumDef (..)
  , EnumValue (..)

    -- * Services
  , ServiceDef (..)
  , RpcDef (..)
  , StreamQualifier (..)

    -- * Options and annotations
  , OptionDef (..)
  , OptionName (..)
  , OptionNamePart (..)
  , Constant (..)

    -- * Extensions
  , ExtensionRange (..)
  , ExtensionRangeBound (..)

    -- * Field numbers
  , FieldNumber (..)
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

-- | A complete .proto file.
data ProtoFile = ProtoFile
  { protoSyntax  :: !Syntax
  , protoPackage :: !(Maybe Text)
  , protoImports :: ![ImportDef]
  , protoOptions :: ![OptionDef]
  , protoTopLevels :: ![TopLevel]
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFData

data Syntax = Proto2 | Proto3
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass NFData

data ImportDef = ImportDef
  { importModifier :: !(Maybe ImportModifier)
  , importPath     :: !Text
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFData

data ImportModifier = ImportPublic | ImportWeak
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

data TopLevel
  = TLMessage  !MessageDef
  | TLEnum     !EnumDef
  | TLService  !ServiceDef
  | TLExtend   !Text ![FieldDef]  -- extend <type> { fields }
  | TLOption   !OptionDef
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

-- | A protobuf message definition.
data MessageDef = MessageDef
  { msgName     :: !Text
  , msgElements :: ![MessageElement]
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFData

data MessageElement
  = MEField        !FieldDef
  | MEEnum         !EnumDef
  | MEMessage      !MessageDef
  | MEOneof        !OneofDef
  | MEMapField     !MapField
  | MEReserved     !ReservedDef
  | MEExtensions   ![ExtensionRange]
  | MEOption       !OptionDef
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

data FieldDef = FieldDef
  { fieldLabel   :: !(Maybe FieldLabel)
  , fieldType    :: !FieldType
  , fieldName    :: !Text
  , fieldNumber  :: !FieldNumber
  , fieldOptions :: ![OptionDef]
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFData

newtype FieldNumber = FieldNumber { unFieldNumber :: Int }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass NFData

data FieldLabel = Optional | Required | Repeated
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass NFData

data FieldType
  = FTScalar   !ScalarType
  | FTNamed    !Text         -- message or enum reference (possibly qualified)
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

data ScalarType
  = SDouble | SFloat
  | SInt32  | SInt64
  | SUInt32 | SUInt64
  | SSInt32 | SSInt64
  | SFixed32 | SFixed64
  | SSFixed32 | SSFixed64
  | SBool
  | SString | SBytes
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass NFData

data MapField = MapField
  { mapKeyType   :: !ScalarType
  , mapValueType :: !FieldType
  , mapFieldName :: !Text
  , mapFieldNum  :: !FieldNumber
  , mapOptions   :: ![OptionDef]
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFData

data OneofDef = OneofDef
  { oneofName   :: !Text
  , oneofFields :: ![OneofField]
  , oneofOptions :: ![OptionDef]
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFData

data OneofField = OneofField
  { oneofFieldType    :: !FieldType
  , oneofFieldName    :: !Text
  , oneofFieldNumber  :: !FieldNumber
  , oneofFieldOptions :: ![OptionDef]
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFData

data ReservedDef
  = ReservedNumbers ![ReservedRange]
  | ReservedNames   ![Text]
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

data ReservedRange
  = ReservedSingle !Int
  | ReservedRange  !Int !Int
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

data EnumDef = EnumDef
  { enumName    :: !Text
  , enumValues  :: ![EnumValue]
  , enumOptions :: ![OptionDef]
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFData

data EnumValue = EnumValue
  { evName    :: !Text
  , evNumber  :: !Int
  , evOptions :: ![OptionDef]
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFData

data ServiceDef = ServiceDef
  { svcName    :: !Text
  , svcRpcs    :: ![RpcDef]
  , svcOptions :: ![OptionDef]
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFData

data RpcDef = RpcDef
  { rpcName       :: !Text
  , rpcInput      :: !Text
  , rpcInputStr   :: !StreamQualifier
  , rpcOutput     :: !Text
  , rpcOutputStr  :: !StreamQualifier
  , rpcOptions    :: ![OptionDef]
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFData

data StreamQualifier = NoStream | Streaming
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

-- | An option (including custom options with extension names).
data OptionDef = OptionDef
  { optName  :: !OptionName
  , optValue :: !Constant
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFData

-- | An option name can be a simple identifier or a parenthesized extension name,
-- optionally followed by dotted sub-field access.
data OptionName = OptionName
  { optNameParts :: ![OptionNamePart]
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFData

data OptionNamePart
  = SimpleOption !Text         -- e.g. "java_package"
  | ExtensionOption !Text      -- e.g. "(my_custom_option)"
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

data Constant
  = CIdent     !Text
  | CInt       !Integer
  | CFloat     !Double
  | CString    !Text
  | CBool      !Bool
  | CAggregate ![(Text, Constant)]  -- { key: value, ... } aggregate literals
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

data ExtensionRange = ExtensionRange
  { extStart :: !Int
  , extEnd   :: !ExtensionRangeBound
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFData

data ExtensionRangeBound
  = ExtBoundNum !Int
  | ExtBoundMax
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData
