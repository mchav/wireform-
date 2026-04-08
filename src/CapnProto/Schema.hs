-- | Cap'n Proto schema types.
--
-- Defines the abstract syntax tree for Cap'n Proto schema definitions,
-- including structs with data\/pointer sections, enums, constants,
-- interfaces (RPC), unions, and the Cap'n Proto type system.
--
-- @
-- import CapnProto.Parser (parseCapnProto)
-- import CapnProto.Schema
--
-- let Right schema = parseCapnProto \"struct Point { x \@0 :Float64; y \@1 :Float64; }\"
-- print (csDecls schema)
-- @
module CapnProto.Schema
  ( -- * Schema
    CapnProtoSchema (..)
    -- * Declarations
  , Declaration (..)
  , StructDef (..)
  , FieldDef (..)
  , EnumDef (..)
  , UnionDef (..)
  , InterfaceDef (..)
  , MethodDef (..)
    -- * Types
  , CapnType (..)
  ) where

import Data.Text (Text)
import Data.Vector (Vector)
import Data.Word (Word16, Word64)

data CapnProtoSchema = CapnProtoSchema
  { csFileId  :: !(Maybe Word64)
  , csImports :: !(Vector Text)
  , csDecls   :: !(Vector Declaration)
  } deriving stock (Show, Eq)

data Declaration
  = DStruct !StructDef
  | DEnum !EnumDef
  | DConst !Text !CapnType !Text
  | DInterface !InterfaceDef
  | DAnnotation !Text !CapnType
  deriving stock (Show, Eq)

data StructDef = StructDef
  { sdName   :: !Text
  , sdFields :: !(Vector FieldDef)
  , sdNested :: !(Vector Declaration)
  , sdUnions :: !(Vector UnionDef)
  } deriving stock (Show, Eq)

data FieldDef = FieldDef
  { fdName        :: !Text
  , fdOrdinal     :: !Word16
  , fdType        :: !CapnType
  , fdDefault     :: !(Maybe Text)
  , fdAnnotations :: !(Vector (Text, Maybe Text))
  } deriving stock (Show, Eq)

data CapnType
  = CTVoid | CTBool | CTInt8 | CTInt16 | CTInt32 | CTInt64
  | CTUInt8 | CTUInt16 | CTUInt32 | CTUInt64
  | CTFloat32 | CTFloat64 | CTText | CTData
  | CTList !CapnType | CTNamed !Text
  deriving stock (Show, Eq)

data EnumDef = EnumDef
  { edName   :: !Text
  , edValues :: !(Vector (Text, Word16))
  } deriving stock (Show, Eq)

data UnionDef = UnionDef
  { udFields :: !(Vector FieldDef)
  } deriving stock (Show, Eq)

data InterfaceDef = InterfaceDef
  { idName    :: !Text
  , idMethods :: !(Vector MethodDef)
  } deriving stock (Show, Eq)

data MethodDef = MethodDef
  { mdName   :: !Text
  , mdParams :: !(Vector (Text, CapnType))
  , mdReturn :: !CapnType
  } deriving stock (Show, Eq)
