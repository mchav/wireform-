{-# LANGUAGE BangPatterns #-}
-- | Thrift schema AST — a complete representation of Thrift IDL constructs.
--
-- Covers base types, containers, structs, unions, exceptions, enums,
-- typedefs, constants, and services.
module Thrift.Schema
  ( -- * Top-level schema
    ThriftSchema (..)

    -- * Types
  , ThriftType (..)

    -- * Struct / Union / Exception
  , ThriftStruct (..)
  , StructKind (..)
  , ThriftField (..)
  , Requiredness (..)

    -- * Enum
  , ThriftEnum (..)

    -- * Service
  , ThriftService (..)
  , ThriftMethod (..)

    -- * Const
  , ThriftConst (..)
  , ThriftConstValue (..)

    -- * Typedef
  , ThriftTypedef (..)
  ) where

import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Vector (Vector)

--------------------------------------------------------------------------------
-- Requiredness
--------------------------------------------------------------------------------

-- | Field requiredness as specified in the Thrift IDL.
data Requiredness
  = Required  -- ^ The field must be present.
  | Optional  -- ^ The field may be absent.
  | Default   -- ^ Requiredness not specified (Thrift default behavior).
  deriving stock (Show, Eq, Ord, Enum, Bounded)

--------------------------------------------------------------------------------
-- Thrift types
--------------------------------------------------------------------------------

-- | All types expressible in Thrift IDL.
data ThriftType
  = TBool
  | TByte
  | TI16
  | TI32
  | TI64
  | TDouble
  | TString
  | TBinary
  | TUUID
  | TStruct  !Text
  | TEnum    !Text
  | TTypedef !Text
  | TList    !ThriftType
  | TSet     !ThriftType
  | TMap     !ThriftType !ThriftType
  deriving stock (Show, Eq, Ord)

--------------------------------------------------------------------------------
-- Fields
--------------------------------------------------------------------------------

-- | A single field in a struct, union, or exception.
data ThriftField = ThriftField
  { tfFieldId      :: {-# UNPACK #-} !Int32
  , tfFieldName    :: !Text
  , tfFieldType    :: !ThriftType
  , tfRequiredness :: !Requiredness
  , tfDefault      :: !(Maybe ThriftConstValue)
  , tfAnnotations  :: !(Vector (Text, Text))
  } deriving stock (Show, Eq)

--------------------------------------------------------------------------------
-- Struct / Union / Exception
--------------------------------------------------------------------------------

-- | Struct kind tag.
data StructKind = StructNormal | StructUnion | StructException
  deriving stock (Show, Eq, Ord, Enum, Bounded)

-- | A Thrift struct, union, or exception.
data ThriftStruct = ThriftStruct
  { tsName        :: !Text
  , tsKind        :: !StructKind
  , tsFields      :: ![ThriftField]
  , tsAnnotations :: !(Vector (Text, Text))
  } deriving stock (Show, Eq)

--------------------------------------------------------------------------------
-- Enum
--------------------------------------------------------------------------------

-- | A Thrift enum definition.
data ThriftEnum = ThriftEnum
  { teName   :: !Text
  , teValues :: ![(Text, Int32)]
  } deriving stock (Show, Eq)

--------------------------------------------------------------------------------
-- Typedef
--------------------------------------------------------------------------------

-- | A Thrift typedef.
data ThriftTypedef = ThriftTypedef
  { ttName :: !Text
  , ttType :: !ThriftType
  } deriving stock (Show, Eq)

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

-- | Constant values in the Thrift IDL.
data ThriftConstValue
  = TCVInt    {-# UNPACK #-} !Int64
  | TCVDouble {-# UNPACK #-} !Double
  | TCVString !Text
  | TCVBool   !Bool
  | TCVList   ![ThriftConstValue]
  | TCVMap    ![(ThriftConstValue, ThriftConstValue)]
  | TCVIdent  !Text
  deriving stock (Show, Eq)

-- | A top-level constant definition.
data ThriftConst = ThriftConst
  { tcName  :: !Text
  , tcType  :: !ThriftType
  , tcValue :: !ThriftConstValue
  } deriving stock (Show, Eq)

--------------------------------------------------------------------------------
-- Service
--------------------------------------------------------------------------------

-- | A single method in a Thrift service.
data ThriftMethod = ThriftMethod
  { tmName       :: !Text
  , tmReturnType :: !(Maybe ThriftType)
  , tmParams     :: ![ThriftField]
  , tmThrows     :: ![ThriftField]
  , tmOneway     :: !Bool
  } deriving stock (Show, Eq)

-- | A Thrift service definition.
data ThriftService = ThriftService
  { tsvName    :: !Text
  , tsvExtends :: !(Maybe Text)
  , tsvMethods :: ![ThriftMethod]
  } deriving stock (Show, Eq)

--------------------------------------------------------------------------------
-- Top-level schema
--------------------------------------------------------------------------------

-- | A complete Thrift schema (one .thrift file).
data ThriftSchema = ThriftSchema
  { tsStructs   :: ![ThriftStruct]
  , tsEnums     :: ![ThriftEnum]
  , tsTypedefs  :: ![ThriftTypedef]
  , tsConsts    :: ![ThriftConst]
  , tsServices  :: ![ThriftService]
  } deriving stock (Show, Eq)
