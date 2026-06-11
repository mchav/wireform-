{- | Microsoft Bond schema types.

Defines the abstract syntax tree for Bond IDL schemas, including
struct declarations, field definitions with modifiers, enums,
and Bond's type system (primitives, containers, nullable, bonded).
-}
module Bond.Schema (
  BondSchema (..),
  BondDecl (..),
  BondStruct (..),
  BondField (..),
  BondEnum (..),
  BondEnumValue (..),
  BondFieldModifier (..),
  BondFieldType (..),
) where

import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Vector (Vector)


data BondFieldModifier
  = BondRequired
  | BondOptional
  | BondRequiredOptional
  deriving stock (Show, Eq, Ord, Enum, Bounded)


data BondFieldType
  = BFTBool
  | BFTInt8
  | BFTInt16
  | BFTInt32
  | BFTInt64
  | BFTUInt8
  | BFTUInt16
  | BFTUInt32
  | BFTUInt64
  | BFTFloat
  | BFTDouble
  | BFTString
  | BFTWString
  | BFTBlob
  | BFTNamed !Text
  | BFTList !BondFieldType
  | BFTSet !BondFieldType
  | BFTMap !BondFieldType !BondFieldType
  | BFTNullable !BondFieldType
  deriving stock (Show, Eq)


data BondField = BondField
  { bfFieldId :: {-# UNPACK #-} !Int32
  , bfModifier :: !BondFieldModifier
  , bfType :: !BondFieldType
  , bfName :: !Text
  , bfDefault :: !(Maybe Text)
  , bfAttributes :: !(Vector (Text, Maybe Text))
  }
  deriving stock (Show, Eq)


data BondStruct = BondStruct
  { bsName :: !Text
  , bsTypeParam :: !(Maybe Text)
  , bsFields :: ![BondField]
  , bsAttributes :: !(Vector (Text, Maybe Text))
  }
  deriving stock (Show, Eq)


data BondEnumValue = BondEnumValue
  { bevName :: !Text
  , bevValue :: !(Maybe Int32)
  }
  deriving stock (Show, Eq)


data BondEnum = BondEnum
  { beName :: !Text
  , beValues :: ![BondEnumValue]
  }
  deriving stock (Show, Eq)


data BondDecl
  = BondDeclStruct !BondStruct
  | BondDeclEnum !BondEnum
  deriving stock (Show, Eq)


data BondSchema = BondSchema
  { bondNamespace :: !(Maybe Text)
  , bondImports :: ![Text]
  , bondDecls :: ![BondDecl]
  }
  deriving stock (Show, Eq)
