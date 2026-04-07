-- | Ion Schema Language (ISL) schema types.
--
-- Defines the abstract syntax tree for Ion Schema declarations,
-- including type definitions with constraints (fields, valid_values,
-- occurs), and schema imports.
module Ion.ISLSchema
  ( ISLSchema(..)
  , ISLType(..)
  , ISLField(..)
  , ISLConstraint(..)
  , Occurs(..)
  , ISLImport(..)
  ) where

import Data.Int (Int64)
import Data.Text (Text)
import Data.Vector (Vector)

data ISLSchema = ISLSchema
  { islTypes   :: !(Vector ISLType)
  , islImports :: !(Vector ISLImport)
  } deriving stock (Show, Eq)

data ISLType = ISLType
  { islTypeName    :: !Text
  , islBaseType    :: !(Maybe Text)
  , islFields      :: !(Maybe (Vector ISLField))
  , islValidValues :: !(Maybe ISLConstraint)
  , islOccurs      :: !(Maybe Occurs)
  } deriving stock (Show, Eq)

data ISLField = ISLField !Text !ISLType
  deriving stock (Show, Eq)

data ISLConstraint
  = RangeVal !(Maybe Int64) !(Maybe Int64)
  | EnumVal !(Vector Text)
  deriving stock (Show, Eq)

data Occurs
  = ORequired
  | OOptional
  | ORange !Int !Int
  deriving stock (Show, Eq)

data ISLImport = ISLImport !Text !(Maybe Text)
  deriving stock (Show, Eq)
