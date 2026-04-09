-- | ASN.1 Module Definition Language schema types.
--
-- Defines the abstract syntax tree for ASN.1 module definitions,
-- including SEQUENCE, CHOICE, ENUMERATED, and basic ASN.1 types
-- with constraint support (range, size).
module ASN1.Schema
  ( ASN1Module(..)
  , TagMode(..)
  , TypeAssignment(..)
  , ASN1TypeDef(..)
  , ComponentType(..)
  , Constraint(..)
  ) where

import Data.Int (Int64)
import Data.Text (Text)
import Data.Vector (Vector)

data ASN1Module = ASN1Module
  { asnModuleName   :: !Text
  , asnTagMode      :: !TagMode
  , asnAssignments  :: !(Vector TypeAssignment)
  } deriving stock (Show, Eq)

data TagMode
  = AutomaticTags
  | ImplicitTags
  | ExplicitTags
  | DefaultTags
  deriving stock (Show, Eq, Ord, Enum, Bounded)

data TypeAssignment = TypeAssignment !Text !ASN1TypeDef
  deriving stock (Show, Eq)

data ASN1TypeDef
  = TDSequence !(Vector ComponentType)
  | TDChoice !(Vector ComponentType)
  | TDEnumerated !(Vector (Text, Maybe Int))
  | TDInteger !(Maybe Constraint)
  | TDBitString
  | TDOctetString !(Maybe Constraint)
  | TDBoolean
  | TDNULL
  | TDUTF8String
  | TDPrintableString
  | TDIA5String
  | TDVisibleString
  | TDSequenceOf !ASN1TypeDef
  | TDSetOf !ASN1TypeDef
  | TDNamedType !Text
  | TDOptional !ASN1TypeDef
  | TDDefault !ASN1TypeDef !Text
  deriving stock (Show, Eq)

data ComponentType = ComponentType !Text !ASN1TypeDef !Bool
  deriving stock (Show, Eq)

data Constraint
  = RangeConstraint !(Maybe Int64) !(Maybe Int64)
  | SizeConstraint !(Maybe Int64) !(Maybe Int64)
  deriving stock (Show, Eq)
