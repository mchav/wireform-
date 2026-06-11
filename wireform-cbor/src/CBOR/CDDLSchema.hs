{- | CDDL (RFC 8610) schema types.

Defines the abstract syntax tree for CDDL schemas, which describe
CBOR data structures. Supports maps, arrays, choices, tagged types,
occurrence indicators, and built-in CBOR types.
-}
module CBOR.CDDLSchema (
  CDDLSchema (..),
  CDDLRule (..),
  CDDLType (..),
  CDDLMember (..),
  Occurrence (..),
) where

import Data.Text (Text)
import Data.Vector (Vector)
import Data.Word (Word64)


data CDDLSchema = CDDLSchema !(Vector CDDLRule)
  deriving stock (Show, Eq)


data CDDLRule = CDDLRule !Text !CDDLType
  deriving stock (Show, Eq)


data CDDLType
  = CTUint
  | CTNint
  | CTInt
  | CTTstr
  | CTBstr
  | CTFloat
  | CTBool
  | CTNil
  | CTAny
  | CTMap !(Vector CDDLMember)
  | CTArray !(Vector CDDLMember)
  | CTChoice !(Vector CDDLType)
  | CTRef !Text
  | CTTagged !Word64 !CDDLType
  | CTLiteral !Text
  deriving stock (Show, Eq)


data CDDLMember = CDDLMember !Text !CDDLType !Occurrence
  deriving stock (Show, Eq)


data Occurrence
  = Once
  | Optional
  | ZeroOrMore
  | OneOrMore
  deriving stock (Show, Eq, Ord, Enum, Bounded)
