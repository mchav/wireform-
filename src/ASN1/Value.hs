-- | ASN.1 value representation, covering the common universal types.
module ASN1.Value
  ( Value(..)
  , TagClass(..)
  ) where

import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Word (Word64)
import GHC.Generics (Generic)

data TagClass = Universal | Application | ContextSpecific | Private
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)

data Value
  = Boolean !Bool
  | Integer !Integer
  | BitString !Int !ByteString
  | OctetString !ByteString
  | Null
  | OID !(Vector Word64)
  | UTF8String !Text
  | PrintableString !Text
  | IA5String !Text
  | UTCTime !Text
  | GeneralizedTime !Text
  | Sequence !(Vector Value)
  | Set !(Vector Value)
  | Tagged !TagClass !Int !Value
  | Other !TagClass !Bool !Int !ByteString
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)
