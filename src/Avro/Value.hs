-- | Avro runtime value representation.
--
-- A generic, schema-agnostic value type that can represent any Avro datum.
-- Used by 'Avro.Encode' and 'Avro.Decode' for schema-driven serialisation.
module Avro.Value
  ( AvroValue(..)
  ) where

import Data.ByteString (ByteString)
import Data.Int (Int32, Int64)
import Data.Text (Text)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

-- | A dynamically-typed Avro value.  Each constructor corresponds to
-- exactly one Avro schema type.
data AvroValue
  = AvNull
  | AvBool   !Bool
  | AvInt    {-# UNPACK #-} !Int32
  | AvLong   {-# UNPACK #-} !Int64
  | AvFloat  {-# UNPACK #-} !Float
  | AvDouble {-# UNPACK #-} !Double
  | AvBytes  !ByteString
  | AvString !Text
  | AvRecord ![AvroValue]
  | AvEnum   {-# UNPACK #-} !Int
  | AvArray  ![AvroValue]
  | AvMap    ![(Text, AvroValue)]
  | AvUnion  {-# UNPACK #-} !Int !AvroValue
  | AvFixed  !ByteString
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)
