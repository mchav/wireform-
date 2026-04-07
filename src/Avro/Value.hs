-- | Avro runtime value representation.
--
-- A generic, schema-agnostic value type that can represent any Avro datum.
-- Used by 'Avro.Encode' and 'Avro.Decode' for schema-driven serialisation.
module Avro.Value
  ( Value(..)
  ) where

import Data.ByteString (ByteString)
import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

-- | A dynamically-typed Avro value.  Each constructor corresponds to
-- exactly one Avro schema type.
data Value
  = Null
  | Bool   !Bool
  | Int    {-# UNPACK #-} !Int32
  | Long   {-# UNPACK #-} !Int64
  | Float  {-# UNPACK #-} !Float
  | Double {-# UNPACK #-} !Double
  | Bytes  !ByteString
  | String !Text
  | Record !(Vector Value)
  | Enum   {-# UNPACK #-} !Int
  | Array  !(Vector Value)
  | Map    !(Vector (Text, Value))
  | Union  {-# UNPACK #-} !Int !Value
  | Fixed  !ByteString
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)
