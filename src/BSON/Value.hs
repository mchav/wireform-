-- | BSON (Binary JSON) value representation.
--
-- BSON is the binary serialization format used by MongoDB. This module
-- defines a dynamically-typed value covering all BSON element types:
-- doubles, strings, documents (ordered key-value maps), arrays, binary
-- data, booleans, datetimes, null, 32-bit and 64-bit integers, ObjectIds,
-- and regular expressions.
--
-- @
-- import qualified BSON.Value as B
-- import qualified BSON.Encode as BE
-- import qualified BSON.Decode as BD
-- import qualified Data.Vector as V
--
-- let doc = B.Document (V.fromList [(\"name\", B.String \"Alice\"), (\"age\", B.Int32 30)])
-- let bytes = BE.encode doc
-- let Right decoded = BD.decode bytes
-- @
module BSON.Value
  ( Value(..)
  ) where

import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics (Generic)

data Value
  = Double   {-# UNPACK #-} !Double
  | String   !Text
  | Document !(Vector (Text, Value))
  | Array    !(Vector Value)
  | Binary   !ByteString
  | Bool     !Bool
  | DateTime {-# UNPACK #-} !Int64
  | Null
  | Int32    {-# UNPACK #-} !Int32
  | Int64    {-# UNPACK #-} !Int64
  | ObjectId !ByteString
  | Regex    !Text !Text
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)
