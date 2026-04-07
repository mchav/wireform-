-- | Amazon Ion binary value representation.
--
-- Amazon Ion is a richly-typed, self-describing data format used by AWS
-- services. This module defines a dynamically-typed value covering Ion's
-- core types: null, bool, int, float, string, blob, clob, list, struct,
-- symbol, and annotation.
--
-- @
-- import qualified Ion.Value as I
-- import qualified Ion.Encode as IE
-- import qualified Ion.Decode as ID
-- import qualified Data.Vector as V
--
-- let val = I.Struct (V.fromList [(\"name\", I.String \"Alice\"), (\"age\", I.Int 30)])
-- let bytes = IE.encode val
-- let Right decoded = ID.decode bytes
-- @
module Ion.Value
  ( Value(..)
  ) where

import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics (Generic)

data Value
  = Null
  | Bool       !Bool
  | Int        {-# UNPACK #-} !Int64
  | Float      {-# UNPACK #-} !Double
  | String     !Text
  | Blob       !ByteString
  | Clob       !ByteString
  | List       !(Vector Value)
  | Struct     !(Vector (Text, Value))
  | Symbol     !Text
  | Annotation !Text !Value
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)
