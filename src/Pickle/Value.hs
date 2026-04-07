-- | Python Pickle value representation (protocol 2).
--
-- Python's pickle format serializes arbitrary Python objects. This module
-- defines a Haskell value type covering the common pickle data model:
-- None, booleans, integers, floats, strings (bytes and unicode), lists,
-- tuples, dicts, sets, and global references.
--
-- @
-- import qualified Pickle.Value as P
-- import qualified Pickle.Encode as PE
-- import qualified Pickle.Decode as PD
-- import qualified Data.Vector as V
--
-- let val = P.Dict (V.fromList [(P.Unicode \"key\", P.Int 42)])
-- let bytes = PE.encode val
-- let Right decoded = PD.decode bytes
-- @
module Pickle.Value
  ( Value(..)
  ) where

import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics (Generic)

data Value
  = None
  | Bool !Bool
  | Int !Int64
  | Float !Double
  | Bytes !ByteString
  | String !Text
  | List !(Vector Value)
  | Tuple !(Vector Value)
  | Dict !(Vector (Value, Value))
  | Set !(Vector Value)
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)
