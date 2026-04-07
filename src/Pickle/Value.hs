-- | Python Pickle value representation (protocol 2).
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
