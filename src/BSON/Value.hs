-- | BSON (Binary JSON) value representation.
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
