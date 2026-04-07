-- | Amazon Ion binary value representation.
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
