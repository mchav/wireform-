-- | Cap'n Proto value representation.
module CapnProto.Value
  ( Value(..)
  ) where

import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.Generics (Generic)

data Value
  = Void
  | Bool       !Bool
  | Int8       {-# UNPACK #-} !Int8
  | Int16      {-# UNPACK #-} !Int16
  | Int32      {-# UNPACK #-} !Int32
  | Int64      {-# UNPACK #-} !Int64
  | UInt8      {-# UNPACK #-} !Word8
  | UInt16     {-# UNPACK #-} !Word16
  | UInt32     {-# UNPACK #-} !Word32
  | UInt64     {-# UNPACK #-} !Word64
  | Float32    {-# UNPACK #-} !Float
  | Float64    {-# UNPACK #-} !Double
  | Text       !Text
  | Data       !ByteString
  | Struct     !(Vector Value) !(Vector Value)
  | List       !(Vector Value)
  | Enum       {-# UNPACK #-} !Word16
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)
