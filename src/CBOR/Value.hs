-- | CBOR (RFC 8949) runtime value representation.
module CBOR.Value
  ( Value(..)
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Word (Word8, Word64)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

-- | A dynamically-typed CBOR value. Each constructor corresponds to a
-- CBOR major type or simple value.
data Value
  = UInt       {-# UNPACK #-} !Word64
  | NInt       {-# UNPACK #-} !Word64       -- ^ negative: represents -1 - n
  | Bool       !Bool
  | Null
  | Undefined
  | Float16    {-# UNPACK #-} !Float        -- ^ half-precision (stored as Float)
  | Float32    {-# UNPACK #-} !Float
  | Float64    {-# UNPACK #-} !Double
  | ByteString !ByteString
  | TextString !Text
  | Array      !(Vector Value)
  | Map        !(Vector (Value, Value))
  | Tag        {-# UNPACK #-} !Word64 !Value
  | Simple     {-# UNPACK #-} !Word8
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)
