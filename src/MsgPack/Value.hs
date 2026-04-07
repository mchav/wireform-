-- | MessagePack value representation.
--
-- An Aeson-style dynamically-typed value that can represent any MessagePack
-- datum. Used by 'MsgPack.Encode' and 'MsgPack.Decode' for binary
-- serialisation.
module MsgPack.Value
  ( Value(..)
  ) where

import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import Data.Int (Int8, Int64)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Word (Word32, Word64)
import GHC.Generics (Generic)

data Value
  = Nil
  | Bool   !Bool
  | Int    {-# UNPACK #-} !Int64
  | Word   {-# UNPACK #-} !Word64
  | Float  {-# UNPACK #-} !Float
  | Double {-# UNPACK #-} !Double
  | String !Text
  | Binary !ByteString
  | Array  !(Vector Value)
  | Map    !(Vector (Value, Value))
  | Ext    {-# UNPACK #-} !Int8 !ByteString
  | Timestamp {-# UNPACK #-} !Int64 {-# UNPACK #-} !Word32
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)
