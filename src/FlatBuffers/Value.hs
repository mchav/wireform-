-- | FlatBuffers value representation.
--
-- FlatBuffers is Google's zero-copy flat serialization format. This module
-- defines a dynamically-typed value covering FlatBuffers' type system:
-- scalars (integers, floats, booleans), strings, vectors, tables (with
-- optional fields), structs (fixed-size inline), and unions.
--
-- @
-- import qualified FlatBuffers.Value as FB
-- import qualified FlatBuffers.Encode as FBE
-- import qualified FlatBuffers.Decode as FBD
-- import qualified Data.Vector as V
--
-- let val = FB.Table (V.fromList [Just (FB.FBString \"hello\"), Just (FB.Int32 42)])
-- let bytes = FBE.encode val
-- let Right decoded = FBD.decode bytes
-- @
module FlatBuffers.Value
  ( Value(..)
  ) where

import Control.DeepSeq (NFData)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.Generics (Generic)

data Value
  = VBool    !Bool
  | VInt8    {-# UNPACK #-} !Int8
  | VInt16   {-# UNPACK #-} !Int16
  | VInt32   {-# UNPACK #-} !Int32
  | VInt64   {-# UNPACK #-} !Int64
  | VWord8   {-# UNPACK #-} !Word8
  | VWord16  {-# UNPACK #-} !Word16
  | VWord32  {-# UNPACK #-} !Word32
  | VWord64  {-# UNPACK #-} !Word64
  | VFloat   {-# UNPACK #-} !Float
  | VDouble  {-# UNPACK #-} !Double
  | VString  !Text
  | VVector  !(Vector Value)
  | VTable   !(Vector (Maybe Value))
  | VStruct  !(Vector Value)
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)
