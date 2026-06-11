{- | MessagePack value representation.

MessagePack is a compact binary serialization format. This module
provides an Aeson-style dynamically-typed value that can represent any
MessagePack datum: nil, booleans, integers (signed\/unsigned), floats,
doubles, strings, binary data, arrays, maps, ext types, and timestamps.

@
import qualified MsgPack.Value as MP
import qualified MsgPack.Encode as MPE
import qualified MsgPack.Decode as MPD
import qualified Data.Vector as V

let val = MP.Map (V.fromList [(MP.String \"key\", MP.Int 42)])
let bytes = MPE.encode val
let Right decoded = MPD.decode bytes
@
-}
module MsgPack.Value (
  Value (..),
) where

import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import Data.Int (Int64, Int8)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Word (Word32, Word64)
import GHC.Generics (Generic)


data Value
  = Nil
  | Bool !Bool
  | Int {-# UNPACK #-} !Int64
  | Word {-# UNPACK #-} !Word64
  | Float {-# UNPACK #-} !Float
  | Double {-# UNPACK #-} !Double
  | String !Text
  | Binary !ByteString
  | Array !(Vector Value)
  | Map !(Vector (Value, Value))
  | Ext {-# UNPACK #-} !Int8 !ByteString
  | Timestamp {-# UNPACK #-} !Int64 {-# UNPACK #-} !Word32
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)
