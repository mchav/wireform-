-- | Shared Value type for high-level Thrift encode\/decode.
--
-- Each variant carries its wire data directly. Struct fields are
-- tagged by their Int16 field ID so both protocols can encode them
-- with the correct field headers. Covers bool, byte, i16, i32, i64,
-- double, string, binary, list, set, map, and struct.
--
-- @
-- import qualified Thrift.Value as T
-- import qualified Data.Vector as V
--
-- let person = T.Struct (V.fromList [(1, T.String \"Alice\"), (2, T.I32 30)])
-- @
module Thrift.Value
  ( Value (..)
  , thriftTypeOf
  ) where

import Data.ByteString (ByteString)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Text (Text)
import Data.Vector (Vector)
import Thrift.Wire (ThriftType (..))

data Value
  = Bool   Bool
  | Byte   Int8
  | I16    Int16
  | I32    Int32
  | I64    Int64
  | Double Double
  | String Text
  | Binary ByteString
  | Struct (Vector (Int16, Value))
  | Map    ThriftType ThriftType (Vector (Value, Value))
  | List   ThriftType (Vector Value)
  | Set    ThriftType (Vector Value)
  | UUID   ByteString
  deriving stock (Show, Eq)

thriftTypeOf :: Value -> ThriftType
thriftTypeOf = \case
  Bool{}   -> TT_BOOL
  Byte{}   -> TT_BYTE
  I16{}    -> TT_I16
  I32{}    -> TT_I32
  I64{}    -> TT_I64
  Double{} -> TT_DOUBLE
  String{} -> TT_STRING
  Binary{} -> TT_STRING
  Struct{} -> TT_STRUCT
  Map{}    -> TT_MAP
  List{}   -> TT_LIST
  Set{}    -> TT_SET
  UUID{}   -> TT_UUID
