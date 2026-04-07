-- | Shared ThriftValue type for high-level Thrift encode/decode.
--
-- Each variant carries its wire data directly. Struct fields are
-- tagged by their Int16 field ID so both protocols can encode them
-- with the correct field headers.
module Thrift.Value
  ( ThriftValue (..)
  , thriftTypeOf
  ) where

import Data.ByteString (ByteString)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Text (Text)
import Thrift.Wire (ThriftType (..))

data ThriftValue
  = TVBool   Bool
  | TVByte   Int8
  | TVI16    Int16
  | TVI32    Int32
  | TVI64    Int64
  | TVDouble Double
  | TVString Text
  | TVBinary ByteString
  | TVStruct [(Int16, ThriftValue)]
  | TVMap    ThriftType ThriftType [(ThriftValue, ThriftValue)]
  | TVList   ThriftType [ThriftValue]
  | TVSet    ThriftType [ThriftValue]
  | TVUUID   ByteString
  deriving stock (Show, Eq)

thriftTypeOf :: ThriftValue -> ThriftType
thriftTypeOf = \case
  TVBool{}   -> TT_BOOL
  TVByte{}   -> TT_BYTE
  TVI16{}    -> TT_I16
  TVI32{}    -> TT_I32
  TVI64{}    -> TT_I64
  TVDouble{} -> TT_DOUBLE
  TVString{} -> TT_STRING
  TVBinary{} -> TT_STRING
  TVStruct{} -> TT_STRUCT
  TVMap{}    -> TT_MAP
  TVList{}   -> TT_LIST
  TVSet{}    -> TT_SET
  TVUUID{}   -> TT_UUID
