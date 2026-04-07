-- | Microsoft Bond value representation.
--
-- A dynamically-typed value type for the Bond serialization framework.
-- Supports all Bond primitive types, containers, structs, and nullable.
module Bond.Value
  ( Value(..)
  , BondType(..)
  , bondTypeId
  ) where

import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.Generics (Generic)

data Value
  = Bool     !Bool
  | Int8     {-# UNPACK #-} !Int8
  | Int16    {-# UNPACK #-} !Int16
  | Int32    {-# UNPACK #-} !Int32
  | Int64    {-# UNPACK #-} !Int64
  | UInt8    {-# UNPACK #-} !Word8
  | UInt16   {-# UNPACK #-} !Word16
  | UInt32   {-# UNPACK #-} !Word32
  | UInt64   {-# UNPACK #-} !Word64
  | Float    {-# UNPACK #-} !Float
  | Double   {-# UNPACK #-} !Double
  | String   !Text
  | WString  !Text
  | Blob     !ByteString
  | List     !BondType !(Vector Value)
  | Set      !BondType !(Vector Value)
  | Map      !BondType !BondType !(Vector (Value, Value))
  | Struct   !(Vector Value) !(Vector (Word16, BondType, Value))
  | Nullable !(Maybe Value)
  | Enum     {-# UNPACK #-} !Int32
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

data BondType
  = BT_BOOL
  | BT_INT8
  | BT_INT16
  | BT_INT32
  | BT_INT64
  | BT_UINT8
  | BT_UINT16
  | BT_UINT32
  | BT_UINT64
  | BT_FLOAT
  | BT_DOUBLE
  | BT_STRING
  | BT_WSTRING
  | BT_LIST
  | BT_SET
  | BT_MAP
  | BT_STRUCT
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (NFData)

-- | Wire type ID for Bond Compact Binary protocol.
bondTypeId :: BondType -> Word8
bondTypeId BT_BOOL   = 2
bondTypeId BT_INT8   = 3
bondTypeId BT_INT16  = 4
bondTypeId BT_INT32  = 5
bondTypeId BT_INT64  = 6
bondTypeId BT_UINT8  = 7
bondTypeId BT_UINT16 = 8
bondTypeId BT_UINT32 = 9
bondTypeId BT_UINT64 = 10
bondTypeId BT_FLOAT  = 11
bondTypeId BT_DOUBLE = 12
bondTypeId BT_STRING = 13
bondTypeId BT_WSTRING = 14
bondTypeId BT_LIST   = 15
bondTypeId BT_SET    = 16
bondTypeId BT_MAP    = 17
bondTypeId BT_STRUCT = 18
