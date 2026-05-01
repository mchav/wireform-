-- | Apache ORC file metadata types.
--
-- ORC (Optimized Row Columnar) uses Protocol Buffers for its metadata.
-- These types model the ORC file footer, stripe information, column types,
-- and column statistics as defined in the ORC specification.
module ORC.Types
  ( ORCFooter(..)
  , FooterEncryption(..)
  , StripeInformation(..)
  , ORCType(..)
  , TypeKind(..)
  , ColumnStatistics(..)
  , CompressionKind(..)
  , typeKindToInt
  , intToTypeKind
  , compressionFromInt
  ) where

import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Word (Word32, Word64)
import GHC.Generics (Generic)

data TypeKind
  = TKBoolean
  | TKByte
  | TKShort
  | TKInt
  | TKLong
  | TKFloat
  | TKDouble
  | TKString
  | TKBinary
  | TKTimestamp
  | TKList
  | TKMap
  | TKStruct
  | TKUnion
  | TKDecimal
  | TKDate
  | TKVarchar
  | TKChar
  deriving stock (Show, Eq, Enum, Bounded, Ord, Generic)
  deriving anyclass (NFData)

typeKindToInt :: TypeKind -> Int
typeKindToInt = \case
  TKBoolean   -> 0
  TKByte      -> 1
  TKShort     -> 2
  TKInt       -> 3
  TKLong      -> 4
  TKFloat     -> 5
  TKDouble    -> 6
  TKString    -> 7
  TKBinary    -> 8
  TKTimestamp -> 9
  TKList      -> 10
  TKMap       -> 11
  TKStruct    -> 12
  TKUnion     -> 13
  TKDecimal   -> 14
  TKDate      -> 15
  TKVarchar   -> 16
  TKChar      -> 17

intToTypeKind :: Int -> Maybe TypeKind
intToTypeKind = \case
  0  -> Just TKBoolean
  1  -> Just TKByte
  2  -> Just TKShort
  3  -> Just TKInt
  4  -> Just TKLong
  5  -> Just TKFloat
  6  -> Just TKDouble
  7  -> Just TKString
  8  -> Just TKBinary
  9  -> Just TKTimestamp
  10 -> Just TKList
  11 -> Just TKMap
  12 -> Just TKStruct
  13 -> Just TKUnion
  14 -> Just TKDecimal
  15 -> Just TKDate
  16 -> Just TKVarchar
  17 -> Just TKChar
  _  -> Nothing

data CompressionKind
  = CompressionNone
  | CompressionZlib
  | CompressionSnappy
  | CompressionLZO
  | CompressionLZ4
  | CompressionZstd
  deriving stock (Show, Eq, Enum, Bounded, Ord, Generic)
  deriving anyclass (NFData)

compressionFromInt :: Word64 -> Maybe CompressionKind
compressionFromInt = \case
  0 -> Just CompressionNone
  1 -> Just CompressionZlib
  2 -> Just CompressionSnappy
  3 -> Just CompressionLZO
  4 -> Just CompressionLZ4
  5 -> Just CompressionZstd
  _ -> Nothing

data StripeInformation = StripeInformation
  { siOffset       :: !Word64
  , siIndexLength  :: !Word64
  , siDataLength   :: !Word64
  , siFooterLength :: !Word64
  , siNumberOfRows :: !Word64
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)

data ORCType = ORCType
  { otKind       :: !TypeKind
  , otSubtypes   :: !(Vector Word32)
  , otFieldNames :: !(Vector Text)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)

data ColumnStatistics = ColumnStatistics
  { csNumberOfValues :: !(Maybe Word64)
  , csHasNull        :: !(Maybe Bool)
  , csBytesOnDisk    :: !(Maybe Word64)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)

data ORCFooter = ORCFooter
  { orcHeaderLength  :: !Word64
  , orcContentLength :: !Word64
  , orcStripes       :: !(Vector StripeInformation)
  , orcTypes         :: !(Vector ORCType)
  , orcMetadata      :: !(Vector (Text, ByteString))
  , orcNumberOfRows  :: !Word64
  , orcStatistics    :: !(Vector ColumnStatistics)
  , orcEncryption    :: !(Maybe FooterEncryption)
    -- ^ ORC 1.6+ column encryption metadata (protobuf
    -- @Footer.encryption@, field 10). Carried as a raw byte string so
    -- the footer codec can round-trip it without depending on
    -- "ORC.Encryption" — the higher-level 'ORC.Encryption.Encryption'
    -- record is encoded to / decoded from the same bytes at the
    -- integration layer.
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)

-- | Opaque container for the serialized @Footer.encryption@ bytes.
-- Keeping this as 'ByteString' rather than the parsed
-- 'ORC.Encryption.Encryption' record avoids a circular dependency
-- (the encryption record lives in "ORC.Encryption", which in turn
-- needs the protobuf encoders).
newtype FooterEncryption = FooterEncryption
  { feBytes :: ByteString
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)
