{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

{-|
Module      : Kafka.Protocol.RecordBatch
Description : Kafka RecordBatch v2 format encoding and decoding
Copyright   : (c) 2025
License     : BSD-3-Clause
Maintainer  : kafka-native

This module implements the Kafka RecordBatch v2 format (magic byte 2), which is
the current standard format for storing and transmitting records in Kafka.

= RecordBatch Format

The RecordBatch v2 format has the following structure:

@
RecordBatch =>
  BaseOffset => Int64
  Length => Int32
  PartitionLeaderEpoch => Int32
  Magic => Int8 (value = 2)
  CRC => Uint32
  Attributes => Int16
  LastOffsetDelta => Int32
  BaseTimestamp => Int64
  MaxTimestamp => Int64
  ProducerId => Int64
  ProducerEpoch => Int16
  BaseSequence => Int32
  RecordsCount => Int32
  Records => [Record]
@

The CRC covers the data from the attributes to the end of the batch (i.e., all
bytes following the CRC). It uses the CRC-32C (Castagnoli) polynomial.

= Attributes Field

The attributes field (16 bits) contains:

* Bits 0-2: Compression type (0=none, 1=gzip, 2=snappy, 3=lz4, 4=zstd)
* Bit 3: Timestamp type (0=CreateTime, 1=LogAppendTime)
* Bit 4: Transactional flag (0=non-transactional, 1=transactional)
* Bit 5: Control flag (0=normal, 1=control)
* Bit 6: Delete horizon flag (0=no, 1=yes)
* Bits 7-15: Unused (must be 0)

= Records

Individual records within a batch use a different format from the batch header.
See 'Record' for details.

-}
module Kafka.Protocol.RecordBatch
  ( -- * RecordBatch Types
    RecordBatch(..)
  , Record(..)
  , RecordHeader(..)
    -- * Batch Construction
  , mkRecordBatch
  , mkSimpleBatch
    -- * Batch Attributes
  , TimestampType(..)
  , Attributes(..)
  , mkAttributes
  , defaultAttributes
  , encodeAttributes
  , decodeAttributes
    -- * Encoding/Decoding
  , encodeRecordBatch
  , decodeRecordBatch
  , encodeRecordBatchWithCompression
  , encodeRecordBatchWithCompressionLevel
  , decodeRecordBatchWithDecompression
  , encodeRecord
  , decodeRecord
    -- * Constants
  , magicV2
  , noProducerId
  , noProducerEpoch
  , noSequence
  , noPartitionLeaderEpoch
  , noTimestamp
    -- * Utilities
  , calculateBatchSize
  , recordBatchOverhead
  ) where

import Control.Monad (replicateM)
import Data.Bits ((.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Bytes.Get (MonadGet, getByteString, runGetS)
import Data.Bytes.Put (MonadPut, putByteString, runPutS)
import Data.Bytes.Serial
import Data.Int
import Data.Word
import GHC.Generics (Generic)
import qualified Data.Vector as V

import qualified Kafka.Compression.Types as Compression
import qualified Kafka.Protocol.CRC32C as CRC
import qualified Kafka.Protocol.Primitives as P

-- | Magic byte value for RecordBatch v2 format.
magicV2 :: Int8
magicV2 = 2

-- | Value indicating no producer ID is set.
noProducerId :: Int64
noProducerId = -1

-- | Value indicating no producer epoch is set.
noProducerEpoch :: Int16
noProducerEpoch = -1

-- | Value indicating no sequence number is set.
noSequence :: Int32
noSequence = -1

-- | Value indicating no partition leader epoch is set.
noPartitionLeaderEpoch :: Int32
noPartitionLeaderEpoch = -1

-- | Value indicating no timestamp is set.
noTimestamp :: Int64
noTimestamp = -1

-- | Fixed overhead of the RecordBatch header (61 bytes).
recordBatchOverhead :: Int
recordBatchOverhead = 61

-- -----------------------------------------------------------------------------
-- Timestamp Type
-- -----------------------------------------------------------------------------

-- | Timestamp type for records.
data TimestampType
  = CreateTime     -- ^ Timestamp set by the producer
  | LogAppendTime  -- ^ Timestamp set by the broker when appended
  deriving (Eq, Show, Ord, Generic)

-- -----------------------------------------------------------------------------
-- Attributes
-- -----------------------------------------------------------------------------

-- | Attributes field of a RecordBatch.
data Attributes = Attributes
  { attrCompressionType :: !Compression.CompressionCodec
    -- ^ Compression codec used for the records
  , attrTimestampType :: !TimestampType
    -- ^ How timestamps are assigned
  , attrIsTransactional :: !Bool
    -- ^ Whether this batch is part of a transaction
  , attrIsControl :: !Bool
    -- ^ Whether this is a control batch
  , attrHasDeleteHorizon :: !Bool
    -- ^ Whether the delete horizon flag is set
  } deriving (Eq, Show, Generic)

-- | Create attributes with the specified values.
mkAttributes
  :: Compression.CompressionCodec
  -> TimestampType
  -> Bool  -- ^ Transactional
  -> Bool  -- ^ Control
  -> Bool  -- ^ Delete horizon
  -> Attributes
mkAttributes = Attributes

-- | Default attributes: no compression, create time, non-transactional, non-control.
defaultAttributes :: Attributes
defaultAttributes = Attributes
  { attrCompressionType = Compression.NoCompression
  , attrTimestampType = CreateTime
  , attrIsTransactional = False
  , attrIsControl = False
  , attrHasDeleteHorizon = False
  }

-- | Encode attributes to 16-bit integer.
encodeAttributes :: Attributes -> Int16
encodeAttributes Attributes{..} =
  let compressionBits = fromIntegral (Compression.codecId attrCompressionType) .&. 0x07
      timestampBit = if attrTimestampType == LogAppendTime then 0x08 else 0x00
      transactionalBit = if attrIsTransactional then 0x10 else 0x00
      controlBit = if attrIsControl then 0x20 else 0x00
      deleteHorizonBit = if attrHasDeleteHorizon then 0x40 else 0x00
  in compressionBits .|. timestampBit .|. transactionalBit .|. controlBit .|. deleteHorizonBit

-- | Decode attributes from 16-bit integer.
decodeAttributes :: Int16 -> Either String Attributes
decodeAttributes attrs =
  let compressionId = fromIntegral (attrs .&. 0x07) :: Int8
      timestampType = if (attrs .&. 0x08) /= 0 then LogAppendTime else CreateTime
      isTransactional = (attrs .&. 0x10) /= 0
      isControl = (attrs .&. 0x20) /= 0
      hasDeleteHorizon = (attrs .&. 0x40) /= 0
      compressionCodec = case compressionId of
        0 -> Compression.NoCompression
        1 -> Compression.Gzip
        2 -> Compression.Snappy
        3 -> Compression.Lz4
        4 -> Compression.Zstd
        _ -> Compression.NoCompression  -- Unknown codec, default to none
  in Right $ Attributes compressionCodec timestampType isTransactional isControl hasDeleteHorizon

-- -----------------------------------------------------------------------------
-- Record Header
-- -----------------------------------------------------------------------------

-- | A header key-value pair within a record.
data RecordHeader = RecordHeader
  { headerKey :: !ByteString
    -- ^ Header key
  , headerValue :: !(Maybe ByteString)
    -- ^ Header value (nullable)
  } deriving (Eq, Show, Generic)

-- -----------------------------------------------------------------------------
-- Record
-- -----------------------------------------------------------------------------

-- | An individual record within a RecordBatch.
-- 
-- Records are encoded with variable-length integers using ZigZag encoding:
--
-- @
-- Record =>
--   Length => VarInt
--   Attributes => Int8
--   TimestampDelta => VarLong
--   OffsetDelta => VarInt
--   KeyLength => VarInt (-1 for null)
--   Key => Bytes
--   ValueLength => VarInt (-1 for null)
--   Value => Bytes
--   HeadersCount => VarInt
--   Headers => [RecordHeader]
-- @
data Record = Record
  { recordTimestampDelta :: !Int64
    -- ^ Timestamp delta from batch base timestamp
  , recordOffsetDelta :: !Int32
    -- ^ Offset delta from batch base offset
  , recordKey :: !(Maybe ByteString)
    -- ^ Record key (nullable)
  , recordValue :: !ByteString
    -- ^ Record value
  , recordHeaders :: ![RecordHeader]
    -- ^ Record headers
  } deriving (Eq, Show, Generic)

-- -----------------------------------------------------------------------------
-- RecordBatch
-- -----------------------------------------------------------------------------

-- | A RecordBatch in v2 format (magic byte 2).
data RecordBatch = RecordBatch
  { batchBaseOffset :: !Int64
    -- ^ Base offset of this batch
  , batchPartitionLeaderEpoch :: !Int32
    -- ^ Partition leader epoch (set by broker, use 'noPartitionLeaderEpoch' for producer)
  , batchAttributes :: !Attributes
    -- ^ Batch attributes
  , batchLastOffsetDelta :: !Int32
    -- ^ Offset delta of the last record in the batch
  , batchBaseTimestamp :: !Int64
    -- ^ Base timestamp for the batch
  , batchMaxTimestamp :: !Int64
    -- ^ Maximum timestamp in the batch
  , batchProducerId :: !Int64
    -- ^ Producer ID (for idempotent/transactional producers)
  , batchProducerEpoch :: !Int16
    -- ^ Producer epoch
  , batchBaseSequence :: !Int32
    -- ^ Base sequence number
  , batchRecords :: !(V.Vector Record)
    -- ^ Records in this batch
  } deriving (Eq, Show, Generic)

-- | Create a RecordBatch with specified parameters.
mkRecordBatch
  :: Int64                -- ^ Base offset
  -> Int32                -- ^ Partition leader epoch
  -> Attributes           -- ^ Attributes
  -> Int64                -- ^ Base timestamp
  -> Int64                -- ^ Producer ID
  -> Int16                -- ^ Producer epoch
  -> Int32                -- ^ Base sequence
  -> V.Vector Record      -- ^ Records
  -> RecordBatch
mkRecordBatch baseOffset leaderEpoch attrs baseTimestamp producerId producerEpoch baseSequence records =
  let lastOffsetDelta = if V.null records then 0 else fromIntegral (V.length records) - 1
      maxTimestamp = if V.null records
                     then baseTimestamp
                     else maximum (baseTimestamp : V.toList (V.map (\r -> baseTimestamp + recordTimestampDelta r) records))
  in RecordBatch
    { batchBaseOffset = baseOffset
    , batchPartitionLeaderEpoch = leaderEpoch
    , batchAttributes = attrs
    , batchLastOffsetDelta = lastOffsetDelta
    , batchBaseTimestamp = baseTimestamp
    , batchMaxTimestamp = maxTimestamp
    , batchProducerId = producerId
    , batchProducerEpoch = producerEpoch
    , batchBaseSequence = baseSequence
    , batchRecords = records
    }

-- | Create a simple RecordBatch with default values for non-idempotent producer.
mkSimpleBatch
  :: Int64            -- ^ Base offset
  -> Int64            -- ^ Base timestamp
  -> V.Vector Record  -- ^ Records
  -> RecordBatch
mkSimpleBatch baseOffset baseTimestamp records =
  mkRecordBatch
    baseOffset
    noPartitionLeaderEpoch
    defaultAttributes
    baseTimestamp
    noProducerId
    noProducerEpoch
    noSequence
    records

-- -----------------------------------------------------------------------------
-- CRC32C Checksum
-- -----------------------------------------------------------------------------

-- | Calculate CRC32C checksum using Castagnoli polynomial.
-- 
-- This uses a fast C implementation with hardware acceleration (SSE4.2/AVX512)
-- when available, falling back to a software lookup table on unsupported
-- architectures.
--
-- The CRC32C polynomial is 0x1EDC6F41 (Castagnoli).
crc32c :: ByteString -> Word32
crc32c = CRC.crc32c

-- -----------------------------------------------------------------------------
-- Record Encoding/Decoding
-- -----------------------------------------------------------------------------

-- | Encode a record header.
encodeRecordHeader :: MonadPut m => RecordHeader -> m ()
encodeRecordHeader RecordHeader{..} = do
  -- Key length and key
  serialize (P.VarInt $ fromIntegral $ BS.length headerKey)
  putByteString headerKey
  -- Value length and value
  case headerValue of
    Nothing -> serialize (P.VarInt (-1))
    Just v -> do
      serialize (P.VarInt $ fromIntegral $ BS.length v)
      putByteString v

-- | Decode a record header.
decodeRecordHeader :: MonadGet m => m RecordHeader
decodeRecordHeader = do
  -- Key length and key
  P.VarInt keyLen <- deserialize
  headerKey <- getByteString (fromIntegral keyLen)
  -- Value length and value
  P.VarInt valueLen <- deserialize
  headerValue <- if valueLen < 0
                 then return Nothing
                 else Just <$> getByteString (fromIntegral valueLen)
  return RecordHeader{..}

-- | Encode a single record.
encodeRecord :: MonadPut m => Record -> m ()
encodeRecord Record{..} = do
  -- First, encode the record body to calculate the length
  let bodyBytes = runPutS $ do
        -- Attributes (unused for now, always 0)
        serialize (0 :: Int8)
        -- Timestamp delta
        serialize (P.VarLong recordTimestampDelta)
        -- Offset delta
        serialize (P.VarInt recordOffsetDelta)
        -- Key
        case recordKey of
          Nothing -> serialize (P.VarInt (-1))
          Just k -> do
            serialize (P.VarInt $ fromIntegral $ BS.length k)
            putByteString k
        -- Value
        serialize (P.VarInt $ fromIntegral $ BS.length recordValue)
        putByteString recordValue
        -- Headers
        serialize (P.VarInt $ fromIntegral $ length recordHeaders)
        mapM_ encodeRecordHeader recordHeaders
  
  -- Length of the record
  serialize (P.VarInt $ fromIntegral $ BS.length bodyBytes)
  -- Record body
  putByteString bodyBytes

-- | Decode a single record.
decodeRecord :: MonadGet m => m Record
decodeRecord = do
  -- Length (we don't use it for decoding, but it's in the format)
  P.VarInt _length <- deserialize
  -- Attributes (unused)
  _attributes :: Int8 <- deserialize
  -- Timestamp delta
  P.VarLong recordTimestampDelta <- deserialize
  -- Offset delta
  P.VarInt recordOffsetDelta <- deserialize
  -- Key
  P.VarInt keyLen <- deserialize
  recordKey <- if keyLen < 0
               then return Nothing
               else Just <$> getByteString (fromIntegral keyLen)
  -- Value
  P.VarInt valueLen <- deserialize
  recordValue <- if valueLen < 0
                 then return BS.empty  -- Treat null as empty for simplicity
                 else getByteString (fromIntegral valueLen)
  -- Headers
  P.VarInt headersCount <- deserialize
  recordHeaders <- replicateM (fromIntegral headersCount) decodeRecordHeader
  
  return Record{..}

-- -----------------------------------------------------------------------------
-- RecordBatch Encoding/Decoding
-- -----------------------------------------------------------------------------

-- | Encode a RecordBatch to bytes.
encodeRecordBatch :: RecordBatch -> ByteString
encodeRecordBatch RecordBatch{..} =
  let
    recordsBytes = runPutS $ V.mapM_ encodeRecord batchRecords
    attributes = encodeAttributes batchAttributes
    bodyBytes = runPutS $ do
      serialize attributes
      serialize batchLastOffsetDelta
      serialize batchBaseTimestamp
      serialize batchMaxTimestamp
      serialize batchProducerId
      serialize batchProducerEpoch
      serialize batchBaseSequence
      serialize (fromIntegral (V.length batchRecords) :: Int32)
      putByteString recordsBytes
    crc = crc32c bodyBytes
    lengthValue = 4 + 1 + 4 + BS.length bodyBytes
    batchBytes = runPutS $ do
      serialize batchBaseOffset
      serialize (fromIntegral lengthValue :: Int32)
      serialize batchPartitionLeaderEpoch
      serialize magicV2
      serialize crc
      putByteString bodyBytes
  in
    batchBytes

-- | Decode a RecordBatch from bytes.
decodeRecordBatch :: ByteString -> Either String RecordBatch
decodeRecordBatch bs = runGetS deserializeRecordBatch bs
  where
    deserializeRecordBatch :: MonadGet m => m RecordBatch
    deserializeRecordBatch = do
      -- Base offset (8 bytes)
      baseOffset <- deserialize
      -- Length (4 bytes)
      lengthValue <- deserialize
      -- Partition leader epoch (4 bytes)
      leaderEpoch <- deserialize
      -- Magic (1 byte)
      magic <- deserialize
      if (magic :: Int8) /= magicV2
        then fail $ "Unsupported magic byte: " ++ show magic ++ " (expected " ++ show magicV2 ++ ")"
        else do
          -- CRC (4 bytes)
          storedCrc <- deserialize
          -- Read the body (for CRC verification)
          let bodyLength = fromIntegral (lengthValue :: Int32) - 4 - 1 - 4  -- Subtract leader epoch, magic, and crc
          bodyBytes <- getByteString bodyLength
          
          -- Verify CRC
          let computedCrc = crc32c bodyBytes
          if (storedCrc :: Word32) /= computedCrc
            then fail $ "CRC mismatch: stored=" ++ show storedCrc ++ ", computed=" ++ show computedCrc
            else do
              -- Parse the body
              case runGetS (parseBody baseOffset leaderEpoch) bodyBytes of
                Left err -> fail err
                Right result -> return result
    
    parseBody :: MonadGet m => Int64 -> Int32 -> m RecordBatch
    parseBody baseOffset leaderEpoch = do
      -- Attributes (2 bytes)
      attributesValue <- deserialize
      attrs <- case decodeAttributes (attributesValue :: Int16) of
        Left err -> fail err
        Right a -> return a
      -- Last offset delta (4 bytes)
      lastOffsetDelta <- deserialize
      -- Base timestamp (8 bytes)
      baseTimestamp <- deserialize
      -- Max timestamp (8 bytes)
      maxTimestamp <- deserialize
      -- Producer ID (8 bytes)
      producerId <- deserialize
      -- Producer epoch (2 bytes)
      producerEpoch <- deserialize
      -- Base sequence (4 bytes)
      baseSequence <- deserialize
      -- Records count (4 bytes)
      recordsCount <- deserialize
      -- Records
      recordsList <- replicateM (fromIntegral (recordsCount :: Int32)) decodeRecord
      let records = V.fromList recordsList
      
      return RecordBatch
        { batchBaseOffset = baseOffset
        , batchPartitionLeaderEpoch = leaderEpoch
        , batchAttributes = attrs
        , batchLastOffsetDelta = lastOffsetDelta
        , batchBaseTimestamp = baseTimestamp
        , batchMaxTimestamp = maxTimestamp
        , batchProducerId = producerId
        , batchProducerEpoch = producerEpoch
        , batchBaseSequence = baseSequence
        , batchRecords = records
        }

-- | Calculate the size of an encoded RecordBatch in bytes.
calculateBatchSize :: RecordBatch -> Int
calculateBatchSize batch = BS.length (encodeRecordBatch batch)

-- | Encode a RecordBatch with compression applied to the records section.
-- The compression codec is taken from the batch attributes.
-- If NoCompression is specified, this is equivalent to 'encodeRecordBatch'.
--
-- This function compresses the entire records section and updates the
-- batch length and CRC accordingly.
encodeRecordBatchWithCompression :: RecordBatch -> IO (Either String ByteString)
encodeRecordBatchWithCompression batch@RecordBatch{..} = do
  let codec = attrCompressionType batchAttributes
  let recordsBytes = runPutS $ V.mapM_ encodeRecord batchRecords
  
  -- Compress the records if needed
  compressedRecordsResult <- Compression.compress codec recordsBytes
  
  case compressedRecordsResult of
    Left err -> return $ Left err
    Right compressedRecords -> do
      -- Encode attributes
      let attributes = encodeAttributes batchAttributes
      
      -- Encode the batch body (everything after the CRC field)
      let bodyBytes = runPutS $ do
            -- Attributes (2 bytes)
            serialize attributes
            -- Last offset delta (4 bytes)
            serialize batchLastOffsetDelta
            -- Base timestamp (8 bytes)
            serialize batchBaseTimestamp
            -- Max timestamp (8 bytes)
            serialize batchMaxTimestamp
            -- Producer ID (8 bytes)
            serialize batchProducerId
            -- Producer epoch (2 bytes)
            serialize batchProducerEpoch
            -- Base sequence (4 bytes)
            serialize batchBaseSequence
            -- Records count (4 bytes)
            serialize (fromIntegral (V.length batchRecords) :: Int32)
            -- Compressed records
            putByteString compressedRecords
      
      -- Calculate CRC32C over the body
      let crc = crc32c bodyBytes
      
      -- Calculate the length (everything after the Length field)
      -- = partition leader epoch (4) + magic (1) + crc (4) + body
      let lengthValue = 4 + 1 + 4 + BS.length bodyBytes
      
      -- Encode the complete batch
      let batchBytes = runPutS $ do
            -- Base offset (8 bytes)
            serialize batchBaseOffset
            -- Length (4 bytes)
            serialize (fromIntegral lengthValue :: Int32)
            -- Partition leader epoch (4 bytes)
            serialize batchPartitionLeaderEpoch
            -- Magic (1 byte)
            serialize magicV2
            -- CRC (4 bytes)
            serialize crc
            -- Body
            putByteString bodyBytes
      
      return $ Right batchBytes

-- | Encode a RecordBatch with compression applied to the records section at a specific level.
-- This is similar to 'encodeRecordBatchWithCompression' but allows specifying the
-- compression level (KIP-353/776/909).
--
-- The compression codec is taken from the batch attributes, and the compression level
-- parameter controls the speed/ratio tradeoff.
--
-- If NoCompression is specified, the level is ignored and this is equivalent to 'encodeRecordBatch'.
encodeRecordBatchWithCompressionLevel :: RecordBatch -> Compression.CompressionLevel -> IO (Either String ByteString)
encodeRecordBatchWithCompressionLevel batch@RecordBatch{..} level = do
  let codec = attrCompressionType batchAttributes
  let recordsBytes = runPutS $ V.mapM_ encodeRecord batchRecords
  
  -- Compress the records with specified level if needed
  compressedRecordsResult <- Compression.compressWithLevel codec level recordsBytes
  
  case compressedRecordsResult of
    Left err -> return $ Left err
    Right compressedRecords -> do
      -- Encode attributes
      let attributes = encodeAttributes batchAttributes
      
      -- Encode the batch body (everything after the CRC field)
      let bodyBytes = runPutS $ do
            -- Attributes (2 bytes)
            serialize attributes
            -- Last offset delta (4 bytes)
            serialize batchLastOffsetDelta
            -- Base timestamp (8 bytes)
            serialize batchBaseTimestamp
            -- Max timestamp (8 bytes)
            serialize batchMaxTimestamp
            -- Producer ID (8 bytes)
            serialize batchProducerId
            -- Producer epoch (2 bytes)
            serialize batchProducerEpoch
            -- Base sequence (4 bytes)
            serialize batchBaseSequence
            -- Records count (4 bytes)
            serialize (fromIntegral (V.length batchRecords) :: Int32)
            -- Compressed records
            putByteString compressedRecords
      
      -- Calculate CRC32C over the body
      let crc = crc32c bodyBytes
      
      -- Calculate the length (everything after the Length field)
      -- = partition leader epoch (4) + magic (1) + crc (4) + body
      let lengthValue = 4 + 1 + 4 + BS.length bodyBytes
      
      -- Encode the complete batch
      let batchBytes = runPutS $ do
            -- Base offset (8 bytes)
            serialize batchBaseOffset
            -- Length (4 bytes)
            serialize (fromIntegral lengthValue :: Int32)
            -- Partition leader epoch (4 bytes)
            serialize batchPartitionLeaderEpoch
            -- Magic (1 byte)
            serialize magicV2
            -- CRC (4 bytes)
            serialize crc
            -- Body
            putByteString bodyBytes
      
      return $ Right batchBytes

-- | Decode a RecordBatch with automatic decompression.
-- This function reads the compression codec from the batch attributes
-- and automatically decompresses the records section if needed.
--
-- If NoCompression is specified in the attributes, this is equivalent
-- to 'decodeRecordBatch'.
decodeRecordBatchWithDecompression :: ByteString -> IO (Either String RecordBatch)
decodeRecordBatchWithDecompression bs = do
  case runGetS deserializeRecordBatch bs of
    Left err -> return $ Left err
    Right (baseOffset, leaderEpoch, attrs, bodyBytes) -> do
      -- Check if decompression is needed
      let codec = attrCompressionType attrs
      
      -- Parse the metadata part of the body (everything before records)
      -- Metadata is: attributes (2) + last offset delta (4) + base timestamp (8) +
      -- max timestamp (8) + producer id (8) + producer epoch (2) + base sequence (4) + records count (4) = 40 bytes
      let metadataSize = 40
      let recordsBytes = BS.drop metadataSize bodyBytes
      
      case runGetS parseMetadata bodyBytes of
        Left err -> return $ Left err
        Right (lastOffsetDelta, baseTimestamp, maxTimestamp, producerId, producerEpoch, baseSequence, recordsCount) -> do
          -- Decompress records if needed
          decompressedResult <- Compression.decompress codec recordsBytes
          
          case decompressedResult of
            Left err -> return $ Left $ "Decompression failed: " ++ err
            Right decompressedRecords -> do
              -- Parse the decompressed records
              case runGetS (parseRecords recordsCount) decompressedRecords of
                Left err -> return $ Left err
                Right records -> return $ Right RecordBatch
                  { batchBaseOffset = baseOffset
                  , batchPartitionLeaderEpoch = leaderEpoch
                  , batchAttributes = attrs
                  , batchLastOffsetDelta = lastOffsetDelta
                  , batchBaseTimestamp = baseTimestamp
                  , batchMaxTimestamp = maxTimestamp
                  , batchProducerId = producerId
                  , batchProducerEpoch = producerEpoch
                  , batchBaseSequence = baseSequence
                  , batchRecords = V.fromList records
                  }
  where
    deserializeRecordBatch :: MonadGet m => m (Int64, Int32, Attributes, ByteString)
    deserializeRecordBatch = do
      -- Base offset (8 bytes)
      baseOffset <- deserialize
      -- Length (4 bytes)
      lengthValue <- deserialize
      -- Partition leader epoch (4 bytes)
      leaderEpoch <- deserialize
      -- Magic (1 byte)
      magic <- deserialize
      if (magic :: Int8) /= magicV2
        then fail $ "Unsupported magic byte: " ++ show magic ++ " (expected " ++ show magicV2 ++ ")"
        else do
          -- CRC (4 bytes)
          storedCrc <- deserialize
          -- Read the body (for CRC verification)
          let bodyLength = fromIntegral (lengthValue :: Int32) - 4 - 1 - 4  -- Subtract leader epoch, magic, and crc
          bodyBytes <- getByteString bodyLength
          
          -- Verify CRC
          let computedCrc = crc32c bodyBytes
          if (storedCrc :: Word32) /= computedCrc
            then fail $ "CRC mismatch: stored=" ++ show storedCrc ++ ", computed=" ++ show computedCrc
            else do
              -- Parse attributes to get compression codec
              case runGetS parseAttrs bodyBytes of
                Left err -> fail err
                Right attrs -> return (baseOffset, leaderEpoch, attrs, bodyBytes)
    
    parseAttrs :: MonadGet m => m Attributes
    parseAttrs = do
      attributesValue <- deserialize
      case decodeAttributes (attributesValue :: Int16) of
        Left err -> fail err
        Right attrs -> return attrs
    
    parseMetadata :: MonadGet m => m (Int32, Int64, Int64, Int64, Int16, Int32, Int32)
    parseMetadata = do
      -- Attributes (2 bytes)
      _ :: Int16 <- deserialize
      -- Last offset delta (4 bytes)
      lastOffsetDelta <- deserialize
      -- Base timestamp (8 bytes)
      baseTimestamp <- deserialize
      -- Max timestamp (8 bytes)
      maxTimestamp <- deserialize
      -- Producer ID (8 bytes)
      producerId <- deserialize
      -- Producer epoch (2 bytes)
      producerEpoch <- deserialize
      -- Base sequence (4 bytes)
      baseSequence <- deserialize
      -- Records count (4 bytes)
      recordsCount <- deserialize
      return (lastOffsetDelta, baseTimestamp, maxTimestamp, producerId, producerEpoch, baseSequence, recordsCount)
    
    parseRecords :: MonadGet m => Int32 -> m [Record]
    parseRecords count = replicateM (fromIntegral count) decodeRecord

