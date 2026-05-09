# Memory Management Strategy for Kafka-Native

**Status**: Design Document  
**Created**: 2025-11-03  
**Purpose**: Reduce ByteString fragmentation and memory waste through buffer pooling, unpacked types, and zero-copy processing

## Table of Contents

1. [Overview](#overview)
2. [Current Memory Allocation Patterns](#current-memory-allocation-patterns)
3. [Proposed Improvements](#proposed-improvements)
4. [Zero-Copy Processing Opportunities](#zero-copy-processing-opportunities)
5. [Implementation Priority](#implementation-priority)
6. [Testing Strategy](#testing-strategy)
7. [References](#references)

## Overview

Haskell's `ByteString` library, while efficient, can cause memory fragmentation when used carelessly. This document outlines a comprehensive strategy to minimize allocations, reuse buffers, and implement zero-copy patterns throughout the kafka-native codebase.

### Goals

- **Reduce allocations**: Pool and reuse buffers instead of constantly allocating new ones
- **Minimize copies**: Use ByteString views and zero-copy techniques where possible
- **Improve cache locality**: Use unpacked and unboxed types to reduce pointer chasing
- **Lower GC pressure**: Fewer allocations mean less work for the garbage collector

### Key Techniques

1. **Buffer Pooling**: Reuse memory buffers across operations
2. **Unpacked Types**: Use `unpacked-either` and `unpacked-maybe` to eliminate pointer indirection
3. **Vector Optimization**: Convert lists to vectors, prefer unboxed/storable vectors for primitives
4. **Zero-Copy Views**: Reference existing buffers instead of copying data
5. **Pre-allocation**: Calculate sizes before encoding to allocate exactly once

## Current Memory Allocation Patterns

### 1. Serialization Layer

**Location**: `src/Kafka/Protocol/Encoding.hs`, `src/Kafka/Protocol/Primitives.hs`

**Issues**:
- `runPutS` creates new ByteStrings for every encode operation
- `encodeMessage` (line 250-255) allocates size prefix and message body separately
- Tagged fields, strings, and arrays each allocate independently

**Example**:
```haskell
-- Each of these creates a new ByteString
encodeMessage msg = 
  let msgBytes = runPutS (serialize msg)      -- Allocation 1
      msgSize = BS.length msgBytes
      sizeBytes = runPutS (serialize msgSize) -- Allocation 2
  in sizeBytes <> msgBytes                    -- Potentially allocation 3
```

### 2. Record Batch Encoding

**Location**: `src/Kafka/Protocol/RecordBatch.hs`

**Issues**:
- Line 437: `recordsBytes = runPutS $ V.mapM_ encodeRecord batchRecords` 
- Line 443: `bodyBytes = runPutS $ do...` - separate allocation
- Line 471: `batchBytes = runPutS $ do...` - third allocation for same batch
- Each record encoding allocates separately

**Impact**: Encoding a 1MB batch might perform 50+ allocations

### 3. Compression

**Location**: `src/Kafka/Compression/*.hs`

**Issues**:

**Gzip** (`Gzip.hs` line 31):
```haskell
Right $ BL.toStrict $ GZip.compressWith params $ BL.fromStrict bs
-- strict → lazy → strict conversions allocate unnecessarily
```

**LZ4/Snappy** (`Lz4.hs` line 58, `Snappy.hs` similar):
```haskell
compressedBS <- BS.packCStringLen (compressedPtr, fromIntegral compressedLen)
free compressedPtr  
-- Allocates new Haskell ByteString, then frees C buffer that could be reused
```

### 4. Network I/O

**Location**: `src/Kafka/Client/Internal/Request.hs`

**Issues**:
- Line 115: `chunk <- connectionGet conn remaining` - allocates for each read
- Line 119: `let newAcc = acc <> chunk` - repeated concatenation in `readExactly`

**Impact**: Reading a 1MB response in 64KB chunks = 16 allocations + 15 concatenations

### 5. Producer Path

**Location**: `src/Kafka/Client/Internal/BatchAccumulator.hs`, `ProducerSender.hs`

**Issues**:
- Line 220-223: Records accumulate in `Seq Record` (boxed sequence)
- Line 269: `runPutS $ PR.encodeProduceRequest` - entire request allocated at once
- Line 344: `recordBytes = RB.encodeRecordBatch recordBatch` - full batch allocation
- No buffer reuse between batches

### 6. Consumer Path

**Location**: `src/Kafka/Client/Consumer.hs`

**Issues**:
- Line 777: `decodeAllBatches recordsBytes` - decodes all records eagerly
- Line 828-841: Each record copies its key/value from fetch response buffer
- Line 807: Uses `BS.drop` (good - creates view) but then copies data out

## Proposed Improvements

### 1. Buffer Pool Architecture

**Design Pattern**: Arena-style memory management with size classes

**New Module**: `Kafka.Memory.BufferPool`

```haskell
module Kafka.Memory.BufferPool
  ( BufferPool
  , createBufferPool
  , withPooledBuffer
  , acquireBuffer
  , returnBuffer
  ) where

data BufferPool = BufferPool
  { smallBuffers  :: !(TVar [MutableByteArray])  -- < 4KB
  , mediumBuffers :: !(TVar [MutableByteArray])  -- 4KB - 64KB  
  , largeBuffers  :: !(TVar [MutableByteArray])  -- > 64KB
  , allocatedBytes :: !(TVar Int64)              -- For metrics
  , maxBuffers :: !Int                           -- Pool size limit
  }

-- Smart allocation that checks pool first
withPooledBuffer :: BufferPool -> Int -> (MutableByteArray -> IO a) -> IO a
withPooledBuffer pool size action = do
  buffer <- acquireBuffer pool size
  result <- try (action buffer)
  returnBuffer pool buffer
  case result of
    Left (e :: SomeException) -> throwIO e
    Right val -> return val

-- Returns buffer to pool for reuse (or discards if pool full)
returnBuffer :: BufferPool -> MutableByteArray -> IO ()
```

**Key Integration Points**:
- `ProducerSender.sendToBroker`: Reuse encoding buffers across requests
- `RecordBatch.encodeRecordBatch`: Pre-allocate based on calculated size
- `Request.frameRequest`: Pool the framing buffer
- Compression codecs: Pool input/output buffers

### 2. Unpacked Types for Reduced Pointer Chasing

**Dependencies to Add**:
```yaml
dependencies:
  - unpacked-either >= 0.1
  - unpacked-maybe >= 0.1
```

**Hot Paths to Optimize**:

```haskell
-- In Kafka.Protocol.Primitives
-- Current: data Nullable a = Null | NotNull a
-- Proposed: Use Data.Maybe.Unpacked

import qualified Data.Maybe.Unpacked as U

-- Replace Nullable with Unpacked Maybe where appropriate
newtype KafkaString = KafkaString (U.Maybe Text)
newtype KafkaBytes = KafkaBytes (U.Maybe ByteString)

-- In Kafka.Protocol.RecordBatch  
-- Current: recordKey :: !(Maybe ByteString)
-- Proposed: recordKey :: !(U.Maybe ByteString)

-- In ProducerSender.hs (error handling paths)
-- Current: IO (Either String ByteString)
-- Proposed: 
import qualified Data.Either.Unpacked as UE
IO (UE.Either String ByteString)

-- In Kafka.Compression.Types
-- Current: compress :: CompressionCodec -> ByteString -> IO (Either String ByteString)
-- Proposed: compress :: CompressionCodec -> ByteString -> IO (UE.Either String ByteString)
```

**Benefits**: 
- `Either String ByteString` goes from 3 words (pointer + tag + payload) to 2 words (unboxed sum)
- `Maybe ByteString` goes from 2-3 words to 1-2 words
- Better cache locality, fewer GC pointers
- Reduced memory fragmentation

**Reference**: [unpacked-either on Hackage](https://hackage.haskell.org/package/unpacked-either-0.1.0.0/docs/Data-Either-Unpacked.html)

### 3. Vector Conversions for Sequential Data

**Lists → Vectors in Hot Paths**:

```haskell
-- In BatchAccumulator.hs line 93
-- Current: batchRecords :: !(Seq Record)
-- Analysis: Seq is spine-strict but elements are boxed
-- Proposed: Evaluate using Vector or Vector.Unboxed where appropriate

-- In RecordBatch.hs line 278
-- Current: batchRecords :: !(V.Vector Record)  
-- Status: ✓ Already using Vector (good!)
-- Can optimize further: Consider Storable instance for Record

-- In ProducerSender.hs line 354
-- Current: records = V.fromList $ toList $ BA.batchRecords batch
-- Issue: Seq → List → Vector conversion
-- Proposed: Keep as Vector from the start in BatchAccumulator

-- In Primitives.hs line 251 (RecordHeader)
-- Current: recordHeaders :: ![RecordHeader]
-- Proposed: recordHeaders :: !(V.Vector RecordHeader)
```

**Unboxed Vector Candidates**:
- Partition IDs (Int32)
- Offsets (Int64)
- Timestamps (Int64)
- Error codes (Int16)
- Any array of primitives from protocol messages

**Example Optimization**:
```haskell
-- Before: Boxed vector of Int32 partition IDs
partitions :: V.Vector Int32  -- 8n + overhead bytes

-- After: Unboxed vector
import qualified Data.Vector.Unboxed as VU
partitions :: VU.Vector Int32  -- 4n bytes (2x memory savings + better cache)
```

### 4. Compression Buffer Management

**New Module**: `Kafka.Memory.CompressionBuffer`

```haskell
module Kafka.Memory.CompressionBuffer where

data CompressionContext = CompressionContext
  { compressionPool :: !BufferPool
  , maxOutputSize :: !Int  -- Upper bound estimate
  }

compressWithContext 
  :: CompressionContext 
  -> CompressionCodec 
  -> ByteString 
  -> IO ByteString
-- Allocates from pool, returns to pool on completion
```

**Gzip Optimization**:
```haskell
-- Instead of: BL.toStrict $ GZip.compressWith params $ BL.fromStrict bs
-- Use zlib-bindings for direct ByteString → ByteString
import qualified Codec.Zlib as Z

compressGzipDirect :: ByteString -> IO ByteString
-- Avoids lazy ByteString entirely
```

**FFI Compression (LZ4, Snappy)**:
```haskell
-- Current: Always frees C buffer after BS.packCStringLen
-- Proposed: Keep C buffers in pool, use unsafePackMallocCString
-- OR: Pre-allocate Haskell buffer, FFI writes directly into it

compressLz4Pooled :: BufferPool -> ByteString -> IO (Either String ByteString)
compressLz4Pooled pool input = do
  -- Estimate output size (LZ4 worst case is input_size + input_size/255 + 16)
  let maxOutputSize = BS.length input + (BS.length input `div` 255) + 16
  withPooledBuffer pool maxOutputSize $ \outBuffer ->
    withMutableByteArray outBuffer $ \outPtr ->
      BSU.unsafeUseAsCStringLen input $ \(inPtr, inLen) ->
        -- Write directly to pooled buffer
        c_lz4_compress_to_buffer inPtr inLen outPtr maxOutputSize
```

### 5. Serialization Pre-allocation

**Size Calculation Before Encoding**:

```haskell
-- Add to Kafka.Protocol.Encoding
class SizeCalculable a where
  calculateSize :: ApiVersion -> a -> Int

-- Implement for all protocol messages
instance SizeCalculable ProduceRequest where
  calculateSize version req = 
    sum [ 4  -- Size prefix
        , sizeOfString (transactionalId req)
        , 2  -- acks
        , 4  -- timeout
        , sizeOfArray (calculateSize version) (topicData req)
        ]

-- Then in ProducerSender.buildProduceRequest
let requestSize = calculateSize apiVersion request
withPooledBuffer pool requestSize $ \buffer ->
  runPutToBuffer buffer (serialize request)
```

**Benefits**: Single allocation per request, buffer can be reused

### 6. Unboxing Protocol Primitives

**Pack Attributes into Single Word**:

```haskell
-- In RecordBatch.hs line 148-159
-- Current: data Attributes with 5 fields (40+ bytes with padding)
data Attributes = Attributes
  { attrCompressionType :: !CompressionCodec
  , attrTimestampType :: !TimestampType
  , attrIsTransactional :: !Bool
  , attrIsControl :: !Bool
  , attrHasDeleteHorizon :: !Bool
  }

-- Proposed: Pack into single Word16
newtype Attributes = Attributes Word16
  deriving (Eq, Show, Storable)

-- Provide smart accessors
compressionType :: Attributes -> CompressionCodec
compressionType (Attributes w) = toCompressionCodec (w .&. 0x07)

timestampType :: Attributes -> TimestampType
timestampType (Attributes w) = if testBit w 3 then LogAppendTime else CreateTime

-- Benefits: 40 bytes → 2 bytes per Attributes value
```

**Optimize BatchState**:

```haskell
-- In BatchAccumulator.hs line 76-82
-- Current:
data BatchState = Filling | Ready | Sending | Complete | Failed !Text

-- Proposed: Use unpacked-either for Failed case
import qualified Data.Either.Unpacked as UE
data BatchState = BatchState (UE.Either Word8 Text)
-- Left 0 = Filling, Left 1 = Ready, etc.
-- Right msg = Failed msg

-- Or encode as separate fields:
data BatchState = BatchState
  { bsStatus :: !Word8  -- 0=Filling, 1=Ready, 2=Sending, 3=Complete, 4=Failed
  , bsError :: !(U.Maybe Text)  -- Only for Failed state
  }
```

### 7. Network Buffer Strategy

**Proposed Enhancement to ConnectionManager**:

```haskell
-- In Kafka.Network.Connection
data ConnectionManager = ConnectionManager
  { connectionMap :: !(StmMap.Map BrokerAddress Connection)
  , sendBufferPool :: !BufferPool      -- NEW: per-connection buffers
  , recvBufferPool :: !BufferPool      -- NEW: for readExactly
  }

-- In Request.hs readExactly (line 108-121)
-- Current: Uses (<>) to concatenate chunks repeatedly
-- Proposed: Pre-allocate full buffer based on size prefix

readExactlyPooled :: BufferPool -> Connection -> Int -> IO ByteString
readExactlyPooled pool conn n = do
  buffer <- acquireBuffer pool n
  fillBuffer buffer 0 n
  where
    fillBuffer buf offset remaining
      | remaining <= 0 = freezeBuffer buf
      | otherwise = do
          chunk <- connectionGet conn remaining  
          copyToBuffer buf offset chunk
          fillBuffer buf (offset + BS.length chunk) (remaining - BS.length chunk)
```

**Benefits**: 1MB read goes from 16+ allocations to 1 allocation

## Zero-Copy Processing Opportunities

### 1. Consumer Record Views (CRITICAL - 10x+ Impact)

**Problem**: `Consumer.hs` lines 797-841 copies every record key/value from fetch buffer

**Current Flow**:
1. Receive 1MB fetch response into buffer
2. Parse RecordBatch headers
3. For each record, allocate new ByteString for key
4. For each record, allocate new ByteString for value
5. Result: 1MB response → potentially 2-3MB allocated

**Proposed: Zero-Copy Record Views**

**New Module**: `Kafka.Protocol.RecordView`

```haskell
module Kafka.Protocol.RecordView where

-- Record view that references original buffer
data RecordView = RecordView
  { rvBuffer :: !ByteString          -- Original fetch response buffer
  , rvKeyOffset :: !Int              -- Offset to key in buffer
  , rvKeyLength :: !Int              -- Length of key (-1 if null)
  , rvValueOffset :: !Int            -- Offset to value
  , rvValueLength :: !Int            -- Length of value
  , rvHeadersOffset :: !Int          -- Offset to headers section
  , rvBaseOffset :: !Int64           -- Absolute offset in partition
  , rvTimestamp :: !Int64            -- Record timestamp
  }

-- Zero-copy accessor - only allocates when actually accessed
recordViewKey :: RecordView -> Maybe ByteString
recordViewKey rv = if rvKeyLength rv < 0 
  then Nothing
  else Just $ BS.take (rvKeyLength rv) $ BS.drop (rvKeyOffset rv) (rvBuffer rv)

recordViewValue :: RecordView -> ByteString
recordViewValue rv = 
  BS.take (rvValueLength rv) $ BS.drop (rvValueOffset rv) (rvBuffer rv)

-- Consumer can choose: keep view for streaming, or force copy for storage
materializeRecord :: RecordView -> ConsumerRecord
materializeRecord rv = ConsumerRecord
  { crKey = fmap BS.copy (recordViewKey rv)      -- Explicit copy
  , crValue = BS.copy (recordViewValue rv)       -- Explicit copy
  , ...
  }

-- For streaming consumers that process and discard
processRecordView :: (RecordView -> IO ()) -> RecordView -> IO ()
-- No allocation at all - work directly with buffer slice
```

**Benefits**: 
- Fetch 1MB of records → only allocate what you actually use
- Streaming consumers never materialize unused records  
- High-throughput consumers: 50-90% reduction in memory allocation
- Can keep original buffer pinned until batch is processed

### 2. Text Decoding - UTF-8 Validation Without Copying

**Problem**: `Primitives.hs` lines 291-301, 316-326

```haskell
-- Current: T.encodeUtf8 allocates, T.decodeUtf8 allocates
serialize (KafkaString (NotNull t)) = do
  let bs = T.encodeUtf8 t  -- NEW ByteString allocation
  serialize (fromIntegral (BS.length bs) :: Int16)
  putByteString bs
```

**Proposed: ShortText for Small Strings**

```haskell
-- Use text-short package for strings < 256 bytes (topic names, client IDs)
-- ShortText is stored as unpinned ShortByteString - no GC overhead
import qualified Data.Text.Short as TS

newtype KafkaString = KafkaString (Nullable ShortText)

-- Benefits of ShortText:
-- - Stored as UTF-8 bytes (no Text's UTF-16 overhead)
-- - Unpinned memory (doesn't fragment GHC heap)
-- - Perfect for small, immutable strings
-- - 90% of Kafka strings are < 256 bytes (topics, groups, client IDs)

-- For large strings or string views:
data KafkaStringView = KafkaStringView !ByteString !Int !Int
-- Zero-copy view into larger buffer, validate UTF-8 lazily

validateUtf8Slice :: ByteString -> Int -> Int -> Either String Text
-- Can validate without allocating Text until needed
```

### 3. Record Batch Streaming - Lazy Decoding

**Problem**: `RecordBatch.hs` lines 488-556 eagerly decodes all records

```haskell
-- Current: Allocates all records upfront
decodeRecordBatch :: ByteString -> Either String RecordBatch
-- Line 542: recordsList <- replicateM recordsCount decodeRecord
-- Decodes ALL records immediately, even if consumer only wants first 10
```

**Proposed: Streaming Record Iterator**

**New Module**: `Kafka.Protocol.RecordBatch.Stream`

```haskell
module Kafka.Protocol.RecordBatch.Stream where

-- Streaming decoder that decodes on demand
data RecordBatchStream = RecordBatchStream
  { rbsBuffer :: !ByteString         -- Original buffer (zero-copy reference)
  , rbsRecordsOffset :: !Int         -- Where records section starts
  , rbsRecordsEnd :: !Int            -- Where it ends
  , rbsRecordsRemaining :: !Int      -- How many records left
  , rbsCurrentOffset :: !(IORef Int) -- Mutable position for iteration
  , rbsBatchMetadata :: !BatchMetadata
  }

data BatchMetadata = BatchMetadata
  { bmBaseOffset :: !Int64
  , bmBaseTimestamp :: !Int64
  , bmAttributes :: !Attributes
  }

-- Create stream from buffer (minimal parsing - just header)
streamRecordBatch :: ByteString -> IO (Either String RecordBatchStream)

-- Pull-based streaming - only decode what's requested
nextRecord :: RecordBatchStream -> IO (Maybe RecordView)
nextRecord stream = do
  currentPos <- readIORef (rbsCurrentOffset stream)
  if currentPos >= rbsRecordsEnd stream
    then return Nothing
    else do
      -- Decode one record, update position
      -- Return RecordView (zero-copy reference to buffer)
      ...

-- Convenience functions for different use cases
takeRecords :: Int -> RecordBatchStream -> IO [RecordView]
-- Take N records (low memory for sampling)

foldRecords :: (a -> RecordView -> IO a) -> a -> RecordBatchStream -> IO a
-- Fold over records (constant memory)

toList :: RecordBatchStream -> IO [Record]
-- Materialize all (current behavior)
```

**Usage in Consumer**:

```haskell
-- Before:
batches <- decodeAllBatches recordsBytes  -- Eager
let allRecords = concatMap convertBatchToRecords batches

-- After:
streams <- streamAllBatches recordsBytes
-- Process on demand
forM_ streams $ \stream ->
  foldRecords processRecord () stream  -- Constant memory

-- Or selectively materialize:
firstTen <- takeRecords 10 stream  -- Only decode 10 records
```

**Specific Locations to Update**:
- `Consumer.hs` line 777: `decodeAllBatches` → `streamAllBatches`
- `Consumer.hs` line 829: `convertBatchToRecords` → return iterator
- `Simple.hs` line 394: `decodeBatches` → stream instead of accumulate

### 4. Compression - In-Place Decompression

**Gzip Optimization**:

```haskell
-- Current (Gzip.hs line 31): Multiple conversions
compressGzip bs = Right $ BL.toStrict $ GZip.compressWith params $ BL.fromStrict bs

-- Proposed: Use zlib-bindings for direct operation
import qualified Codec.Zlib as Z

compressGzipDirect :: ByteString -> IO ByteString
compressGzipDirect input = do
  -- Use WindowBits to match gzip format
  deflate <- Z.initDeflate 6 (Z.WindowBits 31)
  -- Feed input
  Z.feedDeflate deflate input
  -- Get output
  chunks <- Z.finishDeflate deflate
  return $ BS.concat chunks  -- Or use Builder for efficiency
```

**LZ4/Snappy Zero-Copy Decompression**:

```haskell
-- Current (Lz4.hs line 56-60): Allocate Haskell ByteString, free C buffer
compressedBS <- BS.packCStringLen (compressedPtr, fromIntegral compressedLen)
free compressedPtr

-- Proposed: Pre-allocate output buffer, decompress directly into it
decompressLz4ZeroCopy :: ByteString -> BufferPool -> IO (Either String ByteString)
decompressLz4ZeroCopy input pool = do
  -- Get uncompressed size from LZ4 frame header
  let estimatedSize = readLZ4FrameSize input
  
  withPooledBuffer pool estimatedSize $ \buffer ->
    withMutableByteArray buffer $ \outPtr ->
      BSU.unsafeUseAsCStringLen input $ \(inPtr, inLen) -> do
        actualSize <- c_lz4_decompress_to_buffer inPtr inLen outPtr estimatedSize
        if actualSize < 0
          then return $ Left "LZ4 decompression failed"
          else do
            -- Return buffer slice (zero-copy)
            frozen <- freezeByteArray buffer 0 (fromIntegral actualSize)
            return $ Right frozen
```

**Shared Decompression Buffer**:

```haskell
-- In ConnectionManager or ConsumerState
data ConsumerState = ConsumerState
  { ...
  , decompressionBuffer :: !(TVar (Maybe MutableByteArray))
  -- Reuse between fetches - most record batches are similar size
  }

decompressWithSharedBuffer :: ConsumerState -> CompressionCodec -> ByteString -> IO ByteString
-- Acquires shared buffer, decompresses, returns result
-- Buffer stays allocated for next fetch
```

### 5. Network I/O - Direct Buffer Reading

**Problem**: `Request.hs` lines 108-121

```haskell
readExactly conn n = go BS.empty n 0
  where
    go acc remaining emptyReads = do
      chunk <- connectionGet conn remaining
      let newAcc = acc <> chunk  -- REPEATED CONCATENATION
```

**Analysis**: Reading 1MB in 64KB chunks = 16 allocations + 15 concatenations

**Proposed: Pre-allocated Read Buffer**:

```haskell
-- New in Kafka.Network.Connection
data ConnectionState = ConnectionState
  { connHandle :: !Connection
  , connReadBuffer :: !(IORef MutableByteArray)  -- Reusable read buffer
  , connWriteBuffer :: !(IORef MutableByteArray) -- Reusable write buffer
  }

readExactlyDirect :: ConnectionState -> Int -> IO ByteString
readExactlyDirect state n = do
  buffer <- acquireOrGrowBuffer (connReadBuffer state) n
  withMutableByteArray buffer $ \ptr ->
    fillBufferDirect (connHandle state) ptr 0 n
  freezeByteArray buffer 0 n  -- Single allocation at end

-- Helper: Fill buffer directly from socket
fillBufferDirect :: Connection -> Ptr Word8 -> Int -> Int -> IO ()
fillBufferDirect conn ptr offset remaining
  | remaining <= 0 = return ()
  | otherwise = do
      -- Use connectionGetExact or similar to read directly into buffer
      bytesRead <- connectionReadToPtr conn (ptr `plusPtr` offset) remaining
      fillBufferDirect conn ptr (offset + bytesRead) (remaining - bytesRead)
```

**Benefits**: 1MB read = 1 allocation instead of 16+

### 6. CRC Calculation - Already Zero-Copy! ✓

**Good News**: `CRC32C.hs` is already optimized!

```haskell
crc32c :: ByteString -> Word32
crc32c bs =
  unsafePerformIO $
    BSU.unsafeUseAsCStringLen bs $ \(ptr, len) ->
      return $! c_crc32c (castPtr ptr :: Ptr Word8) (fromIntegral len)
```

This operates directly on ByteString's internal buffer with **no copy**. Keep this pattern!

### 7. Request/Response Framing - Eliminate Intermediate Allocations

**Problem**: `Request.hs` lines 48-68

```haskell
frameRequest apiKey apiVersion correlationId clientId requestBody =
  let
    headerBytes = runPutS $ RH.encodeRequestHeader 1 header  -- Allocation 1
    messageBytes = headerBytes <> requestBody                -- Allocation 2
    messageSize = BS.length messageBytes
    sizeBytes = runPutS $ serialize messageSize              -- Allocation 3
  in
    sizeBytes <> messageBytes  -- Allocation 4
-- Total: Up to 4 allocations for single request frame!
```

**Proposed: Single-Pass Encoding**:

```haskell
-- Add size calculation to protocol types
class SizeCalculable a where
  calculateSize :: ApiVersion -> a -> Int

instance SizeCalculable RequestHeader where
  calculateSize version hdr = 
    2 + 2 + 4 + sizeOfString (requestHeaderClientId hdr)

-- Frame in one pass:
frameRequestDirect :: BufferPool -> ApiKey -> ApiVersion -> ... -> IO ByteString
frameRequestDirect pool apiKey version corrId clientId body = do
  let headerSize = calculateHeaderSize version clientId
      totalSize = 4 + headerSize + BS.length body
  
  withPooledBuffer pool totalSize $ \buffer ->
    runPutToBuffer buffer $ do
      serialize (fromIntegral (headerSize + BS.length body) :: Int32)
      encodeRequestHeader version header
      putByteString body
```

**Benefits**: 4 allocations → 1 allocation per request

### 8. Array Decoding - Unboxed Vectors for Primitives

**Problem**: `Encoding.hs` lines 234-246

```haskell
-- Problem: List intermediate, boxed Vector for primitive types
vec <- V.fromList <$> forM [1..actualLen] (\_ -> decodeFn version)
return (P.mkKafkaArray vec)
```

**Proposed: Direct Unboxed Decoding**:

```haskell
-- For arrays of Int32, Int64, Int16 (partition IDs, offsets, timestamps, error codes)
import qualified Data.Vector.Unboxed as VU

decodeInt32Array :: MonadGet m => Int -> m (VU.Vector Int32)
decodeInt32Array n = 
  VU.replicateM n deserialize  -- No list intermediate!

decodeInt64Array :: MonadGet m => Int -> m (VU.Vector Int64)
decodeInt64Array n = 
  VU.replicateM n deserialize

-- Memory comparison:
-- Boxed Vector Int32: 8n + overhead bytes (pointer per element)
-- Unboxed Vector Int32: 4n bytes (2x memory savings + better cache)
```

**Hot Paths to Convert**:
- Partition ID arrays in metadata responses
- Offset arrays in ListOffsets responses
- Error code arrays in batch responses
- Timestamp arrays

### 9. Producer Path - Incremental Batch Building

**Problem**: `ProducerSender.hs` lines 341-378

```haskell
-- Build entire RecordBatch in memory, then encode all at once
buildRecordBatch :: ProducerBatch -> RecordBatch
-- Line 354: records = V.fromList $ toList $ BA.batchRecords batch
-- Line 344: recordBytes = RB.encodeRecordBatch recordBatch
-- Problem: Records are serialized once into Seq, then re-serialized when encoding batch
```

**Proposed: Incremental Serialization**:

```haskell
-- In BatchAccumulator: maintain serialized form alongside records
data ProducerBatch = ProducerBatch
  { batchTopicPartition :: !TopicPartition
  , batchRecords :: !(Seq Record)           -- Keep for metadata
  , batchSerializedRecords :: !(IORef Builder)  -- NEW: Incrementally built
  , batchSizeBytes :: !Int
  , ...
  }

-- When adding record:
appendRecordWithCallback accumulator tp record callback = do
  ...
  -- Serialize immediately, append to builder
  let recordBytes = encodeRecordToBuilder record
  modifyIORef' (batchSerializedRecords batch) (<> recordBytes)
  ...

-- When sending:
-- Just use the pre-serialized Builder, wrap with batch header
buildRecordBatchFromSerialized :: ProducerBatch -> IO ByteString
```

**Benefits**: 
- No re-encoding when sending
- Incremental memory use
- Can start sending batch before it's complete (streaming)

## Zero-Copy Priority Ranking

### Critical (10x+ Impact)
1. **Consumer record views** - Eliminates 50-90% of fetch response allocations
2. **Network read buffers** - Reduces network I/O allocations by ~15x

### High (5-10x Impact)
3. **Compression buffer reuse** - Major savings for compressed topics
4. **Request framing optimization** - Affects every request/response
5. **Record batch streaming** - Big win for high-throughput consumers

### Medium (2-5x Impact)
6. **Text/ShortText optimization** - Lots of small strings in Kafka
7. **Producer incremental serialization** - Saves re-encoding overhead

### Low (1-2x Impact)
8. **Unboxed vectors for primitives** - Incremental improvements
9. **Attribute packing** - Minor but worth doing

## Implementation Priority

### Phase 1: Foundation (High Impact, Low Risk)
1. **Unpacked Either/Maybe** in error paths
   - Files: `Compression/Types.hs`, `ProducerSender.hs`, `Consumer.hs`
   - Risk: Low - drop-in replacement with pattern synonyms
   - Effort: 1-2 days

2. **Buffer pooling infrastructure**
   - New module: `Kafka.Memory.BufferPool`
   - Risk: Low - additive, doesn't change existing code
   - Effort: 3-4 days

3. **List → Vector conversions**
   - Files: `BatchAccumulator.hs`, `RecordBatch.hs`, protocol types
   - Risk: Low - straightforward refactoring
   - Effort: 2-3 days

### Phase 2: High-Impact Optimizations (Moderate Risk)
4. **Buffer pooling for serialization**
   - Files: `ProducerSender.hs`, `RecordBatch.hs`, `Request.hs`
   - Risk: Medium - requires careful buffer lifecycle management
   - Effort: 1 week

5. **Zero-copy record views**
   - New module: `Kafka.Protocol.RecordView`
   - Files: `Consumer.hs`, `Simple.hs`
   - Risk: Medium - changes consumer API
   - Effort: 1 week

6. **Network I/O direct buffers**
   - Files: `Connection.hs`, `Request.hs`
   - Risk: Medium - core network code
   - Effort: 1 week

### Phase 3: Specialized Optimizations (Higher Risk)
7. **Compression buffer pooling with FFI**
   - Files: `Compression/*.hs`, FFI wrappers in `cbits/`
   - Risk: High - FFI and memory management
   - Effort: 1-2 weeks

8. **Record batch streaming decoder**
   - New module: `Kafka.Protocol.RecordBatch.Stream`
   - Files: `Consumer.hs`, `Simple.hs`
   - Risk: High - complex state management
   - Effort: 1-2 weeks

9. **Producer incremental serialization**
   - Files: `BatchAccumulator.hs`, `ProducerSender.hs`
   - Risk: High - changes core producer path
   - Effort: 1 week

### Phase 4: Polish (Low Impact, Low Risk)
10. **Unboxed vectors for primitive arrays**
    - Files: Throughout protocol layer
    - Risk: Low - gradual conversion
    - Effort: Ongoing

11. **ShortText for small strings**
    - Files: `Primitives.hs`, protocol messages
    - Risk: Low - can do incrementally
    - Effort: Ongoing

## Testing Strategy

### Memory Profiling

Use GHC's profiling tools to measure impact:

```bash
# Heap profiling - see allocation patterns
cabal run kafka-native-consumer --enable-profiling -- +RTS -s -h -i0.1

# Generate graph
hp2ps -c kafka-native-consumer.hp

# Detailed allocation info
cabal run -- +RTS -s -RTS
```

**Key Metrics to Track**:
- Total bytes allocated
- Peak memory usage (max_bytes_used)
- GC time percentage
- Allocation rate (bytes/second)

### Heap Fragmentation Analysis

```bash
# Type-based heap profile
cabal run -- +RTS -hT -i0.1

# Look for:
# - Many small ByteString allocations
# - Large number of Thunk allocations
# - Excessive closure allocations
```

### Benchmarks

Add to `bench/Benchmarks/`:

```haskell
-- bench/Benchmarks/Memory.hs
benchBufferReuse :: Benchmark
benchBufferReuse = bgroup "Buffer Reuse"
  [ bench "no pooling" $ nfIO (encodeWithoutPool request)
  , bench "with pooling" $ nfIO (encodeWithPool pool request)
  ]

benchZeroCopy :: Benchmark
benchZeroCopy = bgroup "Zero Copy"
  [ bench "eager decode" $ nf decodeAllRecords fetchResponse
  , bench "lazy views" $ nf createRecordViews fetchResponse
  ]
```

**Run with allocation tracking**:
```bash
cabal bench --benchmark-options='+RTS -s -RTS'
```

### Property Tests

Ensure unpacked types behave identically:

```haskell
-- test/Protocol/UnpackedSpec.hs
import qualified Data.Either.Unpacked as UE
import qualified Data.Maybe.Unpacked as UM

prop_eitherEquivalent :: String -> Bool
prop_eitherEquivalent s =
  let boxed = Left s :: Either String Int
      unpacked = UE.Left s :: UE.Either String Int
  in (isLeft boxed) == (UE.isLeft unpacked)

prop_maybeEquivalent :: Int -> Bool
prop_maybeEquivalent n =
  let boxed = Just n :: Maybe Int
      unpacked = UM.Just n :: UM.Maybe Int
  in (isJust boxed) == (UM.isJust unpacked)
```

### Integration Tests

Verify no performance regression:

```haskell
-- test-integration/Integration/MemorySpec.hs
spec :: Spec
spec = describe "Memory Management" $ do
  it "produces 1000 messages without excessive allocation" $ do
    initialStats <- getRTSStats
    produceManyMessages 1000
    finalStats <- getRTSStats
    
    let allocatedBytes = allocated_bytes finalStats - allocated_bytes initialStats
    -- Should be roughly: 1000 * avgMessageSize, not 10x that
    allocatedBytes `shouldSatisfy` (< 1000 * avgMessageSize * 2)
```

### Buffer Pool Tests

```haskell
-- test/Memory/BufferPoolSpec.hs
spec :: Spec
spec = describe "BufferPool" $ do
  it "reuses buffers of similar size" $ do
    pool <- createBufferPool
    buf1 <- acquireBuffer pool 1024
    returnBuffer pool buf1
    buf2 <- acquireBuffer pool 1024
    -- Should get same buffer back
    bufferAddress buf2 `shouldBe` bufferAddress buf1
  
  it "limits maximum pool size" $ do
    pool <- createBufferPool { maxBuffers = 10 }
    bufs <- replicateM 20 (acquireBuffer pool 1024)
    mapM_ (returnBuffer pool) bufs
    poolSize <- getPoolSize pool
    poolSize `shouldBe` 10  -- Should not exceed max
```

## References

### Haskell Libraries
- [unpacked-either](https://hackage.haskell.org/package/unpacked-either) - Unboxed sum types for Either
- [unpacked-maybe](https://hackage.haskell.org/package/unpacked-maybe) - Unboxed Maybe
- [text-short](https://hackage.haskell.org/package/text-short) - Memory-efficient short text
- [vector](https://hackage.haskell.org/package/vector) - Efficient arrays
- [stm-containers](https://hackage.haskell.org/package/stm-containers) - Lock-free concurrent containers

### Performance Resources
- [GHC User Guide - Profiling](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/profiling.html)
- [ByteString Internals](https://hackage.haskell.org/package/bytestring/docs/Data-ByteString-Internal.html)
- [Parallel and Concurrent Programming in Haskell](http://chimera.labs.oreilly.com/books/1230000000929) - Chapter on performance

### Related Work
- Java Kafka Client - Uses ByteBuffer pooling extensively
- librdkafka - Arena allocators and buffer reuse
- Rust rdkafka - Zero-copy with bytes crate

## Document History

- 2025-11-03: Initial design document created
- Future: Implementation tracking, benchmark results, lessons learned

