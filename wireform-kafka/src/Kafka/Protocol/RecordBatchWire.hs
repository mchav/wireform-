{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Kafka.Protocol.RecordBatchWire
Description : Direct-poke 'Wire'-codec encoder for 'RecordBatch'

Replaces the @runPutS@-per-record path in
"Kafka.Protocol.RecordBatch" with a single-allocation, single-pass
encoder that pokes the entire batch into one contiguous buffer.
The bytes are byte-identical with 'RB.encodeRecordBatch' (proven by
the round-trip + cross-codec equivalence properties in
'Protocol.RecordBatchWireSpec').

Why a separate module:

  * The legacy 'RB.encodeRecordBatch' is still used by callers
    that expect the @ByteString@ shape; switching every site is a
    bigger change than necessary for the perf win.
  * Compression + the version-aware envelope have their own paths
    in @RecordBatch@; this module focuses on the uncompressed
    happy path, which is what the producer's hot send loop hits
    most of the time.

== Hot-path savings (per-record, GHC 9.6.4 -O1):

  * legacy `encodeRecordBatch` (100 records): ~107 µs total /
    ~1.07 µs per record
  * `encodeRecordBatchWire`     (100 records): see
    'Benchmarks.HotPath' for the latest number; the win comes
    from one 'mallocForeignPtrBytes' instead of @runPutS@'s
    Builder + chunk-coalesce.
-}
module Kafka.Protocol.RecordBatchWire
  ( encodeRecordBatchWire
  , recordBatchWireSize
  ) where

import Data.Bits ((.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Data.Foldable (foldl')
import Data.Int (Int16, Int32, Int64)
import Data.Word (Word8)
import Foreign.ForeignPtr (mallocForeignPtrBytes, withForeignPtr)
import Foreign.Ptr (Ptr, minusPtr, plusPtr)
import Foreign.Storable (peek, poke)
import GHC.IO (unsafePerformIO)
import qualified Data.Vector as V

import qualified Kafka.Compression.Types as Compression
import qualified Kafka.Protocol.CRC32C as CRC
import Kafka.Protocol.RecordBatch
  ( Attributes (..)
  , Record (..)
  , RecordBatch (..)
  , RecordHeader (..)
  , TimestampType (..)
  , magicV2
  , recordBatchOverhead
  )
import Kafka.Protocol.Wire
  ( pokeByteString
  , pokeInt16BE
  , pokeInt32BE
  , pokeInt64BE
  , pokeVarInt
  , pokeVarLong
  , pokeWord32BE
  , pokeWord8
  )

----------------------------------------------------------------------
-- Public surface
----------------------------------------------------------------------

-- | Direct-poke encoder for 'RecordBatch'. Allocates one
-- 'ByteString' of size 'recordBatchWireSize' and writes the entire
-- batch in a single pass.
--
-- Byte-identical to 'Kafka.Protocol.RecordBatch.encodeRecordBatch'
-- for any non-compressed batch; for compressed batches use the
-- existing 'encodeRecordBatchWithCompression' (Wire wrapping for
-- those goes in a follow-up).
{-# INLINEABLE encodeRecordBatchWire #-}
encodeRecordBatchWire :: RecordBatch -> ByteString
encodeRecordBatchWire batch = unsafePerformIO $ do
  let !sz = recordBatchWireSize batch
  fp <- mallocForeignPtrBytes sz
  withForeignPtr fp $ \basePtr -> do
    !endPtr <- pokeBatch basePtr batch
    let !len = endPtr `minusPtr` basePtr
    pure (BSI.fromForeignPtr fp 0 len)

-- | Upper bound on the bytes 'encodeRecordBatchWire' may write.
-- For a typical record batch the upper bound is exact (records
-- have known sizes); the only worst-case fudge is per-record
-- VarInt headers (5-byte cap each).
{-# INLINEABLE recordBatchWireSize #-}
recordBatchWireSize :: RecordBatch -> Int
recordBatchWireSize RecordBatch{..} =
  recordBatchOverhead + V.foldl' (\acc r -> acc + recordWireSize r) 0 batchRecords

----------------------------------------------------------------------
-- Per-record sizing
----------------------------------------------------------------------

-- | Upper bound on a single record's wire size.
{-# INLINE recordWireSize #-}
recordWireSize :: Record -> Int
recordWireSize Record{..} =
  let !keyLen   = maybe 0 BS.length recordKey
      !valLen   = BS.length recordValue
      !hdrCount = length recordHeaders
      !hdrSize  = foldl' (\acc h -> acc + headerWireSize h) 0 recordHeaders
      -- 5-byte VarInt cap on every length / delta + 1 byte attrs.
      -- Conservative; the actual encoded size is typically smaller
      -- (small records fit in 1-byte varints).
      !innerSize = 1                -- record attrs
                 + 10               -- timestampDelta (VarLong, max 10)
                 + 5                -- offsetDelta (VarInt, max 5)
                 + 5 + keyLen       -- key length + bytes
                 + 5 + valLen       -- value length + bytes
                 + 5                -- headers count
                 + hdrSize
  in 5 + innerSize  -- outer record length VarInt

{-# INLINE headerWireSize #-}
headerWireSize :: RecordHeader -> Int
headerWireSize RecordHeader{..} =
  let !valLen = maybe 0 BS.length headerValue
  in 5 + BS.length headerKey + 5 + valLen

----------------------------------------------------------------------
-- The actual poke
----------------------------------------------------------------------

-- | Write the entire 'RecordBatch' starting at 'basePtr', returning
-- the pointer past the last byte written.
--
-- Layout (RecordBatch v2):
--
--     [0  .. 8)  baseOffset           Int64 BE
--     [8  .. 12) length               Int32 BE   (filled in last)
--     [12 .. 16) partitionLeaderEpoch Int32 BE
--     [16]       magic                Int8       (2)
--     [17 .. 21) crc                  Word32 BE  (filled in last)
--     [21 ..  )  body (attrs, deltas, producer-id+epoch+seq, count, records)
{-# INLINE pokeBatch #-}
pokeBatch :: Ptr Word8 -> RecordBatch -> IO (Ptr Word8)
pokeBatch basePtr RecordBatch{..} = do
  -- Header (offsets [0 .. 21))
  _ <- pokeInt64BE basePtr batchBaseOffset
  -- length (4 bytes) — placeholder; rewritten at the end.
  let !lengthPtr = basePtr `plusPtr` 8
  _ <- pokeInt32BE lengthPtr 0
  let !leaderPtr = basePtr `plusPtr` 12
  _ <- pokeInt32BE leaderPtr batchPartitionLeaderEpoch
  let !magicPtr = basePtr `plusPtr` 16
  _ <- pokeWord8 magicPtr (fromIntegral magicV2 :: Word8)
  let !crcPtr   = basePtr `plusPtr` 17
  _ <- pokeWord32BE crcPtr 0  -- placeholder
  -- Body starts at offset 21.
  let !bodyStart = basePtr `plusPtr` 21
  bodyEnd <- pokeBody bodyStart batchAttributes batchLastOffsetDelta
                       batchBaseTimestamp batchMaxTimestamp
                       batchProducerId batchProducerEpoch
                       batchBaseSequence batchRecords
  -- Patch length (= bytes from offset 12 to bodyEnd).
  let !lenValue = fromIntegral (bodyEnd `minusPtr` leaderPtr) :: Int32
  _ <- pokeInt32BE lengthPtr lenValue
  -- Patch CRC = CRC32C over the body bytes (everything from
  -- attributes onward, i.e. [21 .. bodyEnd)). Uses the raw-Ptr
  -- entry on the C side to skip the body memcpy a 'ByteString'-
  -- shaped helper would force.
  let !bodyLen = bodyEnd `minusPtr` bodyStart
  !crc <- CRC.crc32cPtr bodyStart bodyLen
  _ <- pokeWord32BE crcPtr crc
  pure bodyEnd

{-# INLINE pokeBody #-}
pokeBody
  :: Ptr Word8
  -> Attributes
  -> Int32         -- last offset delta
  -> Int64         -- base timestamp
  -> Int64         -- max timestamp
  -> Int64         -- producer id
  -> Int16         -- producer epoch
  -> Int32         -- base sequence
  -> V.Vector Record
  -> IO (Ptr Word8)
pokeBody p attrs lastOffsetDelta baseTs maxTs pid pep baseSeq records = do
  p1 <- pokeInt16BE p (encodeAttributes attrs)
  p2 <- pokeInt32BE p1 lastOffsetDelta
  p3 <- pokeInt64BE p2 baseTs
  p4 <- pokeInt64BE p3 maxTs
  p5 <- pokeInt64BE p4 pid
  p6 <- pokeInt16BE p5 pep
  p7 <- pokeInt32BE p6 baseSeq
  p8 <- pokeInt32BE p7 (fromIntegral (V.length records) :: Int32)
  pokeRecords p8 records

{-# INLINE pokeRecords #-}
pokeRecords :: Ptr Word8 -> V.Vector Record -> IO (Ptr Word8)
pokeRecords p0 records = V.foldM' pokeOne p0 records
  where
    pokeOne !p r = pokeRecord p r

----------------------------------------------------------------------
-- Per-record poke
----------------------------------------------------------------------

-- | Encode a single 'Record' directly into the buffer.
--
-- The Kafka record format is a length-prefixed body: we write the
-- body first into a /scratch span/, then prepend the actual length
-- as a VarInt. To do that single-pass we exploit the upper-bound
-- size: write the body into the buffer starting at @p + 5@ (the
-- maximum varint length), then copy the body backwards by however
-- many bytes the actual VarInt occupies. With small records (the
-- common case) the actual VarInt is 1 byte and we copy 4 fewer
-- bytes than reserved.
--
-- This trades one short memcpy per record for the cleaner "two-pass
-- size + copy" the legacy encoder did via @runPutS@. The break-even
-- is around 16 bytes / record; for typical Kafka records (hundreds
-- of bytes) the saving is large.
{-# INLINE pokeRecord #-}
pokeRecord :: Ptr Word8 -> Record -> IO (Ptr Word8)
pokeRecord !p Record{..} = do
  -- Reserve 5 bytes for the outer length VarInt; write the body
  -- starting at p + 5.
  let !bodyStart = p `plusPtr` 5
  -- Body: attrs (1) + tsDelta (VarLong) + offsetDelta (VarInt)
  --     + key (VarInt + bytes) + value (VarInt + bytes)
  --     + headers count (VarInt) + each header.
  pa <- pokeWord8 bodyStart 0   -- attributes byte (always 0 in v2)
  pb <- pokeVarLong pa recordTimestampDelta
  pc <- pokeVarInt  pb recordOffsetDelta
  pd <- case recordKey of
          Nothing -> pokeVarInt pc (-1)
          Just k  -> do
            p' <- pokeVarInt pc (fromIntegral (BS.length k))
            pokeByteString p' k
  pe <- do
          p' <- pokeVarInt pd (fromIntegral (BS.length recordValue))
          pokeByteString p' recordValue
  pf <- pokeVarInt pe (fromIntegral (length recordHeaders))
  pg <- foldHeaders pf recordHeaders
  let !bodyEnd = pg
      !bodyLen = bodyEnd `minusPtr` bodyStart
  -- Write the actual VarInt length at p, then shift the body
  -- left if the VarInt is shorter than the 5 bytes we reserved.
  -- Use Wire's pokeVarInt which returns the new pointer.
  lenEnd <- pokeVarInt p (fromIntegral bodyLen :: Int32)
  let !lenWritten = lenEnd `minusPtr` p
      !shift_     = 5 - lenWritten
  if shift_ == 0
    then pure bodyEnd
    else do
      -- memmove the body left by 'shift'. We shift bytes that
      -- live at @bodyStart .. bodyEnd@ down to @lenEnd ..
      -- lenEnd + bodyLen@. dst < src, so a forward
      -- byte-by-byte copy is safe.
      memmoveLeft
        bodyStart   -- src: where the body currently lives
        lenEnd      -- dst: just past the actual VarInt length
        bodyLen
      pure (bodyEnd `plusPtr` (negate shift_))

{-# INLINE foldHeaders #-}
foldHeaders :: Ptr Word8 -> [RecordHeader] -> IO (Ptr Word8)
foldHeaders = go
  where
    go !p []     = pure p
    go !p (h:hs) = do
      p' <- pokeHeader p h
      go p' hs

{-# INLINE pokeHeader #-}
pokeHeader :: Ptr Word8 -> RecordHeader -> IO (Ptr Word8)
pokeHeader !p RecordHeader{..} = do
  pk <- pokeVarInt p (fromIntegral (BS.length headerKey))
  pk2 <- pokeByteString pk headerKey
  case headerValue of
    Nothing -> pokeVarInt pk2 (-1)
    Just v  -> do
      pv <- pokeVarInt pk2 (fromIntegral (BS.length v))
      pokeByteString pv v

----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

-- | Encode the v2 'Attributes' word the same way
-- "Kafka.Protocol.RecordBatch" does.
{-# INLINE encodeAttributes #-}
encodeAttributes :: Attributes -> Int16
encodeAttributes Attributes{..} =
  let !comp  = fromIntegral (Compression.codecId attrCompressionType) .&. 0x07
      !ts    = if attrTimestampType == LogAppendTime then 0x08 else 0x00
      !txn   = if attrIsTransactional   then 0x10 else 0x00
      !ctrl  = if attrIsControl         then 0x20 else 0x00
      !del   = if attrHasDeleteHorizon  then 0x40 else 0x00
  in comp .|. ts .|. txn .|. ctrl .|. del

-- | @memmoveLeft src dst n@ copies @n@ bytes from @src@ to @dst@,
-- where @dst < src@ (i.e. shifting the data left in the buffer).
-- Implemented as a forward byte-by-byte loop because @memcpy@ is
-- undefined when the regions overlap; for the small shifts we do
-- (1..4 bytes per record) the loop is fine.
{-# INLINE memmoveLeft #-}
memmoveLeft :: Ptr Word8 -> Ptr Word8 -> Int -> IO ()
memmoveLeft !src !dst !n = go 0
  where
    go !i
      | i >= n = pure ()
      | otherwise = do
          b <- peekByte (src `plusPtr` i)
          pokeByte (dst `plusPtr` i) b
          go (i + 1)

{-# INLINE peekByte #-}
peekByte :: Ptr Word8 -> IO Word8
peekByte = peek

{-# INLINE pokeByte #-}
pokeByte :: Ptr Word8 -> Word8 -> IO ()
pokeByte = poke
