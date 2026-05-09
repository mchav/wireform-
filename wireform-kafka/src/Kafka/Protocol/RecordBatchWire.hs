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
    -- * Records-only encoder (used by the compressed path)
  , encodeRecordsWire
  , recordsWireSize
    -- * Direct-poke decoder
  , decodeRecordBatchWire
  ) where

import Control.Exception (SomeException, try)
import Data.Bits ((.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Data.Foldable (foldl')
import Data.Int (Int16, Int32, Int64)
import Data.Word (Word8)
import Foreign.ForeignPtr
  ( ForeignPtr, mallocForeignPtrBytes, withForeignPtr )
import Foreign.Ptr (Ptr, minusPtr, plusPtr)
import Foreign.Storable (peek, poke)
import GHC.IO (unsafePerformIO)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

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
  , peekInt16BE
  , peekInt32BE
  , peekInt64BE
  , peekWord32BE
  , peekWord8
  , peekVarInt
  , peekVarLong
  , peekByteStringSlice
  , ensureBytes
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
  recordBatchOverhead + recordsWireSize batchRecords

-- | Encode just the records section of a 'RecordBatch' (no batch
-- header, no CRC, no length prefix). Used by the compressed
-- encoder, which:
--
--   1. encodes the records via 'encodeRecordsWire',
--   2. compresses the resulting bytes,
--   3. wraps the compressed payload in a normal batch header.
--
-- Step 1 was previously the hot bottleneck for compressed
-- producers (it called 'runPutS' once per record); this entry
-- gives them the same single-allocation, single-pass shape the
-- uncompressed path enjoys.
{-# INLINEABLE encodeRecordsWire #-}
encodeRecordsWire :: V.Vector Record -> ByteString
encodeRecordsWire records = unsafePerformIO $ do
  let !sz = recordsWireSize records
  if sz == 0
    then pure BS.empty
    else do
      fp <- mallocForeignPtrBytes sz
      withForeignPtr fp $ \basePtr -> do
        endPtr <- pokeRecords basePtr records
        let !len = endPtr `minusPtr` basePtr
        pure (BSI.fromForeignPtr fp 0 len)

{-# INLINE recordsWireSize #-}
recordsWireSize :: V.Vector Record -> Int
recordsWireSize = V.foldl' (\acc r -> acc + recordWireSize r) 0

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

----------------------------------------------------------------------
-- Decoder
----------------------------------------------------------------------

-- | Direct-poke decoder for 'RecordBatch'. Reads the entire batch
-- in a single pass straight off the source 'ByteString' buffer
-- (held alive by its 'ForeignPtr'), with one mutable 'V.Vector'
-- allocation for the records.
--
-- Equivalent to 'Kafka.Protocol.RecordBatch.decodeRecordBatch' on
-- well-formed inputs (proven by the round-trip property in
-- 'Protocol.RecordBatchWireSpec'). Errors come back as @Left@ with
-- the same shape of message the legacy decoder produces, so callers
-- that pattern-match on the message text won't break.
--
-- == Per-record steady-state cost (GHC 9.6.4 -O1):
--
--   * legacy 'decodeRecordBatch' (100 records): ~89 µs / 890 ns/rec
--   * 'decodeRecordBatchWire'    (100 records): see
--     'Benchmarks.HotPath' / WireDecode for the latest figure;
--     the win comes from one mutable vector + one CRC32C raw-ptr
--     call instead of N 'getByteString' wrappers.
{-# INLINEABLE decodeRecordBatchWire #-}
decodeRecordBatchWire :: ByteString -> Either String RecordBatch
decodeRecordBatchWire bs = unsafePerformIO $ do
  let (fp, off, len) = BSI.toForeignPtr bs
  withForeignPtr fp $ \basePtr -> do
    let !startPtr = basePtr `plusPtr` off
        !endPtr   = startPtr `plusPtr` len
    -- Catch every exception because both 'WireError' (from the Wire
    -- primitives) and 'IOError' (from 'errOut' below) can fire.
    r <- try (peekBatch fp basePtr startPtr endPtr)
           :: IO (Either SomeException RecordBatch)
    case r of
      Left e   -> pure (Left (show e))
      Right rb -> pure (Right rb)

-- | The source 'ForeignPtr' threads down into 'peekRecord' so
-- per-record key + value + header reads can hand back
-- /zero-copy slices/ over the input buffer instead of memcpy'ing
-- each one. The slices keep the source 'ForeignPtr' alive
-- through their own reference, so leaving the
-- 'withForeignPtr' scope is safe.
{-# INLINE peekBatch #-}
peekBatch
  :: ForeignPtr Word8
  -> Ptr Word8           -- ^ basePtr (start of the source buffer in this scope)
  -> Ptr Word8           -- ^ start of this batch
  -> Ptr Word8           -- ^ end of buffer
  -> IO RecordBatch
peekBatch fp basePtr p endPtr = do
  ensureBytes p endPtr 21 "RecordBatch header"
  (baseOffset, p1) <- peekInt64BE p endPtr
  (lenValue,   p2) <- peekInt32BE p1 endPtr
  (leaderEp,   p3) <- peekInt32BE p2 endPtr
  (magic,      p4) <- peekWord8   p3 endPtr
  if fromIntegral magic /= magicV2
    then errOut ("Unsupported magic byte: " ++ show magic)
    else do
      (storedCrc, p5) <- peekWord32BE p4 endPtr
      let !bodyStart = p5
          !bodyLen   = fromIntegral lenValue - 4 - 1 - 4
              -- minus partition leader epoch (4) + magic (1) + crc (4)
          !bodyEnd   = bodyStart `plusPtr` bodyLen
      ensureBytes bodyStart endPtr bodyLen "RecordBatch body"
      computedCrc <- CRC.crc32cPtr bodyStart bodyLen
      if computedCrc /= storedCrc
        then errOut ("CRC mismatch: stored=" ++ show storedCrc
                       ++ ", computed=" ++ show computedCrc)
        else do
          (attrsW, q1) <- peekInt16BE bodyStart bodyEnd
          attrs <- case decodeAttributes attrsW of
            Left e  -> errOut e
            Right a -> pure a
          (lastDelta, q2) <- peekInt32BE q1 bodyEnd
          (baseTs,    q3) <- peekInt64BE q2 bodyEnd
          (maxTs,     q4) <- peekInt64BE q3 bodyEnd
          (pid,       q5) <- peekInt64BE q4 bodyEnd
          (pep,       q6) <- peekInt16BE q5 bodyEnd
          (baseSeq,   q7) <- peekInt32BE q6 bodyEnd
          (recCount,  q8) <- peekInt32BE q7 bodyEnd
          let !n = fromIntegral recCount :: Int
          records <-
            if n <= 0
              then pure V.empty
              else readRecords fp basePtr n q8 bodyEnd
          pure RecordBatch
            { batchBaseOffset           = baseOffset
            , batchPartitionLeaderEpoch = leaderEp
            , batchAttributes           = attrs
            , batchLastOffsetDelta      = lastDelta
            , batchBaseTimestamp        = baseTs
            , batchMaxTimestamp         = maxTs
            , batchProducerId           = pid
            , batchProducerEpoch        = pep
            , batchBaseSequence         = baseSeq
            , batchRecords              = records
            }

-- | Inline copy of 'RB.decodeAttributes' so we don't pay an
-- allocator round-trip for the @Either@ on the hot path. The
-- legacy module currently keeps the helper private; once we
-- export it from "Kafka.Protocol.RecordBatch" this can be removed.
{-# INLINE decodeAttributes #-}
decodeAttributes :: Int16 -> Either String Attributes
decodeAttributes attrs =
  let !cId    = fromIntegral (attrs .&. 0x07)
      !ts     = if (attrs .&. 0x08) /= 0 then LogAppendTime else CreateTime
      !txn    = (attrs .&. 0x10) /= 0
      !ctrl   = (attrs .&. 0x20) /= 0
      !del    = (attrs .&. 0x40) /= 0
      !codec  = case (cId :: Int) of
        0 -> Compression.NoCompression
        1 -> Compression.Gzip
        2 -> Compression.Snappy
        3 -> Compression.Lz4
        4 -> Compression.Zstd
        _ -> Compression.NoCompression
  in Right $ Attributes codec ts txn ctrl del

-- | Decode @n@ records into a freshly allocated 'V.Vector' using
-- one mutable buffer. Beats the legacy
-- @replicateM n decodeRecord >>= return . V.fromList@ by skipping
-- the intermediate list / array fusion handshake.
{-# INLINE readRecords #-}
readRecords
  :: ForeignPtr Word8
  -> Ptr Word8                -- ^ basePtr (start of source buffer)
  -> Int -> Ptr Word8 -> Ptr Word8 -> IO (V.Vector Record)
readRecords fp basePtr n start endPtr = do
  mv <- MV.unsafeNew n
  let go !i !p
        | i >= n = pure ()
        | otherwise = do
            (rec, p') <- peekRecord fp basePtr p endPtr
            MV.unsafeWrite mv i rec
            go (i + 1) p'
  go 0 start
  V.unsafeFreeze mv

-- | Per-record peek that hands back zero-copy slices of the
-- source buffer for the key, value, and each header's bytes
-- (rather than memcpy'ing each into a fresh 'ByteString'). For
-- a typical 50 MiB fetch response with thousands of records
-- that's tens of MiB of memcpy / allocation eliminated; the
-- record's lifetime keeps the source 'ForeignPtr' alive.
{-# INLINE peekRecord #-}
peekRecord
  :: ForeignPtr Word8
  -> Ptr Word8
  -> Ptr Word8
  -> Ptr Word8
  -> IO (Record, Ptr Word8)
peekRecord fp basePtr p endPtr = do
  -- Outer length VarInt (we don't need it, but it has to be
  -- consumed so the cursor advances past it).
  (_len, p0)        <- peekVarInt p endPtr
  -- Record attributes byte (always 0 in v2; we tolerate any value).
  (_attrs, p1)      <- peekWord8 p0 endPtr
  (tsDelta, p2)     <- peekVarLong p1 endPtr
  (offDelta, p3)    <- peekVarInt p2 endPtr
  (mKey, p4)        <- peekVarBytesSlice fp basePtr p3 endPtr
  (mVal, p5)        <- peekVarBytesSlice fp basePtr p4 endPtr
  -- The legacy decoder turns null (length=-1) value into BS.empty;
  -- preserve that.
  let !value = case mVal of
                  Nothing -> BS.empty
                  Just v  -> v
  (hdrCount, p6)    <- peekVarInt p5 endPtr
  (hdrs, p7)        <- readHeaders fp basePtr (fromIntegral hdrCount) p6 endPtr
  pure ( Record
           { recordTimestampDelta = tsDelta
           , recordOffsetDelta    = offDelta
           , recordKey            = mKey
           , recordValue          = value
           , recordHeaders        = hdrs
           }
       , p7
       )

-- | Length-prefixed bytes, returned as a zero-copy slice over
-- the source 'ForeignPtr'.
{-# INLINE peekVarBytesSlice #-}
peekVarBytesSlice
  :: ForeignPtr Word8
  -> Ptr Word8
  -> Ptr Word8
  -> Ptr Word8
  -> IO (Maybe ByteString, Ptr Word8)
peekVarBytesSlice fp basePtr p endPtr = do
  (n, p1) <- peekVarInt p endPtr
  if n < 0
    then pure (Nothing, p1)
    else do
      let !ni = fromIntegral n
      (bs, p2) <- peekByteStringSlice fp basePtr p1 endPtr ni
      pure (Just bs, p2)

{-# INLINE readHeaders #-}
readHeaders
  :: ForeignPtr Word8
  -> Ptr Word8
  -> Int -> Ptr Word8 -> Ptr Word8 -> IO ([RecordHeader], Ptr Word8)
readHeaders _ _ 0 !p _ = pure ([], p)
readHeaders fp basePtr n !p endPtr = do
  -- Build a list right-to-left so we don't reverse afterwards;
  -- each header is small enough that a list is fine here.
  (hs, p') <- go n p []
  pure (reverse hs, p')
  where
    go 0 q acc = pure (acc, q)
    go !k q acc = do
      (h, q') <- peekHeader fp basePtr q endPtr
      go (k - 1) q' (h : acc)

{-# INLINE peekHeader #-}
peekHeader
  :: ForeignPtr Word8
  -> Ptr Word8
  -> Ptr Word8
  -> Ptr Word8
  -> IO (RecordHeader, Ptr Word8)
peekHeader fp basePtr p endPtr = do
  (kLen, p1) <- peekVarInt p endPtr
  let !ki = fromIntegral kLen
  (k, p2) <- peekByteStringSlice fp basePtr p1 endPtr ki
  (mv, p3) <- peekVarBytesSlice fp basePtr p2 endPtr
  pure (RecordHeader { headerKey = k, headerValue = mv }, p3)

errOut :: String -> IO a
errOut = ioError . userError
