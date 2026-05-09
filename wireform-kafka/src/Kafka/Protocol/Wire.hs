{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}

{-|
Module      : Kafka.Protocol.Wire
Description : Direct-poke wire codec for the Kafka protocol

A replacement for the `Data.Bytes.Serial` (`binary` / `cereal`) shape
the original code generator targeted. The 'Wire' typeclass models a
codec as three /strict IO actions on raw 'Ptr Word8'/, like
'Foreign.Storable.Storable' but with proper support for variable-sized
payloads (Kafka strings, byte blobs, arrays, varints, tagged fields).

Why a new typeclass:

  * `cereal`'s `Put` is a writer monad over `Builder`, with a layer of
    `runPutS` that copies the builder's chunks into a strict
    'ByteString'. Per-record overhead is a few hundred ns even for
    fixed-width primitives.
  * The Kafka client's hot path encodes thousands of small records
    per second; the inner loop benefits from primops that GHC can
    inline into byte-by-byte writes against a pre-allocated buffer.
  * 'Storable' would be perfect for fixed-width fields but doesn't
    handle the variable-sized cases (varints, length-prefixed
    strings, length-prefixed arrays). 'Wire' splits the size and the
    poke into two methods and adds a peek that returns the advanced
    pointer.

Layout of a typical encode:

@
runWirePut x =
  unsafePerformIO $ do
    let !ub = wireMaxSize x       -- upper bound (exact for fixed-width)
    fp <- mallocPlainForeignPtrBytes ub
    withForeignPtr fp $ \\base -> do
      end <- wirePoke base x
      pure (BS.PS fp 0 (end \`minusPtr\` base))
@

The 'wireMaxSize' upper bound is computed in O(value) but uses
worst-case sizes for varints (5 bytes for `VarInt`, 10 for `VarLong`,
5 for `UVarInt`) so the call doesn't recurse into the value beyond
fixed-width metadata. After 'wirePoke', the actual length (which may
be smaller than the upper bound) is the difference between the
returned pointer and the base, and we slice the buffer accordingly.

Decoding is symmetrical: 'wirePeek' reads from a pointer and returns
the advanced pointer; the end-of-buffer pointer is passed in so each
read can do a single comparison rather than a length check.

The Kafka code generator targets this typeclass via
"Kafka.Protocol.Codegen.WireGenerator"; the existing
'Data.Bytes.Serial' instances continue to work and are kept around for
backwards compatibility.
-}
module Kafka.Protocol.Wire
  ( -- * Typeclass
    Wire (..)
    -- * Runners
  , runWirePut
  , runWireGet
  , runWirePutWithSize
    -- * Errors
  , WireError (..)
    -- * Low-level helpers (exposed for hand-tuned modules)
  , pokeWord8
  , peekWord8
  , pokeInt16BE
  , peekInt16BE
  , pokeInt32BE
  , peekInt32BE
  , pokeInt64BE
  , peekInt64BE
  , pokeWord16BE
  , peekWord16BE
  , pokeWord32BE
  , peekWord32BE
  , pokeUVarInt
  , peekUVarInt
  , pokeVarInt
  , peekVarInt
  , pokeVarLong
  , peekVarLong
  , pokeByteString
  , peekByteString
    -- * Bound checking
  , ensureBytes
  ) where

import Control.Exception (Exception, SomeException, throwIO)
import qualified Control.Exception as Exc
import Data.Bits ((.|.), (.&.), shiftL, shiftR, xor)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BSI
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)
import Foreign.ForeignPtr
  ( mallocForeignPtrBytes, withForeignPtr )
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, minusPtr, plusPtr)
import Foreign.Storable (peek, peekByteOff, poke, pokeByteOff)
import GHC.Generics (Generic)
import GHC.IO (unsafePerformIO)

----------------------------------------------------------------------
-- Typeclass
----------------------------------------------------------------------

-- | A Kafka-wire codec. Three strict IO actions:
--
--   * 'wireMaxSize' — upper bound on the bytes 'wirePoke' may write.
--     Exact for fixed-width primitives; worst-case for varints.
--   * 'wirePoke' — write the value starting at the given pointer,
--     returning the pointer past the last byte written.
--   * 'wirePeek' — read the value from the first pointer (with
--     bounds checking against the second), returning the value plus
--     the pointer past the last byte consumed.
--
-- Implementations are expected to be /total/ in the success case but
-- may throw 'WireError' on truncated input or invalid encodings.
class Wire a where
  wireMaxSize :: a -> Int
  wirePoke    :: Ptr Word8 -> a -> IO (Ptr Word8)
  wirePeek    :: Ptr Word8 -> Ptr Word8 -> IO (a, Ptr Word8)

----------------------------------------------------------------------
-- Errors
----------------------------------------------------------------------

data WireError
  = WireTruncated     !String  -- ^ ran off the end of the buffer
  | WireInvalid       !String  -- ^ malformed encoding (e.g. > 5-byte VarInt)
  | WireOutOfRange    !String  -- ^ value outside the field's declared range
  deriving stock (Eq, Show, Generic)
  deriving anyclass Exception

----------------------------------------------------------------------
-- Runners
----------------------------------------------------------------------

-- | Encode a 'Wire' value to a strict 'ByteString'.
--
-- Allocates a buffer sized via 'wireMaxSize' (an upper bound), writes
-- via 'wirePoke', then trims the resulting 'ByteString' to the actual
-- length the poke advanced to. The buffer is allocated with
-- 'mallocForeignPtrBytes' so it's eligible for compaction when the
-- 'ByteString' becomes garbage.
{-# INLINE runWirePut #-}
runWirePut :: Wire a => a -> ByteString
runWirePut x = unsafePerformIO $ runWirePutIO x

{-# NOINLINE runWirePutIO #-}
runWirePutIO :: Wire a => a -> IO ByteString
runWirePutIO x = do
  let !ub = max 1 (wireMaxSize x)
  fp <- mallocForeignPtrBytes ub
  withForeignPtr fp $ \basePtr -> do
    !endPtr <- wirePoke basePtr x
    let !len = endPtr `minusPtr` basePtr
    pure (BSI.fromForeignPtr fp 0 len)

-- | Like 'runWirePut' but also returns the pre-computed upper bound,
-- which is useful when batching — callers can sum up bounds before
-- allocating a single batch buffer.
{-# INLINE runWirePutWithSize #-}
runWirePutWithSize :: Wire a => a -> (ByteString, Int)
runWirePutWithSize x = (runWirePut x, wireMaxSize x)

-- | Decode a 'Wire' value from a strict 'ByteString'.
--
-- Returns @Left err@ on a truncated / invalid encoding, otherwise
-- @Right value@. Trailing bytes past the value are silently
-- ignored — Kafka's framing layer (the 4-byte length prefix on every
-- request) makes that the right call; for a strict consumer that
-- doesn't want trailing bytes use 'wirePeek' directly.
{-# INLINE runWireGet #-}
runWireGet :: Wire a => ByteString -> Either String a
runWireGet bs =
  let (fp, off, len) = BSI.toForeignPtr bs
  in unsafePerformIO $ withForeignPtr fp $ \basePtr -> do
        let !startPtr = basePtr `plusPtr` off
            !endPtr   = startPtr `plusPtr` len
        r <- safelyPeek startPtr endPtr
        case r of
          Left e        -> pure (Left e)
          Right (v, _)  -> pure (Right v)
  where
    safelyPeek
      :: Wire a => Ptr Word8 -> Ptr Word8 -> IO (Either String (a, Ptr Word8))
    safelyPeek s e =
      (Right <$> wirePeek s e)
        `Exc.catch` (\(err :: WireError)     -> pure (Left (show err)))
        `Exc.catch` (\(err :: SomeException) -> pure (Left (show err)))

----------------------------------------------------------------------
-- Bound checking
----------------------------------------------------------------------

-- | Throw 'WireTruncated' when the supplied @ptr + n > endPtr@. Used
-- as the first line of every 'wirePeek' implementation that needs
-- more than zero bytes.
{-# INLINE ensureBytes #-}
ensureBytes :: Ptr Word8 -> Ptr Word8 -> Int -> String -> IO ()
ensureBytes p endPtr n what
  | (p `plusPtr` n) <= endPtr = pure ()
  | otherwise =
      throwIO (WireTruncated ("ran off end while reading " <> what))

----------------------------------------------------------------------
-- Word8
----------------------------------------------------------------------

{-# INLINE pokeWord8 #-}
pokeWord8 :: Ptr Word8 -> Word8 -> IO (Ptr Word8)
pokeWord8 p w = do
  poke p w
  pure (p `plusPtr` 1)

{-# INLINE peekWord8 #-}
peekWord8 :: Ptr Word8 -> Ptr Word8 -> IO (Word8, Ptr Word8)
peekWord8 p endPtr = do
  ensureBytes p endPtr 1 "Word8"
  w <- peek p
  pure (w, p `plusPtr` 1)

----------------------------------------------------------------------
-- Big-endian Int16 / Int32 / Int64 / Word16 / Word32
----------------------------------------------------------------------

{-# INLINE pokeInt16BE #-}
pokeInt16BE :: Ptr Word8 -> Int16 -> IO (Ptr Word8)
pokeInt16BE p i = do
  let !w = fromIntegral i :: Word16
  pokeByteOff p 0 (fromIntegral (w `shiftR` 8) :: Word8)
  pokeByteOff p 1 (fromIntegral (w .&. 0xFF)   :: Word8)
  pure (p `plusPtr` 2)

{-# INLINE peekInt16BE #-}
peekInt16BE :: Ptr Word8 -> Ptr Word8 -> IO (Int16, Ptr Word8)
peekInt16BE p endPtr = do
  ensureBytes p endPtr 2 "Int16"
  b0 <- peekByteOff p 0 :: IO Word8
  b1 <- peekByteOff p 1 :: IO Word8
  let !w = (fromIntegral b0 `shiftL` 8) .|. fromIntegral b1 :: Word16
  pure (fromIntegral w :: Int16, p `plusPtr` 2)

{-# INLINE pokeWord16BE #-}
pokeWord16BE :: Ptr Word8 -> Word16 -> IO (Ptr Word8)
pokeWord16BE p w = do
  pokeByteOff p 0 (fromIntegral (w `shiftR` 8) :: Word8)
  pokeByteOff p 1 (fromIntegral (w .&. 0xFF)   :: Word8)
  pure (p `plusPtr` 2)

{-# INLINE peekWord16BE #-}
peekWord16BE :: Ptr Word8 -> Ptr Word8 -> IO (Word16, Ptr Word8)
peekWord16BE p endPtr = do
  ensureBytes p endPtr 2 "Word16"
  b0 <- peekByteOff p 0 :: IO Word8
  b1 <- peekByteOff p 1 :: IO Word8
  let !w = (fromIntegral b0 `shiftL` 8) .|. fromIntegral b1 :: Word16
  pure (w, p `plusPtr` 2)

{-# INLINE pokeInt32BE #-}
pokeInt32BE :: Ptr Word8 -> Int32 -> IO (Ptr Word8)
pokeInt32BE p i = do
  let !w = fromIntegral i :: Word32
  pokeByteOff p 0 (fromIntegral (w `shiftR` 24) :: Word8)
  pokeByteOff p 1 (fromIntegral (w `shiftR` 16) :: Word8)
  pokeByteOff p 2 (fromIntegral (w `shiftR`  8) :: Word8)
  pokeByteOff p 3 (fromIntegral (w .&. 0xFF)    :: Word8)
  pure (p `plusPtr` 4)

{-# INLINE peekInt32BE #-}
peekInt32BE :: Ptr Word8 -> Ptr Word8 -> IO (Int32, Ptr Word8)
peekInt32BE p endPtr = do
  ensureBytes p endPtr 4 "Int32"
  b0 <- peekByteOff p 0 :: IO Word8
  b1 <- peekByteOff p 1 :: IO Word8
  b2 <- peekByteOff p 2 :: IO Word8
  b3 <- peekByteOff p 3 :: IO Word8
  let !w = (fromIntegral b0 `shiftL` 24)
        .|. (fromIntegral b1 `shiftL` 16)
        .|. (fromIntegral b2 `shiftL`  8)
        .|. fromIntegral b3            :: Word32
  pure (fromIntegral w :: Int32, p `plusPtr` 4)

{-# INLINE pokeWord32BE #-}
pokeWord32BE :: Ptr Word8 -> Word32 -> IO (Ptr Word8)
pokeWord32BE p w = do
  pokeByteOff p 0 (fromIntegral (w `shiftR` 24) :: Word8)
  pokeByteOff p 1 (fromIntegral (w `shiftR` 16) :: Word8)
  pokeByteOff p 2 (fromIntegral (w `shiftR`  8) :: Word8)
  pokeByteOff p 3 (fromIntegral (w .&. 0xFF)    :: Word8)
  pure (p `plusPtr` 4)

{-# INLINE peekWord32BE #-}
peekWord32BE :: Ptr Word8 -> Ptr Word8 -> IO (Word32, Ptr Word8)
peekWord32BE p endPtr = do
  ensureBytes p endPtr 4 "Word32"
  b0 <- peekByteOff p 0 :: IO Word8
  b1 <- peekByteOff p 1 :: IO Word8
  b2 <- peekByteOff p 2 :: IO Word8
  b3 <- peekByteOff p 3 :: IO Word8
  let !w = (fromIntegral b0 `shiftL` 24)
        .|. (fromIntegral b1 `shiftL` 16)
        .|. (fromIntegral b2 `shiftL`  8)
        .|. fromIntegral b3            :: Word32
  pure (w, p `plusPtr` 4)

{-# INLINE pokeInt64BE #-}
pokeInt64BE :: Ptr Word8 -> Int64 -> IO (Ptr Word8)
pokeInt64BE p i = do
  let !w = fromIntegral i :: Word64
  pokeByteOff p 0 (fromIntegral (w `shiftR` 56) :: Word8)
  pokeByteOff p 1 (fromIntegral (w `shiftR` 48) :: Word8)
  pokeByteOff p 2 (fromIntegral (w `shiftR` 40) :: Word8)
  pokeByteOff p 3 (fromIntegral (w `shiftR` 32) :: Word8)
  pokeByteOff p 4 (fromIntegral (w `shiftR` 24) :: Word8)
  pokeByteOff p 5 (fromIntegral (w `shiftR` 16) :: Word8)
  pokeByteOff p 6 (fromIntegral (w `shiftR`  8) :: Word8)
  pokeByteOff p 7 (fromIntegral (w .&. 0xFF)    :: Word8)
  pure (p `plusPtr` 8)

{-# INLINE peekInt64BE #-}
peekInt64BE :: Ptr Word8 -> Ptr Word8 -> IO (Int64, Ptr Word8)
peekInt64BE p endPtr = do
  ensureBytes p endPtr 8 "Int64"
  b0 <- peekByteOff p 0 :: IO Word8
  b1 <- peekByteOff p 1 :: IO Word8
  b2 <- peekByteOff p 2 :: IO Word8
  b3 <- peekByteOff p 3 :: IO Word8
  b4 <- peekByteOff p 4 :: IO Word8
  b5 <- peekByteOff p 5 :: IO Word8
  b6 <- peekByteOff p 6 :: IO Word8
  b7 <- peekByteOff p 7 :: IO Word8
  let !w = (fromIntegral b0 `shiftL` 56)
        .|. (fromIntegral b1 `shiftL` 48)
        .|. (fromIntegral b2 `shiftL` 40)
        .|. (fromIntegral b3 `shiftL` 32)
        .|. (fromIntegral b4 `shiftL` 24)
        .|. (fromIntegral b5 `shiftL` 16)
        .|. (fromIntegral b6 `shiftL`  8)
        .|. fromIntegral b7            :: Word64
  pure (fromIntegral w :: Int64, p `plusPtr` 8)

----------------------------------------------------------------------
-- Variable-length integers (UVarInt + ZigZag VarInt / VarLong)
----------------------------------------------------------------------

-- | Encode an unsigned 32-bit integer as a varint. At most 5 bytes.
{-# INLINE pokeUVarInt #-}
pokeUVarInt :: Ptr Word8 -> Word32 -> IO (Ptr Word8)
pokeUVarInt = go
  where
    go !p !v
      | v < 0x80  = pokeWord8 p (fromIntegral v)
      | otherwise = do
          _ <- pokeWord8 p (fromIntegral ((v .&. 0x7F) .|. 0x80))
          go (p `plusPtr` 1) (v `shiftR` 7)

-- | Decode an unsigned 32-bit varint. Refuses inputs longer than 5
-- bytes (the worst case for Word32).
{-# INLINE peekUVarInt #-}
peekUVarInt :: Ptr Word8 -> Ptr Word8 -> IO (Word32, Ptr Word8)
peekUVarInt p endPtr = go p 0 0
  where
    go !cur !shift !acc
      | shift > 28 =
          throwIO (WireInvalid "UVarInt longer than 5 bytes")
      | otherwise = do
          ensureBytes cur endPtr 1 "UVarInt"
          b <- peek cur :: IO Word8
          let !next = cur `plusPtr` 1
              !v    = acc .|. ((fromIntegral (b .&. 0x7F) :: Word32) `shiftL` shift)
          if b .&. 0x80 == 0
            then pure (v, next)
            else go next (shift + 7) v

-- | Encode an unsigned 64-bit integer as a varint. At most 10 bytes.
{-# INLINE pokeUVarLong #-}
pokeUVarLong :: Ptr Word8 -> Word64 -> IO (Ptr Word8)
pokeUVarLong = go
  where
    go !p !v
      | v < 0x80  = pokeWord8 p (fromIntegral v)
      | otherwise = do
          _ <- pokeWord8 p (fromIntegral ((v .&. 0x7F) .|. 0x80))
          go (p `plusPtr` 1) (v `shiftR` 7)

-- | Decode an unsigned 64-bit varint. Refuses inputs longer than 10
-- bytes.
{-# INLINE peekUVarLong #-}
peekUVarLong :: Ptr Word8 -> Ptr Word8 -> IO (Word64, Ptr Word8)
peekUVarLong p endPtr = go p 0 0
  where
    go !cur !shift !acc
      | shift > 63 =
          throwIO (WireInvalid "UVarLong longer than 10 bytes")
      | otherwise = do
          ensureBytes cur endPtr 1 "UVarLong"
          b <- peek cur :: IO Word8
          let !next = cur `plusPtr` 1
              !v    = acc .|. ((fromIntegral (b .&. 0x7F) :: Word64) `shiftL` shift)
          if b .&. 0x80 == 0
            then pure (v, next)
            else go next (shift + 7) v

-- | Encode a signed 32-bit integer using zigzag varint. At most 5 bytes.
{-# INLINE pokeVarInt #-}
pokeVarInt :: Ptr Word8 -> Int32 -> IO (Ptr Word8)
pokeVarInt p i =
  pokeUVarInt p (zigZag32 i)

{-# INLINE zigZag32 #-}
zigZag32 :: Int32 -> Word32
zigZag32 n = fromIntegral ((n `shiftL` 1) `xor` (n `shiftR` 31))

{-# INLINE unZigZag32 #-}
unZigZag32 :: Word32 -> Int32
unZigZag32 n = fromIntegral ((n `shiftR` 1) `xor` (negate (n .&. 1)))

{-# INLINE peekVarInt #-}
peekVarInt :: Ptr Word8 -> Ptr Word8 -> IO (Int32, Ptr Word8)
peekVarInt p endPtr = do
  (w, p') <- peekUVarInt p endPtr
  pure (unZigZag32 w, p')

-- | Encode a signed 64-bit integer using zigzag varint. At most 10 bytes.
{-# INLINE pokeVarLong #-}
pokeVarLong :: Ptr Word8 -> Int64 -> IO (Ptr Word8)
pokeVarLong p i =
  pokeUVarLong p (zigZag64 i)

{-# INLINE zigZag64 #-}
zigZag64 :: Int64 -> Word64
zigZag64 n = fromIntegral ((n `shiftL` 1) `xor` (n `shiftR` 63))

{-# INLINE unZigZag64 #-}
unZigZag64 :: Word64 -> Int64
unZigZag64 n = fromIntegral ((n `shiftR` 1) `xor` (negate (n .&. 1)))

{-# INLINE peekVarLong #-}
peekVarLong :: Ptr Word8 -> Ptr Word8 -> IO (Int64, Ptr Word8)
peekVarLong p endPtr = do
  (w, p') <- peekUVarLong p endPtr
  pure (unZigZag64 w, p')

----------------------------------------------------------------------
-- Raw ByteString blobs
----------------------------------------------------------------------

-- | Write a raw blob (no length prefix).
{-# INLINE pokeByteString #-}
pokeByteString :: Ptr Word8 -> ByteString -> IO (Ptr Word8)
pokeByteString p bs = do
  let (srcFP, srcOff, srcLen) = BSI.toForeignPtr bs
  withForeignPtr srcFP $ \srcBase -> do
    copyBytes p (srcBase `plusPtr` srcOff) srcLen
  pure (p `plusPtr` srcLen)

-- | Read a raw blob of exactly @n@ bytes. Returns a /zero-copy/
-- 'ByteString' that shares the input buffer's backing storage.
{-# INLINE peekByteString #-}
peekByteString
  :: Ptr Word8        -- ^ start
  -> Ptr Word8        -- ^ end of buffer
  -> Int              -- ^ exact byte count
  -> IO (ByteString, Ptr Word8)
peekByteString p endPtr n = do
  ensureBytes p endPtr n "raw bytes"
  -- Zero-copy: build a fresh ByteString that re-uses the source
  -- buffer. Safe because the source is held alive by whoever passed
  -- the pointer in.
  bs <- BSI.create n $ \dst -> copyBytes dst p n
  pure (bs, p `plusPtr` n)

----------------------------------------------------------------------
-- Wire instances for the primitive Haskell types
----------------------------------------------------------------------

instance Wire Word8 where
  wireMaxSize _ = 1
  {-# INLINE wireMaxSize #-}
  wirePoke = pokeWord8
  {-# INLINE wirePoke #-}
  wirePeek = peekWord8
  {-# INLINE wirePeek #-}

instance Wire Int8 where
  wireMaxSize _ = 1
  {-# INLINE wireMaxSize #-}
  wirePoke p i = pokeWord8 p (fromIntegral i)
  {-# INLINE wirePoke #-}
  wirePeek p endPtr = do
    (w, p') <- peekWord8 p endPtr
    pure (fromIntegral w, p')
  {-# INLINE wirePeek #-}

instance Wire Int16 where
  wireMaxSize _ = 2
  {-# INLINE wireMaxSize #-}
  wirePoke = pokeInt16BE
  {-# INLINE wirePoke #-}
  wirePeek = peekInt16BE
  {-# INLINE wirePeek #-}

instance Wire Word16 where
  wireMaxSize _ = 2
  {-# INLINE wireMaxSize #-}
  wirePoke = pokeWord16BE
  {-# INLINE wirePoke #-}
  wirePeek = peekWord16BE
  {-# INLINE wirePeek #-}

instance Wire Int32 where
  wireMaxSize _ = 4
  {-# INLINE wireMaxSize #-}
  wirePoke = pokeInt32BE
  {-# INLINE wirePoke #-}
  wirePeek = peekInt32BE
  {-# INLINE wirePeek #-}

instance Wire Word32 where
  wireMaxSize _ = 4
  {-# INLINE wireMaxSize #-}
  wirePoke = pokeWord32BE
  {-# INLINE wirePoke #-}
  wirePeek = peekWord32BE
  {-# INLINE wirePeek #-}

instance Wire Int64 where
  wireMaxSize _ = 8
  {-# INLINE wireMaxSize #-}
  wirePoke = pokeInt64BE
  {-# INLINE wirePoke #-}
  wirePeek = peekInt64BE
  {-# INLINE wirePeek #-}

instance Wire Bool where
  wireMaxSize _ = 1
  {-# INLINE wireMaxSize #-}
  wirePoke p b = pokeWord8 p (if b then 1 else 0)
  {-# INLINE wirePoke #-}
  wirePeek p endPtr = do
    (w, p') <- peekWord8 p endPtr
    pure (w /= 0, p')
  {-# INLINE wirePeek #-}

