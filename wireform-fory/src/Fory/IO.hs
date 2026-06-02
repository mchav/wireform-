{-# LANGUAGE BangPatterns #-}

{- | An imperative, in-place encoder for the Apache Fory wire
format.

'Encoder' wraps a 'ForeignPtr Word8' that grows on demand; all
emit primitives are 'IO' actions that 'pokeByteOff' the next
bytes into the buffer and bump a position counter. The dedup
pools (meta-string pool, ref-id map, TypeDef pool) live in
'IORef' fields on the same record.

The pure encoder in 'Fory.Encode' wraps these IO primitives in
'unsafeDupablePerformIO'; the buffer is local to one
@encode@ call so the dupable variant is safe.

The point of this module is to keep the per-element overhead
low enough that bulk encode loops (lists, maps, struct
fields) hit @memcpy@-class speed. Compared with the old
writer-state-monad design, every emit drops from

  * a 3-tuple state-update plus a fresh 'Builder' closure

to

  * one or two 'pokeByteOff' calls into a contiguous buffer.
-}
module Fory.IO (
  Encoder,
  newEncoder,
  runEncoder,
  finalizeEncoder,

  -- * State accessors
  encOptions,
  metaStringLookup,
  metaStringRegister,
  refLookup,
  refRegister,
  structRefLookup,
  structRefRegister,
  typeDefLookup,
  typeDefRegister,

  -- * Emit primitives
  emitByte,
  emitBytes,
  emitWord16LE,
  emitWord32LE,
  emitWord64LE,
  emitInt16LE,
  emitInt32LE,
  emitInt64LE,
  emitFloat32LE,
  emitFloat64LE,
  emitVaruint32,
  emitVaruint64,
  emitVarint32,
  emitVarint64,
  emitVaruint36Small,

  -- * Raw-pointer primitives (for tight batch loops)
  withReservedRaw,
  pokeByteRaw,
  pokeWord16LERaw,
  pokeWord32LERaw,
  pokeWord64LERaw,
  pokeInt16LERaw,
  pokeInt32LERaw,
  pokeInt64LERaw,
  pokeFloat32LERaw,
  pokeFloat64LERaw,
  pokeVaruint32Raw,
  pokeVaruint64Raw,
  pokeVarint32Raw,
  pokeVarint64Raw,
) where

import Data.Bits (shiftL, shiftR, xor, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.ByteString.Unsafe qualified as BSU
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.IORef
import Data.Int (Int16, Int32, Int64)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.Text (Text)
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (pokeByteOff)
import Fory.Options qualified as Opt
import Fory.Value qualified as VV
import GHC.Float (castDoubleToWord64, castFloatToWord32)


-- ---------------------------------------------------------------------------
-- Encoder state
-- ---------------------------------------------------------------------------

{- | A mutable buffer + dedup-pool record for one encode pass.

Stored as several 'IORef's rather than one 'IORef' of a strict
record so that ensure-and-grow can update buffer / capacity
without touching the dedup pools (and vice versa). The
per-write cost is @readIORef + pokeByteOff + writeIORef@ on
the position field, plus an 'ensure' check that reads the
capacity ref.
-}
data Encoder = Encoder
  { encFp :: {-# UNPACK #-} !(IORef (ForeignPtr Word8))
  , encPos :: {-# UNPACK #-} !(IORef Int)
  , encCap :: {-# UNPACK #-} !(IORef Int)
  , -- Meta-string deduplication pool.
    encStrPool :: {-# UNPACK #-} !(IORef (HashMap Text Int))
  , encStrNext :: {-# UNPACK #-} !(IORef Int)
  , -- User-supplied 'RefVal' sharing keys -> wire ref id.
    encRefMap :: {-# UNPACK #-} !(IORef (IntMap Int))
  , encRefNext :: {-# UNPACK #-} !(IORef Int)
  , -- Structural-equality ref pool used when 'eoRefTracking' is on.
    encStructRefMap :: {-# UNPACK #-} !(IORef (HashMap VV.Value Int))
  , -- TypeDef pool keyed on (namespace, type name, field names).
    encTypeDefPool :: {-# UNPACK #-} !(IORef (HashMap (Text, Text, [Text]) Int))
  , encTypeDefNext :: {-# UNPACK #-} !(IORef Int)
  , encOptions :: !Opt.EncodeOptions
  }


-- ---------------------------------------------------------------------------
-- Lifecycle
-- ---------------------------------------------------------------------------

{- | Allocate a new encoder with a 256-byte initial capacity.
Tiny enough that the @mallocByteString@ is cheap; the
buffer doubles on the first emit that doesn't fit.
-}
newEncoder :: Opt.EncodeOptions -> IO Encoder
newEncoder !opts = do
  let !initCap = 256
  fp <- BSI.mallocByteString initCap
  fpR <- newIORef fp
  posR <- newIORef 0
  capR <- newIORef initCap
  spR <- newIORef HM.empty
  snR <- newIORef 0
  rmR <- newIORef IM.empty
  rnR <- newIORef 0
  srmR <- newIORef HM.empty
  tdpR <- newIORef HM.empty
  tdnR <- newIORef 0
  pure $!
    Encoder
      { encFp = fpR
      , encPos = posR
      , encCap = capR
      , encStrPool = spR
      , encStrNext = snR
      , encRefMap = rmR
      , encRefNext = rnR
      , encStructRefMap = srmR
      , encTypeDefPool = tdpR
      , encTypeDefNext = tdnR
      , encOptions = opts
      }


{- | Run an action against a fresh encoder, returning the
accumulated 'ByteString'.
-}
runEncoder :: Opt.EncodeOptions -> (Encoder -> IO ()) -> IO ByteString
runEncoder !opts action = do
  e <- newEncoder opts
  action e
  finalizeEncoder e


{- | Truncate the encoder buffer to the bytes actually written
and return them as a 'ByteString'. Subsequent emit operations
are not allowed.
-}
finalizeEncoder :: Encoder -> IO ByteString
finalizeEncoder !e = do
  fp <- readIORef (encFp e)
  pos <- readIORef (encPos e)
  pure $! BSI.BS fp pos


-- ---------------------------------------------------------------------------
-- Buffer growth
-- ---------------------------------------------------------------------------

{- | Ensure at least @need@ bytes of free space at the current
position. Doubles the buffer (at least) if not. Inlined into
the emit primitives so the fast path (capacity already
satisfied) is one comparison + early return.
-}
ensure :: Encoder -> Int -> IO ()
ensure !e !need = do
  pos <- readIORef (encPos e)
  cap <- readIORef (encCap e)
  let !want = pos + need
  if want <= cap
    then pure ()
    else do
      let !newCap = max (cap * 2) (want + 64)
      fpOld <- readIORef (encFp e)
      fpNew <- BSI.mallocByteString newCap
      withForeignPtr fpOld $ \pOld ->
        withForeignPtr fpNew $ \pNew ->
          copyBytes pNew pOld pos
      writeIORef (encFp e) fpNew
      writeIORef (encCap e) newCap


{- | Run an inner @Ptr-writing@ action. Reads the current base
pointer once, lets the inner action poke at @basePtr +
startPos ... startPos + reservedBytes@, and updates the
position to the value the inner action returned.

The inner action /must/ stay within @reservedBytes@ of
@startPos@.
-}
{-# INLINE withReserved #-}
withReserved :: Encoder -> Int -> (Ptr Word8 -> Int -> IO Int) -> IO ()
withReserved !e !need !action = do
  ensure e need
  fp <- readIORef (encFp e)
  pos <- readIORef (encPos e)
  newPos <- withForeignPtr fp $ \p -> action p pos
  writeIORef (encPos e) newPos


{- | Public alias of 'withReserved'. Reserves capacity once and
hands the inner action a raw 'Ptr Word8' base + start
offset; the inner action returns the new offset (which must
be within @startPos + reservedBytes@). This is the building
block for tight inner loops that want to amortise the
per-element @ensure / readIORef / writeIORef@ cost across an
entire batch.
-}
{-# INLINE withReservedRaw #-}
withReservedRaw
  :: Encoder -> Int -> (Ptr Word8 -> Int -> IO Int) -> IO ()
withReservedRaw = withReserved


-- ---------------------------------------------------------------------------
-- Emit primitives
-- ---------------------------------------------------------------------------

{-# INLINE emitByte #-}
emitByte :: Encoder -> Word8 -> IO ()
emitByte !e !b = withReserved e 1 $ \p pos -> do
  pokeByteOff p pos b
  pure (pos + 1)


{-# INLINE emitBytes #-}
emitBytes :: Encoder -> ByteString -> IO ()
emitBytes !e !bs = do
  let !n = BS.length bs
  withReserved e n $ \p pos -> do
    let (BSI.BS fpSrc lenSrc) = bs
    withForeignPtr fpSrc $ \pSrc ->
      copyBytes (p `plusPtr` pos) pSrc lenSrc
    pure (pos + n)


{-# INLINE emitWord16LE #-}
emitWord16LE :: Encoder -> Word16 -> IO ()
emitWord16LE !e !w = withReserved e 2 $ \p pos -> do
  pokeByteOff p pos w -- LE on x86-64 native
  pure (pos + 2)


{-# INLINE emitWord32LE #-}
emitWord32LE :: Encoder -> Word32 -> IO ()
emitWord32LE !e !w = withReserved e 4 $ \p pos -> do
  pokeByteOff p pos w
  pure (pos + 4)


{-# INLINE emitWord64LE #-}
emitWord64LE :: Encoder -> Word64 -> IO ()
emitWord64LE !e !w = withReserved e 8 $ \p pos -> do
  pokeByteOff p pos w
  pure (pos + 8)


{-# INLINE emitInt16LE #-}
emitInt16LE :: Encoder -> Int16 -> IO ()
emitInt16LE !e !n = withReserved e 2 $ \p pos -> do
  pokeByteOff p pos n
  pure (pos + 2)


{-# INLINE emitInt32LE #-}
emitInt32LE :: Encoder -> Int32 -> IO ()
emitInt32LE !e !n = withReserved e 4 $ \p pos -> do
  pokeByteOff p pos n
  pure (pos + 4)


{-# INLINE emitInt64LE #-}
emitInt64LE :: Encoder -> Int64 -> IO ()
emitInt64LE !e !n = withReserved e 8 $ \p pos -> do
  pokeByteOff p pos n
  pure (pos + 8)


{-# INLINE emitFloat32LE #-}
emitFloat32LE :: Encoder -> Float -> IO ()
emitFloat32LE !e !f = emitWord32LE e (castFloatToWord32 f)


{-# INLINE emitFloat64LE #-}
emitFloat64LE :: Encoder -> Double -> IO ()
emitFloat64LE !e !d = emitWord64LE e (castDoubleToWord64 d)


-- ---------------------------------------------------------------------------
-- Variable-length integers
-- ---------------------------------------------------------------------------

{-# INLINE emitVaruint32 #-}
emitVaruint32 :: Encoder -> Word32 -> IO ()
emitVaruint32 !e !v0 = withReserved e 5 $ \p pos -> goVu32 p pos v0
  where
    goVu32 !p !pos !v
      | v < 0x80 = do
          pokeByteOff p pos (fromIntegral v :: Word8)
          pure (pos + 1)
      | otherwise = do
          pokeByteOff
            p
            pos
            (fromIntegral (v .&. 0x7F) .|. 0x80 :: Word8)
          goVu32 p (pos + 1) (v `shiftR` 7)


{-# INLINE emitVaruint64 #-}
emitVaruint64 :: Encoder -> Word64 -> IO ()
emitVaruint64 !e !v0 =
  withReserved e 9 $ \p pos -> goVu64 (0 :: Int) p pos v0
  where
    -- Explicit 'Int' type sig on the loop counter — see
    -- 'pokeVaruint64Raw' for why (avoids GHC defaulting
    -- the @0@ literal to 'Integer').
    goVu64 :: Int -> Ptr Word8 -> Int -> Word64 -> IO Int
    goVu64 !i !p !pos !v
      | i >= 8 = do
          pokeByteOff p pos (fromIntegral v :: Word8)
          pure (pos + 1)
      | v < 0x80 = do
          pokeByteOff p pos (fromIntegral v :: Word8)
          pure (pos + 1)
      | otherwise = do
          pokeByteOff
            p
            pos
            (fromIntegral (v .&. 0x7F) .|. 0x80 :: Word8)
          goVu64 (i + 1) p (pos + 1) (v `shiftR` 7)


{-# INLINE emitVarint32 #-}
emitVarint32 :: Encoder -> Int32 -> IO ()
emitVarint32 !e !v =
  emitVaruint32
    e
    (fromIntegral ((v `shiftL` 1) `xor` (v `shiftR` 31)))


{-# INLINE emitVarint64 #-}
emitVarint64 :: Encoder -> Int64 -> IO ()
emitVarint64 !e !v =
  emitVaruint64
    e
    (fromIntegral ((v `shiftL` 1) `xor` (v `shiftR` 63)))


{-# INLINE emitVaruint36Small #-}
emitVaruint36Small :: Encoder -> Word64 -> IO ()
emitVaruint36Small = emitVaruint64


-- ---------------------------------------------------------------------------
-- Pool accessors (state)
-- ---------------------------------------------------------------------------

-- The pools are exposed as plain IO operations so 'Fory.Encode'
-- can branch on a hit / miss without going through the Encoder
-- record.

{-# INLINE metaStringLookup #-}
metaStringLookup :: Encoder -> Text -> IO (Maybe Int)
metaStringLookup !e !t = do
  m <- readIORef (encStrPool e)
  pure (HM.lookup t m)


{-# INLINE metaStringRegister #-}
metaStringRegister :: Encoder -> Text -> IO Int
metaStringRegister !e !t = do
  rid <- readIORef (encStrNext e)
  modifyIORef' (encStrPool e) (HM.insert t rid)
  writeIORef (encStrNext e) (rid + 1)
  pure rid


{-# INLINE refLookup #-}
refLookup :: Encoder -> Int -> IO (Maybe Int)
refLookup !e !k = do
  m <- readIORef (encRefMap e)
  pure (IM.lookup k m)


{-# INLINE refRegister #-}
refRegister :: Encoder -> Int -> IO Int
refRegister !e !k = do
  wid <- readIORef (encRefNext e)
  modifyIORef' (encRefMap e) (IM.insert k wid)
  writeIORef (encRefNext e) (wid + 1)
  pure wid


{-# INLINE structRefLookup #-}
structRefLookup :: Encoder -> VV.Value -> IO (Maybe Int)
structRefLookup !e !v = do
  m <- readIORef (encStructRefMap e)
  pure (HM.lookup v m)


{-# INLINE structRefRegister #-}
structRefRegister :: Encoder -> VV.Value -> IO Int
structRefRegister !e !v = do
  wid <- readIORef (encRefNext e)
  modifyIORef' (encStructRefMap e) (HM.insert v wid)
  writeIORef (encRefNext e) (wid + 1)
  pure wid


{-# INLINE typeDefLookup #-}
typeDefLookup :: Encoder -> (Text, Text, [Text]) -> IO (Maybe Int)
typeDefLookup !e !k = do
  m <- readIORef (encTypeDefPool e)
  pure (HM.lookup k m)


{-# INLINE typeDefRegister #-}
typeDefRegister :: Encoder -> (Text, Text, [Text]) -> IO Int
typeDefRegister !e !k = do
  idx <- readIORef (encTypeDefNext e)
  modifyIORef' (encTypeDefPool e) (HM.insert k idx)
  writeIORef (encTypeDefNext e) (idx + 1)
  pure idx


-- ---------------------------------------------------------------------------
-- Raw-pointer poke primitives
-- ---------------------------------------------------------------------------
--
-- These take a base 'Ptr Word8' and a current offset, write
-- the value, and return the new offset. They never touch the
-- 'Encoder' record's 'IORef's, so they're safe to use inside a
-- 'withReservedRaw' batch where the caller has already
-- reserved enough capacity.

{-# INLINE pokeByteRaw #-}
pokeByteRaw :: Ptr Word8 -> Int -> Word8 -> IO Int
pokeByteRaw !p !pos !b = do
  pokeByteOff p pos b
  pure (pos + 1)


{-# INLINE pokeWord16LERaw #-}
pokeWord16LERaw :: Ptr Word8 -> Int -> Word16 -> IO Int
pokeWord16LERaw !p !pos !w = do
  pokeByteOff p pos w
  pure (pos + 2)


{-# INLINE pokeWord32LERaw #-}
pokeWord32LERaw :: Ptr Word8 -> Int -> Word32 -> IO Int
pokeWord32LERaw !p !pos !w = do
  pokeByteOff p pos w
  pure (pos + 4)


{-# INLINE pokeWord64LERaw #-}
pokeWord64LERaw :: Ptr Word8 -> Int -> Word64 -> IO Int
pokeWord64LERaw !p !pos !w = do
  pokeByteOff p pos w
  pure (pos + 8)


{-# INLINE pokeInt16LERaw #-}
pokeInt16LERaw :: Ptr Word8 -> Int -> Int16 -> IO Int
pokeInt16LERaw !p !pos !n = do
  pokeByteOff p pos n
  pure (pos + 2)


{-# INLINE pokeInt32LERaw #-}
pokeInt32LERaw :: Ptr Word8 -> Int -> Int32 -> IO Int
pokeInt32LERaw !p !pos !n = do
  pokeByteOff p pos n
  pure (pos + 4)


{-# INLINE pokeInt64LERaw #-}
pokeInt64LERaw :: Ptr Word8 -> Int -> Int64 -> IO Int
pokeInt64LERaw !p !pos !n = do
  pokeByteOff p pos n
  pure (pos + 8)


{-# INLINE pokeFloat32LERaw #-}
pokeFloat32LERaw :: Ptr Word8 -> Int -> Float -> IO Int
pokeFloat32LERaw !p !pos !f = pokeWord32LERaw p pos (castFloatToWord32 f)


{-# INLINE pokeFloat64LERaw #-}
pokeFloat64LERaw :: Ptr Word8 -> Int -> Double -> IO Int
pokeFloat64LERaw !p !pos !d = pokeWord64LERaw p pos (castDoubleToWord64 d)


{-# INLINE pokeVaruint32Raw #-}
pokeVaruint32Raw :: Ptr Word8 -> Int -> Word32 -> IO Int
pokeVaruint32Raw !p !pos0 !v0 = go pos0 v0
  where
    go !pos !v
      | v < 0x80 = do
          pokeByteOff p pos (fromIntegral v :: Word8)
          pure (pos + 1)
      | otherwise = do
          pokeByteOff
            p
            pos
            (fromIntegral (v .&. 0x7F) .|. 0x80 :: Word8)
          go (pos + 1) (v `shiftR` 7)


{-# INLINE pokeVaruint64Raw #-}
pokeVaruint64Raw :: Ptr Word8 -> Int -> Word64 -> IO Int
pokeVaruint64Raw !p !pos0 !v0 = go (0 :: Int) pos0 v0
  where
    -- Explicit 'Int' type sig on the loop counter is
    -- important: without it, GHC defaults the @0@ literal
    -- to 'Integer' (boxed arbitrary-precision), giving a
    -- @\$wgo :: Integer -> Int# -> Word64# -> ...@ loop
    -- with a 3-case @IS x | IP x | IN x@ pattern-match per
    -- iteration. The 'Int' annotation lets strict-worker-
    -- wrapper unbox @i@ to @Int#@.
    go :: Int -> Int -> Word64 -> IO Int
    go !i !pos !v
      | i >= 8 = do
          pokeByteOff p pos (fromIntegral v :: Word8)
          pure (pos + 1)
      | v < 0x80 = do
          pokeByteOff p pos (fromIntegral v :: Word8)
          pure (pos + 1)
      | otherwise = do
          pokeByteOff
            p
            pos
            (fromIntegral (v .&. 0x7F) .|. 0x80 :: Word8)
          go (i + 1) (pos + 1) (v `shiftR` 7)


{-# INLINE pokeVarint32Raw #-}
pokeVarint32Raw :: Ptr Word8 -> Int -> Int32 -> IO Int
pokeVarint32Raw !p !pos !v =
  pokeVaruint32Raw
    p
    pos
    (fromIntegral ((v `shiftL` 1) `xor` (v `shiftR` 31)))


{-# INLINE pokeVarint64Raw #-}
pokeVarint64Raw :: Ptr Word8 -> Int -> Int64 -> IO Int
pokeVarint64Raw !p !pos !v =
  pokeVaruint64Raw
    p
    pos
    (fromIntegral ((v `shiftL` 1) `xor` (v `shiftR` 63)))


-- Suppress unused-import warnings if a future edit drops one of
-- the Hashable / unsafe-index helpers.
_unused :: ByteString
_unused = BSU.unsafeTake 0 BS.empty
