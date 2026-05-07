{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | Type-driven Fory encode / decode that bypasses the
-- dynamic 'Fory.Value.Value' AST.
--
-- 'Fory.Class.encodeFory' and 'Fory.Class.decodeFory' build a
-- 'Value' tree as an intermediate step, which means a list of
-- 100 'Int's allocates 100 boxed @VarInt64Val n@ heap objects
-- before any byte hits the wire. The 'EncodeDirect' /
-- 'DecodeDirect' classes here serialise typed Haskell values
-- straight into the IO encoder buffer (and out of the IO
-- decoder cursor) — no 'Value' allocation, no pattern-matched
-- dispatch in the inner loop.
--
-- Wire compatibility: 'encodeDirect' produces byte-for-byte
-- the same output as @encodeFory@ for every type that has
-- both a 'Fory.Class.ToFory' and an 'EncodeDirect' instance.
--
-- The supported types currently cover the bench shapes:
-- primitives, 'Text', 'ByteString', '[a]', 'Data.Map.Map',
-- 'Data.HashMap.Strict.HashMap', the 11 primitive-array
-- newtypes, and any user struct exposed via
-- 'EncodeDirectStruct' (see 'Fory.Direct.Struct').
module Fory.Direct
  ( -- * Top-level
    ForyTypeId (..)
  , EncodeDirect (..)
  , DecodeDirect (..)
  , encodeDirect
  , decodeDirect

    -- * Internal helpers exported for instance authors
  , directTagged
  , readSlotAndTag
  , emitForyStringDirect
  , readForyStringDirect
  ) where

import Data.Bits (shiftL, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Data.Char (ord)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Array as TA
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Internal as TI
import qualified Data.Vector as V
import Data.Vector (Vector)
import qualified Data.Vector.Storable as VS
import Data.Word (Word8, Word16, Word32, Word64)
import qualified Foreign.ForeignPtr
import Foreign.ForeignPtr (castForeignPtr)
import qualified Foreign.Marshal.Utils
import qualified Foreign.Ptr
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable, pokeByteOff, sizeOf)
import GHC.Exts (copyByteArrayToAddr#, indexWord64Array#, indexWord8Array#,
                 (>=#), (+#), Int#, isTrue#)
import qualified GHC.Float
import GHC.IO (IO (IO))
import GHC.Int (Int (I#))
import GHC.Ptr (Ptr (Ptr), plusPtr)
import GHC.Word (Word8 (W8#), Word64 (W64#))
import System.IO.Unsafe (unsafeDupablePerformIO)

import qualified Fory.Bulk as B
import qualified Fory.Decode as D
import qualified Fory.IO as IO
import qualified Fory.Options as Opt
import qualified Fory.TypeId as T
import qualified Fory.Value as VV

-- ---------------------------------------------------------------------------
-- Top-level encode / decode
-- ---------------------------------------------------------------------------

-- | The Fory wire-format type tag for a Haskell type. This is
-- the value emitted as the @varuint32@ tag byte by the encoder
-- and consumed by 'readSlotAndTag' on the read side.
class ForyTypeId a where
  directTypeId :: T.TypeId

-- | Type-driven encoder. Walk a typed Haskell value and emit
-- its Fory wire bytes directly into the IO encoder, with no
-- 'Fory.Value.Value' allocation.
class ForyTypeId a => EncodeDirect a where
  -- | Emit the type-tagged payload (no slot flag, no header).
  -- The default 'encodeDirect' wrapper writes the
  -- @0x02@ header byte and the @0xFF NOT_NULL_VALUE@
  -- slot flag, then calls this.
  directEncodePayload :: IO.Encoder -> a -> IO ()

  -- | Optional fast-path writer for inner loops (lists, maps,
  -- struct fields). Returns the maximum byte size of a single
  -- payload plus a raw 'Ptr Word8'-based writer that updates
  -- the cursor without touching the encoder's IORefs. When
  -- 'Just', the list / map encoder reserves @perElem * len@
  -- bytes once with 'IO.withReservedRaw' and writes every
  -- element in a tight loop. When 'Nothing', the inner loop
  -- falls back to per-element 'directEncodePayload' (which
  -- pays the 'ensure' / 'IORef' cycle each call).
  directRawPoke :: Maybe (Int, Ptr Word8 -> Int -> a -> IO Int)
  directRawPoke = Nothing

class ForyTypeId a => DecodeDirect a where
  -- | Read the type-tagged payload assuming the slot byte is
  -- @NOT_NULL_VALUE@ and the tag has already been consumed by
  -- 'readSlotAndTag'.
  directDecodePayload :: D.DecodeM a

  -- | Optional fast-path reader for inner loops. Mirrors
  -- 'directRawPoke' on the encode side: returns a raw
  -- 'Ptr Word8'-based reader that advances the cursor without
  -- touching the decoder's IORefs. When 'Just', the
  -- list / Vector decoder calls 'D.readSameTypeBatch' which
  -- pays the cursor 'IORef' cycle exactly twice for the
  -- entire batch.
  directRawPeek :: Maybe (Ptr Word8 -> Int -> IO (a, Int))
  directRawPeek = Nothing

-- | Encode a typed value to Fory bytes. Equivalent (byte-for-
-- byte) to @encodeFory . toFory@ but allocates no
-- intermediate 'Value' tree.
{-# NOINLINE encodeDirect #-}
encodeDirect :: forall a. EncodeDirect a => a -> ByteString
encodeDirect !x = unsafeDupablePerformIO $
  IO.runEncoder Opt.defaultEncodeOptions $ \e -> do
    IO.emitByte e 0x02              -- xlang header
    IO.emitByte e 0xFF              -- NOT_NULL_VALUE
    let !tag = directTypeId @a
    emitTagD e tag
    directEncodePayload @a e x

-- | Decode a Fory byte string into a typed value. Mirrors
-- 'encodeDirect' — never builds a 'Value'.
{-# NOINLINE decodeDirect #-}
decodeDirect :: forall a. DecodeDirect a => ByteString -> Either String a
decodeDirect !bs = D.runDecodeM (readSlotAndTag @a >> directDecodePayload @a) bs

-- ---------------------------------------------------------------------------
-- Helpers shared by instances
-- ---------------------------------------------------------------------------

emitTagD :: IO.Encoder -> T.TypeId -> IO ()
emitTagD !e (T.TypeId w) = IO.emitVaruint32 e (fromIntegral w)
{-# INLINE emitTagD #-}

-- | Verify the leading header + slot byte + type-tag,
-- producing nothing (the payload reader runs after).
readSlotAndTag :: forall a. ForyTypeId a => D.DecodeM ()
readSlotAndTag = do
  hdr <- D.readByteD
  if hdr `mod` 2 == 0 && hdr /= 0x02
    then D.failD ("Fory.Direct: bad header byte " ++ show hdr)
    else do
      slot <- D.readByteD
      if slot /= 0xFF
        then D.failD ("Fory.Direct: expected NOT_NULL_VALUE slot, got "
                       ++ show slot)
        else do
          tag <- D.readVaruint32D
          let !expected = directTypeId @a
              T.TypeId w = expected
          if fromIntegral tag /= w
            then D.failD ("Fory.Direct: expected type tag "
                          ++ show w ++ ", got " ++ show tag)
            else pure ()

-- | Helper that emits (tag varuint32 + payload) without the
-- header / slot bytes. Useful for instance authors writing
-- list / map / struct payloads where each element is itself
-- type-tagged.
directTagged :: forall a. EncodeDirect a => IO.Encoder -> a -> IO ()
directTagged !e !x = do
  emitTagD e (directTypeId @a)
  directEncodePayload @a e x
{-# INLINE directTagged #-}

-- | Re-export the encoder-side string-emit logic (LATIN-1
-- detection, header, bytes) for instance authors that want
-- to inline a string field without wrapping it in an
-- 'EncodeDirect' instance.
emitForyStringDirect :: IO.Encoder -> Text -> IO ()
emitForyStringDirect !e !t = do
  let !utf8 = TE.encodeUtf8 t
      !len  = BS.length utf8
  if BS.all (< 0x80) utf8
    then do
      let !hdr = (fromIntegral len `shiftL` 2) :: Word64
      IO.emitVaruint64 e hdr
      IO.emitBytes e utf8
    else if T.all (\c -> ord c < 256) t
      then do
        let !raw = B.latin1Bytes t
            !rlen = BS.length raw
            !hdr = (fromIntegral rlen `shiftL` 2) :: Word64
        IO.emitVaruint64 e hdr
        IO.emitBytes e raw
      else do
        let !hdr = (fromIntegral len `shiftL` 2) .|. 2 :: Word64
        IO.emitVaruint64 e hdr
        IO.emitBytes e utf8
{-# INLINE emitForyStringDirect #-}

-- | 'memcpy' from a 'TA.Array' (Text's underlying
-- 'ByteArray#') into a raw 'Ptr Word8'. Compiles to a single
-- @copyByteArrayToAddr#@ primop call. Used by the typed
-- list-of-string fast paths to avoid the per-element
-- 'TE.encodeUtf8' allocation.
copyTextArrayToPtr :: TA.Array -> Int -> Ptr Word8 -> Int -> IO ()
copyTextArrayToPtr (TA.ByteArray arr#) (I# srcOff#) (Ptr dstAddr#) (I# n#) =
  IO $ \s ->
    case copyByteArrayToAddr# arr# srcOff# dstAddr# n# s of
      s' -> (# s', () #)
{-# INLINE copyTextArrayToPtr #-}

readForyStringDirect :: D.DecodeM Text
readForyStringDirect = D.readForyString

-- ---------------------------------------------------------------------------
-- Primitive instances
-- ---------------------------------------------------------------------------

-- | Haskell 'Int' encodes as 'T.VARINT64' on the wire,
-- matching pyfory's default for Python @int@ (zigzag-varuint
-- bytes, 1 byte for small values, up to 9 bytes for
-- @maxBound@). For users who want fixed-8-byte encoding —
-- and the corresponding ~4× speedup on sequence-of-int
-- shapes — use the explicit 'Int64' type (which maps to
-- 'T.INT64'), or 'VS.Vector Int' / 'VS.Vector Int64' for
-- flat, zero-copy 'INT64_ARRAY' payloads. This mirrors
-- pyfory's @int@ vs @int64@ vs @numpy.int64[]@ distinction.
instance ForyTypeId Int where directTypeId = T.VARINT64
instance EncodeDirect Int where
  directEncodePayload e n = IO.emitVarint64 e (fromIntegral n)
  directRawPoke = Just (9, \p off n ->
                          IO.pokeVarint64Raw p off (fromIntegral n))
  {-# INLINE directEncodePayload #-}

instance DecodeDirect Int where
  directDecodePayload = fromIntegral <$> D.readVarint64D
  directRawPeek = Just $ \p off -> do
    (n, off') <- D.peekVarint64Raw p off
    pure (fromIntegral n, off')
  {-# INLINE directDecodePayload #-}

instance ForyTypeId Int8 where directTypeId = T.INT8
instance EncodeDirect Int8 where
  directEncodePayload e n = IO.emitByte e (fromIntegral n)
  directRawPoke = Just (1, \p off n ->
                          IO.pokeByteRaw p off (fromIntegral n))
  {-# INLINE directEncodePayload #-}
instance DecodeDirect Int8 where
  directDecodePayload = fromIntegral <$> D.readByteD
  directRawPeek = Just $ \p off -> do
    (b, off') <- D.peekByteRaw p off
    pure (fromIntegral b, off')
  {-# INLINE directDecodePayload #-}

instance ForyTypeId Int16 where directTypeId = T.INT16
instance EncodeDirect Int16 where
  directEncodePayload = IO.emitInt16LE
  directRawPoke = Just (2, IO.pokeInt16LERaw)
  {-# INLINE directEncodePayload #-}
instance DecodeDirect Int16 where
  directDecodePayload = D.readInt16D
  directRawPeek = Just $ \p off -> do
    (w, off') <- D.peekWord16LERaw p off
    pure (fromIntegral w, off')
  {-# INLINE directDecodePayload #-}

instance ForyTypeId Int32 where directTypeId = T.INT32
instance EncodeDirect Int32 where
  directEncodePayload = IO.emitInt32LE
  directRawPoke = Just (4, IO.pokeInt32LERaw)
  {-# INLINE directEncodePayload #-}
instance DecodeDirect Int32 where
  directDecodePayload = D.readInt32D
  directRawPeek = Just D.peekInt32LERaw
  {-# INLINE directDecodePayload #-}

instance ForyTypeId Int64 where directTypeId = T.INT64
instance EncodeDirect Int64 where
  directEncodePayload = IO.emitInt64LE
  directRawPoke = Just (8, IO.pokeInt64LERaw)
  {-# INLINE directEncodePayload #-}
instance DecodeDirect Int64 where
  directDecodePayload = D.readInt64D
  directRawPeek = Just D.peekInt64LERaw
  {-# INLINE directDecodePayload #-}

instance ForyTypeId Word8 where directTypeId = T.UINT8
instance EncodeDirect Word8 where
  directEncodePayload = IO.emitByte
  directRawPoke = Just (1, IO.pokeByteRaw)
  {-# INLINE directEncodePayload #-}
instance DecodeDirect Word8 where
  directDecodePayload = D.readByteD
  directRawPeek = Just D.peekByteRaw
  {-# INLINE directDecodePayload #-}

instance ForyTypeId Word16 where directTypeId = T.UINT16
instance EncodeDirect Word16 where
  directEncodePayload = IO.emitWord16LE
  directRawPoke = Just (2, IO.pokeWord16LERaw)
  {-# INLINE directEncodePayload #-}
instance DecodeDirect Word16 where
  directDecodePayload = D.readWord16D
  directRawPeek = Just D.peekWord16LERaw
  {-# INLINE directDecodePayload #-}

instance ForyTypeId Word32 where directTypeId = T.UINT32
instance EncodeDirect Word32 where
  directEncodePayload = IO.emitWord32LE
  directRawPoke = Just (4, IO.pokeWord32LERaw)
  {-# INLINE directEncodePayload #-}
instance DecodeDirect Word32 where
  directDecodePayload = D.readWord32D
  directRawPeek = Just D.peekWord32LERaw
  {-# INLINE directDecodePayload #-}

instance ForyTypeId Word64 where directTypeId = T.UINT64
instance EncodeDirect Word64 where
  directEncodePayload = IO.emitWord64LE
  directRawPoke = Just (8, IO.pokeWord64LERaw)
  {-# INLINE directEncodePayload #-}
instance DecodeDirect Word64 where
  directDecodePayload = D.readWord64D
  directRawPeek = Just D.peekWord64LERaw
  {-# INLINE directDecodePayload #-}

instance ForyTypeId Float where directTypeId = T.FLOAT32
instance EncodeDirect Float where
  directEncodePayload = IO.emitFloat32LE
  directRawPoke = Just (4, IO.pokeFloat32LERaw)
  {-# INLINE directEncodePayload #-}
instance DecodeDirect Float where
  directDecodePayload = D.readFloat32D
  directRawPeek = Just $ \p off -> do
    (w, off') <- D.peekWord32LERaw p off
    pure (GHC.Float.castWord32ToFloat w, off')
  {-# INLINE directDecodePayload #-}

instance ForyTypeId Double where directTypeId = T.FLOAT64
instance EncodeDirect Double where
  directEncodePayload = IO.emitFloat64LE
  directRawPoke = Just (8, IO.pokeFloat64LERaw)
  {-# INLINE directEncodePayload #-}
instance DecodeDirect Double where
  directDecodePayload = D.readFloat64D
  directRawPeek = Just $ \p off -> do
    (w, off') <- D.peekWord64LERaw p off
    pure (GHC.Float.castWord64ToDouble w, off')
  {-# INLINE directDecodePayload #-}

instance ForyTypeId Bool where directTypeId = T.BOOL
instance EncodeDirect Bool where
  directEncodePayload e b = IO.emitByte e (if b then 1 else 0)
  directRawPoke = Just (1, \p off b ->
                          IO.pokeByteRaw p off (if b then 1 else 0))
  {-# INLINE directEncodePayload #-}
instance DecodeDirect Bool where
  directDecodePayload = (/= 0) <$> D.readByteD
  directRawPeek = Just $ \p off -> do
    (b, off') <- D.peekByteRaw p off
    pure (b /= 0, off')
  {-# INLINE directDecodePayload #-}

instance ForyTypeId Text where directTypeId = T.STRING
instance EncodeDirect Text where
  directEncodePayload = emitForyStringDirect
  {-# INLINE directEncodePayload #-}
instance DecodeDirect Text where
  directDecodePayload = D.readForyString
  {-# INLINE directDecodePayload #-}

instance ForyTypeId ByteString where directTypeId = T.BINARY
instance EncodeDirect ByteString where
  directEncodePayload e bs = do
    IO.emitVaruint32 e (fromIntegral (BS.length bs))
    IO.emitBytes e bs
  {-# INLINE directEncodePayload #-}
instance DecodeDirect ByteString where
  directDecodePayload = do
    n <- fromIntegral <$> D.readVaruint32D
    D.readBytesD n
  {-# INLINE directDecodePayload #-}

-- ---------------------------------------------------------------------------
-- Lists
-- ---------------------------------------------------------------------------

-- The list payload is the same chunked @collect_flag@ wire
-- format the 'Value'-based encoder produces for
-- @ListVal (V.fromList xs)@. We pick the same-type +
-- no-null + no-ref-tracking shape (collect_flag = IS_SAME_TYPE
-- = 0x08) which is how a homogeneous Haskell '[a]' encodes.
--
-- The inner loop calls 'directEncodePayload' per element with
-- a single 'IO.withReservedRaw' over the entire list when @a@
-- supports a fast batch path (currently only the fixed-size
-- primitives — see 'Fory.Encode.sameTypeFastPath').

instance ForyTypeId [a] where directTypeId = T.LIST
-- The plain '[a]' instance has two paths:
--
-- * If @a@ has a 'directRawPoke' (the fixed-size primitives:
--   Int, Float, Double, Bool, Int*, Word*), forward to the
--   'Vector a' instance via 'V.fromList'. The Vector walk +
--   'IO.withReservedRaw' batching is much faster than a
--   recursive cons-cell pattern match, and 'V.length' is
--   O(1) so we avoid a second list traversal.
-- * Otherwise (Text, ByteString, nested collections,
--   structs), traverse the list directly with one 'length'
--   pass + 'mapM_'. Allocating a Vector wouldn't help here
--   because the per-element work (string encode, sub-encode)
--   dominates the framing.
instance forall a. EncodeDirect a => EncodeDirect [a] where
  directEncodePayload e xs = case directRawPoke @a of
    Just{}  -> directEncodePayload e (V.fromList xs)
    Nothing -> emitListSlowGeneric e xs
  {-# INLINE directEncodePayload #-}

-- | Fallback list emitter for element types without a raw-poke
-- fast path. Walks the list twice (once for length, once for
-- writes) but pays no extra allocation and no per-element
-- closure dispatch beyond the existing class method call.
emitListSlowGeneric
  :: forall a. EncodeDirect a => IO.Encoder -> [a] -> IO ()
emitListSlowGeneric !e !xs = do
  let !len = length xs
  IO.emitVaruint32 e (fromIntegral len)
  if len == 0
    then pure ()
    else do
      IO.emitByte e 0x08
      emitTagD e (directTypeId @a)
      mapM_ (directEncodePayload @a e) xs
{-# INLINE emitListSlowGeneric #-}

-- ---------------------------------------------------------------------------
-- OVERLAPPING fast paths for the most common typed list shapes
-- ---------------------------------------------------------------------------

-- | OVERLAPPING fast path for @[Int]@ — the bench's
-- @list-of-int 100@ shape. Walks the list twice in tight
-- monomorphic loops: once for length, once for the writes.
-- Uses 'T.VARINT64' wire (matching pyfory's default for
-- Python @int@) so each element is a 1–9-byte zigzag-varuint
-- via 'IO.pokeVarint64Raw'.
instance {-# OVERLAPPING #-} EncodeDirect [Int] where
  directEncodePayload e xs = do
    let !len = length xs
    IO.emitVaruint32 e (fromIntegral len)
    if len == 0
      then pure ()
      else do
        IO.emitByte e 0x08
        emitTagD e T.VARINT64
        IO.withReservedRaw e (9 * len) $ \p start -> goInts p start xs
    where
      goInts :: Ptr Word8 -> Int -> [Int] -> IO Int
      goInts !_  !off []       = pure off
      goInts !p !off (n:rest) = do
        !off' <- IO.pokeVarint64Raw p off (fromIntegral n)
        goInts p off' rest
  {-# INLINE directEncodePayload #-}

-- | OVERLAPPING fast path for @[Int32]@ — fixed 4-byte LE
-- per element. Same single-pass-write approach as
-- @[Int]@; total reservation is exactly @4 * length xs@.
instance {-# OVERLAPPING #-} EncodeDirect [Int32] where
  directEncodePayload e xs = do
    let !len = length xs
    IO.emitVaruint32 e (fromIntegral len)
    if len == 0
      then pure ()
      else do
        IO.emitByte e 0x08
        emitTagD e T.INT32
        IO.withReservedRaw e (4 * len) $ \p start ->
          goInt32s p start xs
    where
      goInt32s :: Ptr Word8 -> Int -> [Int32] -> IO Int
      goInt32s !_  !off []       = pure off
      goInt32s !p !off (n:rest) = do
        !off' <- IO.pokeInt32LERaw p off n
        goInt32s p off' rest
  {-# INLINE directEncodePayload #-}

instance {-# OVERLAPPING #-} EncodeDirect [Double] where
  directEncodePayload e xs = do
    let !len = length xs
    IO.emitVaruint32 e (fromIntegral len)
    if len == 0
      then pure ()
      else do
        IO.emitByte e 0x08
        emitTagD e T.FLOAT64
        IO.withReservedRaw e (8 * len) $ \p start ->
          goDoubles p start xs
    where
      goDoubles :: Ptr Word8 -> Int -> [Double] -> IO Int
      goDoubles !_  !off []       = pure off
      goDoubles !p !off (d:rest) = do
        !off' <- IO.pokeFloat64LERaw p off d
        goDoubles p off' rest
  {-# INLINE directEncodePayload #-}

-- | OVERLAPPING fast path for @Vector Int@. Same 'T.VARINT64'
-- wire as @[Int]@ but with O(1) 'V.length'.
instance {-# OVERLAPPING #-} EncodeDirect (Vector Int) where
  directEncodePayload e v = do
    let !len = V.length v
    IO.emitVaruint32 e (fromIntegral len)
    if len == 0
      then pure ()
      else do
        IO.emitByte e 0x08
        emitTagD e T.VARINT64
        IO.withReservedRaw e (9 * len) $ \p start ->
          V.foldM' (\ !off n ->
            IO.pokeVarint64Raw p off (fromIntegral n)) start v
  {-# INLINE directEncodePayload #-}

instance {-# OVERLAPPING #-} EncodeDirect (Vector Int32) where
  directEncodePayload e v = do
    let !len = V.length v
    IO.emitVaruint32 e (fromIntegral len)
    if len == 0
      then pure ()
      else do
        IO.emitByte e 0x08
        emitTagD e T.INT32
        IO.withReservedRaw e (4 * len) $ \p start ->
          V.foldM' (\ !off n -> IO.pokeInt32LERaw p off n) start v
  {-# INLINE directEncodePayload #-}

instance {-# OVERLAPPING #-} EncodeDirect (Vector Double) where
  directEncodePayload e v = do
    let !len = V.length v
    IO.emitVaruint32 e (fromIntegral len)
    if len == 0
      then pure ()
      else do
        IO.emitByte e 0x08
        emitTagD e T.FLOAT64
        IO.withReservedRaw e (8 * len) $ \p start ->
          V.foldM' (\ !off d -> IO.pokeFloat64LERaw p off d) start v
  {-# INLINE directEncodePayload #-}

-- | OVERLAPPING fast path for @[Text]@. Mirrors the
-- Value-side 'emitStringListFast': pre-encodes each element
-- to UTF-8 + classifies it as LATIN-1 vs UTF-8 in one walk
-- (which also gives us the length), sums the upper-bound
-- total bytes, then writes the entire list payload via a
-- single 'IO.withReservedRaw'.
-- | OVERLAPPING fast path for @[Text]@.
--
-- Walks the list twice: once to count + sum the upper-bound
-- byte size, once to write. Neither pass allocates per
-- element — no intermediate @ByteString@ from 'TE.encodeUtf8',
-- no boxed @TextEntry@ tuple. The size estimate uses the
-- 'Text''s underlying UTF-8 byte length (which is exact for
-- ASCII and UTF-8 strings; over-reserves by a factor of
-- two for the rare Latin-1-only case where chars in
-- 128–255 take 2 UTF-8 bytes but only 1 wire byte).
--
-- Per-element write: one 'byteArrayIsAscii' OR-fold (Word64-
-- stride, 1 read per 8 bytes), a 1- or 9-byte header poke,
-- and a 'copyByteArrayToAddr#' memcpy. ASCII strings stay
-- byte-identical to pyfory's LATIN-1 default; the Latin-1
-- fallback uses 'B.latin1Bytes' for correctness.
instance {-# OVERLAPPING #-} EncodeDirect [Text] where
  directEncodePayload !e !xs = do
    let (!n, !total) = sizeListText xs 0 0
    IO.emitVaruint32 e (fromIntegral n)
    if n == 0
      then pure ()
      else do
        IO.emitByte e 0x08
        emitTagD e T.STRING
        IO.withReservedRaw e total $ \p start ->
          goWriteList p start xs
    where
      -- Hand-rolled cons-pattern-matching loop. Recursion is
      -- monomorphic in @[Text] -> IO Int@ so GHC inlines
      -- 'writeTextOnto' fully into the call site.
      goWriteList :: Ptr Word8 -> Int -> [Text] -> IO Int
      goWriteList !_  !off []       = pure off
      goWriteList !p !off (t:rest) = do
        !off' <- writeTextOnto p off t
        goWriteList p off' rest

-- | Single-pass length + size accumulator for @[Text]@.
-- The size is the sum of @9 + utf8ByteLength@ per element
-- (9-byte upper bound for the per-string varuint64 header,
-- plus the underlying ByteArray length).
sizeListText :: [Text] -> Int -> Int -> (Int, Int)
sizeListText []                     !n !sz = (n, sz)
sizeListText (TI.Text _ _ len : ts) !n !sz =
  sizeListText ts (n + 1) (sz + 9 + len)

-- | OVERLAPPING fast path for @Vector Text@. Same two-pass
-- approach as @[Text]@ but with O(1) 'V.length'.
instance {-# OVERLAPPING #-} EncodeDirect (Vector Text) where
  directEncodePayload !e !xs = do
    let !n = V.length xs
    IO.emitVaruint32 e (fromIntegral n)
    if n == 0
      then pure ()
      else do
        IO.emitByte e 0x08
        emitTagD e T.STRING
        let !total =
              V.foldl' (\acc (TI.Text _ _ len) -> acc + 9 + len) 0 xs
        IO.withReservedRaw e total $ \p start ->
          V.foldM' (writeTextOnto p) start xs
  {-# INLINE directEncodePayload #-}

-- | Per-element write for the typed list-of-string fast
-- paths.
--
-- Picks the wire-encoding tag (LATIN-1 / UTF-8) based on a
-- 'Word64'-stride OR-fold over the underlying 'TA.Array'
-- bytes ('byteArrayIsAscii'). For pure-ASCII strings we
-- emit the underlying UTF-8 bytes raw with tag 0 (which
-- equals the LATIN-1 encoding for ASCII bytes); for
-- pure-Latin-1 strings (chars 128–255 only) we re-encode
-- via 'B.latin1Bytes' to keep the wire 1 byte per char;
-- otherwise we emit UTF-8 (tag 2) raw.
--
-- The 'text' package would expose its own SIMD-accelerated
-- ASCII check at 'Data.Text.Internal.IsAscii.isAscii', but
-- the module is marked @other-modules@ (hidden) in
-- text-2.0.2 and text-2.1.x alike, so we use the hand-rolled
-- 'byteArrayIsAscii' instead.
--
-- Short-header fast path: payloads under 128 wire bytes get
-- a single-byte header poke instead of the
-- 'IO.pokeVaruint64Raw' continuation loop.
writeTextOnto :: Ptr Word8 -> Int -> Text -> IO Int
writeTextOnto !p !off t@(TI.Text arr srcOff len)
  | byteArrayIsAscii arr srcOff (srcOff + len) = do
      !off1 <- emitStringHeader p off len 0
      copyTextArrayToPtr arr srcOff (p `plusPtr` off1) len
      pure (off1 + len)
  | T.all (\c -> ord c < 256) t = do
      let !raw  = B.latin1Bytes t
          !rlen = BS.length raw
      !off1 <- emitStringHeader p off rlen 0
      pokeBytesRawDirect p off1 raw
  | otherwise = do
      !off1 <- emitStringHeader p off len 2
      copyTextArrayToPtr arr srcOff (p `plusPtr` off1) len
      pure (off1 + len)
{-# INLINE writeTextOnto #-}

-- | Emit a Fory string-payload header @(len << 2) | tag@.
-- For payloads under 128 bytes, that's a single byte poke;
-- otherwise we fall back to 'IO.pokeVaruint64Raw'.
emitStringHeader :: Ptr Word8 -> Int -> Int -> Int -> IO Int
emitStringHeader !p !off !len !tag
  | len < 32 = do
      pokeByteOff p off
        (fromIntegral ((len `shiftL` 2) .|. tag) :: Word8)
      pure (off + 1)
  | otherwise =
      IO.pokeVaruint64Raw p off
        ((fromIntegral len `shiftL` 2) .|. fromIntegral tag :: Word64)
{-# INLINE emitStringHeader #-}

-- | OR-fold over 'TA.Array' bytes to detect any non-ASCII
-- byte. When the byte offset is 8-aligned (the typical
-- 'T.pack' / 'TI.empty' / 'TE.decodeUtf8' case has offset 0),
-- we run a 'Word64'-stride scan that processes 8 bytes per
-- iteration — that's an OR against the
-- @0x8080_8080_8080_8080@ mask, ~10× faster than the
-- per-byte loop on long strings and at least as fast on
-- short ones. Misaligned offsets fall back to the per-byte
-- recursion.
byteArrayIsAscii :: TA.Array -> Int -> Int -> Bool
byteArrayIsAscii !arr !off !end
  | off `rem` 8 == 0 = goWord64Aligned arr off end
  | otherwise        = goPerByte arr off end
{-# INLINABLE byteArrayIsAscii #-}

-- | Per-byte fallback for misaligned 'TA.Array' segments.
goPerByte :: TA.Array -> Int -> Int -> Bool
goPerByte !arr !i !end
  | i >= end                       = True
  | TA.unsafeIndex arr i >= 0x80   = False
  | otherwise                      = goPerByte arr (i + 1) end
{-# INLINABLE goPerByte #-}

-- | 'Word64'-stride OR-scan starting from an 8-aligned byte
-- offset. Reads at @byteOff `div` 8@ word index from the
-- 'TA.ByteArray''s underlying @ByteArray#@; falls back to a
-- per-byte tail when fewer than 8 bytes remain.
goWord64Aligned :: TA.Array -> Int -> Int -> Bool
goWord64Aligned arr@(TA.ByteArray ba#) !off !end =
  let !w0 = off `quot` 8
      !wEnd = end `quot` 8
  in goW8 ba# w0 wEnd
  where
    goW8 !ba1# !w !wEnd
      | w >= wEnd = goPerByte arr (w * 8) end
      | otherwise = case indexWord64Array# ba1# (unI# w) of
          x# -> if hasHighBitW64 (W64# x#)
                  then False
                  else goW8 ba1# (w + 1) wEnd

    hasHighBitW64 :: Word64 -> Bool
    hasHighBitW64 w = (w .&. 0x8080808080808080) /= 0
    {-# INLINE hasHighBitW64 #-}

    unI# :: Int -> Int#
    unI# (I# i#) = i#
    {-# INLINE unI# #-}
{-# INLINABLE goWord64Aligned #-}

instance forall a. DecodeDirect a => DecodeDirect [a] where
  directDecodePayload = do
    count <- fromIntegral <$> D.readVaruint32D
    if count == 0
      then pure []
      else do
        flag <- D.readByteD
        if flag /= 0x08
          then D.failD ("Fory.Direct: expected IS_SAME_TYPE list, got flag "
                         ++ show flag)
          else do
            _tag <- D.readVaruint32D
            -- For element types with a raw-peek fast path,
            -- batch the reads through 'D.readSameTypeBatchList'
            -- (one cursor 'IORef' cycle for the whole batch).
            -- Otherwise fall back to per-element
            -- 'directDecodePayload'.
            case directRawPeek @a of
              Just rdr -> D.readSameTypeBatchList count rdr
              Nothing  -> replicateMD count (directDecodePayload @a)

-- 'Vector a' uses the same wire shape — boxed Vector is
-- equivalent to '[a]' from the wire's perspective.
instance ForyTypeId (Vector a) where directTypeId = T.LIST
instance forall a. EncodeDirect a => EncodeDirect (Vector a) where
  directEncodePayload e xs = do
    let !len = V.length xs
    IO.emitVaruint32 e (fromIntegral len)
    if len == 0
      then pure ()
      else do
        IO.emitByte e 0x08
        emitTagD e (directTypeId @a)
        case directRawPoke @a of
          Just (perElem, poker) ->
            IO.withReservedRaw e (perElem * len) $ \p start ->
              V.foldM' (\off x -> poker p off x) start xs
          Nothing ->
            V.forM_ xs (directEncodePayload @a e)

instance forall a. DecodeDirect a => DecodeDirect (Vector a) where
  directDecodePayload = do
    count <- fromIntegral <$> D.readVaruint32D
    if count == 0
      then pure V.empty
      else do
        flag <- D.readByteD
        if flag /= 0x08
          then D.failD ("Fory.Direct: expected IS_SAME_TYPE list, got flag "
                         ++ show flag)
          else do
            _tag <- D.readVaruint32D
            case directRawPeek @a of
              Just rdr -> D.readSameTypeBatch count rdr
              Nothing  -> V.replicateM count (directDecodePayload @a)

-- ---------------------------------------------------------------------------
-- Storable-Vector primitive arrays
-- ---------------------------------------------------------------------------
--
-- A 'Data.Vector.Storable.Vector a' encodes as the
-- corresponding @*_ARRAY@ primitive — wire shape is
-- @varuint32 byteLen + raw bytes@, identical to
-- 'Fory.Bulk.{int32,float64,...}ArrayBytes'.

emitStorableArrayPayload
  :: forall a. Storable a => IO.Encoder -> VS.Vector a -> IO ()
emitStorableArrayPayload e v = do
  let (!fp, !n) = VS.unsafeToForeignPtr0 v
      !sz       = sizeOf (undefined :: a)
      !byteLen  = n * sz
      bs = BSI.BS (castForeignPtr fp) byteLen
  IO.emitVaruint32 e (fromIntegral byteLen)
  IO.emitBytes e bs
{-# INLINE emitStorableArrayPayload #-}

readStorableArrayPayload
  :: forall a. Storable a => D.DecodeM (VS.Vector a)
readStorableArrayPayload = do
  byteLen <- fromIntegral <$> D.readVaruint32D
  let elemSize = sizeOf (undefined :: a)
      (_, r)   = byteLen `quotRem` elemSize
  if r /= 0
    then D.failD ("Fory.Direct: array byte length not aligned: "
                   ++ show byteLen ++ " % " ++ show elemSize)
    else do
      raw <- D.readBytesD byteLen
      pure (B.bytesToVecS raw)
{-# INLINE readStorableArrayPayload #-}

-- | 'VS.Vector Int' is the flat, zero-copy fast path for
-- sequences of 'Int' — analogous to a NumPy @int64@ array on
-- the Python side. On a 64-bit platform 'sizeOf (undefined ::
-- Int) == 8', so the wire format is 'T.INT64_ARRAY' and the
-- encode/decode are O(1) 'castForeignPtr' between the
-- 'ByteString' and the vector. This delivers essentially the
-- same speed as 'VS.Vector Int64' and gives 'Int' users
-- access to the same fast path.
--
-- NOTE: requires a 64-bit Haskell platform (which is the
-- default on x86-64 Linux / aarch64 macOS / Windows-x64).
-- A static check would belong here for 32-bit ports.
instance ForyTypeId (VS.Vector Int)    where directTypeId = T.INT64_ARRAY
instance EncodeDirect (VS.Vector Int)  where directEncodePayload = emitStorableArrayPayload
instance DecodeDirect (VS.Vector Int)  where directDecodePayload = readStorableArrayPayload

instance ForyTypeId (VS.Vector Int8)   where directTypeId = T.INT8_ARRAY
instance ForyTypeId (VS.Vector Int16)  where directTypeId = T.INT16_ARRAY
instance ForyTypeId (VS.Vector Int32)  where directTypeId = T.INT32_ARRAY
instance ForyTypeId (VS.Vector Int64)  where directTypeId = T.INT64_ARRAY
instance ForyTypeId (VS.Vector Word8)  where directTypeId = T.UINT8_ARRAY
instance ForyTypeId (VS.Vector Word16) where directTypeId = T.UINT16_ARRAY
instance ForyTypeId (VS.Vector Word32) where directTypeId = T.UINT32_ARRAY
instance ForyTypeId (VS.Vector Word64) where directTypeId = T.UINT64_ARRAY
instance ForyTypeId (VS.Vector Float)  where directTypeId = T.FLOAT32_ARRAY
instance ForyTypeId (VS.Vector Double) where directTypeId = T.FLOAT64_ARRAY

instance EncodeDirect (VS.Vector Int8)   where directEncodePayload = emitStorableArrayPayload
instance EncodeDirect (VS.Vector Int16)  where directEncodePayload = emitStorableArrayPayload
instance EncodeDirect (VS.Vector Int32)  where directEncodePayload = emitStorableArrayPayload
instance EncodeDirect (VS.Vector Int64)  where directEncodePayload = emitStorableArrayPayload
instance EncodeDirect (VS.Vector Word8)  where directEncodePayload = emitStorableArrayPayload
instance EncodeDirect (VS.Vector Word16) where directEncodePayload = emitStorableArrayPayload
instance EncodeDirect (VS.Vector Word32) where directEncodePayload = emitStorableArrayPayload
instance EncodeDirect (VS.Vector Word64) where directEncodePayload = emitStorableArrayPayload
instance EncodeDirect (VS.Vector Float)  where directEncodePayload = emitStorableArrayPayload
instance EncodeDirect (VS.Vector Double) where directEncodePayload = emitStorableArrayPayload

instance DecodeDirect (VS.Vector Int8)   where directDecodePayload = readStorableArrayPayload
instance DecodeDirect (VS.Vector Int16)  where directDecodePayload = readStorableArrayPayload
instance DecodeDirect (VS.Vector Int32)  where directDecodePayload = readStorableArrayPayload
instance DecodeDirect (VS.Vector Int64)  where directDecodePayload = readStorableArrayPayload
instance DecodeDirect (VS.Vector Word8)  where directDecodePayload = readStorableArrayPayload
instance DecodeDirect (VS.Vector Word16) where directDecodePayload = readStorableArrayPayload
instance DecodeDirect (VS.Vector Word32) where directDecodePayload = readStorableArrayPayload
instance DecodeDirect (VS.Vector Word64) where directDecodePayload = readStorableArrayPayload
instance DecodeDirect (VS.Vector Float)  where directDecodePayload = readStorableArrayPayload
instance DecodeDirect (VS.Vector Double) where directDecodePayload = readStorableArrayPayload

-- ---------------------------------------------------------------------------
-- Maps
-- ---------------------------------------------------------------------------
--
-- We emit one chunk per 255 entries (chunk_size is a byte) in
-- the homogeneous (k, v) shape. This matches the bytes the
-- 'Value'-based encoder produces via
-- 'Fory.Encode.emitMapChunkedHomogeneous'.

instance ForyTypeId (Map k v) where directTypeId = T.MAP
instance forall k v. (EncodeDirect k, EncodeDirect v, Ord k) =>
         EncodeDirect (Map k v) where
  directEncodePayload e m = do
    let !n = M.size m
    IO.emitVaruint32 e (fromIntegral n)
    if n == 0
      then pure ()
      else do
        let !keyTag = directTypeId @k
            !valTag = directTypeId @v
            entries = M.toAscList m
        emitMapChunks e entries n keyTag valTag

instance ForyTypeId (HashMap k v) where directTypeId = T.MAP
instance forall k v. (EncodeDirect k, EncodeDirect v, Eq k, Hashable k) =>
         EncodeDirect (HashMap k v) where
  directEncodePayload e m = do
    let !n = HM.size m
    IO.emitVaruint32 e (fromIntegral n)
    if n == 0
      then pure ()
      else do
        let !keyTag = directTypeId @k
            !valTag = directTypeId @v
            entries = HM.toList m
        emitMapChunks e entries n keyTag valTag

emitMapChunks
  :: forall k v. (EncodeDirect k, EncodeDirect v)
  => IO.Encoder -> [(k, v)] -> Int -> T.TypeId -> T.TypeId -> IO ()
emitMapChunks !e !entries !total !keyTag !valTag = go entries total
  where
    go _    0 = pure ()
    go rest remaining = do
      let !cs = min 255 remaining
          (chunk, rest') = splitAt cs rest
      IO.emitByte e 0
      IO.emitByte e (fromIntegral cs)
      emitTagD e keyTag
      emitTagD e valTag
      mapM_ (\(k, v) -> do
        directEncodePayload @k e k
        directEncodePayload @v e v) chunk
      go rest' (remaining - cs)

instance forall k v. (DecodeDirect k, DecodeDirect v, Ord k) =>
         DecodeDirect (Map k v) where
  directDecodePayload = M.fromList <$> readMapEntries

instance forall k v. (DecodeDirect k, DecodeDirect v, Eq k, Hashable k) =>
         DecodeDirect (HashMap k v) where
  directDecodePayload = HM.fromList <$> readMapEntries

-- | OVERLAPPING fast path for the @Map Text Int@ shape that
-- shows up everywhere (string-keyed config, header dicts, ...).
-- Mirrors the Value-side 'emitMapStringVarInt64': pre-encodes
-- each string key, sums the upper bound, then writes the
-- whole homogeneous chunk via 'IO.withReservedRaw'.
instance {-# OVERLAPPING #-} EncodeDirect (Map Text Int) where
  directEncodePayload e m = do
    let !n = M.size m
    IO.emitVaruint32 e (fromIntegral n)
    if n == 0
      then pure ()
      else emitMapTextIntChunks e (M.toAscList m) n

instance {-# OVERLAPPING #-} EncodeDirect (HashMap Text Int) where
  directEncodePayload e m = do
    let !n = HM.size m
    IO.emitVaruint32 e (fromIntegral n)
    if n == 0
      then pure ()
      else emitMapTextIntChunks e (HM.toList m) n

emitMapTextIntChunks :: IO.Encoder -> [(Text, Int)] -> Int -> IO ()
emitMapTextIntChunks !e !entries !total = go entries total
  where
    go _    0 = pure ()
    go rest remaining = do
      let !cs = min 255 remaining
          (chunk, rest') = splitAt cs rest
      IO.emitByte e 0
      IO.emitByte e (fromIntegral cs)
      emitTagD e T.STRING
      emitTagD e T.VARINT64
      -- Pre-encode keys + classify, sum upper bound, then
      -- write the whole chunk through one withReservedRaw.
      encoded <- mapM encOne chunk
      let !totalSize =
            sum [9 + BS.length u + 9 | (u, _, _) <- encoded]
      IO.withReservedRaw e totalSize $ \p start ->
        foldlMList (writeOne p) start encoded
      go rest' (remaining - cs)

    encOne :: (Text, Int) -> IO (ByteString, Bool, Int)
    encOne (t, n) =
      let !u = TE.encodeUtf8 t
          !ascii = BS.all (< 0x80) u
      in pure (u, ascii, n)

    writeOne :: Ptr Word8 -> Int -> (ByteString, Bool, Int) -> IO Int
    writeOne !p !off (!u, !ascii, !n) = do
      let !len = BS.length u
          !hdr = (fromIntegral len `shiftL` 2)
                   .|. (if ascii then 0 else 2) :: Word64
      off1 <- IO.pokeVaruint64Raw p off hdr
      off2 <- pokeBytesRawDirect p off1 u
      IO.pokeVarint64Raw p off2 (fromIntegral n)

pokeBytesRawDirect :: Ptr Word8 -> Int -> ByteString -> IO Int
pokeBytesRawDirect !p !pos !bs = do
  let (BSI.BS fpSrc lenSrc) = bs
  Foreign.ForeignPtr.withForeignPtr fpSrc $ \pSrc ->
    Foreign.Marshal.Utils.copyBytes
      (p `Foreign.Ptr.plusPtr` pos) pSrc lenSrc
  pure (pos + lenSrc)
{-# INLINE pokeBytesRawDirect #-}

readMapEntries
  :: forall k v. (DecodeDirect k, DecodeDirect v)
  => D.DecodeM [(k, v)]
readMapEntries = do
  total <- fromIntegral <$> D.readVaruint32D
  if total == 0
    then pure []
    else loop total []
  where
    loop 0 acc = pure (reverse acc)
    loop remaining acc = do
      hdr <- D.readByteD
      if hdr /= 0
        then D.failD ("Fory.Direct: only homogeneous-no-null map chunks "
                       ++ "supported on direct decode (flag " ++ show hdr ++ ")")
        else do
          cs <- fromIntegral <$> D.readByteD
          _kTag <- D.readVaruint32D
          _vTag <- D.readVaruint32D
          entries <- replicateMD cs $ do
            k <- directDecodePayload @k
            v <- directDecodePayload @v
            pure (k, v)
          loop (remaining - cs) (reverse entries ++ acc)

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

-- DecodeM-friendly replicateM (avoids relying on the Monad
-- instance's default which builds a list of intermediate
-- @DecodeM (a:_)@ thunks).
replicateMD :: Int -> D.DecodeM a -> D.DecodeM [a]
replicateMD !n0 act = go n0 []
  where
    go !i acc
      | i <= 0    = pure (reverse acc)
      | otherwise = do
          x <- act
          go (i - 1) (x : acc)
{-# INLINE replicateMD #-}

-- | 'foldlM' over a plain Haskell list. Local re-implementation
-- so we don't pull a transformers dep just for this. Same
-- shape as 'Data.Vector.foldM''.
foldlMList :: Monad m => (b -> a -> m b) -> b -> [a] -> m b
foldlMList !_ !z []     = pure z
foldlMList !f !z (x:xs) = do
  z' <- f z x
  foldlMList f z' xs
{-# INLINE foldlMList #-}
