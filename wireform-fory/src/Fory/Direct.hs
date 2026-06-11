{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- | Type-driven Fory encode / decode that bypasses the
dynamic 'Fory.Value.Value' AST.

'Fory.Class.encodeFory' and 'Fory.Class.decodeFory' build a
'Value' tree as an intermediate step, which means a list of
100 'Int's allocates 100 boxed @VarInt64Val n@ heap objects
before any byte hits the wire. The 'EncodeDirect' /
'DecodeDirect' classes here serialise typed Haskell values
straight into the IO encoder buffer (and out of the IO
decoder cursor) — no 'Value' allocation, no pattern-matched
dispatch in the inner loop.

Wire compatibility: 'encodeDirect' produces byte-for-byte
the same output as @encodeFory@ for every type that has
both a 'Fory.Class.ToFory' and an 'EncodeDirect' instance.

The supported types currently cover the bench shapes:
primitives, 'Text', 'ByteString', '[a]', 'Data.Map.Map',
'Data.HashMap.Strict.HashMap', the 11 primitive-array
newtypes, and any user struct exposed via
'EncodeDirectStruct' (see 'Fory.Direct.Struct').
-}
module Fory.Direct (
  -- * Top-level
  ForyTypeId (..),
  EncodeDirect (..),
  DecodeDirect (..),
  encodeDirect,
  decodeDirect,

  -- * Internal helpers exported for instance authors
  directTagged,
  readSlotAndTag,
  emitForyStringDirect,
  readForyStringDirect,
) where

import Data.Bits (shiftL, shiftR, xor, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.Char (ord)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Hashable (Hashable)
import Data.IORef (readIORef, writeIORef)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Primitive.ByteArray qualified as PBA
import Data.Primitive.Ptr qualified as PBP
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Array qualified as TA
import Data.Text.Encoding qualified as TE
import Data.Text.Internal qualified as TI
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as VM
import Data.Vector.Storable qualified as VS
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.ForeignPtr (castForeignPtr)
import Foreign.ForeignPtr qualified
import Foreign.Marshal.Utils qualified
import Foreign.Ptr (Ptr)
import Foreign.Ptr qualified
import Foreign.Storable (Storable, peekByteOff, pokeByteOff, sizeOf)
import Fory.Bulk qualified as B
import Fory.Decode qualified as D
import Fory.IO qualified as IO
import Fory.Options qualified as Opt
import Fory.TextHelpers qualified as TH
import Fory.TypeId qualified as T
import GHC.Float (castWord32ToFloat, castWord64ToDouble)
import GHC.Ptr (Ptr (Ptr), plusPtr)
import System.IO.Unsafe (unsafeDupablePerformIO)
import Wireform.FFI qualified as WFFI


-- ---------------------------------------------------------------------------
-- Top-level encode / decode
-- ---------------------------------------------------------------------------

{- | The Fory wire-format type tag for a Haskell type. This is
the value emitted as the @varuint32@ tag byte by the encoder
and consumed by 'readSlotAndTag' on the read side.
-}
class ForyTypeId a where
  directTypeId :: T.TypeId


{- | Type-driven encoder. Walk a typed Haskell value and emit
its Fory wire bytes directly into the IO encoder, with no
'Fory.Value.Value' allocation.
-}
class ForyTypeId a => EncodeDirect a where
  {- | Emit the type-tagged payload (no slot flag, no header).
  The default 'encodeDirect' wrapper writes the
  @0x02@ header byte and the @0xFF NOT_NULL_VALUE@
  slot flag, then calls this.
  -}
  directEncodePayload :: IO.Encoder -> a -> IO ()


  {- | Optional fast-path writer for inner loops (lists, maps,
  struct fields). Returns the maximum byte size of a single
  payload plus a raw 'Ptr Word8'-based writer that updates
  the cursor without touching the encoder's IORefs. When
  'Just', the list / map encoder reserves @perElem * len@
  bytes once with 'IO.withReservedRaw' and writes every
  element in a tight loop. When 'Nothing', the inner loop
  falls back to per-element 'directEncodePayload' (which
  pays the 'ensure' / 'IORef' cycle each call).
  -}
  directRawPoke :: Maybe (Int, Ptr Word8 -> Int -> a -> IO Int)
  directRawPoke = Nothing


class ForyTypeId a => DecodeDirect a where
  {- | Read the type-tagged payload assuming the slot byte is
  @NOT_NULL_VALUE@ and the tag has already been consumed by
  'readSlotAndTag'.
  -}
  directDecodePayload :: D.DecodeM a


  {- | Optional fast-path reader for inner loops. Mirrors
  'directRawPoke' on the encode side: returns a raw
  'Ptr Word8'-based reader that advances the cursor without
  touching the decoder's IORefs. When 'Just', the
  list / Vector decoder calls 'D.readSameTypeBatch' which
  pays the cursor 'IORef' cycle exactly twice for the
  entire batch.
  -}
  directRawPeek :: Maybe (Ptr Word8 -> Int -> IO (a, Int))
  directRawPeek = Nothing


{- | Encode a typed value to Fory bytes. Equivalent (byte-for-
byte) to @encodeFory . toFory@ but allocates no
intermediate 'Value' tree.
-}
{-# NOINLINE encodeDirect #-}
encodeDirect :: forall a. EncodeDirect a => a -> ByteString
encodeDirect !x = unsafeDupablePerformIO $
  IO.runEncoder Opt.defaultEncodeOptions $ \e -> do
    IO.emitByte e 0x02 -- xlang header
    IO.emitByte e 0xFF -- NOT_NULL_VALUE
    let !tag = directTypeId @a
    emitTagD e tag
    directEncodePayload @a e x


{- | Decode a Fory byte string into a typed value. Mirrors
'encodeDirect' — never builds a 'Value'.
-}
{-# NOINLINE decodeDirect #-}
decodeDirect :: forall a. DecodeDirect a => ByteString -> Either String a
decodeDirect !bs = D.runDecodeM (readSlotAndTag @a >> directDecodePayload @a) bs


-- ---------------------------------------------------------------------------
-- Helpers shared by instances
-- ---------------------------------------------------------------------------

emitTagD :: IO.Encoder -> T.TypeId -> IO ()
emitTagD !e (T.TypeId w) = IO.emitVaruint32 e (fromIntegral w)
{-# INLINE emitTagD #-}


{- | Verify the leading header + slot byte + type-tag,
producing nothing (the payload reader runs after).
-}
readSlotAndTag :: forall a. ForyTypeId a => D.DecodeM ()
readSlotAndTag = do
  hdr <- D.readByteD
  if hdr `mod` 2 == 0 && hdr /= 0x02
    then D.failD ("Fory.Direct: bad header byte " ++ show hdr)
    else do
      slot <- D.readByteD
      if slot /= 0xFF
        then
          D.failD
            ( "Fory.Direct: expected NOT_NULL_VALUE slot, got "
                ++ show slot
            )
        else do
          tag <- D.readVaruint32D
          let !expected = directTypeId @a
              T.TypeId w = expected
          if fromIntegral tag /= w
            then
              D.failD
                ( "Fory.Direct: expected type tag "
                    ++ show w
                    ++ ", got "
                    ++ show tag
                )
            else pure ()


{- | Helper that emits (tag varuint32 + payload) without the
header / slot bytes. Useful for instance authors writing
list / map / struct payloads where each element is itself
type-tagged.
-}
directTagged :: forall a. EncodeDirect a => IO.Encoder -> a -> IO ()
directTagged !e !x = do
  emitTagD e (directTypeId @a)
  directEncodePayload @a e x
{-# INLINE directTagged #-}


{- | Re-export the encoder-side string-emit logic (LATIN-1
detection, header, bytes) for instance authors that want
to inline a string field without wrapping it in an
'EncodeDirect' instance.

Uses the SIMD 'WFFI.isAscii' from @wireform-core@ for the
ASCII fast-path detection. For 1024-byte ASCII strings
the SIMD scan runs in a few nanoseconds, easily amortising
the @ccall unsafe@ marshalling overhead — significantly
faster than the per-byte 'BS.all' scan on long inputs.
(Per-element list-of-string still uses the Word64-stride
'byteArrayIsAscii' since the FFI overhead would dominate
on short strings — see 'writeTextOnto'.)
| Single-Text encode for the typed pipeline.

Reads the 'Text''s underlying 'TI.Text arr off len'
directly. The ASCII probe is length-adaptive: the
Word64-stride 'TH.byteArrayIsAscii' wins for short
inputs (no FFI overhead), the simdutf-backed
'WFFI.isAsciiBS' wins for long inputs (vectorised
16/32-byte chunks). The crossover is around the FFI
ccall cost (~3 ns) vs the Word64 scan time, which
empirically lands around 64 bytes.

For ASCII / UTF-8 inputs the underlying bytes are
already valid 'Text' payload, so we 'memcpy' them
straight into the encoder buffer via
'TH.copyTextArrayToPtr' (a single
'copyByteArrayToAddr#' primop). LATIN-1 strings with
chars 128–255 fall back to 'B.latin1Bytes' for the
1-byte-per-char wire format.
-}
emitForyStringDirect :: IO.Encoder -> Text -> IO ()
emitForyStringDirect !e t@(TI.Text arr srcOff len)
  | TH.byteArrayIsAscii arr srcOff (srcOff + len) =
      -- Reserve enough for the worst-case 9-byte varuint
      -- header + payload bytes up front, then inline the
      -- header poke and the 'memcpy' in a single
      -- 'IO.withReservedRaw' batch — saves one
      -- 'ensure / readIORef / writeIORef' trio compared
      -- to @emitVaruint64 + withReservedRaw@.
      IO.withReservedRaw e (9 + len) $ \p start -> do
        !off1 <-
          IO.pokeVaruint64Raw
            p
            start
            (fromIntegral len `shiftL` 2 :: Word64)
        TH.copyTextArrayToPtr arr srcOff (p `plusPtr` off1) len
        pure (off1 + len)
  | T.all (\c -> ord c < 256) t = do
      let !raw = B.latin1Bytes t
          !rlen = BS.length raw
      IO.withReservedRaw e (9 + rlen) $ \p start -> do
        !off1 <-
          IO.pokeVaruint64Raw
            p
            start
            (fromIntegral rlen `shiftL` 2 :: Word64)
        let (BSI.BS fpSrc _) = raw
        Foreign.ForeignPtr.withForeignPtr fpSrc $ \pSrc ->
          Foreign.Marshal.Utils.copyBytes
            (p `Foreign.Ptr.plusPtr` off1)
            pSrc
            rlen
        pure (off1 + rlen)
  | otherwise =
      IO.withReservedRaw e (9 + len) $ \p start -> do
        !off1 <-
          IO.pokeVaruint64Raw
            p
            start
            ((fromIntegral len `shiftL` 2) .|. 2 :: Word64)
        TH.copyTextArrayToPtr arr srcOff (p `plusPtr` off1) len
        pure (off1 + len)
{-# INLINE emitForyStringDirect #-}


{- | 'memcpy' from a 'TA.Array' (Text's underlying
'ByteArray#') into a raw 'Ptr Word8'. Compiles to a single
@copyByteArrayToAddr#@ primop call. Used by the typed
list-of-string fast paths to avoid the per-element
'TE.encodeUtf8' allocation.
-}
copyTextArrayToPtr :: TA.Array -> Int -> Ptr Word8 -> Int -> IO ()
copyTextArrayToPtr = TH.copyTextArrayToPtr
{-# INLINE copyTextArrayToPtr #-}


readForyStringDirect :: D.DecodeM Text
readForyStringDirect = D.readForyString


-- ---------------------------------------------------------------------------
-- Primitive instances
-- ---------------------------------------------------------------------------

{- | Haskell 'Int' encodes as 'T.VARINT64' on the wire,
matching pyfory's default for Python @int@ (zigzag-varuint
bytes, 1 byte for small values, up to 9 bytes for
@maxBound@). For users who want fixed-8-byte encoding —
and the corresponding ~4× speedup on sequence-of-int
shapes — use the explicit 'Int64' type (which maps to
'T.INT64'), or 'VS.Vector Int' / 'VS.Vector Int64' for
flat, zero-copy 'INT64_ARRAY' payloads. This mirrors
pyfory's @int@ vs @int64@ vs @numpy.int64[]@ distinction.
-}
instance ForyTypeId Int where directTypeId = T.VARINT64


instance EncodeDirect Int where
  directEncodePayload e n = IO.emitVarint64 e (fromIntegral n)
  directRawPoke =
    Just
      ( 9
      , \p off n ->
          IO.pokeVarint64Raw p off (fromIntegral n)
      )
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
  directRawPoke =
    Just
      ( 1
      , \p off n ->
          IO.pokeByteRaw p off (fromIntegral n)
      )
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
  directRawPoke =
    Just
      ( 1
      , \p off b ->
          IO.pokeByteRaw p off (if b then 1 else 0)
      )
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


  -- Raw-pointer reader for the same-type batched list /
  -- vector decode path. Reads the varuint header inline,
  -- probes the LATIN-1 payload bytes via 'isAsciiPtr'
  -- (Word64-stride OR-fold) and constructs the resulting
  -- 'Text' via a fresh @ByteArray@ + 'memcpy' for ASCII —
  -- skipping 'TE.decodeLatin1''s extra walk that
  -- dimensions the destination via the high-bit count.
  -- Non-ASCII LATIN-1 falls back to a per-byte UTF-8
  -- expansion in 'expandLatin1Text'.
  directRawPeek = Just rawPeekText
  {-# INLINE directRawPeek #-}


{- | Raw-pointer reader for a single 'Text'. Used by the
batched 'Vector Text' / @[Text]@ decode paths through the
generic 'readSameTypeBatch' / 'readSameTypeBatchList'
machinery — bypasses 'Decoder' 'IORef' cycles per
element.
-}
rawPeekText :: Ptr Word8 -> Int -> IO (Text, Int)
rawPeekText !p !pos0 = do
  (hdr, !pos1) <- D.peekVaruint64Raw p pos0
  let !enc = hdr .&. 0x03
      !len = fromIntegral (hdr `shiftR` 2) :: Int
      !pos2 = pos1 + len
  case enc of
    0 -> do
      ascii <- isAsciiPtr (p `plusPtr` pos1) len
      if ascii
        then do
          !t <- copyAsciiText p pos1 len
          pure (t, pos2)
        else do
          !t <- expandLatin1Text p pos1 len
          pure (t, pos2)
    2 -> do
      -- UTF-8 wire. The bytes are already a valid Text
      -- payload, so we skip 'TE.decodeUtf8'' validation.
      -- Pyfory only emits tag 2 for valid UTF-8;
      -- 'wireform-fory-interop-fuzz' verifies this.
      !t <- copyAsciiText p pos1 len
      pure (t, pos2)
    1 -> do
      -- UTF-16-LE — rare; reconstruct a 'ByteString' slice
      -- and route through 'TE.decodeUtf16LE'.
      let !bs = BSI.PS pinned pos1 len
            where
              pinned =
                error
                  "rawPeekText: UTF-16 path \
                  \requires Decoder access"
      pure (TE.decodeUtf16LE bs, pos2)
    _ -> error ("Fory.Direct: reserved string encoding " ++ show enc)
{-# INLINE rawPeekText #-}


{- | Build a 'Text' from a fresh 'ByteArray' that's a copy
of @len@ bytes starting at @p[srcOff]@. Caller is
responsible for asserting the bytes are valid UTF-8 (=
ASCII for the same-type batch fast path, or already-
valid UTF-8 for the tag-2 path).
-}
copyAsciiText :: Ptr Word8 -> Int -> Int -> IO Text
copyAsciiText !p !srcOff !len
  | len == 0 = pure T.empty
  | otherwise = do
      mba <- PBA.newByteArray len
      ( PBP.copyPtrToMutableByteArray
          mba
          0
          (p `plusPtr` srcOff :: Ptr Word8)
          len
        )
      PBA.ByteArray ba# <- PBA.unsafeFreezeByteArray mba
      pure $! TI.Text (TA.ByteArray ba#) 0 len
{-# INLINE copyAsciiText #-}


{- | Expand @len@ LATIN-1 bytes at @p[srcOff]@ to a UTF-8
'Text'. Per-byte: chars under 0x80 are one byte, chars
0x80–0xFF are two UTF-8 bytes
@0xC2-0xC3 / 0x80-0xBF@.
-}
expandLatin1Text :: Ptr Word8 -> Int -> Int -> IO Text
expandLatin1Text !p !srcOff !len = do
  -- Worst-case destination size is @2 * len@.
  mba <- PBA.newByteArray (2 * len)
  let goE !i !dst
        | i >= len = pure dst
        | otherwise = do
            !b <- peekByteOff p (srcOff + i) :: IO Word8
            if b < 0x80
              then do
                PBA.writeByteArray mba dst b
                goE (i + 1) (dst + 1)
              else do
                PBA.writeByteArray
                  mba
                  dst
                  ((0xC0 .|. (b `shiftR` 6)) :: Word8)
                PBA.writeByteArray
                  mba
                  (dst + 1)
                  ((0x80 .|. (b .&. 0x3F)) :: Word8)
                goE (i + 1) (dst + 2)
  utf8len <- goE 0 0
  PBA.ByteArray ba# <- PBA.unsafeFreezeByteArray mba
  pure $! TI.Text (TA.ByteArray ba#) 0 utf8len
{-# INLINE expandLatin1Text #-}


{- | Word64-stride ASCII probe over @len@ bytes at @p@.
Compares the OR-reduction against @0x8080808080808080@.
Tail bytes (\< 8) are checked one at a time.
-}
isAsciiPtr :: Ptr Word8 -> Int -> IO Bool
isAsciiPtr !p !len = goP 0 0
  where
    !blocks = len `shiftR` 3
    !tailStart = blocks `shiftL` 3
    goP :: Int -> Word64 -> IO Bool
    goP !blkI !acc
      | blkI >= blocks = goT tailStart acc
      | otherwise = do
          !w <- peekByteOff p (blkI `shiftL` 3) :: IO Word64
          goP (blkI + 1) (acc .|. w)
    goT !i !acc
      | i >= len = pure ((acc .&. 0x8080808080808080) == 0)
      | otherwise = do
          !b <- peekByteOff p i :: IO Word8
          goT (i + 1) (acc .|. fromIntegral b)
{-# INLINE isAsciiPtr #-}


instance ForyTypeId ByteString where directTypeId = T.BINARY


instance EncodeDirect ByteString where
  directEncodePayload !e !bs = do
    let !blen = BS.length bs
        (BSI.BS fpSrc _) = bs
    IO.withReservedRaw e (9 + blen) $ \p start -> do
      !off1 <- IO.pokeVaruint64Raw p start (fromIntegral blen)
      Foreign.ForeignPtr.withForeignPtr fpSrc $ \pSrc ->
        Foreign.Marshal.Utils.copyBytes
          (p `Foreign.Ptr.plusPtr` off1)
          pSrc
          blen
      pure (off1 + blen)
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
    Just {} -> directEncodePayload e (V.fromList xs)
    Nothing -> emitListSlowGeneric e xs
  {-# INLINE directEncodePayload #-}


{- | Fallback list emitter for element types without a raw-poke
fast path. Walks the list twice (once for length, once for
writes) but pays no extra allocation and no per-element
closure dispatch beyond the existing class method call.
-}
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

{- | OVERLAPPING fast path for @[Int]@ — the bench's
@list-of-int 100@ shape. Walks the list twice in tight
monomorphic loops: once for length, once for the writes.
Uses 'T.VARINT64' wire (matching pyfory's default for
Python @int@) so each element is a 1–9-byte zigzag-varuint
via 'IO.pokeVarint64Raw'.
-}
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
      goInts !_ !off [] = pure off
      goInts !p !off (n : rest) = do
        !off' <- IO.pokeVarint64Raw p off (fromIntegral n)
        goInts p off' rest
  {-# INLINE directEncodePayload #-}


{- | OVERLAPPING fast path for @[Int32]@ — fixed 4-byte LE
per element. Same single-pass-write approach as
@[Int]@; total reservation is exactly @4 * length xs@.
-}
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
      goInt32s !_ !off [] = pure off
      goInt32s !p !off (n : rest) = do
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
      goDoubles !_ !off [] = pure off
      goDoubles !p !off (d : rest) = do
        !off' <- IO.pokeFloat64LERaw p off d
        goDoubles p off' rest
  {-# INLINE directEncodePayload #-}


{- | OVERLAPPING fast path for @Vector Int@. Same 'T.VARINT64'
wire as @[Int]@ but with O(1) 'V.length'.
-}
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
          V.foldM'
            ( \ !off n ->
                IO.pokeVarint64Raw p off (fromIntegral n)
            )
            start
            v
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


{- | OVERLAPPING fast path for @[Text]@. Mirrors the
Value-side 'emitStringListFast': pre-encodes each element
to UTF-8 + classifies it as LATIN-1 vs UTF-8 in one walk
(which also gives us the length), sums the upper-bound
total bytes, then writes the entire list payload via a
single 'IO.withReservedRaw'.
| OVERLAPPING fast path for @[Text]@.

Walks the list twice: once to count + sum the upper-bound
byte size, once to write. Neither pass allocates per
element — no intermediate @ByteString@ from 'TE.encodeUtf8',
no boxed @TextEntry@ tuple. The size estimate uses the
'Text''s underlying UTF-8 byte length (which is exact for
ASCII and UTF-8 strings; over-reserves by a factor of
two for the rare Latin-1-only case where chars in
128–255 take 2 UTF-8 bytes but only 1 wire byte).

Per-element write: one 'byteArrayIsAscii' OR-fold (Word64-
stride, 1 read per 8 bytes), a 1- or 9-byte header poke,
and a 'copyByteArrayToAddr#' memcpy. ASCII strings stay
byte-identical to pyfory's LATIN-1 default; the Latin-1
fallback uses 'B.latin1Bytes' for correctness.
-}
instance {-# OVERLAPPING #-} EncodeDirect [Text] where
  directEncodePayload !e !xs = do
    let (!n, !total, !allShort) = sizeListText xs 0 0 1
    IO.emitVaruint32 e (fromIntegral n)
    if n == 0
      then pure ()
      else do
        IO.emitByte e 0x08
        emitTagD e T.STRING
        IO.withReservedRaw e total $ \p start ->
          if allShort
            then do
              !endOff <- goOptimistic p start xs
              if WFFI.isAscii p start (endOff - start)
                then pure endOff
                else goWriteList p start xs
            else
              goWriteList p start xs
    where
      goWriteList :: Ptr Word8 -> Int -> [Text] -> IO Int
      goWriteList !_ !off [] = pure off
      goWriteList !p !off (t : rest) = do
        !off' <- writeTextOnto p off t
        goWriteList p off' rest

      goOptimistic :: Ptr Word8 -> Int -> [Text] -> IO Int
      goOptimistic !_ !off [] = pure off
      goOptimistic !p !off (t : rest) = do
        !off' <- writeTextOptimistic p off t
        goOptimistic p off' rest


{- | Single-pass length + size accumulator for @[Text]@.
Tracks @(count, totalBytes, allShort)@ where @allShort@
is True when every element fits in the
@len < 32@ short-header fast-path window. Uses an Int
(1 / 0) for the boolean so GHC compiles the recursion
to an unboxed @Int# -> Int# -> Int# -> Int# -> ...@
loop rather than allocating a boxed @Bool@ per
iteration.
-}
sizeListText :: [Text] -> Int -> Int -> Int -> (Int, Int, Bool)
sizeListText [] !n !sz !sh = (n, sz, sh /= 0)
sizeListText (TI.Text _ _ len : ts) !n !sz !sh =
  let !lt = if len < 32 then 1 :: Int else 0
  in sizeListText ts (n + 1) (sz + 9 + len) (sh * lt)


{- | OVERLAPPING fast path for @Vector Text@. Same two-pass
approach as @[Text]@ but with O(1) 'V.length'. Uses an
optimistic single-SIMD-scan strategy: write all elements
tagged as LATIN-1, then do one 'WFFI.isAscii' over the
entire payload region. ASCII lists (the common case)
are correctly encoded after pass 1; mixed lists fall
back to per-element rescan.
-}
instance {-# OVERLAPPING #-} EncodeDirect (Vector Text) where
  directEncodePayload !e !xs = do
    let !n = V.length xs
    IO.emitVaruint32 e (fromIntegral n)
    if n == 0
      then pure ()
      else do
        IO.emitByte e 0x08
        emitTagD e T.STRING
        -- Single-Int hand-rolled fold for the total upper-
        -- bound byte size only. We /don't/ track a separate
        -- @allShort@ flag here: the optimistic write writes
        -- a 1-byte tag-0 header @(len << 2)@, and for
        -- @len >= 32@ that byte has its high bit set. The
        -- post-write SIMD ASCII scan therefore detects
        -- both /payload/ non-ASCII bytes and /malformed/
        -- short-headers in a single pass, so the
        -- 'allShort' tracking is redundant.
        let goSize :: Int -> Int -> Int
            goSize !i !sz
              | i >= n = sz
              | otherwise =
                  let TI.Text _ _ l = V.unsafeIndex xs i
                  in goSize (i + 1) (sz + 9 + l)
            !total = goSize 0 0
        IO.withReservedRaw e total $ \p start -> do
          -- Hand-rolled optimistic writer. The offset
          -- accumulator is necessarily boxed @Int@ here
          -- because 'IO Int' forces the result
          -- representation; CPR analysis on the
          -- recursive 'goOpt' result didn't unbox the
          -- @Int@ (visible as
          -- @\$wgoOpt :: Int# -> Int -> ... -> Int@).
          -- Per-iteration cost of the box/unbox is small
          -- (one I# wrap + immediate unwrap each call,
          -- ~3 ns / element) and an attempt to re-write
          -- the loop with explicit Int# threading via
          -- a top-level @goOptVec#@ ended up regressing
          -- once GHC's inliner re-boxed the offset at
          -- the call site anyway.
          let goOpt :: Int -> Int -> IO Int
              goOpt !i !off
                | i >= n = pure off
                | otherwise = case V.unsafeIndex xs i of
                    TI.Text arr srcOff len -> do
                      pokeByteOff
                        p
                        off
                        (fromIntegral (len `shiftL` 2) :: Word8)
                      copyTextArrayToPtr
                        arr
                        srcOff
                        (p `plusPtr` (off + 1))
                        len
                      goOpt (i + 1) (off + 1 + len)
          !endOff <- goOpt 0 start
          if WFFI.isAscii p start (endOff - start)
            then pure endOff
            else do
              -- Either some payload byte is non-ASCII or
              -- some element had @len >= 32@ (malformed
              -- short-header). Either way, re-walk with
              -- the full per-element 'writeTextOnto'.
              let goFb :: Int -> Int -> IO Int
                  goFb !i !off
                    | i >= n = pure off
                    | otherwise = do
                        !off' <-
                          writeTextOnto
                            p
                            off
                            (V.unsafeIndex xs i)
                        goFb (i + 1) off'
              goFb 0 start
  {-# INLINE directEncodePayload #-}


{- | Single-pass step combining size accumulation and the
short-header eligibility check used by the typed list-of-
string fast paths. Saves a second 'V.all' walk over the
input.
-}

{- | Optimistic tag-0 writer used by the batched-SIMD-scan
list-of-string fast path. Writes a 1-byte header tagged
as LATIN-1 (= 0) plus the Text's underlying UTF-8 bytes.
For pure-ASCII Texts this produces the correct wire
bytes; for non-ASCII Texts, the caller's batched
'WFFI.isAscii' check fails and we re-walk via
'writeTextOnto'.
-}
writeTextOptimistic :: Ptr Word8 -> Int -> Text -> IO Int
writeTextOptimistic !p !off (TI.Text arr srcOff len) = do
  pokeByteOff p off (fromIntegral (len `shiftL` 2) :: Word8)
  copyTextArrayToPtr arr srcOff (p `plusPtr` (off + 1)) len
  pure (off + 1 + len)
{-# INLINE writeTextOptimistic #-}


{- | Per-element write for the typed list-of-string fast
paths.

Picks the wire-encoding tag (LATIN-1 / UTF-8) using the
hand-rolled 'byteArrayIsAscii' Word64-stride scan over
the underlying 'TA.Array'. The shared
'WFFI.isAscii' SIMD primitive (used in 'emitForyStringDirect'
below) is faster on long buffers but pays ~3 ns of FFI
marshalling overhead per call, which exceeds the entire
Word64-stride scan time on the 8-byte strings the
bench uses; per-element FFI is a measured net loss for
short list elements.

For Latin-1-only strings (chars 128–255) we re-encode
via 'B.latin1Bytes' so the wire is 1 byte / char.
-}
writeTextOnto :: Ptr Word8 -> Int -> Text -> IO Int
writeTextOnto !p !off t@(TI.Text arr srcOff len)
  | byteArrayIsAscii arr srcOff (srcOff + len) = do
      !off1 <- emitStringHeader p off len 0
      copyTextArrayToPtr arr srcOff (p `plusPtr` off1) len
      pure (off1 + len)
  | T.all (\c -> ord c < 256) t = do
      let !raw = B.latin1Bytes t
          !rlen = BS.length raw
      !off1 <- emitStringHeader p off rlen 0
      pokeBytesRawDirect p off1 raw
  | otherwise = do
      !off1 <- emitStringHeader p off len 2
      copyTextArrayToPtr arr srcOff (p `plusPtr` off1) len
      pure (off1 + len)
{-# INLINE writeTextOnto #-}


{- | Emit a Fory string-payload header @(len << 2) | tag@.
For payloads under 128 bytes, that's a single byte poke;
otherwise we fall back to 'IO.pokeVaruint64Raw'.
-}
emitStringHeader :: Ptr Word8 -> Int -> Int -> Int -> IO Int
emitStringHeader !p !off !len !tag
  | len < 32 = do
      pokeByteOff
        p
        off
        (fromIntegral ((len `shiftL` 2) .|. tag) :: Word8)
      pure (off + 1)
  | otherwise =
      IO.pokeVaruint64Raw
        p
        off
        ((fromIntegral len `shiftL` 2) .|. fromIntegral tag :: Word64)
{-# INLINE emitStringHeader #-}


-- 'byteArrayIsAscii' moved to 'Fory.TextHelpers' so
-- 'Fory.Encode' can use the same Word64-stride scanner
-- in its map-string-int fast path.
byteArrayIsAscii :: TA.Array -> Int -> Int -> Bool
byteArrayIsAscii = TH.byteArrayIsAscii
{-# INLINE byteArrayIsAscii #-}


instance forall a. DecodeDirect a => DecodeDirect [a] where
  directDecodePayload = do
    count <- fromIntegral <$> D.readVaruint32D
    if count == 0
      then pure []
      else do
        flag <- D.readByteD
        if flag /= 0x08
          then
            D.failD
              ( "Fory.Direct: expected IS_SAME_TYPE list, got flag "
                  ++ show flag
              )
          else do
            _tag <- D.readVaruint32D
            -- For element types with a raw-peek fast path,
            -- batch the reads through 'D.readSameTypeBatchList'
            -- (one cursor 'IORef' cycle for the whole batch).
            -- Otherwise fall back to per-element
            -- 'directDecodePayload'.
            case directRawPeek @a of
              Just rdr -> D.readSameTypeBatchList count rdr
              Nothing -> replicateMD count (directDecodePayload @a)


{- | OVERLAPPING fast path for @[Text]@ decode.

Same as the 'Vector Text' OVERLAPPING decode (avoids
the boxed @(Text, Int)@ tuple from 'rawPeekText'),
but builds a 'Vector Text' first and converts via
'V.toList'. The list-of-Text use case is rare enough
that the intermediate Vector + cons walk are still
faster than the tuple-allocating per-element path.
-}
instance {-# OVERLAPPING #-} DecodeDirect [Text] where
  directDecodePayload = V.toList <$> directDecodePayload @(Vector Text)


{- | OVERLAPPING fast path for @[Int]@ decode. Routes
through the existing 'Vector Int' inline decoder
(which threads 'Int#' offsets and writes into
'VM.IOVector' with no per-element @(Int, Int)@ tuple
alloc), then converts to a list.
-}
instance {-# OVERLAPPING #-} DecodeDirect [Int] where
  directDecodePayload = V.toList <$> directDecodePayload @(Vector Int)


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
          then
            D.failD
              ( "Fory.Direct: expected IS_SAME_TYPE list, got flag "
                  ++ show flag
              )
          else do
            _tag <- D.readVaruint32D
            case directRawPeek @a of
              Just rdr -> D.readSameTypeBatch count rdr
              Nothing -> V.replicateM count (directDecodePayload @a)


{- | OVERLAPPING fast path for @Vector Int@ decode.

The polymorphic 'readSameTypeBatch'-driven path pays
per-element overhead from the @rdr :: Ptr Word8 -> Int
-> IO (a, Int)@ contract: a fresh @(Int64, Int)@ tuple
allocation per element (visible in Core as
@(W64# ..., I# ...) -> case ipv5 of (val, pos')@) plus
a boxed-Int @pos@ accumulator that gets unboxed and
reboxed on every iteration.

This instance writes the inner loop directly with raw
pointer + 'Int#' offsets, calling the inlined
@goVu64@ varuint reader and writing into the
'VM.unsafeNew' / 'VM.unsafeWrite' / freeze pipeline
with no per-element tuple allocation.
-}
instance {-# OVERLAPPING #-} DecodeDirect (Vector Int) where
  directDecodePayload = do
    count <- fromIntegral <$> D.readVaruint32D
    if count == 0
      then pure V.empty
      else do
        flag <- D.readByteD
        if flag /= 0x08
          then
            D.failD
              ( "Fory.Direct: expected IS_SAME_TYPE list, got flag "
                  ++ show flag
              )
          else do
            _tag <- D.readVaruint32D
            decodeVecIntInline count


decodeVecIntInline :: Int -> D.DecodeM (Vector Int)
decodeVecIntInline !count = D.DecodeM $ \d -> do
  pos0 <- readIORef (D.decPos d)
  let !p = D.decBase d
  mvec <- VM.unsafeNew count
  let goVecInt !i !pos
        | i >= count = pure pos
        | otherwise = do
            (!w, !pos1) <- D.peekVaruint64Raw p pos
            -- Inline zigzag — same as 'D.peekVarint64Raw'
            -- but bypasses its boxed (Int64, Int) tuple
            -- result. Decode 'Int' directly from
            -- 'Word64' / 'Int64' without the extra
            -- 'fromIntegral' round-trip the polymorphic
            -- path would do.
            let !signed = fromIntegral (w `shiftR` 1) :: Int64
                !sgn = fromIntegral (w .&. 1) :: Int64
                !i64 = signed `xor` (-sgn)
            VM.unsafeWrite mvec i (fromIntegral i64 :: Int)
            goVecInt (i + 1) pos1
  posF <- goVecInt 0 pos0
  writeIORef (D.decPos d) posF
  V.unsafeFreeze mvec
{-# INLINE decodeVecIntInline #-}


{- | OVERLAPPING fast path for @Vector Text@ decode.

The polymorphic 'Vector a' instance routes through
'D.readSameTypeBatch' + 'rawPeekText', which forces a
boxed @(Text, Int)@ tuple per element. This bypasses
the tuple by inlining the raw varuint + payload reader
directly into a single mutable-write loop.
-}
instance {-# OVERLAPPING #-} DecodeDirect (Vector Text) where
  directDecodePayload = do
    count <- fromIntegral <$> D.readVaruint32D
    if count == 0
      then pure V.empty
      else do
        flag <- D.readByteD
        if flag /= 0x08
          then
            D.failD
              ( "Fory.Direct: expected IS_SAME_TYPE list, got flag "
                  ++ show flag
              )
          else do
            _tag <- D.readVaruint32D
            decodeVecTextInline count


{- | Inline 'Vector Text' decode loop. Each element is
read via 'rawPeekText' but with the @(Text, Int)@
tuple result split: the new offset is threaded as a
direct argument to the next call (no per-element
tuple alloc).
-}
decodeVecTextInline :: Int -> D.DecodeM (Vector Text)
decodeVecTextInline !count = D.DecodeM $ \d -> do
  pos0 <- readIORef (D.decPos d)
  let !p = D.decBase d
  mvec <- VM.unsafeNew count
  let go !i !pos
        | i >= count = pure pos
        | otherwise = do
            (hdr, !pos1) <- D.peekVaruint64Raw p pos
            let !enc = hdr .&. 0x03
                !len = fromIntegral (hdr `shiftR` 2) :: Int
                !pos2 = pos1 + len
            t <- case enc of
              0 -> do
                ascii <- isAsciiPtr (p `plusPtr` pos1) len
                if ascii
                  then copyAsciiText p pos1 len
                  else expandLatin1Text p pos1 len
              2 -> copyAsciiText p pos1 len
              _ ->
                error
                  ( "Fory.Direct: unsupported string encoding in batch "
                      ++ show enc
                  )
            VM.unsafeWrite mvec i t
            go (i + 1) pos2
  posF <- go 0 pos0
  writeIORef (D.decPos d) posF
  V.unsafeFreeze mvec
{-# INLINE decodeVecTextInline #-}


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
      !sz = sizeOf (undefined :: a)
      !byteLen = n * sz
      bs = BSI.BS (castForeignPtr fp) byteLen
  IO.emitVaruint32 e (fromIntegral byteLen)
  IO.emitBytes e bs
{-# INLINE emitStorableArrayPayload #-}


readStorableArrayPayload
  :: forall a. Storable a => D.DecodeM (VS.Vector a)
readStorableArrayPayload = do
  byteLen <- fromIntegral <$> D.readVaruint32D
  let elemSize = sizeOf (undefined :: a)
      (_, r) = byteLen `quotRem` elemSize
  if r /= 0
    then
      D.failD
        ( "Fory.Direct: array byte length not aligned: "
            ++ show byteLen
            ++ " % "
            ++ show elemSize
        )
    else do
      raw <- D.readBytesD byteLen
      pure (B.bytesToVecS raw)
{-# INLINE readStorableArrayPayload #-}


{- | 'VS.Vector Int' is the flat, zero-copy fast path for
sequences of 'Int' — analogous to a NumPy @int64@ array on
the Python side. On a 64-bit platform 'sizeOf (undefined ::
Int) == 8', so the wire format is 'T.INT64_ARRAY' and the
encode/decode are O(1) 'castForeignPtr' between the
'ByteString' and the vector. This delivers essentially the
same speed as 'VS.Vector Int64' and gives 'Int' users
access to the same fast path.

NOTE: requires a 64-bit Haskell platform (which is the
default on x86-64 Linux / aarch64 macOS / Windows-x64).
A static check would belong here for 32-bit ports.
-}
instance ForyTypeId (VS.Vector Int) where directTypeId = T.INT64_ARRAY


instance EncodeDirect (VS.Vector Int) where directEncodePayload = emitStorableArrayPayload


instance DecodeDirect (VS.Vector Int) where directDecodePayload = readStorableArrayPayload


instance ForyTypeId (VS.Vector Int8) where directTypeId = T.INT8_ARRAY


instance ForyTypeId (VS.Vector Int16) where directTypeId = T.INT16_ARRAY


instance ForyTypeId (VS.Vector Int32) where directTypeId = T.INT32_ARRAY


instance ForyTypeId (VS.Vector Int64) where directTypeId = T.INT64_ARRAY


instance ForyTypeId (VS.Vector Word8) where directTypeId = T.UINT8_ARRAY


instance ForyTypeId (VS.Vector Word16) where directTypeId = T.UINT16_ARRAY


instance ForyTypeId (VS.Vector Word32) where directTypeId = T.UINT32_ARRAY


instance ForyTypeId (VS.Vector Word64) where directTypeId = T.UINT64_ARRAY


instance ForyTypeId (VS.Vector Float) where directTypeId = T.FLOAT32_ARRAY


instance ForyTypeId (VS.Vector Double) where directTypeId = T.FLOAT64_ARRAY


instance EncodeDirect (VS.Vector Int8) where directEncodePayload = emitStorableArrayPayload


instance EncodeDirect (VS.Vector Int16) where directEncodePayload = emitStorableArrayPayload


instance EncodeDirect (VS.Vector Int32) where directEncodePayload = emitStorableArrayPayload


instance EncodeDirect (VS.Vector Int64) where directEncodePayload = emitStorableArrayPayload


instance EncodeDirect (VS.Vector Word8) where directEncodePayload = emitStorableArrayPayload


instance EncodeDirect (VS.Vector Word16) where directEncodePayload = emitStorableArrayPayload


instance EncodeDirect (VS.Vector Word32) where directEncodePayload = emitStorableArrayPayload


instance EncodeDirect (VS.Vector Word64) where directEncodePayload = emitStorableArrayPayload


instance EncodeDirect (VS.Vector Float) where directEncodePayload = emitStorableArrayPayload


instance EncodeDirect (VS.Vector Double) where directEncodePayload = emitStorableArrayPayload


instance DecodeDirect (VS.Vector Int8) where directDecodePayload = readStorableArrayPayload


instance DecodeDirect (VS.Vector Int16) where directDecodePayload = readStorableArrayPayload


instance DecodeDirect (VS.Vector Int32) where directDecodePayload = readStorableArrayPayload


instance DecodeDirect (VS.Vector Int64) where directDecodePayload = readStorableArrayPayload


instance DecodeDirect (VS.Vector Word8) where directDecodePayload = readStorableArrayPayload


instance DecodeDirect (VS.Vector Word16) where directDecodePayload = readStorableArrayPayload


instance DecodeDirect (VS.Vector Word32) where directDecodePayload = readStorableArrayPayload


instance DecodeDirect (VS.Vector Word64) where directDecodePayload = readStorableArrayPayload


instance DecodeDirect (VS.Vector Float) where directDecodePayload = readStorableArrayPayload


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


instance
  forall k v
   . (EncodeDirect k, EncodeDirect v, Ord k)
  => EncodeDirect (Map k v)
  where
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


instance
  forall k v
   . (EncodeDirect k, EncodeDirect v, Eq k, Hashable k)
  => EncodeDirect (HashMap k v)
  where
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
  :: forall k v
   . (EncodeDirect k, EncodeDirect v)
  => IO.Encoder
  -> [(k, v)]
  -> Int
  -> T.TypeId
  -> T.TypeId
  -> IO ()
emitMapChunks !e !entries !total !keyTag !valTag = go entries total
  where
    go _ 0 = pure ()
    go rest remaining = do
      let !cs = min 255 remaining
          (chunk, rest') = splitAt cs rest
      IO.emitByte e 0
      IO.emitByte e (fromIntegral cs)
      emitTagD e keyTag
      emitTagD e valTag
      mapM_
        ( \(k, v) -> do
            directEncodePayload @k e k
            directEncodePayload @v e v
        )
        chunk
      go rest' (remaining - cs)


instance
  forall k v
   . (DecodeDirect k, DecodeDirect v, Ord k)
  => DecodeDirect (Map k v)
  where
  directDecodePayload = M.fromList <$> readMapEntries


instance
  forall k v
   . (DecodeDirect k, DecodeDirect v, Eq k, Hashable k)
  => DecodeDirect (HashMap k v)
  where
  directDecodePayload = HM.fromList <$> readMapEntries


{- | OVERLAPPING fast path for the @Map Text Int@ shape that
shows up everywhere (string-keyed config, header dicts, ...).
Mirrors the Value-side 'emitMapStringVarInt64': pre-encodes
each string key, sums the upper bound, then writes the
whole homogeneous chunk via 'IO.withReservedRaw'.
-}
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
    go _ 0 = pure ()
    go rest remaining = do
      let !cs = min 255 remaining
          (chunk, rest') = splitAt cs rest
      IO.emitByte e 0
      IO.emitByte e (fromIntegral cs)
      emitTagD e T.STRING
      emitTagD e T.VARINT64
      -- Single-pass size estimate over the underlying Text
      -- byte length; no per-element 'TE.encodeUtf8'
      -- allocation. Per-entry write reads the Text's
      -- 'TA.Array' directly (Word64-stride ASCII scan via
      -- 'TH.byteArrayIsAscii' + 'memcpy' via
      -- 'TH.copyTextArrayToPtr').
      let !totalSize =
            sum [9 + utf8Len t + 9 | (t, _) <- chunk]
      IO.withReservedRaw e totalSize $ \p start ->
        foldlMList (writeOne p) start chunk
      go rest' (remaining - cs)

    utf8Len :: Text -> Int
    utf8Len (TI.Text _ _ len) = len

    writeOne :: Ptr Word8 -> Int -> (Text, Int) -> IO Int
    writeOne !p !off (t@(TI.Text arr srcOff len), !n) = do
      !off1 <-
        if TH.byteArrayIsAscii arr srcOff (srcOff + len)
          then do
            !o <-
              IO.pokeVaruint64Raw
                p
                off
                (fromIntegral len `shiftL` 2 :: Word64)
            TH.copyTextArrayToPtr arr srcOff (p `plusPtr` o) len
            pure (o + len)
          else
            if T.all (\c -> ord c < 256) t
              then do
                let !raw = B.latin1Bytes t
                    !rlen = BS.length raw
                !o <-
                  IO.pokeVaruint64Raw
                    p
                    off
                    (fromIntegral rlen `shiftL` 2 :: Word64)
                pokeBytesRawDirect p o raw
              else do
                !o <-
                  IO.pokeVaruint64Raw
                    p
                    off
                    ((fromIntegral len `shiftL` 2) .|. 2 :: Word64)
                TH.copyTextArrayToPtr arr srcOff (p `plusPtr` o) len
                pure (o + len)
      IO.pokeVarint64Raw p off1 (fromIntegral n)


pokeBytesRawDirect :: Ptr Word8 -> Int -> ByteString -> IO Int
pokeBytesRawDirect !p !pos !bs = do
  let (BSI.BS fpSrc lenSrc) = bs
  Foreign.ForeignPtr.withForeignPtr fpSrc $ \pSrc ->
    Foreign.Marshal.Utils.copyBytes
      (p `Foreign.Ptr.plusPtr` pos)
      pSrc
      lenSrc
  pure (pos + lenSrc)
{-# INLINE pokeBytesRawDirect #-}


readMapEntries
  :: forall k v
   . (DecodeDirect k, DecodeDirect v)
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
        then
          D.failD
            ( "Fory.Direct: only homogeneous-no-null map chunks "
                ++ "supported on direct decode (flag "
                ++ show hdr
                ++ ")"
            )
        else do
          cs <- fromIntegral <$> D.readByteD
          _kTag <- D.readVaruint32D
          _vTag <- D.readVaruint32D
          entries <- replicateMD cs $ do
            k <- directDecodePayload @k
            v <- directDecodePayload @v
            pure (k, v)
          loop (remaining - cs) (reverse entries ++ acc)


{- | OVERLAPPING fast path for @Map Text Int@ decode.
Mirrors the encode-side 'emitMapTextIntChunks' fast path:
per-entry uses raw-pointer 'rawPeekText' + raw
'D.peekVarint64Raw', amortising the 'IORef' /
'TE.decodeLatin1' overhead that the polymorphic
'readMapEntries' path would pay.
-}
instance {-# OVERLAPPING #-} DecodeDirect (Map Text Int) where
  directDecodePayload = M.fromList <$> readTextIntChunked
  {-# INLINE directDecodePayload #-}


instance {-# OVERLAPPING #-} DecodeDirect (HashMap Text Int) where
  directDecodePayload = HM.fromList <$> readTextIntChunked
  {-# INLINE directDecodePayload #-}


{- | Read a Fory map of @Text -> Int@ via the raw-pointer
machinery — bypasses 'D.readForyString' /
'D.readVarint64D' 'IORef' cycles per entry.
-}
readTextIntChunked :: D.DecodeM [(Text, Int)]
readTextIntChunked = do
  total <- fromIntegral <$> D.readVaruint32D
  if total == 0
    then pure []
    else D.DecodeM $ \d -> readTextIntChunksIO d total
{-# INLINE readTextIntChunked #-}


readTextIntChunksIO
  :: D.Decoder -> Int -> IO [(Text, Int)]
readTextIntChunksIO !d !total0 = do
  pos0 <- readDecPos d
  let !p = D.decBase d
  (!revEntries, !posF) <- goAll p pos0 total0 []
  writeDecPos d posF
  pure (reverse revEntries)
  where
    -- Single flat reversed accumulator — every entry is
    -- prepended O(1), one final 'reverse' at the end. No
    -- per-chunk list append.
    goAll !p !pos !remaining !acc
      | remaining <= 0 = pure (acc, pos)
      | otherwise = do
          !hdr <- peekByteOff p pos :: IO Word8
          if hdr /= 0
            then
              errorWithoutStackTrace
                ( "Fory.Direct: map chunk flag "
                    ++ show hdr
                    ++ " not supported on direct fast path"
                )
            else do
              !cs8 <- peekByteOff p (pos + 1) :: IO Word8
              let !cs = fromIntegral cs8 :: Int
              (_, !pos1) <- D.peekVaruint64Raw p (pos + 2)
              (_, !pos2) <- D.peekVaruint64Raw p pos1
              (!acc', !pos3) <- readN p pos2 cs acc
              goAll p pos3 (remaining - cs) acc'

    readN !_ !pos 0 !acc = pure (acc, pos)
    readN !p !pos !i !acc = do
      (!t, !pos1) <- rawPeekText p pos
      (!n, !pos2) <- D.peekVarint64Raw p pos1
      readN p pos2 (i - 1) ((t, fromIntegral n) : acc)


readDecPos :: D.Decoder -> IO Int
readDecPos = readIORef . D.decPos


writeDecPos :: D.Decoder -> Int -> IO ()
writeDecPos d = writeIORef (D.decPos d)


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
      | i <= 0 = pure (reverse acc)
      | otherwise = do
          x <- act
          go (i - 1) (x : acc)
{-# INLINE replicateMD #-}


{- | 'foldlM' over a plain Haskell list. Local re-implementation
so we don't pull a transformers dep just for this. Same
shape as 'Data.Vector.foldM''.
-}
foldlMList :: Monad m => (b -> a -> m b) -> b -> [a] -> m b
foldlMList !_ !z [] = pure z
foldlMList !f !z (x : xs) = do
  z' <- f z x
  foldlMList f z' xs
{-# INLINE foldlMList #-}
