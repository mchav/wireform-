{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      : Kafka.Protocol.Wire.Codec
Description : Wire-shaped runners over the codegen-emitted native pokes

Every Kafka message ships a 'WireCodec' instance whose 'wireCodec'
field is a native 'WireCodecImpl' populated by codegen-emitted
'wirePokeFoo' / 'wirePeekFoo' / 'wireMaxSizeFoo' functions from
"Kafka.Protocol.Codegen.WireGenerator". This is the direct-poke
path: one 'mallocForeignPtrBytes', a single 'wirePokeFor', and a
slice — no 'Builder', no parser monad, no per-record allocations.

There is no Serial fallback in the runtime path: the runners
('runEncodeVer' / 'runDecodeVer' / 'runEncodeVerInto') dispatch
unconditionally through the 'WireCodec' instance. Call sites pick
the message type via 'TypeApplications':

@
WC.runEncodeVer \@Module.Foo apiVersion msg
@

The codegen no longer emits 'Data.Bytes.Serial'-shaped
@encodeFoo@ / @decodeFoo@ functions; that machinery is deprecated
and gone from every Generated/*.hs module.
-}
module Kafka.Protocol.Wire.Codec (
  -- * Versioned encode / decode
  runEncodeVer,
  runDecodeVer,

  -- * Versioned encode straight into a caller-supplied buffer
  runEncodeVerInto,

  -- * Wire-codec typeclass + dispatch record
  WireCodec (..),
  WireCodecImpl (..),
) where

import Control.Exception (SomeException)
import Control.Exception qualified as Exc
import Data.ByteString (ByteString)
import Data.ByteString.Internal qualified as BSI
import Data.Int (Int16)
import Data.Word (Word8)
import Foreign.ForeignPtr (
  ForeignPtr,
  mallocForeignPtrBytes,
  withForeignPtr,
 )
import Foreign.Ptr (Ptr, minusPtr, plusPtr)
import GHC.IO (unsafePerformIO)


----------------------------------------------------------------------
-- Wire-codec typeclass + dispatch record
----------------------------------------------------------------------

{- | Bundle of three direct-poke functions for a message type:
a worst-case size estimator, an encoder that writes directly
into a caller-supplied buffer, and a decoder that reads from a
pointer pair and returns the value plus the advanced cursor.

This is what the codegen-emitted @wireMaxSizeFoo@ /
@wirePokeFoo@ / @wirePeekFoo@ are bundled into.
-}
data WireCodecImpl a = WireCodecImpl
  { wireMaxSizeFor :: !(Int16 -> a -> Int)
  {- ^ Upper bound on the bytes 'wirePokeFor' may write at the
  given API version. Exact for fixed-width primitives;
  worst-case for varints / variable-length payloads.
  -}
  , wirePokeFor :: !(Int16 -> Ptr Word8 -> a -> IO (Ptr Word8))
  {- ^ Write the value at the given API version starting at the
  pointer, returning the pointer past the last byte
  written.
  -}
  , wirePeekFor
      :: !( Int16
            -> ForeignPtr Word8
            -> Ptr Word8
            -> Ptr Word8
            -> Ptr Word8
            -> IO (a, Ptr Word8)
          )
  {- ^ Read the value at the given API version. Arguments:

    * source 'ForeignPtr' (kept alive while the result
      references slices of it),
    * @basePtr@ — the start of the source buffer inside the
      active 'withForeignPtr' scope (used by the
      zero-copy slice helpers in "Kafka.Protocol.Wire"),
    * cursor pointer (where to start reading),
    * end-of-buffer pointer (one past the last valid byte).

  Returns the decoded value plus the cursor past the last
  byte consumed.
  -}
  }


{- | A message type with a 'Wire' codec.

Every generated module emits an instance pointing at native
codegen-emitted 'wirePokeFoo' / 'wirePeekFoo' / 'wireMaxSizeFoo'
functions.

The class deliberately /does not/ provide a default
implementation. An empty @instance WireCodec MyType where {}@
is a compile error: every message type must explicitly populate
a 'WireCodecImpl' (the codegen does this for every Generated
module).
-}
class WireCodec a where
  wireCodec :: WireCodecImpl a


----------------------------------------------------------------------
-- Runners
----------------------------------------------------------------------

{- | Encode a versioned message to a fresh 'ByteString'. Pick the
message type at the call site via @TypeApplications@:

@
bs = WC.runEncodeVer \@Module.Foo apiVersion msg
@

Single-allocation, single-pass write into a freshly-malloced
buffer via the message type's 'WireCodec' instance.
-}
{-# INLINEABLE runEncodeVer #-}
runEncodeVer
  :: forall a
   . WireCodec a
  => Int16 -> a -> ByteString
runEncodeVer version msg = runWireEncode (wireCodec @a) version msg


{- | Decode a versioned message from a 'ByteString'. Pick the
expected type at the call site via @TypeApplications@:

@
WC.runDecodeVer \@Module.Foo apiVersion bs
@

Trailing bytes past the value are silently ignored — Kafka's
framing layer (the 4-byte length prefix on every request) makes
that the right call.
-}
{-# INLINEABLE runDecodeVer #-}
runDecodeVer
  :: forall a
   . WireCodec a
  => Int16 -> ByteString -> Either String a
runDecodeVer version bs = runWireDecode (wireCodec @a) version bs


{- | Encode a versioned message directly into a caller-supplied
'Ptr Word8', returning the pointer just past the last byte
written. Single-pass write into the destination buffer — no
allocation, no copy.

Used by the request framing layer to write a request body
straight into the final framed buffer (after the 4-byte
size prefix + header), avoiding the @ByteString@ '<>'
concat that would otherwise stitch the size + header + body
together.
-}
{-# INLINEABLE runEncodeVerInto #-}
runEncodeVerInto
  :: forall a
   . WireCodec a
  => Int16 -> a -> Ptr Word8 -> IO (Ptr Word8)
runEncodeVerInto version msg dst =
  wirePokeFor (wireCodec @a) version dst msg


----------------------------------------------------------------------
-- Wire-impl runners
----------------------------------------------------------------------

{- | Single-allocation encode using the supplied 'WireCodecImpl'.
Allocates @max 1 (wireMaxSizeFor impl version msg)@ bytes,
writes via 'wirePokeFor', then trims the resulting
'ByteString' to the actual length the poke advanced to.
-}
{-# INLINE runWireEncode #-}
runWireEncode :: WireCodecImpl a -> Int16 -> a -> ByteString
runWireEncode impl version msg = unsafePerformIO $ do
  let !ub = max 1 (wireMaxSizeFor impl version msg)
  fp <- mallocForeignPtrBytes ub
  withForeignPtr fp $ \basePtr -> do
    !endPtr <- wirePokeFor impl version basePtr msg
    let !len = endPtr `minusPtr` basePtr
    pure (BSI.fromForeignPtr fp 0 len)


{-# INLINE runWireDecode #-}
runWireDecode :: forall a. WireCodecImpl a -> Int16 -> ByteString -> Either String a
runWireDecode impl version bs =
  let (fp, off, len) = BSI.toForeignPtr bs
  in unsafePerformIO $ withForeignPtr fp $ \basePtr -> do
       let !startPtr = basePtr `plusPtr` off
           !endPtr = startPtr `plusPtr` len
       r <-
         Exc.try (wirePeekFor impl version fp basePtr startPtr endPtr)
           :: IO (Either SomeException (a, Ptr Word8))
       pure $ case r of
         Left e -> Left (show e)
         Right (v, _) -> Right v
