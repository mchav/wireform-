{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-|
Module      : Kafka.Protocol.Wire.Codec
Description : Wire-shaped runners over the codegen-emitted Serial encoders

The bulk of the protocol surface is emitted by the codegen as

@encodeFoo :: MonadPut m => ApiVersion -> Foo -> m ()@
@decodeFoo :: MonadGet m => ApiVersion -> m Foo@

— the 'Data.Bytes.Serial' shape. That's correct on the wire (every
flexible-version field, tagged-fields trailer, etc. lines up with
what the broker expects) but it always pays the 'runPutS' /
'runGetS' cost: a 'Builder' build + materialise on the encode side,
a 'Get'-monad walk on the decode side.

Every Kafka message ships a 'WireCodec' instance whose 'wireCodec'
field is a 'Just'-valued 'WireCodecImpl'. The instance is one of:

  * the /native/ codec, populated by codegen-emitted
    'wirePokeFoo' / 'wirePeekFoo' / 'wireMaxSizeFoo' functions from
    "Kafka.Protocol.Codegen.WireGenerator". This is the direct-poke
    path: one 'mallocForeignPtrBytes', a single 'wirePokeFor', and a
    slice — no 'Builder', no parser monad, no per-record allocations.
  * the /Serial shim/ ('serialShimCodec'), used by the codegen for
    schemas it can't yet emit native code for (typically those with
    arrays of nested structs). Behaves byte-identically to the
    legacy 'runPutS' / 'runGetS' shape — wraps them inside a
    'WireCodecImpl' so the dispatch surface stays uniform.

The runners ('runEncodeVer' / 'runDecodeVer') case on the resulting
'Maybe'; the @Nothing@ branch is dead code in the generated output
and is kept only as a safety net for hand-written callers that
forget to provide a codec.

== Why a typeclass instead of changing the runner signature

Every existing call site looks like

@
WC.runEncodeVer Module.encodeFoo apiVersion msg
@

— it passes a top-level @encodeFoo@ function. We don't want to
churn ~95 call sites just to switch the underlying codec. The
'WireCodec a' constraint on 'runEncodeVer' is satisfied
transparently by the instance the generated module exports, so
adding the constraint is invisible at the call site (the instance
ships with the message type).

Migration shape:

  * /Today/. Every generated module emits a 'Just'-valued
    'WireCodec' instance — most pointing at the Serial shim, three
    (RequestHeader, ResponseHeader, ApiVersionsRequest) at native
    pokes. Wire-bytes are byte-identical for both shapes.
  * /Per-message migration/. As the WireGenerator learns to emit
    native code for new schema shapes (arrays, nested structs, ...),
    the generated module's 'WireCodec' instance flips from the shim
    constructor to the native pokes. Same public surface, no caller
    changes.
-}
module Kafka.Protocol.Wire.Codec
  ( -- * Versioned encode / decode
    runEncodeVer
  , runDecodeVer
    -- * Versioned encode straight into a caller-supplied buffer
  , runEncodeVerInto
    -- * Aliases the codegen will use
  , SerialEncoder
  , SerialDecoder
    -- * Wire-codec typeclass + dispatch record
  , WireCodec (..)
  , WireCodecImpl (..)
    -- * Serial shim — lifts a 'SerialEncoder' / 'SerialDecoder' pair
    -- into a 'WireCodecImpl' so messages whose codegen still emits
    -- the legacy 'Data.Bytes.Serial' shape ship through the same
    -- dispatch surface as the natively-generated ones. No
    -- @wireCodec = Nothing@ fallback survives in the generated
    -- output: every message has a 'Just'-valued codec.
  , serialShimCodec
    -- * Internal: Serial-fallback runners (exposed for testing /
    -- benchmarking — bypasses the WireCodec dispatch).
  , runEncodeVerSerial
  , runDecodeVerSerial
  ) where

import Control.Exception (SomeException, throwIO)
import qualified Control.Exception as Exc
import Data.Bytes.Get (MonadGet, runGetS)
import Data.Bytes.Put (MonadPut, runPutS)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Data.Int (Int16)
import Foreign.ForeignPtr
  ( ForeignPtr, mallocForeignPtrBytes, withForeignPtr )
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, minusPtr, plusPtr)
import GHC.IO (unsafePerformIO)
import Data.Word (Word8)

import Kafka.Protocol.Wire (WireError (WireInvalid))

----------------------------------------------------------------------
-- Aliases
----------------------------------------------------------------------

-- | The shape every generated @encodeFoo@ has: takes an api
-- version + the message + builds in an unspecified
-- 'MonadPut'. Use as the parameter type for 'runEncodeVer'.
type SerialEncoder a =
  forall m. MonadPut m => Int16 -> a -> m ()

-- | The shape every generated @decodeFoo@ has: takes an api
-- version + reads in an unspecified 'MonadGet'.
type SerialDecoder a =
  forall m. (MonadGet m, MonadFail m) => Int16 -> m a

----------------------------------------------------------------------
-- Wire-codec typeclass + dispatch record
----------------------------------------------------------------------

-- | Bundle of three direct-poke functions for a message type:
-- a worst-case size estimator, an encoder that writes directly
-- into a caller-supplied buffer, and a decoder that reads from a
-- pointer pair and returns the value plus the advanced cursor.
--
-- This is what the codegen-emitted @wireMaxSizeFoo@ /
-- @wirePokeFoo@ / @wirePeekFoo@ are bundled into.
data WireCodecImpl a = WireCodecImpl
  { wireMaxSizeFor :: !(Int16 -> a -> Int)
    -- ^ Upper bound on the bytes 'wirePokeFor' may write at the
    --   given API version. Exact for fixed-width primitives;
    --   worst-case for varints / variable-length payloads.
  , wirePokeFor    :: !(Int16 -> Ptr Word8 -> a -> IO (Ptr Word8))
    -- ^ Write the value at the given API version starting at the
    --   pointer, returning the pointer past the last byte
    --   written.
  , wirePeekFor    :: !(Int16 -> ForeignPtr Word8 -> Ptr Word8
                          -> Ptr Word8 -> Ptr Word8
                          -> IO (a, Ptr Word8))
    -- ^ Read the value at the given API version. Arguments:
    --
    --     * source 'ForeignPtr' (kept alive while the result
    --       references slices of it),
    --     * @basePtr@ — the start of the source buffer inside the
    --       active 'withForeignPtr' scope (used by the
    --       zero-copy slice helpers in "Kafka.Protocol.Wire"),
    --     * cursor pointer (where to start reading),
    --     * end-of-buffer pointer (one past the last valid byte).
    --
    --   Returns the decoded value plus the cursor past the last
    --   byte consumed.
  }

-- | A message type with a 'Wire' codec.
--
-- Every generated module emits an instance whose 'wireCodec' field
-- is 'Just' — either pointing at codegen-emitted native pokes or at
-- the 'serialShimCodec' wrapper around the legacy 'Serial'-shape
-- encoder + decoder. The 'Maybe' wrapper is kept on the method's
-- return type so the runners can keep their original case-on-Maybe
-- shape without a churn-y caller migration; the @Nothing@ branch
-- is dead in any non-pathological generated module.
--
-- The class deliberately /does not/ provide a default
-- implementation. An empty @instance WireCodec MyType where {}@
-- is a compile error (and a hint to use 'serialShimCodec' if a
-- native codec isn't yet available).
class WireCodec a where
  wireCodec :: Maybe (WireCodecImpl a)

----------------------------------------------------------------------
-- Runners
----------------------------------------------------------------------

-- | Encode a versioned message to a fresh 'ByteString'.
--
-- If the message type has a native 'Wire' codec ('wireCodec'
-- returns @Just impl@), this is a single-allocation, single-pass
-- write into a freshly-malloced buffer. Otherwise it falls back
-- to 'runPutS' over the supplied 'SerialEncoder' — bytewise
-- identical with the previous shape.
{-# INLINEABLE runEncodeVer #-}
runEncodeVer
  :: forall a. WireCodec a
  => SerialEncoder a -> Int16 -> a -> ByteString
runEncodeVer encoder version msg =
  case wireCodec @a of
    Nothing   -> runPutS (encoder version msg)
    Just impl -> runWireEncode impl version msg

-- | Decode a versioned message from a 'ByteString'.
--
-- Returns @Right value@ on success, @Left err@ otherwise. Uses
-- the 'WireCodec' instance's native peek if available; falls
-- back to 'runGetS' otherwise. Trailing bytes past the value
-- are silently ignored — Kafka's framing layer (the 4-byte
-- length prefix on every request) makes that the right call.
{-# INLINEABLE runDecodeVer #-}
runDecodeVer
  :: forall a. WireCodec a
  => SerialDecoder a -> Int16 -> ByteString -> Either String a
runDecodeVer decoder version bs =
  case wireCodec @a of
    Nothing   -> runGetS (decoder version) bs
    Just impl -> runWireDecode impl version bs

-- | Encode a versioned message directly into a caller-supplied
-- 'Ptr Word8', returning the pointer just past the last byte
-- written.
--
-- For the Serial fallback this allocates a transient buffer via
-- 'runPutS' and copies the bytes into the destination. For
-- messages with a native 'Wire' codec it's a single-pass write
-- into the destination buffer — no allocation, no copy.
--
-- Used by the request framing layer to write a request body
-- straight into the final framed buffer (after the 4-byte
-- size prefix + header), avoiding the @ByteString@ '<>'
-- concat that would otherwise stitch the size + header + body
-- together.
{-# INLINEABLE runEncodeVerInto #-}
runEncodeVerInto
  :: forall a. WireCodec a
  => SerialEncoder a -> Int16 -> a -> Ptr Word8 -> IO (Ptr Word8)
runEncodeVerInto encoder version msg dst =
  case wireCodec @a of
    Just impl -> wirePokeFor impl version dst msg
    Nothing   -> do
      let !bs           = runPutS (encoder version msg)
          !(fp, off, n) = BSI.toForeignPtr bs
      withForeignPtr fp $ \src -> do
        copyBytes dst (src `plusPtr` off) n
      pure (dst `plusPtr` n)

----------------------------------------------------------------------
-- Internal: Serial fallback (exposed for tests / benchmarks that
-- want to compare native-Wire output against the Serial baseline)
----------------------------------------------------------------------

-- | Force the Serial-runner path even when the message type has a
-- native Wire codec available. Useful for the cross-codec
-- equivalence property tests in @Protocol.WireCodecSpec@.
{-# INLINE runEncodeVerSerial #-}
runEncodeVerSerial :: SerialEncoder a -> Int16 -> a -> ByteString
runEncodeVerSerial encoder version msg =
  runPutS (encoder version msg)

----------------------------------------------------------------------
-- Serial shim
----------------------------------------------------------------------

-- | Build a 'WireCodecImpl' from a legacy 'Serial' encoder + decoder
-- pair. The resulting @impl@ behaves as if the message type had no
-- native 'Wire' codec — encode goes through 'runPutS' + a single
-- @memcpy@ into the destination buffer, decode goes through
-- 'runGetS' against a slice that's still backed by the source
-- 'ForeignPtr' (no extra copy on the decode hot path).
--
-- Used by the codegen so every generated module ships
-- @wireCodec = Just (serialShimCodec encodeFoo decodeFoo)@ even when
-- the WireGenerator can't yet emit a native codec for the schema.
-- Keeps the dispatch shape uniform — no 'Nothing' branch in any
-- 'WireCodec' instance — so callers stay on a single code path and
-- migrating a message to a native codec is a self-contained,
-- per-message diff.
--
-- The shim's 'wireMaxSizeFor' returns the actual byte count via a
-- one-shot 'runPutS'; it's exact (not a worst-case estimate) so the
-- buffer the runner allocates is sized correctly the first time.
{-# INLINEABLE serialShimCodec #-}
serialShimCodec
  :: SerialEncoder a
  -> SerialDecoder a
  -> WireCodecImpl a
serialShimCodec encoder decoder = WireCodecImpl
  { wireMaxSizeFor = \v msg ->
      -- Exact size: encode once, take the length. Cheaper than a
      -- worst-case-padding estimator for messages with variable
      -- payloads (records, large arrays) since the runner won't
      -- over-allocate.
      BS.length (runPutS (encoder v msg))
  , wirePokeFor = \v dst msg -> do
      let !bs            = runPutS (encoder v msg)
          !(fp, off, n)  = BSI.toForeignPtr bs
      withForeignPtr fp $ \src ->
        copyBytes dst (src `plusPtr` off) n
      pure (dst `plusPtr` n)
  , wirePeekFor = \v fp basePtr cur endPtr -> do
      let !off = cur `minusPtr` basePtr
          !len = endPtr `minusPtr` cur
          !bs  = BSI.fromForeignPtr fp off len
      case runGetS (decoder v) bs of
        Right val -> pure (val, endPtr)
        Left  err -> throwIO (WireInvalid err)
  }

-- | Force the Serial-runner path on the decode side. Mirror of
-- 'runEncodeVerSerial'.
{-# INLINE runDecodeVerSerial #-}
runDecodeVerSerial
  :: SerialDecoder a -> Int16 -> ByteString -> Either String a
runDecodeVerSerial decoder version bs =
  runGetS (decoder version) bs

----------------------------------------------------------------------
-- Wire-impl runners
----------------------------------------------------------------------

-- | Single-allocation encode using the supplied 'WireCodecImpl'.
-- Allocates @max 1 (wireMaxSizeFor impl version msg)@ bytes,
-- writes via 'wirePokeFor', then trims the resulting
-- 'ByteString' to the actual length the poke advanced to.
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
            !endPtr   = startPtr `plusPtr` len
        r <- Exc.try (wirePeekFor impl version fp basePtr startPtr endPtr)
               :: IO (Either SomeException (a, Ptr Word8))
        pure $ case r of
          Left e         -> Left (show e)
          Right (v, _)   -> Right v

-- 'BS.ByteString' / 'BS.length' kept imported in case future
-- helpers want to surface size up-front — keeps the warning
-- noise out of @-Wall@.
_keepBSLength :: ByteString -> Int
_keepBSLength = BS.length
