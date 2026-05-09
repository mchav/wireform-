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

This module provides Wire-style runners ('runEncodeVer' /
'runDecodeVer') that the rest of the client uses uniformly. The
runners check whether the message type @a@ has a native 'Wire'
codec available via the 'WireCodec' typeclass:

  * If 'wireCodec' returns @Just impl@ the runner dispatches into
    the direct-poke path — one 'mallocForeignPtrBytes', a single
    'wirePokeFor', and a slice. This is what the codegen-emitted
    'wirePokeFoo' / 'wirePeekFoo' from
    "Kafka.Protocol.Codegen.WireGenerator" populate.
  * If 'wireCodec' returns 'Nothing' the runner falls back to the
    'runPutS' / 'runGetS' shape.

The default 'WireCodec' instance every generated module emits is
@wireCodec = Nothing@; modules that have been migrated to native
'Wire' override it with @wireCodec = Just WireCodecImpl{...}@.

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

  * /Today/. Most generated modules emit @wireCodec = Nothing@; the
    runners go through 'runPutS' / 'runGetS' under the hood. Same
    wire bytes, no perf change vs. inline 'runPutS' call sites.
  * /Per-module migration/. As the WireGenerator emits native
    'wirePokeFoo' / 'wirePeekFoo', the generated module overrides
    'wireCodec' to dispatch into the native pokes — same public
    surface, no caller changes.
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
    -- * Internal: Serial-fallback runners (exposed for testing /
    -- benchmarking — bypasses the WireCodec dispatch).
  , runEncodeVerSerial
  , runDecodeVerSerial
  ) where

import Control.Exception (SomeException)
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

-- | A message type with an /optional/ native 'Wire' codec.
--
-- Default @wireCodec = Nothing@ — falls back to the Serial
-- runner. Generated modules that have been migrated to a native
-- 'Wire' codec override this with @wireCodec = Just impl@.
--
-- The instance is always defined (even when 'Nothing') so the
-- 'WireCodec' constraint on 'runEncodeVer' is transparently
-- satisfied at every call site. Adding the constraint required
-- no caller changes — the instance ships with the message type.
class WireCodec a where
  wireCodec :: Maybe (WireCodecImpl a)
  wireCodec = Nothing

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
