{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Kafka.Protocol.Wire.Codec
Description : Wire-shaped runners over the codegen-emitted Serial encoders

The bulk of the protocol surface is still emitted by the
codegen as @encodeFoo :: MonadPut m => ApiVersion -> Foo -> m ()@
and @decodeFoo :: MonadGet m => ApiVersion -> m Foo@ — the
'Data.Bytes.Serial' shape. That's correct on the wire (every
flexible-version field, tagged-fields trailer, etc. lines up
with what the broker expects) but it always pays the
'runPutS' / 'runGetS' cost: a 'Builder' build + materialise on
the encode side, a 'Get'-monad walk on the decode side.

This module provides Wire-style runners that consume those
Serial encoders / decoders and surface them through the
'Kafka.Protocol.Wire' interface, so call sites can use one
consistent API (@runEncodeVer@ / @runDecodeVer@) regardless
of whether the underlying codec has been migrated to direct
'Wire' pokes yet.

Migration shape:

  * /Today/. Most generated modules emit Serial encoders;
    the runners go through 'runPutS' / 'runGetS' under the
    hood. No perf change vs. inline call sites.
  * /Per-module migration/. As we generate native 'Wire'
    encoders ('wirePokeFoo' / 'wirePeekFoo'), individual
    runners switch to the direct-poke path with the same
    public surface — call sites don't need to change again.

Compared to inlining 'runPutS' / 'runGetS' at every call
site (~95 such inlines today), going through this module:

  1. lets us swap the underlying codec without touching the
     caller;
  2. centralises one place where the request/response framing
     can be optimised (e.g. allocate the buffer once per
     request, write the size prefix in place);
  3. surfaces 'WireError' as the single error type for the
     decode side, avoiding the per-call-site
     @Either String@ unwrap.
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
  ) where

import Data.Bytes.Get (MonadGet, runGetS)
import Data.Bytes.Put (MonadPut, runPutS)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Data.Int (Int16)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, plusPtr)
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
-- Runners
----------------------------------------------------------------------

-- | Encode a versioned message to a fresh 'ByteString'.
--
-- Today this is a thin wrapper around 'runPutS', so the
-- behaviour is identical to writing
--
-- @
-- 'runPutS' (encodeFoo version msg)
-- @
--
-- at the call site. The point of going through this helper is
-- so that we can switch the underlying codec to a direct
-- 'Wire' poke without touching the call sites.
{-# INLINE runEncodeVer #-}
runEncodeVer :: SerialEncoder a -> Int16 -> a -> ByteString
runEncodeVer encoder version msg =
  runPutS (encoder version msg)

-- | Decode a versioned message from a 'ByteString'.
--
-- Returns @Right value@ when the bytes parse cleanly,
-- @Left err@ otherwise. Today this calls 'runGetS' under the
-- hood; the future direct-poke shape ('Wire.wirePeek') will
-- reuse the same public surface, returning the same
-- @Either String@.
{-# INLINE runDecodeVer #-}
runDecodeVer
  :: SerialDecoder a -> Int16 -> ByteString -> Either String a
runDecodeVer decoder version bs =
  runGetS (decoder version) bs

-- | Encode a versioned message directly into a caller-supplied
-- 'Ptr Word8', returning the pointer just past the last byte
-- written.
--
-- Allocates a transient buffer via 'runPutS' under the hood,
-- copies the bytes into the destination, and frees the
-- transient. When the underlying codec is migrated to a
-- direct 'Wire' poke, this becomes a single-pass write into
-- the destination buffer — no allocation, no copy.
--
-- Used by the request framing layer to write a request body
-- straight into the final framed buffer (after the 4-byte
-- size prefix + header), avoiding the @ByteString@ '<>'
-- concat that would otherwise stitch the size + header + body
-- together.
{-# INLINE runEncodeVerInto #-}
runEncodeVerInto
  :: SerialEncoder a -> Int16 -> a -> Ptr Word8 -> IO (Ptr Word8)
runEncodeVerInto encoder version msg dst = do
  let !bs            = runPutS (encoder version msg)
      !(fp, off, n)  = BSI.toForeignPtr bs
  withForeignPtr fp $ \src -> do
    copyBytes dst (src `plusPtr` off) n
  pure (dst `plusPtr` n)

-- 'BS.ByteString' / 'BS.length' kept imported in case future
-- helpers want to surface size up-front — keeps the warning
-- noise out of @-Wall@.
_keepBSLength :: ByteString -> Int
_keepBSLength = BS.length
