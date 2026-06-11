{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE RankNTypes #-}

{- | Double-mapped (\"magic\") ring buffer.

A buffer of size @N@ (power of two, page-aligned) backed by a single
shared-memory object mapped twice into a contiguous @2N@-byte virtual
region.  Any read of up to @N@ bytes starting anywhere in
@[base, base + N)@ is contiguous in virtual memory — the MMU handles
the wrap transparently.

The parser's pointer-bumping primitives rely on this property: they
never contain wrap logic.

== Scoping

'MagicRing' carries a phantom type parameter @s@ that works like the
@s@ on 'Control.Monad.ST.ST'.  'withMagicRing' is rank-2:

> withMagicRing :: Int -> (forall s. MagicRing s -> IO a) -> IO a

Slices produced from the ring ('RingSlice') inherit @s@ and therefore
cannot appear in @a@ — they are unable to outlive the buffer that
backs them and would otherwise alias bytes that subsequent refills
overwrite.  When a slice has to leave the scope, 'copyRingSlice'
materialises a fresh 'ByteString' by memcpy.
-}
module Wireform.Ring (
  -- * The ring
  MagicRing,
  withMagicRing,
  ringBase,
  ringSize,
  ringMask,
  MagicRingException (..),

  -- * Slices
  RingSlice,
  ringSlice,
  ringSliceAtPos,
  ringSliceLength,
  withRingSlice,
  peekRingSliceByte,
  copyRingSlice,
) where

import Wireform.Ring.Internal

