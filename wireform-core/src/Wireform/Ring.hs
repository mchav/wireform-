{-# LANGUAGE CApiFFI #-}

-- | Double-mapped (\"magic\") ring buffer.
--
-- A buffer of size @N@ (power of two, page-aligned) backed by a single
-- shared-memory object mapped twice into a contiguous @2N@-byte virtual
-- region.  Any read of up to @N@ bytes starting anywhere in
-- @[base, base + N)@ is contiguous in virtual memory — the MMU handles
-- the wrap transparently.
--
-- The parser's pointer-bumping primitives rely on this property: they
-- never contain wrap logic.
module Wireform.Ring
  ( MagicRing
  , newMagicRing
  , destroyMagicRing
  , withMagicRing
  , ringBase
  , ringSize
  , ringMask
  , MagicRingException (..)
  ) where

import Wireform.Ring.Internal
