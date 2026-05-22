{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Wireform.Ring.Internal
  ( MagicRing (..)
  , newMagicRing
  , destroyMagicRing
  , withMagicRing
  , ringBase
  , ringSize
  , ringMask
  , MagicRingException (..)
  ) where

import Control.Exception (Exception, bracket, throwIO)
import Data.Bits ((.&.))
import Data.Typeable (Typeable)
import Data.Word (Word8)
import Foreign.C.Types (CInt (..), CLong (..), CSize (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (Storable (..), peek, poke)

-- | Opaque handle to a double-mapped ring buffer.
data MagicRing = MagicRing
  { mrBase :: {-# UNPACK #-} !(Ptr Word8)
  , mrSize :: {-# UNPACK #-} !Int
  }

-- C struct layout: { void *base; size_t size; }
-- On Windows there's an extra HANDLE field, but we don't need it from
-- the Haskell side — hs_ring_destroy handles cleanup.
data CRing = CRing
  { crBase :: {-# UNPACK #-} !(Ptr Word8)
  , crSize :: {-# UNPACK #-} !CSize
  }

instance Storable CRing where
  sizeOf _ = sizeOf (undefined :: Ptr ()) + sizeOf (undefined :: CSize)
    -- conservative: ignores padding. On all target platforms
    -- both fields are pointer-sized and naturally aligned.
  alignment _ = alignment (undefined :: Ptr ())
  peek p = do
    b <- peekByteOff p 0
    s <- peekByteOff p (sizeOf (undefined :: Ptr ()))
    pure (CRing b s)
  poke p (CRing b s) = do
    pokeByteOff p 0 b
    pokeByteOff p (sizeOf (undefined :: Ptr ())) s

foreign import capi unsafe "magic_ring.h hs_ring_create"
  c_ring_create :: CSize -> Ptr CRing -> IO CInt

foreign import capi unsafe "magic_ring.h hs_ring_destroy"
  c_ring_destroy :: Ptr CRing -> IO ()

foreign import capi unsafe "magic_ring.h hs_page_size"
  c_page_size :: IO CLong

data MagicRingException
  = MagicRingUnavailable !String
  deriving stock (Show, Typeable)

instance Exception MagicRingException

-- | Allocate a magic ring of at least the requested size (in bytes).
-- The actual size is rounded up to the nearest page-size multiple and
-- power of two.  Throws 'MagicRingUnavailable' on failure.
newMagicRing :: Int -> IO MagicRing
newMagicRing requested = alloca $ \p -> do
  poke p (CRing nullPtr 0)
  rc <- c_ring_create (fromIntegral requested) p
  if rc /= 0
    then throwIO (MagicRingUnavailable $
           "hs_ring_create failed for requested size " <> show requested)
    else do
      cr <- peek p
      pure MagicRing
        { mrBase = crBase cr
        , mrSize = fromIntegral (crSize cr)
        }

-- | Release the ring's virtual mappings.  Safe to call multiple times.
destroyMagicRing :: MagicRing -> IO ()
destroyMagicRing mr = alloca $ \p -> do
  poke p (CRing (mrBase mr) (fromIntegral (mrSize mr)))
  c_ring_destroy p

-- | Scoped allocation: creates a ring, runs the action, destroys the
-- ring on exit (normal or exceptional).
withMagicRing :: Int -> (MagicRing -> IO a) -> IO a
withMagicRing n = bracket (newMagicRing n) destroyMagicRing

-- | Base pointer to the start of the ring (first of the two mappings).
ringBase :: MagicRing -> Ptr Word8
ringBase = mrBase
{-# INLINE ringBase #-}

-- | Physical size of the ring in bytes (N, not 2N).
ringSize :: MagicRing -> Int
ringSize = mrSize
{-# INLINE ringSize #-}

-- | Bitmask for cheap @pos .&. ringMask@ indexing.
-- Always @ringSize - 1@ since the size is a power of two.
ringMask :: MagicRing -> Int
ringMask mr = mrSize mr - 1
{-# INLINE ringMask #-}
