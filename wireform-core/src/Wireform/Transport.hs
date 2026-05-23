{-# LANGUAGE RankNTypes #-}

module Wireform.Transport
  ( Transport (..)
  , WaitResult (..)
  , transportRing
  , transportRingBase
  , transportRingSize
  , transportRingMask
  ) where

import Control.Exception (SomeException)
import Data.Word (Word64, Word8)
import Foreign.Ptr (Ptr)
import Wireform.Ring.Internal (MagicRing (..))

-- | Abstraction over a producer filling a magic ring.
-- The parser and driver interact with data exclusively through this.
--
-- @Word64@ cursor positions are monotonic (never wrap during process
-- lifetime).  The byte at logical position @p@ lives at
-- @ringBase + (p .&. ringMask)@ in the ring.
--
-- The ring is represented here as its three primitive fields (base
-- pointer, size, mask) rather than as a 'MagicRing': this lets the
-- 'Transport' type stay free of the ring's phantom @s@ parameter, so
-- the rank-2 scope safety of 'Wireform.Ring.withMagicRing' is something
-- callers opt into at the slice layer rather than something that
-- ripples through every 'Transport'-using package.  The 'transportRing'
-- getter reconstructs a polymorphic 'MagicRing' for callers that still
-- want to call 'ringBase' / 'ringSize' / 'ringMask' on a record value.
data Transport = Transport
  { transportRingBaseField :: {-# UNPACK #-} !(Ptr Word8)
    -- ^ Base address of the ring's first mapping.

  , transportRingSizeField :: {-# UNPACK #-} !Int
    -- ^ Physical ring size (N, not 2N).  Always a power of two.

  , transportRingMaskField :: {-# UNPACK #-} !Int
    -- ^ @ringSize - 1@, cached for cheap @pos .&. mask@.

  , transportLoadHead   :: !(IO Word64)
    -- ^ Read the producer's current head position (monotonically increasing).

  , transportAdvanceTail :: !(Word64 -> IO ())
    -- ^ Consumer publishes a new tail position (monotonically increasing).
    -- The transport may use this for backpressure or buffer reuse.

  , transportWaitData   :: !(Word64 -> IO WaitResult)
    -- ^ Block until head advances past the given position.
    -- Parks on the IO manager when blocking (same thread as the parser).

  , transportClose      :: !(IO ())
    -- ^ Release resources.  Idempotent.
  }

-- | Reconstruct the underlying 'MagicRing'.  Polymorphic in the
-- phantom @s@: the resulting handle does /not/ inherit the scope of
-- whatever ring originally produced these bytes, so callers should
-- treat it as un-scoped (i.e. equivalent to the pre-@MagicRing s@
-- world).  Use 'Wireform.Ring.withMagicRing' directly when you need
-- the type-system-enforced safety.
transportRing :: Transport -> MagicRing s
transportRing t =
  MagicRing (transportRingBaseField t) (transportRingSizeField t)
{-# INLINE transportRing #-}

-- | Base pointer of the transport's ring.
transportRingBase :: Transport -> Ptr Word8
transportRingBase = transportRingBaseField
{-# INLINE transportRingBase #-}

-- | Physical size of the transport's ring (N).
transportRingSize :: Transport -> Int
transportRingSize = transportRingSizeField
{-# INLINE transportRingSize #-}

-- | Wrap-mask of the transport's ring (@N - 1@).
transportRingMask :: Transport -> Int
transportRingMask = transportRingMaskField
{-# INLINE transportRingMask #-}

-- | Outcome of waiting for more data.
data WaitResult
  = MoreData {-# UNPACK #-} !Word64
    -- ^ New head position.
  | EndOfInput
    -- ^ Clean end (sticky: once returned, all subsequent calls return this).
  | TransportError !SomeException
    -- ^ Producer-side failure (sticky).
  deriving stock (Show)
