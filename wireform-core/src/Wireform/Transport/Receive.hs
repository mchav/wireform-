{-# LANGUAGE RankNTypes #-}

-- | Receive-side magic-ring transport.
--
-- The parser consumes bytes from a 'ReceiveTransport'.  The producer
-- (a network recv loop, a TLS context, an io_uring backend, an
-- in-memory test fixture, …) writes into the magic ring at @head@
-- and the parser drains @[tail, head)@.
--
-- == Cursor model
--
-- 'Word64' positions are monotonic (never wrap during process
-- lifetime).  The byte at logical position @p@ lives at
-- @ringBase + (p .&. ringMask)@ in the ring.
--
-- == Why the ring is stored as three primitive fields
--
-- 'ReceiveTransport' embeds the ring's base pointer, size, and mask
-- directly rather than carrying a 'Wireform.Ring.Internal.MagicRing'.
-- That keeps the type free of the ring's phantom @s@ parameter, so
-- the rank-2 scope safety of 'Wireform.Ring.withMagicRing' is something
-- callers opt into at the slice layer rather than something that
-- ripples through every 'ReceiveTransport'-using package.
-- 'receiveRing' reconstructs a polymorphic 'MagicRing' for callers
-- that still want to call 'ringBase' / 'ringSize' / 'ringMask' on a
-- record value.
module Wireform.Transport.Receive
  ( ReceiveTransport (..)
  , ReceiveWait (..)
  , receiveRing
  ) where

import Control.Exception (SomeException)
import Data.Word (Word64, Word8)
import Foreign.Ptr (Ptr)

import Wireform.Ring.Internal (MagicRing (..))

-- | A producer-side cursor + a slot to wait on more data.
--
-- The parser interacts with data exclusively through this record.
-- Implementations bind one to a socket, TLS context, io_uring
-- instance, or an in-memory fixture.
data ReceiveTransport = ReceiveTransport
  { receiveRingBase    :: {-# UNPACK #-} !(Ptr Word8)
    -- ^ Base address of the ring's first mapping.

  , receiveRingSize    :: {-# UNPACK #-} !Int
    -- ^ Physical ring size (N, not 2N).  Always a power of two.

  , receiveRingMask    :: {-# UNPACK #-} !Int
    -- ^ @ringSize - 1@, cached for cheap @pos .&. mask@.

  , receiveLoadHead    :: !(IO Word64)
    -- ^ Read the producer's current head position
    -- (monotonically increasing).

  , receiveAdvanceTail :: !(Word64 -> IO ())
    -- ^ Consumer publishes a new tail position
    -- (monotonically increasing).  The transport may use this for
    -- backpressure or buffer reuse.

  , receiveWaitData    :: !(Word64 -> IO ReceiveWait)
    -- ^ Block until head advances past the given position.
    -- Parks on the IO manager when blocking (same thread as the
    -- parser).

  , receiveClose       :: !(IO ())
    -- ^ Release resources.  Idempotent.
  }

-- | Outcome of waiting for more data.
data ReceiveWait
  = ReceiveMoreData {-# UNPACK #-} !Word64
    -- ^ New head position.
  | ReceiveEndOfInput
    -- ^ Clean end (sticky: once returned, all subsequent calls
    -- return this).
  | ReceiveFailed !SomeException
    -- ^ Producer-side failure (sticky).
  deriving stock (Show)

-- | Reconstruct the underlying 'MagicRing'.  Polymorphic in the
-- phantom @s@: the resulting handle does /not/ inherit the scope of
-- whatever ring originally produced these bytes, so callers should
-- treat it as un-scoped (i.e. equivalent to the pre-@MagicRing s@
-- world).  Use 'Wireform.Ring.withMagicRing' directly when you need
-- the type-system-enforced safety.
receiveRing :: ReceiveTransport -> MagicRing s
receiveRing t = MagicRing (receiveRingBase t) (receiveRingSize t)
{-# INLINE receiveRing #-}
