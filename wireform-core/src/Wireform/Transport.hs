module Wireform.Transport
  ( Transport (..)
  , WaitResult (..)
  ) where

import Control.Exception (SomeException)
import Data.Word (Word64, Word8)
import Foreign.Ptr (Ptr)
import Wireform.Ring.Internal (MagicRing)

-- | Abstraction over a producer filling a magic ring.
-- The parser and driver interact with data exclusively through this.
--
-- @Word64@ cursor positions are monotonic (never wrap during process
-- lifetime).  The byte at logical position @p@ lives at
-- @ringBase + (p .&. ringMask)@ in the ring.
data Transport = Transport
  { transportRing       :: !MagicRing
    -- ^ The shared ring buffer.

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

-- | Outcome of waiting for more data.
data WaitResult
  = MoreData {-# UNPACK #-} !Word64
    -- ^ New head position.
  | EndOfInput
    -- ^ Clean end (sticky: once returned, all subsequent calls return this).
  | TransportError !SomeException
    -- ^ Producer-side failure (sticky).
  deriving stock (Show)
