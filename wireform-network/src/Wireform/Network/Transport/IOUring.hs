{-# LANGUAGE CPP #-}

-- | Linux io_uring-based transport.
--
-- This module provides a transport implementation that uses Linux's
-- io_uring interface for asynchronous I/O.  It integrates with the
-- GHC IO manager via eventfd.
--
-- __Requires__: Linux kernel 5.1+, liburing.
-- Build with @-f iouring@ flag.
--
-- @
-- withIOUringTransport cfg sock $ \\t ->
--   runParserLoop t parser handler
-- @
module Wireform.Network.Transport.IOUring
  (
#if defined(HAVE_IOURING)
    withIOUringTransport
#endif
  ) where

#if defined(HAVE_IOURING)

import Control.Exception (bracket)
import Data.Bits ((.&.))
import Data.IORef
import Data.Word (Word8, Word64)
import Foreign.Ptr (Ptr, plusPtr)
import Network.Socket (Socket)

import Wireform.Ring.Internal
import Wireform.Transport
import Wireform.Transport.Config

-- | Create an io_uring-based transport for the given socket.
--
-- Uses the io_uring submission/completion queue for async recv
-- operations.  On kernel 5.19+, uses provided buffers for
-- zero-copy ring filling.  On kernel 6.0+, uses multishot recv.
--
-- Integrates with the GHC IO manager via eventfd: the parser
-- thread parks on @threadWaitRead eventFd@ when the completion
-- queue is empty.
withIOUringTransport :: TransportConfig -> Socket -> (Transport -> IO a) -> IO a
withIOUringTransport cfg sock action = do
  -- TODO: Full io_uring implementation requires:
  -- 1. io_uring_queue_init with configured depth
  -- 2. eventfd creation + io_uring_register_eventfd
  -- 3. Initial recv SQE submission
  -- 4. CQE processing loop in transportWaitData
  -- 5. Provided buffer ring setup (kernel 5.19+)
  -- 6. SQPOLL mode (UltraLowLatency profile)
  --
  -- For now, fall back to recv transport.
  error "io_uring transport not yet implemented; use withRecvTransport"

#endif
