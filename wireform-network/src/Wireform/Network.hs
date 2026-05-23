-- | Convenience re-exports for socket-based wireform usage.
--
-- @
-- import Wireform.Parser
-- import Wireform.Network
--
-- main = withReceiveTransport (profileConfig Throughput) sock $ \\t ->
--   runParserLoop t myParser $ \\msg -> do
--     handleMessage msg
--     pure Continue
-- @
module Wireform.Network
  ( -- * Receive-side transports
    withReceiveTransport
  , withReceiveBufTransport
  , newReceiveBufTransport
  , ReceiveFn
  , chunkedReceiveFn
  , ReceiveRingExhausted (..)

    -- * Send-side transports
  , withSendTransport
  , withSendBufTransport
  , newSendBufTransport
  , SendFn
  , sinkSendFn

    -- * Duplex transports (paired send + receive on one wire)
  , DuplexTransport (..)
  , withDuplexTransport
  , newDuplexTransport
  , withDuplexBufTransport
  , newDuplexBufTransport
  , closeDuplexTransport

    -- * In-memory pipe (testing)
  , newDuplexPipe

    -- * Re-exports
  , module Wireform.Transport
  , module Wireform.Transport.Config
  ) where

import Wireform.Network.Transport.Receive
  ( withReceiveTransport
  , withReceiveBufTransport
  , newReceiveBufTransport
  , ReceiveFn
  , chunkedReceiveFn
  , ReceiveRingExhausted (..)
  )
import Wireform.Network.Transport.Send
  ( withSendTransport
  , withSendBufTransport
  , newSendBufTransport
  , SendFn
  , sinkSendFn
  )
import Wireform.Network.Transport.Duplex
  ( DuplexTransport (..)
  , withDuplexTransport
  , newDuplexTransport
  , withDuplexBufTransport
  , newDuplexBufTransport
  , closeDuplexTransport
  )
import Wireform.Network.Transport.Pipe (newDuplexPipe)
import Wireform.Network.Transport.Profile ()
import Wireform.Transport
import Wireform.Transport.Config
