-- | Convenience re-exports for socket-based wireform usage.
--
-- @
-- import Wireform.Parser
-- import Wireform.Network
--
-- main = withRecvTransport (profileConfig Throughput) sock $ \\t ->
--   runParserLoop t myParser $ \\msg -> do
--     handleMessage msg
--     pure Continue
-- @
module Wireform.Network
  ( -- * Transport construction
    withRecvTransport
  , withRecvBufTransport
  , newRecvBufTransport
  , RecvFn
  , chunkedRecvFn

    -- * Re-exports
  , module Wireform.Transport
  , module Wireform.Transport.Config
  ) where

import Wireform.Network.Transport.Recv
  ( withRecvTransport
  , withRecvBufTransport
  , newRecvBufTransport
  , RecvFn
  , chunkedRecvFn
  )
import Wireform.Network.Transport.Profile ()
import Wireform.Transport
import Wireform.Transport.Config
