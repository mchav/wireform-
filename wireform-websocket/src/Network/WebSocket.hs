{- | RFC 6455 WebSocket on the wireform stack.

This umbrella re-exports the modules application code typically
imports together:

* "Network.WebSocket.Frame" \u2014 the wire frame, plus the
  'Wireform.Parser'-based decoder and 'Wireform.Builder'-based
  encoder.
* "Network.WebSocket.Handshake" \u2014 RFC 6455 \u00a74
  handshake helpers (SHA-1 + base64 via "Wireform.Base64").
* "Network.WebSocket.Connection" \u2014 the live connection
  abstraction on top of 'Wireform.Transport.Send.SendTransport'
  \/ 'Wireform.Transport.Receive.ReceiveTransport'.
* "Network.WebSocket.Message" \u2014 text \/ binary message
  reassembly and high-level send helpers.
* "Network.WebSocket.Server" \u2014 standalone TCP \/ TLS
  listener plus the @acceptWebSocketOn*@ hand-off used to
  upgrade a wireform-http request.
* "Network.WebSocket.Client" \u2014 client connect over
  @ws:\/\/@ \/ @wss:\/\/@.

Server authors usually import "Network.WebSocket.Server"
directly; this umbrella is for one-import smoke tests and the
examples directory.
-}
module Network.WebSocket (
  module Network.WebSocket.Frame,
  module Network.WebSocket.Handshake,
  module Network.WebSocket.Connection,
  module Network.WebSocket.Message,
  module Network.WebSocket.Server,
  module Network.WebSocket.Client,
  module Network.WebSocket.URI,
) where

import Network.WebSocket.Client
import Network.WebSocket.Connection
import Network.WebSocket.Frame
import Network.WebSocket.Handshake
import Network.WebSocket.Message
import Network.WebSocket.Server
import Network.WebSocket.URI

