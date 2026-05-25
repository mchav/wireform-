# wireform-websocket

RFC 6455 WebSocket implementation on the wireform stack.

The parser and builder come straight from `wireform-core`
(`Wireform.Parser` streaming mode + `Wireform.Builder`), the magic-ring
transport from `wireform-core` / `wireform-network`, and TLS from
`Wireform.Network.TLS.OpenSSL` — the same backend `wireform-http`
uses, so `wss://` is a one-line opt-in.

The package integrates with `wireform-http`:

* Server-side: the handshake parser consumes a wireform-http unified
  `Request`, and the 101 reply is a wireform-http `Response`.  The
  standalone listener (`runWebSocketServer`) accepts plain TCP and TLS
  connections; the per-connection `acceptWebSocketOnSocket` /
  `acceptWebSocketOnTls` hooks let you upgrade a wireform-http
  connection in your own dispatch loop.
* Client-side: `withWebSocketClient` connects, completes the
  handshake, and hands a live `Connection` to the caller.  Plain TCP
  and TLS (via `Wireform.Network.TLS.OpenSSL`) are both supported.

## Quick start

### Server

```haskell
import Network.WebSocket

main :: IO ()
main = runWebSocketServer (defaultWebSocketServerConfig
  { wscPort    = "8443"
  , wscHandler = echo
  , wscTls     = Just WebSocketTlsConfig
      { wstCertPath = "cert.pem"
      , wstKeyPath  = "key.pem"
      , wstAlpn     = []  -- ALPN optional for wss://
      }
  })
  where
    echo _ conn = forEachMessage conn defaultMessageLimit $ \m ->
      case m of
        TextMessage   t  -> sendTextMessage   conn t
        BinaryMessage bs -> sendBinaryMessage conn bs
```

### Client

```haskell
import Network.WebSocket

main :: IO ()
main = do
  let cfg = (defaultWebSocketClientConfig "echo.example" "443" "/echo")
              { wcTls = Just wsTlsDefault }
  withWebSocketClient cfg $ \conn -> do
    sendTextMessage conn "hi"
    TextMessage reply <- receiveMessage conn defaultMessageLimit
    print reply
```

## Modules

| Module | Purpose |
| ------ | ------- |
| `Network.WebSocket.Frame` | Frame ADT, streaming parser, builder; mode-polymorphic over `Wireform.Parser`. |
| `Network.WebSocket.Handshake` | RFC 6455 §4 handshake; SHA-1 + base64 via `Wireform.Base64`. |
| `Network.WebSocket.Connection` | `Connection` over `SendTransport` / `ReceiveTransport`; frame I/O, ping/pong, close. |
| `Network.WebSocket.Message` | Reassembled text / binary messages across continuation frames. |
| `Network.WebSocket.Server` | Standalone TCP / TLS listener + per-connection hand-off. |
| `Network.WebSocket.Client` | Client connect over `ws://` / `wss://`. |

## Tests

```
cabal test wireform-websocket
```

The test suite covers the RFC 6455 §5.7 wire vectors, the §4.2.2
Sec-WebSocket-Accept vector, masking self-inverse, large payload
round-trip, and end-to-end echo over both TCP and TLS.
