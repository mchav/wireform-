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

#### From a URL

```haskell
withWebSocketClientURI "wss://echo.example/echo" $ \conn -> do
  sendTextMessage conn "hi"
  TextMessage reply <- receiveMessage conn defaultMessageLimit
  print reply
```

#### Sub-protocol negotiation

The client offers a list of sub-protocols and reads back which one the
server selected. The handshake fails (`WebSocketClientError`) if the
server picks something the client didn't offer.

```haskell
let cfg = (defaultWebSocketClientConfig "chat.example" "443" "/ws")
            { wcTls          = Just wsTlsDefault
            , wcSubProtocols = ["chat.v2", "chat.v1"]
            }
withWebSocketClient' cfg $ \shr conn ->
  case shrSelectedProtocol shr of
    Just "chat.v2" -> runV2 conn
    Just "chat.v1" -> runV1 conn
    Just other     -> error ("server selected unexpected: " <> show other)
    Nothing        -> error "server declined to negotiate"
```

The server side mirrors this through `wscSelectSubProtocol`:

```haskell
runWebSocketServer defaultWebSocketServerConfig
  { wscHandler           = chatHandler
  , wscSelectSubProtocol = \req ->
      -- Prefer "chat.v2" if the client offered it; else fall back.
      pick ["chat.v2", "chat.v1"] (wsReqProtocols req)
  }
  where
    pick prefs offered = lookup () [((), p) | p <- prefs, p `elem` offered]
```

## Modules

| Module | Purpose |
| ------ | ------- |
| `Network.WebSocket.Frame` | Frame ADT, streaming parser, builder; mode-polymorphic over `Wireform.Parser`. |
| `Network.WebSocket.Handshake` | RFC 6455 §4 handshake; SHA-1 + base64 via `Wireform.Base64`. |
| `Network.WebSocket.Connection` | `Connection` over `SendTransport` / `ReceiveTransport`; frame I/O, ping/pong, close. |
| `Network.WebSocket.Message` | Reassembled text / binary messages across continuation frames. |
| `Network.WebSocket.Server` | Standalone TCP / TLS listener + per-connection hand-off. |
| `Network.WebSocket.Client` | Client connect over `ws://` / `wss://`. `withWebSocketClient` for the simple case, `withWebSocketClient'` to inspect the server's `ServerHandshakeResult` (selected sub-protocol, extensions, full response headers). |
| `Network.WebSocket.URI` | `parseWebSocketURI` / `renderWebSocketURI` and `clientConfigFromURI` so callers can plug a URL string straight into the client. |

## Tests

```
cabal test wireform-websocket
```

The unit test suite covers the RFC 6455 §5.7 wire vectors, the
§4.2.2 Sec-WebSocket-Accept vector, masking self-inverse, large
payload round-trip, and end-to-end echo over both TCP and TLS.

## Conformance: Autobahn|Testsuite

The package ships with an Autobahn conformance harness that runs
the upstream Crossbar `crossbario/autobahn-testsuite` Docker image
against the included echo server. Drive it with:

```
wireform-websocket/scripts/run-autobahn.sh
```

The script builds `wireform-websocket-autobahn-echo`, starts it on
`127.0.0.1:9001`, runs all non-`9.x`/`12.x`/`13.x` cases (`9.x` is
performance; `12.x`/`13.x` are permessage-deflate, which we do not
implement), and prints a per-section summary.

Last full run: **247 / 247 passed** across sections 1 (framing),
2 (ping/pong), 3 (reserved bits), 4 (opcodes), 5 (fragmentation),
6 (UTF-8 handling), 7 (close handling), and 10 (misc). Excluded
sections: `9.*` performance (not a conformance check), `12.*` /
`13.*` permessage-deflate (RFC 7692, not yet wired through the
RSV1 bit).
