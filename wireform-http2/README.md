# wireform-http2

[![BSD-3-Clause](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)

> [!CAUTION]
> wireform is in heavy development and has not been published to Hackage
> yet. APIs may change.

From-scratch HTTP/2 (RFC 9113) implementation following wireform's
performance philosophy: zero-copy frame encode/decode, SWAR/SIMD-
accelerated HPACK Huffman, exact-size allocation, pinned recv ring
buffer, allocation-free hot loops.

## What's in here

| Module                                  | Role |
| --------------------------------------- | ---- |
| `Network.HTTP2.Frame`                   | RFC 9113 frame layer — DATA, HEADERS, PRIORITY, SETTINGS, PING, GOAWAY, WINDOW_UPDATE, RST_STREAM, CONTINUATION, PUSH_PROMISE. Pattern-synonym ADT + zero-copy decode/encode. |
| `Network.HTTP2.HPACK`                   | RFC 7541 HPACK encode/decode with a hand-tuned C Huffman codec (`cbits/hpack_huffman.c`) and a static + dynamic table. |
| `Network.HTTP2.Connection`              | Per-connection state: settings, flow-control windows, stream table, HPACK encoder/decoder, send lock, pinned send + recv buffers. Talks to the outside world through a `Transport`. |
| `Network.HTTP2.Transport`               | I/O abstraction the connection layer runs on top of: send-all + send-many + recv-into-Ptr + close. `socketTransport` is the default for plain TCP; `bufferedRecvTransport` is the bridge used by the TLS layer. |
| `Network.HTTP2.Client`                  | Cleartext HTTP/2 client (h2c) over a TCP socket. |
| `Network.HTTP2.Server`                  | Cleartext HTTP/2 server (h2c) over a TCP socket. |
| `Network.HTTP2.TLS`                     | Shared TLS plumbing: the `h2` ALPN identifier, the `ALPNFailed` exception, and `tlsTransport` (wraps a `Network.TLS.Context` as a wireform-http2 `Transport`). |
| `Network.HTTP2.TLS.Client`              | HTTP/2-over-TLS client. Does the TLS handshake against the peer, advertises `h2` via ALPN, and runs the existing `Network.HTTP2.Client` loop over the resulting TLS context. |
| `Network.HTTP2.TLS.Server`              | HTTP/2-over-TLS server. Accepts TLS connections, picks `h2` from the ALPN list (drops connections that ask for anything else), and runs the existing `Network.HTTP2.Server` loop. |

## Cabal flags

| Flag    | Default | Pulls in                                  | Disables when off                                  |
| ------- | ------- | ----------------------------------------- | -------------------------------------------------- |
| `+tls`  | True    | `tls`, `crypton-x509`, `crypton-x509-store` | `Network.HTTP2.TLS.*` modules, TLS test suite |

## Hello world — cleartext

```haskell
import qualified Data.ByteString as BS
import qualified Network.HTTP2.Server as H2

main :: IO ()
main = H2.runServer cfg
  where
    cfg = H2.defaultServerConfig
      { H2.serverHost = "0.0.0.0"
      , H2.serverPort = "8080"
      , H2.serverHandler = \_req respond ->
          respond H2.Response
            { H2.responseStatus = 200
            , H2.responseHeaders = [("content-type", "text/plain")]
            , H2.responseBody = H2.ResponseBodyBS (BS.pack [104,105,10])
            }
      }
```

## Hello world — TLS / h2

```haskell
import qualified Data.X509      as X509
import qualified Data.X509.File as X509
import qualified Network.TLS    as TLS
import qualified Network.HTTP2.TLS.Server as TLS

main :: IO ()
main = do
  certs <- X509.readSignedObject "server.crt"
  key:_ <- X509.readKeyFile "server.key"
  let cfg = (TLS.defaultTLSServerConfig (X509.CertificateChain certs) key)
        { TLS.tlsServerConfig = TLS.defaultServerConfig
            { TLS.serverHost = "0.0.0.0"
            , TLS.serverPort = "8443"
            }
        }
  TLS.runTLSServer cfg
```

The client side is `Network.HTTP2.TLS.Client.withTLSConnection`:

```haskell
import qualified Network.HTTP2.TLS.Client as TLS

main :: IO ()
main = do
  let httpCfg = TLS.defaultClientConfig { TLS.clientHost = "127.0.0.1", TLS.clientPort = "8443" }
      cfg     = (TLS.defaultTLSClientConfig "localhost") { TLS.tlsClientConfig = httpCfg }
  TLS.withTLSConnection cfg $ \conn -> do
    _ <- TLS.sendRequest conn $ TLS.ClientRequest
            { TLS.crMethod    = "GET"
            , TLS.crPath      = "/"
            , TLS.crScheme    = "https"
            , TLS.crAuthority = "localhost"
            , TLS.crHeaders   = []
            , TLS.crBody      = Nothing
            }
    pure ()
```

`ALPNFailed` is thrown if the peer refuses to negotiate `h2`. ALPN is
non-optional: there is intentionally no HTTP/1.1 fallback in this
package. Plug a different transport adapter on top of
`Network.HTTP2.Transport.Transport` if you want a Connect-style
upgrade or h2c-over-TLS.

## Server push

Server push (`PUSH_PROMISE`) is parsed and the frame type is exposed,
but the high-level server API does not initiate pushes. Server push
was de facto removed from the web platform (Chrome 106 / Firefox 96
disabled it; the gRPC profile never used it). We do not plan to add a
push API to `Network.HTTP2.Server` unless a concrete user need turns up.

## Relationship with `wireform-grpc`

[`wireform-grpc`](../wireform-grpc/) is the vendored gRPC binding (from
`grapesy`) and currently pins `http2 == 5.3.9` and `http2-tls < 0.5`
because grapesy upstream is wired into a very specific
`http-semantics` shape — streaming bodies via `OutBodyIface`, trailer
makers, half-close semantics, server `Config` with `confWriteBuffer` /
`confReadN` / `confTimeoutManager` / `confPositionReadMaker`, plus
ping / empty-frame / settings / rst rate limits.

This package currently provides everything the **transport** of that
stack needs (TLS / ALPN, flow control, settings negotiation), but not
the **`http-semantics`-shaped server / client surface**
`grapesy` consumes. Fully swapping `wireform-grpc` off `http2` + `http2-tls`
requires building:

- An `OutBodyIface`-equivalent streaming body API on top of
  `Network.HTTP2.Server.ResponseBody`, including outbound trailers and
  cancellation hooks.
- An inbound trailer + half-close path on `Network.HTTP2.Server.Request`
  (the current `requestBody` callback yields chunks; it does not surface
  the final HEADERS-with-END_STREAM trailer frame).
- A `Config`-shaped knob bundle (rate limits, write buffer, time manager)
  to match what `Network.GRPC.Util.HTTP2.withConfigForInsecure` /
  `withConfigForSecure` produce today.

These are intentionally additive — they don't change the existing
zero-copy hot paths — but they're a non-trivial surface. The Transport
refactor in this PR is the foundation; the gRPC swap will land in a
follow-up.

## Tests

```
cabal test wireform-http2:wireform-http2-test
```

The `Test.TLS` group spins up `runTLSServerOnSocket` on a freshly
bound random port, runs `withTLSConnection` against it, and asserts
that ALPN agreed on `h2`. It uses a precomputed self-signed cert from
`test/data/` so the test is hermetic and never touches the system
trust store.

`cabal test wireform-http2:wireform-http2-conformance` runs the
`h2spec` conformance suite against the cleartext server (requires the
`h2spec` binary on `PATH`).

## License

BSD-3-Clause. Copyright Ian Duncan, 2026.
