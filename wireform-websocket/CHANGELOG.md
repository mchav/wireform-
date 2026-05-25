# Changelog

## 0.1.0.0 -- 2026

Initial release.

* Frame parser / builder on `Wireform.Parser` + `Wireform.Builder`.
* RFC 6455 §4 handshake (SHA-1 + base64 via `Wireform.Base64`).
* Connection layer on `SendTransport` / `ReceiveTransport`.
* Higher-level text / binary message reassembly.
* Standalone server with TCP and TLS (OpenSSL) listeners.
* Client connect over `ws://` and `wss://`:
    * `withWebSocketClient` for the common case.
    * `withWebSocketClient'` exposes the server's
      `ServerHandshakeResult` — selected sub-protocol, extension
      list, full response header block — for cases that need to
      know what the server agreed to.
    * `Network.WebSocket.URI` parses `ws://` and `wss://` URLs
      (default ports, IPv6 literals, fragment tolerance, case-
      insensitive scheme) into a `WebSocketURI` and on into a
      `WebSocketClientConfig`.
    * `withWebSocketClientURI` is the one-liner entry point.
* Server-side sub-protocol selection via
  `wscSelectSubProtocol :: WebSocketRequest -> Maybe ByteString`
  on `WebSocketServerConfig`.  Comma-separated
  `Sec-WebSocket-Protocol` header values are split into individual
  tokens before being handed to the callback (RFC 6455 §4.1).
* End-to-end echo tests (TCP + TLS), URI parser test suite,
  sub-protocol negotiation round-trip (positive + negative).
* HTTP/1.1 parsing and rendering of the handshake bytes delegates
  to `wireform-http1`'s SIMD-backed
  `Network.HTTP1.Parser.parseRequest` /
  `Network.HTTP1.Parser.parseResponse` and chunked-builder-based
  `Network.HTTP1.Encode.encodeRequestHead` /
  `Network.HTTP1.Encode.encodeResponseHead` — same parser the
  `wireform-http` server itself uses, so the WebSocket handshake
  inherits every conformance and smuggling guard the unified HTTP
  stack carries (RFC 9112 §3.2 Host validation, target /
  request-line strictness, etc.).
* Autobahn|Testsuite conformance: **247 / 247 passing**, covering
  framing (§1), ping/pong (§2), reserved bits (§3), opcodes (§4),
  fragmentation (§5), UTF-8 handling (§6), close handling (§7),
  and miscellaneous (§10). Driven by
  `wireform-websocket/scripts/run-autobahn.sh` against the
  `wireform-websocket-autobahn-echo` executable. CI integration in
  `.github/workflows/wireform-websocket-autobahn.yml`. Performance
  (§9) and `permessage-deflate` (§12 / §13) are out of scope until
  the RFC 7692 hook lands.
* `receiveFrame` now threads the consumer position through
  `runParserInternal` instead of round-tripping through
  `receiveLoadHead` between frames — without this, two frames
  buffered in the recv ring at the time of a frame parse would
  cause the second frame to be silently skipped (the
  Autobahn-driven fragmentation tests caught this).
* Close frames are idempotent at the send path
  (`Network.WebSocket.Connection.sendFrame` short-circuits on
  `OpClose` after the first close has gone out) so the protocol-
  error close sent by the receive validators isn't doubled by the
  server runner's polite-close path.
* Receive-side validators now use `failConnection` to emit a
  close frame with the appropriate RFC 6455 §7.4 status code
  before raising `WebSocketProtocolError` — 1002 for protocol
  violations, 1007 for invalid UTF-8, 1009 for messages over the
  configured limit. The auto-close echo in
  `Network.WebSocket.Message` validates the peer's close payload
  (1-byte payload = malformed, out-of-range code, non-UTF-8
  reason) and downgrades the echo to 1002 on any of these.
* Hot-path optimisations driven by reading the GHC Core dump:
  * `frameHeaderBytes` no longer builds a `[Word8]` list +
    `BS.pack`s it.  New `writeFrameHeader :: Frame -> Ptr Word8
    -> IO Int` pokes the 2–14 byte header directly into a
    caller-supplied buffer; `frameHeaderBytes` is a tiny
    `BSI.unsafeCreate` wrapper for the rare standalone use.
  * `sendOneFrame` writes header + payload (and applies SIMD
    masking when applicable) directly into the send ring inside
    one `reserveSend` reservation, skipping the `[hdr, payload]`
    list + `sendByteStringMany` cons-cell allocations.
  * Result: end-to-end 64 B echo round-trip dropped from 28.7 µs
    to 11.96 µs (2.4× faster), 128 KiB binary from 93.8 µs to
    51.24 µs (1.8× faster); `wireform-websocket` now beats the
    Haskell `websockets` package on every payload size benched
    and beats `tungstenite-rs` on large frames (33 % faster on
    128 KiB binary).
* Imperative client API: `openWebSocketClient` /
  `openWebSocketClient'` / `openWebSocketClientURI` /
  `closeWebSocketClient`.  The bracketed `withWebSocketClient*`
  variants are now thin wrappers over the imperative pair —
  same code path, no separate handshake / TLS / socket setup.
  This unblocks call sites that need a `Connection` whose
  lifetime is longer than any single function call (benchmark
  harnesses, supervisor-managed worker pools, REPL exploration);
  previously they had to fork a thread that called
  `withWebSocketClient` and parked inside the action so the
  bracket would never unwind.
* `attachCleanup :: Connection -> IO () -> IO ()` lets callers
  register finalizers on a 'Connection' — used by
  `openWebSocketClient`'s TLS path to chain the
  `SSL_shutdown` + `SSL_CTX_free` + raw-socket close that the
  layered bracket form used to handle implicitly.
* Allocation-free message send fast path.
  `sendTextMessage` and `sendBinaryMessage` now route through a
  new `sendDataFrame :: Connection -> Opcode -> ByteString -> IO ()`
  that skips the `Frame` record allocation, the
  `applyOutboundMask` dispatch, and the `frameHeaderBytes`
  intermediate `ByteString`.  Internal worker
  `sendDataFrameBytes` pokes header bytes straight into the
  send-ring reservation.
* Per-connection mask cache.  Client-role connections refill a
  buffer of 64 pre-rolled per-frame masking keys in one trip
  through the global splitmix generator; subsequent sends pull
  off the cache without touching the global @MVar@.
* Opt-in lock-free connections.  `WebSocketClientConfig` and
  `WebSocketServerConfig` now expose `wcSingleThreaded` /
  `wscSingleThreaded` (default 'True').  When set, the
  per-direction `MVar` locks on the `Connection` are absent and
  `sendFrame` / `receiveFrame` skip the `takeMVar` / `putMVar`
  round-trip entirely.  Use the default for the typical one
  thread-per-connection pattern; flip to 'False' for
  broadcast / fan-out shapes that share a 'Connection' across
  threads.
* `newConnectionUnlocked` constructs a lock-free 'Connection'
  directly.  `newConnection` continues to provide locking
  semantics.
* `withFrameBatch :: Connection -> (Connection -> IO a) -> IO a`
  exposes the magic-ring send cork at the WebSocket layer.
  Frames sent inside the callback accumulate in the send ring
  without triggering per-frame `sendmsg` / `SSL_write`; one
  publish covers everything written.  Throughput optimisation
  for fan-out and large-batch workloads.
