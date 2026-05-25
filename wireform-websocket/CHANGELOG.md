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
