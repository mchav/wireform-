# Changelog

## 0.1.0.0 -- 2026

Initial release.

* Frame parser / builder on `Wireform.Parser` + `Wireform.Builder`.
* RFC 6455 §4 handshake (SHA-1 + base64 via `Wireform.Base64`).
* Connection layer on `SendTransport` / `ReceiveTransport`.
* Higher-level text / binary message reassembly.
* Standalone server with TCP and TLS (OpenSSL) listeners.
* Client connect over `ws://` and `wss://`.
* End-to-end echo tests (TCP + TLS).
