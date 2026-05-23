# TLS on the magic ring — architectural notes

## Where we are today

For HTTP/1, HTTP/2, and Kafka, TLS connections plumb into the magic-ring
transport through the same bridge:

```
TLS.Context
   │  recvData :: IO ByteString
   ▼
bufferedRecvTransport   (small leftover IORef-held ByteString)
   │  tRecvBuf :: Ptr Word8 -> Int -> IO Int
   ▼
withRecvBufTransport / newRecvBufTransport   (magic ring)
   │  Wireform.Transport
   ▼
Network.HTTP{1,2}.StreamingReader / Kafka.Network.FrameParser
```

The bridge `bufferedRecvTransport` lives in `Network.HTTP{1,2}.Transport`
and is used by `Network.HTTP1.TLS.tlsTransport` and
`Network.HTTP2.TLS.tlsTransport`.  Every `recvData` call yields a fresh
heap `ByteString` containing one TLS record's plaintext; the bridge
memcpys from that `ByteString` into the ring on each `tRecvBuf` call.

That's **one extra memcpy + one fresh `ByteString` per TLS record** on
top of the in-place decryption the `tls` package does internally.  At
~16 KiB per record and ~5 GB/s memcpy speed this is in the
sub-microsecond regime per record — measurable on benchmarks that
saturate plaintext throughput, but invisible on workloads where the
AES-GCM crypto or the application handler dominates.

## Why we don't decrypt directly into the ring today

The `tls` package's public API (`Network.TLS`,
`Network.TLS.Backend`) only exposes:

* `recvData :: Context -> IO ByteString` — high-level, returns decrypted
  plaintext as a fresh `ByteString`.
* `Backend { backendRecv :: Int -> IO ByteString,  backendSend :: ByteString -> IO () }` —
  the **encrypted** byte stream the TLS state machine reads from and
  writes to.

There is no `recvDataInto :: Context -> Ptr Word8 -> Int -> IO Int`
that would write plaintext bytes directly into a caller-supplied
buffer.  The decrypted bytes always come back as a `ByteString`
allocated by `tls` internals.

To actually decrypt straight into the magic ring we would need one of:

1. **Patch `tls` upstream** to add a Ptr-based recv.  The change is
   small in spirit (re-route the decrypt output into a caller-supplied
   buffer) but `tls`'s internal record-layer code is single-buffer:
   it currently writes into a fresh `ByteString` chunk because the
   decoded record size isn't known until after the AEAD tag is
   verified.  A correct implementation would either decrypt into a
   small staging buffer + memcpy to the caller's Ptr (saving one
   allocation, not the memcpy) or grow a caller-supplied resizable
   buffer (which complicates the API).  Probably accepted but a
   multi-week effort including upstream review.

2. **Reimplement the TLS record layer** on top of `crypton`'s
   AES-GCM / ChaCha20-Poly1305 primitives, delegating only the
   handshake to `tls`.  Roughly:
   * After handshake, extract the cipher state from `TLS.Context`
     via `Network.TLS.Internal` (if exposed; not entirely public
     today).
   * Read the 5-byte record header off the socket directly into the
     ring's "header buffer".
   * Read the ciphertext + tag straight into the ring at the
     plaintext offset.
   * Call AES-GCM-decrypt-in-place to overwrite the ciphertext with
     plaintext.
   * The parser sees plaintext at the same ring offset.

   This is the architecturally clean shape — no extra allocations,
   no memcpys, plaintext lives on the ring.  Estimated 1-2 weeks
   of dedicated work plus a sustained maintenance commitment
   (record-layer code talks to crypto primitives; bugs are
   security-sensitive).

3. **Use `kTLS`** — Linux's in-kernel TLS offload.  The application
   does the handshake; the kernel decrypts records and presents
   plaintext to userspace via the normal socket recv path.  The
   magic ring's existing `recvBuf` path then writes plaintext
   straight into the ring with zero extra work.  This is the
   smallest-code-change option but adds a kernel-version dependency
   (kTLS shipped in 4.13+ but the application-handshake APIs
   evolved through 5.x).

## Recommended path

For now the bridge is correct, defensible (~1 extra memcpy per
record) and lets us reuse `tls`'s well-vetted handshake +
record-layer implementation.

The biggest architectural improvement that's still tractable is
**kTLS** (option 3): we hand the kernel the cipher state after
handshake, then the ring's `Network.Socket.recvBuf` path receives
plaintext directly with no extra Haskell-side work.  Worth pursuing
when production deployments would benefit; for now the bridge is
what ships.

Option 2 (rewrite the record layer) is a research project we should
not undertake without a concrete throughput-bound benchmark
demonstrating the need.

## Compression contrast

For comparison, **compression** (`Kafka.Compression.Ring`) does
operate directly on ring memory: the source pointer is a slice of
the response body (or of the magic ring if the caller arranges that)
and for snappy + zstd the destination is sized exactly via
`BSI.mallocByteString` so the C decompressor writes directly into the
result `ByteString` with no intermediate `malloc` + `memcpy` + `free`
round-trip.  The reason compression-on-ring is tractable where
TLS-on-ring is not: snappy / zstd / lz4 / zlib all expose Ptr-based
decompress APIs; the `tls` package does not.
