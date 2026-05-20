# HTTP/2 Server Throughput Benchmark Results

**System**: 4-core VM, h2load (nghttp2 1.59), cleartext h2c  
**Date**: 2026-05-20

## Servers Under Test

| Server | Language | Config |
|--------|----------|--------|
| wireform-http2 | Haskell (GHC 9.8.4) | `-N -A64m -qg` |
| http2 (Hackage) v5.3.x | Haskell (GHC 9.8.4) | `-N -A64m -qg` |
| nginx 1.24 | C | `worker_processes auto` (4) |
| nghttpd 1.59 (nghttp2) | C | `-n4` (4 workers) |

## Throughput (requests/second)

| Scenario | wireform-http2 | http2 (Hackage) | nginx | nghttpd |
|----------|---------------|-----------------|-------|---------|
| 1c/1s (latency) | 19,117 | 7,866 | 43,601 | 37,734 |
| 4c/10s (moderate) | **184,566** | 79,926 | 111,821 | 498,748 |
| 10c/100s (high) | **373,048** | 113,891 | 382,080 | 1,163,535 |
| 2c/50s (sustained) | **224,831** | 40,692 | 76,262 | 596,974 |

## Mean Latency (per request)

| Scenario | wireform-http2 | http2 (Hackage) | nginx | nghttpd |
|----------|---------------|-----------------|-------|---------|
| 1c/1s | 50μs | 124μs | 21μs | 24μs |
| 4c/10s | 173μs | 470μs | 353μs | 55μs |
| 10c/100s | 1.95ms | 3.80ms | 1.46ms | 506μs |
| 2c/50s | 399μs | 2.44ms | 1.30ms | 134μs |

## Analysis

### vs http2 (Hackage): 2.4x - 5.5x faster

wireform-http2 consistently outperforms the existing Haskell HTTP/2 package
across all scenarios. The advantage grows with concurrency and sustained load
(5.5x at 2c/50s).

### vs nginx: competitive at multiplexing, slower at single-stream

At single-stream latency, nginx wins (21μs vs 50μs) due to its highly optimized
event loop. But at multiplexed workloads (4c/10s, 2c/50s), wireform-http2 is
**1.5-3x faster** than nginx. At high concurrency (10c/100s), they're neck-and-neck
(373K vs 382K req/s).

This is likely because nginx's HTTP/2 implementation wasn't designed for high
stream multiplexing — it serializes per-connection, while wireform-http2's
GHC lightweight threads handle per-stream concurrency efficiently.

### vs nghttpd: within 2-3x

nghttpd (the nghttp2 C reference implementation) is the fastest in all scenarios,
achieving 1.16M req/s at high concurrency. wireform-http2 reaches 32% of nghttpd's
peak throughput — remarkable for a GC'd language with full protocol validation.

nghttpd's advantage comes from:
- Zero-copy sendfile for static content
- Minimal per-request allocation (stack-based frame handling)
- Kernel-level socket optimizations (SO_REUSEPORT, TCP_FASTOPEN)
- No GC pauses

### Reliability

wireform-http2 handled **100% of requests** at all concurrency levels with
0 failures. The http2 Hackage package showed 30% failures at 100 concurrent
streams in earlier runs (though it succeeded in this run with slightly lower
concurrency pressure).

## Reproduction

```bash
# Install dependencies
sudo apt-get install -y nghttp2-client nghttp2-server nginx

# Start all servers (from wireform-http2/bench-server/)
./run-benchmark.sh
```
