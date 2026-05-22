# Performance Guide

## Picking a Profile

If you don't know which to use: **`Throughput`**. It uses one thread, the
system IO manager, and standard pages. It doesn't burn CPU when idle.

Use **`LowLatency`** if you're seeing measurable wakeup latency in your
benchmarks and you have a dedicated core to give the parser. It spins
briefly before parking, pins the thread near the data, and uses huge
pages where available.

Use **`UltraLowLatency`** only if you've measured `LowLatency` and need
lower. It busy-polls forever, which means it eats a CPU core at 100%
even when idle.

```haskell
withRecvTransport (profileConfig Throughput) sock $ \t -> ...
```

To tweak individual knobs, start from a profile and update fields:

```haskell
let cfg = (profileConfig LowLatency) { ringSizeHint = 4 * 1024 * 1024 }
withRecvTransport cfg sock $ \t -> ...
```

## Platform Capability Matrix

| Knob | Linux | macOS | FreeBSD | Windows |
|------|-------|-------|---------|---------|
| Magic ring | yes | yes | yes | yes (Win10 1803+) |
| CPU pinning | yes | no (QoS hint) | yes | yes |
| Huge pages | yes | no | partial | yes (admin) |
| NUMA | yes | no | yes | yes |
| io_uring | yes | no | no | no |
| mlock | yes | yes (limits) | yes | yes |

All transports use the GHC IO manager (epoll/kqueue/IOCP) by default.

## Performance vs flatparse

On whole-input parsing (via `parseByteString`), the parser matches
flatparse performance on most workloads:

| Operation | wireform / flatparse |
|-----------|---------------------|
| anyWord8 | 1.0x (parity) |
| anyWord32be | 0.64x (faster) |
| length-prefixed take | 1.0x (parity) |
| tagged alternatives | 1.3x |
| anyChar (UTF-8) | 0.5x (faster) |

The streaming path adds zero overhead on the fast path — suspension
via `control0#` only fires when the parser exhausts its buffer window.

## Tips

- Use CPS primitives (`withAnyWord8`, `withAnyWord32`, `withSatisfyAscii`)
  in hot loops to avoid boxing intermediate values.
- Use `cut` to commit early — it allows the driver to advance the ring
  tail and give the producer more room.
- For large messages, use `checkpoint` mid-parse to release ring capacity.
- `takeBs` returns a zero-copy slice. Use `takeBsCopy` only if the result
  must outlive the transport scope.
