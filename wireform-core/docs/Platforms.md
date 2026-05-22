# Platform Notes

## Minimum Versions

- **Linux**: kernel 3.17+ (memfd_create). Effectively all production Linux.
- **macOS**: 10.7+ (shm_open). Realistically 10.13+.
- **FreeBSD**: 11+ for SHM_ANON; earlier with unique-name shm_open path.
- **Windows**: 10 version 1803+ (VirtualAlloc2 placeholder support).
  Earlier Windows lacks the API for atomic double-mapping.

## GHC Version

GHC 9.6+ required for `prompt#` / `control0#`. GHC 9.10+ strongly
recommended — the primops were significantly refined in 9.8 and 9.10.

## Linux-Specific

- **io_uring**: available on kernel 5.1+. Provided buffers require 5.19+.
  Multishot recv requires 6.0+. SQPOLL works on all io_uring kernels.
  All features are runtime-probed via `detectCapabilities`.

- **Huge pages**: requires `vm.nr_hugepages` sysctl or transparent huge
  pages (`madvise(MADV_HUGEPAGE)`).

- **NUMA**: `mbind()` for ring placement. Requires libnuma or direct
  syscall. Build with `numa` flag enabled.

- **Isolated cores**: detected from `/sys/devices/system/cpu/isolated`.
  Used by `PinIsolated` policy.

- **Containers**: some hardened runtimes (gVisor, restricted Docker
  profiles) block `memfd_create`. The library throws
  `MagicRingUnavailable` — it does not silently degrade.

## macOS-Specific

- No CPU pinning API. `PinToCore` / `PinIsolated` are no-ops.
  The library sets `QOS_CLASS_USER_INTERACTIVE` as a best-effort hint.
- No huge page API. `PreferHugePages` is a no-op.
- No NUMA (single-socket). `NumaAutoFromFd` is a no-op.
- Apple Silicon uses 16KB pages. The ring rounds up correctly.
- App Sandbox restricts `shm_open` — library-using apps shipped through
  the Mac App Store may not work.

## Windows-Specific

- `VirtualAlloc2` placeholder API required (Windows 10 1803+).
- Huge pages via `MEM_LARGE_PAGES` require `SeLockMemoryPrivilege`
  (admin grant). Document this to users.
- CPU pinning via `SetThreadAffinityMask`.
- NUMA via `VirtualAllocExNuma` (rarely useful on single-socket).

## WSL

- WSL2: works as Linux.
- WSL1: does NOT have `memfd_create`. Not supported.

## FreeBSD

- `SHM_ANON` for anonymous shared memory (FreeBSD 11+).
- CPU pinning via `cpuset_setaffinity`.
- Superpage promotion at 2MB alignment (no explicit API).
- NUMA via `domainset` APIs.
