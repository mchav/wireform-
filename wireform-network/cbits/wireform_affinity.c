/*
 * wireform_affinity.c
 *
 * CPU-affinity helper backing the `foreign import ccall
 * "sched_setaffinity_single"` in Wireform.Network.Transport.Profile.
 *
 * That foreign import had no C definition, so any build that resolves
 * symbols eagerly (e.g. a dynamic / shared-object build, or the GHC
 * interpreter dlopen'ing the package during a Template Haskell splice)
 * failed with `undefined symbol: sched_setaffinity_single`. Static
 * builds happened to get away with it because archive members are only
 * pulled in on demand and `c_pin_thread` is rarely referenced.
 *
 * The helper pins the *calling thread* (pid 0) to a single core. It is
 * Linux-only: the cabal file compiles this translation unit only on
 * Linux, matching the `#if defined(linux_HOST_OS)` guard around the
 * Haskell foreign import.
 */

#define _GNU_SOURCE
#include <sched.h>

#include "HsFFI.h"

/* Returns 0 on success, or -1 (with errno set by sched_setaffinity) on
 * failure. HsInt is used for both the argument and the result so the C
 * ABI matches the Haskell `Int -> IO Int` foreign import exactly. */
HsInt sched_setaffinity_single(HsInt core)
{
    cpu_set_t set;
    CPU_ZERO(&set);
    CPU_SET((int) core, &set);
    return (HsInt) sched_setaffinity(0, sizeof(set), &set);
}
