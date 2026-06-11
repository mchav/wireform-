{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

{- |
Module      : Kafka.Time
Description : Coarse / fast wall-clock readers for the Kafka client hot path

The producer's batch accumulator, the sender's delivery-timeout
check, the heartbeat thread, and several other hot-path callers
need the current wall-clock time in milliseconds (or microseconds)
on every invocation.

The obvious shape is

@
round . (* 1000) \<$\> Time.getPOSIXTime
@

which goes through 'Data.Time.Clock.POSIX.getPOSIXTime' →
'gettimeofday'\/'GetSystemTimeAsFileTime' + a 'Pico'-typed
multiply / divide. That's around 30–50 ns on Linux and not
really cacheable by the runtime.

This module replaces it with a per-OS fast path:

  * /Linux/ uses @clock_gettime(CLOCK_REALTIME_COARSE, ...)@
    via FFI. The COARSE variant reads the kernel's last-tick
    timestamp out of a vDSO-mapped page — no syscall, ~4 ns
    total. Resolution matches the kernel HZ (typically 1 ms or
    4 ms) which is plenty for ms-granularity Kafka batch
    timestamps.
  * /macOS/ \/ /BSD/ uses @clock_gettime(CLOCK_REALTIME, ...)@
    via FFI. There's no COARSE variant on Apple's clock, but
    the regular call is also vDSO-fast (mach_absolute_time
    under the hood).
  * /Windows/ falls back to 'Data.Time.Clock.POSIX.getPOSIXTime'.
    @GetSystemTimeAsFileTime@ on Windows is itself a fast page
    read (no syscall) and the Haskell wrapper's overhead is
    similar to the Pico-typed conversion we'd pay either way.

Both 'currentTimeMillis' and 'currentTimeMicros' return
'Int64'-typed counts since the POSIX epoch. Treat the result
as suitable for ordering and elapsed-time math; do not assume
sub-millisecond accuracy from the COARSE clock.
-}
module Kafka.Time (
  currentTimeMillis,
  currentTimeMicros,
) where

import Data.Int (Int64)


#if defined(mingw32_HOST_OS) || defined(__MINGW32__) || defined(__MINGW64__) \
  || defined(_WIN32) || defined(_WIN64)
#define WIREFORM_PLATFORM_WINDOWS 1
#endif

#ifdef WIREFORM_PLATFORM_WINDOWS

import qualified Data.Time.Clock.POSIX as Time

-- | Current wall-clock time in milliseconds since the POSIX
-- epoch. See module documentation.
{-# INLINE currentTimeMillis #-}
currentTimeMillis :: IO Int64
currentTimeMillis = round . (* 1000) <$> Time.getPOSIXTime

-- | Current wall-clock time in microseconds since the POSIX
-- epoch. See module documentation.
{-# INLINE currentTimeMicros #-}
currentTimeMicros :: IO Int64
currentTimeMicros = round . (* 1_000_000) <$> Time.getPOSIXTime

#else

-- POSIX path. See cbits/wireform_time.c for the C side; the
-- chosen 'clock_gettime' clock id is selected at C-compile time
-- (CLOCK_REALTIME_COARSE on Linux, CLOCK_REALTIME elsewhere).

foreign import ccall unsafe "wireform_current_time_millis"
  c_currentTimeMillis :: IO Int64

foreign import ccall unsafe "wireform_current_time_micros"
  c_currentTimeMicros :: IO Int64

-- | Current wall-clock time in milliseconds since the POSIX
-- epoch. See module documentation.
{-# INLINE currentTimeMillis #-}
currentTimeMillis :: IO Int64
currentTimeMillis = c_currentTimeMillis

-- | Current wall-clock time in microseconds since the POSIX
-- epoch. See module documentation.
{-# INLINE currentTimeMicros #-}
currentTimeMicros :: IO Int64
currentTimeMicros = c_currentTimeMicros

#endif
