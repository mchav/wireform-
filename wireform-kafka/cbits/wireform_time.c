/* wireform_time.c
 *
 * Coarse / fast wall-clock readers for the producer + consumer
 * hot path. Returns the current wall-clock time as an Int64
 * count of milliseconds (or microseconds) since the POSIX epoch.
 *
 * Implementation strategy:
 *
 *   * Linux: 'clock_gettime(CLOCK_REALTIME_COARSE, ...)'. The
 *     COARSE variant reads the kernel's last-tick timestamp out
 *     of a vDSO-mapped page — no syscall, ~4 ns total. Resolution
 *     matches the kernel's HZ (typically 1 ms or 4 ms) which is
 *     plenty for ms-granularity Kafka batch timestamps.
 *
 *   * macOS / FreeBSD / generic POSIX: 'clock_gettime(CLOCK_REALTIME, ...)'.
 *     There's no COARSE variant on Apple's clock, but the regular
 *     'CLOCK_REALTIME' on macOS is also vDSO-fast (mach_absolute_time
 *     under the hood). On other BSDs the call is similarly cheap.
 *
 * Windows is handled on the Haskell side (it falls back to
 * 'Data.Time.Clock.POSIX.getPOSIXTime' which there resolves to
 * 'GetSystemTimeAsFileTime' — also a fast page read, no syscall).
 * We don't compile this file on Windows.
 */

#include <stdint.h>
#include <time.h>

#if defined(__linux__) && defined(CLOCK_REALTIME_COARSE)
#  define WIREFORM_REALTIME_CLOCK CLOCK_REALTIME_COARSE
#else
#  define WIREFORM_REALTIME_CLOCK CLOCK_REALTIME
#endif

/* Current wall-clock time in milliseconds since the POSIX epoch.
 *
 * On Linux this uses CLOCK_REALTIME_COARSE (vDSO read, no
 * syscall); on macOS / BSD it uses CLOCK_REALTIME (also vDSO-fast).
 * On either platform it's strictly faster than the Haskell
 * 'getPOSIXTime' path, which goes through 'gettimeofday' + a
 * 'Pico'-typed multiply / divide. */
int64_t
wireform_current_time_millis(void)
{
  struct timespec ts;
  clock_gettime(WIREFORM_REALTIME_CLOCK, &ts);
  return (int64_t)ts.tv_sec * 1000
       + (int64_t)ts.tv_nsec / 1000000;
}

/* Current wall-clock time in microseconds since the POSIX epoch.
 *
 * Same backing clock as 'wireform_current_time_millis'; the
 * difference is the conversion factor. Useful for the stats
 * emitter, which reports microsecond timestamps in its JSON
 * payload (matching librdkafka's stats.json format). */
int64_t
wireform_current_time_micros(void)
{
  struct timespec ts;
  clock_gettime(WIREFORM_REALTIME_CLOCK, &ts);
  return (int64_t)ts.tv_sec * 1000000
       + (int64_t)ts.tv_nsec / 1000;
}
