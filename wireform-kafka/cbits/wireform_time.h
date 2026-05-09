#ifndef WIREFORM_TIME_H
#define WIREFORM_TIME_H

#include <stdint.h>

/* See wireform_time.c. Both functions read the wall clock via
 * the fastest path available on the host — CLOCK_REALTIME_COARSE
 * on Linux (vDSO, no syscall, ~4 ns), CLOCK_REALTIME on macOS /
 * BSD (also vDSO-fast). */

int64_t wireform_current_time_millis(void);
int64_t wireform_current_time_micros(void);

#endif
