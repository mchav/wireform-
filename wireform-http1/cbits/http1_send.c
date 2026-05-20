/* Tiny @send(2)@ wrapper that exposes the Linux @MSG_MORE@ flag.
 *
 * @MSG_MORE@ tells the TCP stack "the caller has more data coming; do
 * not transmit yet". The kernel buffers the bytes and ships them when
 * a subsequent send without @MSG_MORE@ (or a @sendfile(2)@ call)
 * supplies the rest of the segment. Effect-equivalent to setting
 * @TCP_CORK@ but without the setsockopt syscall pair per request.
 *
 * The HTTP/1.x server uses this for the head + sendfile pair so
 * everything lands in a single packet:
 *
 *   1. hs_http1_send_more(sock, head_bytes, head_len);
 *   2. sendfile(sock, fd, &off, body_len);
 *
 * After step 2 the kernel flushes the buffered head along with the
 * body — one TCP segment for the whole response (or as few as the
 * body size requires).
 *
 * MSG_MORE has been in Linux since 2.4.4; on systems without it the
 * send still works, just without the coalescing benefit.
 */

#include <stddef.h>
#include <sys/socket.h>
#include <errno.h>

#ifndef MSG_MORE
#define MSG_MORE 0
#endif

#ifndef MSG_NOSIGNAL
#define MSG_NOSIGNAL 0
#endif

/* Returns the number of bytes sent (>= 0), or -errno on failure. */
ssize_t hs_http1_send_more(int sock, const void *buf, size_t len) {
  ssize_t n;
  do {
    n = send(sock, buf, len, MSG_MORE | MSG_NOSIGNAL);
  } while (n < 0 && errno == EINTR);
  if (n < 0) return -errno;
  return n;
}

/* Plain send() that loops on EINTR and uses MSG_NOSIGNAL so a closed
 * peer doesn't kill our process with SIGPIPE.
 */
ssize_t hs_http1_send_all(int sock, const void *buf, size_t len) {
  const char *p = (const char *)buf;
  size_t remaining = len;
  while (remaining > 0) {
    ssize_t n;
    do {
      n = send(sock, p, remaining, MSG_NOSIGNAL);
    } while (n < 0 && errno == EINTR);
    if (n < 0) return -errno;
    if (n == 0) break;  /* peer closed */
    p += n;
    remaining -= n;
  }
  return len - remaining;
}
