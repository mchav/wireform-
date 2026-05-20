/* SIMD-accelerated scanners for HTTP/1.x message parsing.
 *
 * Each scanner returns the absolute offset (from the buffer base) of the
 * first byte satisfying the predicate, or `len` if none was found within
 * `[offset, len)`. This is the same convention used by
 * wireform-core / wireform-html.
 *
 * Build with -O3. On x86 we use SSE2 (universally available since 2003);
 * the scalar fallback is correct on every platform.
 */
#ifndef WIREFORM_HTTP1_SCAN_H
#define WIREFORM_HTTP1_SCAN_H

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Scan for CR (\r) starting at offset. Returns absolute offset or len. */
int hs_http1_find_cr(const void *buf, int offset, int len);

/* Scan for LF (\n) starting at offset. Returns absolute offset or len. */
int hs_http1_find_lf(const void *buf, int offset, int len);

/* Scan for the first byte that is NOT a valid HTTP token character
 * (RFC 9110 § 5.6.2 tchar). Used to locate the end of method, header
 * field name, and Transfer-Encoding coding name.
 *
 *   tchar = "!" / "#" / "$" / "%" / "&" / "'" / "*"
 *         / "+" / "-" / "." / "^" / "_" / "`" / "|" / "~"
 *         / DIGIT / ALPHA
 *
 * Returns absolute offset of the first non-tchar byte, or len.
 */
int hs_http1_find_non_token(const void *buf, int offset, int len);

/* Scan for the first byte that is NOT a valid field-value byte
 * (RFC 9110 § 5.5 field-vchar / SP / HTAB). Anything else (NUL, CR, LF,
 * or other control char) terminates the field-value.
 *
 *   field-vchar = VCHAR / obs-text   (i.e. 0x21..0x7E / 0x80..0xFF)
 *
 * SP (0x20) and HTAB (0x09) are also permitted within the value.
 * Returns absolute offset of the first disallowed byte, or len.
 */
int hs_http1_find_non_fieldvalue(const void *buf, int offset, int len);

/* ASCII-lowercase a buffer into a caller-supplied destination. Used for
 * normalising HTTP header field names. SIMD-accelerated. */
void hs_http1_to_lower_ascii(const void *src, void *dst, int len);

/* Compare two buffers for ASCII case-insensitive equality. Returns 1
 * on match, 0 otherwise. Both buffers are exactly `len` bytes. */
int hs_http1_ascii_ieq(const void *a, const void *b, int len);

/* Parse up to `len` hex digits at `buf+offset`, stopping at the first
 * non-hex byte (or after `max_digits` digits, whichever comes first).
 *
 *   On success, *out is the parsed Word64 value and *consumed is the
 *   number of hex digits consumed (>= 0).
 *   Returns 1 on success (>=1 digit consumed), 0 if zero digits at the
 *   given offset, -1 on overflow (more than 16 hex digits parsed).
 */
int hs_http1_parse_hex(const void *buf, int offset, int len, uint64_t *out, int *consumed);

#ifdef __cplusplus
}
#endif

#endif /* WIREFORM_HTTP1_SCAN_H */
