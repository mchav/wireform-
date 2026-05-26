/*
 * wf_pmd.h
 *
 * RFC 7692 permessage-deflate FFI shim.
 *
 * Provides persistent z_stream contexts that survive across
 * WebSocket messages so the deflate dictionary can be reused
 * ("context takeover" in RFC 7692 parlance).  Each direction is a
 * separate stream; the caller pairs them per Connection.
 *
 * Why a dedicated shim and not Codec.Compression.Zlib:
 *
 * * The Haskell 'zlib' package buffers input/output through lazy
 *   ByteStrings; for the WebSocket hot path we want to drive the
 *   underlying z_stream directly so the deflate output can land in
 *   the magic-ring send buffer with a single memcpy (or, with a
 *   future zero-copy pass, no copy at all).
 *
 * * PMD requires Z_SYNC_FLUSH after each message and a strip /
 *   append of the trailing 00 00 FF FF marker.  Doing that on top of
 *   the lazy ByteString API is awkward and forces a re-copy.
 *
 * * Context-takeover semantics require the same z_stream across
 *   messages, but with reset_inflate / reset_deflate when the
 *   *_no_context_takeover extension parameter is negotiated.  Easy
 *   to express against raw zlib, awkward against the lazy ByteString
 *   wrapper.
 */

#ifndef WF_PMD_H
#define WF_PMD_H

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/* All entry points return one of these. */
#define WF_PMD_OK              0
#define WF_PMD_NEEDS_MORE_OUT -1
#define WF_PMD_BAD_INPUT      -2
#define WF_PMD_OOM            -3
#define WF_PMD_INIT_FAIL      -4

/* Opaque handle.  The Haskell side stores a 'Ptr ()' and passes it
 * back into each entry point.  Internally it's a z_stream* allocated
 * by malloc + zlib's deflateInit2 / inflateInit2. */
typedef struct wf_pmd_stream wf_pmd_stream;

/* Allocate a raw-deflate decompressor.
 *
 * @window_bits is the *positive* RFC 7692 server_max_window_bits /
 *   client_max_window_bits parameter (8..15).  We pass it to zlib as
 *   '-window_bits' to select raw deflate (no zlib header).
 *
 * Returns NULL on allocation failure.  Caller frees with wf_pmd_free. */
wf_pmd_stream *wf_pmd_inflate_new(int window_bits);

/* Allocate a raw-deflate compressor.
 *
 * @level is the standard zlib level (-1 = default, 0..9).
 * @window_bits and @mem_level are the standard zlib parameters; if
 *   you don't have a reason, pass 8 for mem_level. */
wf_pmd_stream *wf_pmd_deflate_new(int window_bits, int level, int mem_level);

/* Release a stream.  Safe to call on NULL. */
void wf_pmd_free(wf_pmd_stream *s);

/* Inflate one PMD message.
 *
 * @src / @src_len: caller's message payload with the trailing
 *   00 00 FF FF marker already appended (RFC 7692 §7.2.2).
 * @dst / @dst_cap: caller-provided output buffer.  Returns
 *   WF_PMD_NEEDS_MORE_OUT if @dst_cap was too small; the partial
 *   output is in @dst (0..@dst_cap bytes) and *out_produced is set
 *   so the caller can grow + retry against the remaining input
 *   (the stream's internal state advances on each call).
 *
 * On WF_PMD_OK the message is fully decompressed.
 *
 * @sync_flush is *informational* on the inflate side: we always run
 *   inflate with Z_SYNC_FLUSH so multi-fragment messages assembled
 *   ahead of the call go through cleanly. */
int wf_pmd_inflate(wf_pmd_stream *s,
                   const uint8_t *src, size_t src_len,
                   uint8_t *dst, size_t dst_cap,
                   size_t *out_produced);

/* Deflate one PMD message.
 *
 * @src / @src_len: caller's plaintext message payload.
 * @dst / @dst_cap: caller-provided output buffer.  The output ends
 *   with 00 00 FF FF (Z_SYNC_FLUSH); the Haskell side strips those
 *   four bytes per RFC 7692 §7.2.1 before sending.
 *
 * Returns WF_PMD_NEEDS_MORE_OUT if @dst_cap was too small; the
 * partial output is in @dst and the stream's internal state has
 * advanced — the caller should grow and call again with @src=NULL,
 * @src_len=0 to drain the remaining buffered output. */
int wf_pmd_deflate(wf_pmd_stream *s,
                   const uint8_t *src, size_t src_len,
                   uint8_t *dst, size_t dst_cap,
                   size_t *out_produced);

/* Reset the stream's context.  Called after every message when the
 * connection negotiated *_no_context_takeover. */
int wf_pmd_reset_inflate(wf_pmd_stream *s);
int wf_pmd_reset_deflate(wf_pmd_stream *s);

#ifdef __cplusplus
}
#endif

#endif /* WF_PMD_H */
