/*
 * wireform_decompress.c
 *
 * Direct-into-buffer decompression wrappers for the Kafka codecs
 * (snappy / gzip / lz4 / zstd).  Each one takes a source pointer +
 * length AND a destination pointer + capacity from the Haskell side
 * — typically a slice of the magic ring's backing memory — and
 * writes the decompressed bytes directly into the destination
 * without going through any intermediate Haskell-side ByteString
 * allocation.
 *
 * The two "sized" codecs (snappy, zstd) can report the expected
 * output length up front; the Haskell side allocates a destination
 * of exactly that size before calling decompress.  The two
 * "streaming" codecs (gzip, lz4-frame) cannot, so we expose them as
 * incremental functions that the Haskell side can drive in a loop
 * against a growing destination ring (when the destination is a
 * magic ring of fixed size, one call is usually enough; the
 * Haskell side checks for "needs more output" and resizes /
 * advances by the produced count).
 *
 * Error handling: every wrapper returns 0 on success and a small
 * negative int on failure, with the produced/consumed counts
 * written to out-pointers so the Haskell side can decide what to
 * do (truncate, retry with bigger buffer, raise a user-visible
 * error, etc.).
 */

#include <stddef.h>
#include <stdint.h>
#include <string.h>

#include <zlib.h>
#include <lz4frame.h>
#include <zstd.h>

/* ------------------------------------------------------------------- *
 *  Return codes used by every entry point in this file.                *
 * ------------------------------------------------------------------- */

#define WF_DECOMPRESS_OK              0
#define WF_DECOMPRESS_NEEDS_MORE_OUT -1  /* dst too small; grow + retry */
#define WF_DECOMPRESS_BAD_INPUT      -2  /* malformed compressed bytes  */
#define WF_DECOMPRESS_OOM            -3  /* libc malloc / alloc failed  */
#define WF_DECOMPRESS_INIT_FAIL      -4  /* lib init returned an error  */

/* =================================================================== *
 *  Gzip (RFC 1952) via libz                                            *
 * =================================================================== *
 *
 * No public "give me the uncompressed size" API in zlib — the size
 * isn't stored in the gzip header.  We drive 'inflate' against the
 * caller-supplied destination buffer; if 'avail_out' hits zero
 * before the stream ends, return WF_DECOMPRESS_NEEDS_MORE_OUT so
 * the Haskell side can grow + retry against a fresh / larger ring
 * region.
 *
 * For Kafka recv path the gzip-compressed Records section is
 * bounded by the wire frame, so a one-shot call with a destination
 * sized to the connection-level @receive.message.max.bytes@ (the
 * existing magic-ring size) will succeed.
 */

int wf_gzip_inflate_into(const uint8_t *src, size_t src_len,
                         uint8_t *dst, size_t dst_cap,
                         size_t *out_produced) {
    if (out_produced) *out_produced = 0;

    z_stream zs;
    memset(&zs, 0, sizeof(zs));

    /* 15 + 32 enables both gzip wrapper (RFC 1952) and zlib wrapper
     * (RFC 1950) auto-detection.  Kafka uses gzip; the auto-detect
     * adds no per-call overhead. */
    int rc = inflateInit2(&zs, 15 + 32);
    if (rc != Z_OK) {
        return WF_DECOMPRESS_INIT_FAIL;
    }

    zs.next_in  = (Bytef *) src;
    zs.avail_in = (uInt) src_len;
    zs.next_out = (Bytef *) dst;
    zs.avail_out = (uInt) dst_cap;

    rc = inflate(&zs, Z_FINISH);

    size_t produced = dst_cap - zs.avail_out;
    inflateEnd(&zs);

    if (out_produced) *out_produced = produced;

    if (rc == Z_STREAM_END) {
        return WF_DECOMPRESS_OK;
    } else if (rc == Z_BUF_ERROR || rc == Z_OK) {
        /* Z_BUF_ERROR with non-zero produced count means we ran out
         * of destination space mid-stream; caller should grow + retry. */
        return WF_DECOMPRESS_NEEDS_MORE_OUT;
    } else {
        return WF_DECOMPRESS_BAD_INPUT;
    }
}

/* =================================================================== *
 *  LZ4 frame via liblz4                                                *
 * =================================================================== *
 *
 * The frame header optionally encodes the content size — but Kafka
 * producers (the JVM client, librdkafka) typically omit it.  Use
 * the streaming decompressor.  One call handles the whole stream
 * when the destination buffer fits; if not, return
 * WF_DECOMPRESS_NEEDS_MORE_OUT so the Haskell side can grow / chunk.
 *
 * Allocates a per-call decompression context (tiny — a few hundred
 * bytes — and pooling across calls is a follow-up if it shows up
 * in benchmarks).
 */

int wf_lz4f_decompress_into(const uint8_t *src, size_t src_len,
                            uint8_t *dst, size_t dst_cap,
                            size_t *out_produced, size_t *out_consumed) {
    if (out_produced) *out_produced = 0;
    if (out_consumed) *out_consumed = 0;

    LZ4F_dctx *dctx = NULL;
    LZ4F_errorCode_t err = LZ4F_createDecompressionContext(&dctx, LZ4F_VERSION);
    if (LZ4F_isError(err)) {
        return WF_DECOMPRESS_INIT_FAIL;
    }

    size_t dst_size = dst_cap;
    size_t src_size = src_len;
    /* options: NULL ⇒ default (no checksum override) */
    size_t hint = LZ4F_decompress(dctx, dst, &dst_size, src, &src_size, NULL);

    LZ4F_freeDecompressionContext(dctx);

    if (out_produced) *out_produced = dst_size;
    if (out_consumed) *out_consumed = src_size;

    if (LZ4F_isError(hint)) {
        return WF_DECOMPRESS_BAD_INPUT;
    }
    /* hint == 0 means the frame is fully consumed.  Non-zero hint
     * means the decompressor wants more input — but we passed it
     * the whole frame from Kafka, so a non-zero hint here means we
     * also ran out of output space.  Tell the caller to grow. */
    return hint == 0 ? WF_DECOMPRESS_OK : WF_DECOMPRESS_NEEDS_MORE_OUT;
}

/* =================================================================== *
 *  Zstd via libzstd                                                    *
 * =================================================================== *
 *
 * Zstd frames typically embed the decompressed size in the header.
 * 'wf_zstd_get_frame_content_size' returns it (or
 * (size_t)-1 / -2 sentinels for unknown / error per the
 * upstream conventions); the Haskell side uses that to size the
 * destination buffer exactly, then calls 'wf_zstd_decompress_into'
 * for a single-shot decompress.
 *
 * For frames that don't carry the size up front (rare in Kafka)
 * the Haskell side can fall back to driving 'ZSTD_decompressStream'
 * directly against a growing ring — that path lives in the
 * Haskell module so it can pick the right destination strategy
 * without C↔Haskell round-trips on each grow.
 */

size_t wf_zstd_get_frame_content_size(const uint8_t *src, size_t src_len) {
    /* Returns: actual size, ZSTD_CONTENTSIZE_UNKNOWN (-1ULL), or
     * ZSTD_CONTENTSIZE_ERROR (-2ULL). */
    return ZSTD_getFrameContentSize(src, src_len);
}

int wf_zstd_decompress_into(const uint8_t *src, size_t src_len,
                            uint8_t *dst, size_t dst_cap,
                            size_t *out_produced) {
    if (out_produced) *out_produced = 0;

    size_t produced = ZSTD_decompress(dst, dst_cap, src, src_len);
    if (ZSTD_isError(produced)) {
        /* String-match against ZSTD_getErrorName because the typed
         * error-code API (ZSTD_getErrorCode / ZSTD_error_*) lives
         * behind the EXPERIMENTAL section of zstd.h and isn't
         * built into the standard libzstd distribution by default. */
        const char *name = ZSTD_getErrorName(produced);
        if (name && strstr(name, "Destination buffer is too small")) {
            return WF_DECOMPRESS_NEEDS_MORE_OUT;
        }
        return WF_DECOMPRESS_BAD_INPUT;
    }
    if (out_produced) *out_produced = produced;
    return WF_DECOMPRESS_OK;
}

/* =================================================================== *
 *  Snappy block format                                                 *
 * =================================================================== *
 *
 * 'snappy_ffi.c' already has 'snappy_decompress_into' +
 * 'snappy_get_uncompressed_length_wrapper'.  Nothing to add here.
 */
