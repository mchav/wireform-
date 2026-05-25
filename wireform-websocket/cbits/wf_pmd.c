/*
 * wf_pmd.c
 *
 * RFC 7692 permessage-deflate FFI shim.  See wf_pmd.h for the API.
 *
 * Implementation notes:
 *
 * * Raw deflate (no zlib header) selected via negative
 *   window_bits to inflateInit2 / deflateInit2.  RFC 7692 sec 7.2
 *   says compressed messages are raw DEFLATE blocks per RFC 1951.
 *
 * * The PMD framing wraps each message with a Z_SYNC_FLUSH, which
 *   emits a non-compressed-block trailer of 00 00 FF FF.  The
 *   Haskell side strips that on send and appends it on receive
 *   (RFC 7692 sec 7.2.1 / 7.2.2).
 *
 * * No global state, no thread-local state, no locks.  The Haskell
 *   side serialises per-connection access via the existing
 *   connection-level send / recv locks (or single-threaded mode,
 *   where the user has already promised no concurrent use).
 *
 * * The opaque 'wf_pmd_stream' struct currently just wraps the
 *   z_stream, but is laid out as its own type so we can add bookkeeping
 *   later (e.g. an output-overflow position so resumed deflate
 *   calls can pick up where the previous WF_PMD_NEEDS_MORE_OUT
 *   left off without the Haskell side having to retain state).
 */

#include <stdlib.h>
#include <string.h>
#include <zlib.h>

#include "wf_pmd.h"

struct wf_pmd_stream {
    z_stream zs;
    int      kind;  /* 0 = inflate, 1 = deflate */
};

wf_pmd_stream *wf_pmd_inflate_new(int window_bits) {
    if (window_bits < 8 || window_bits > 15) {
        return NULL;
    }
    /* zlib does not actually support window_bits == 8 for inflate
     * even though RFC 7692 allows it on the wire; bump to 9.  This
     * matches what nginx and Chrome do internally. */
    if (window_bits == 8) {
        window_bits = 9;
    }

    wf_pmd_stream *s = (wf_pmd_stream *) calloc(1, sizeof(*s));
    if (!s) {
        return NULL;
    }
    s->kind = 0;

    int rc = inflateInit2(&s->zs, -window_bits);
    if (rc != Z_OK) {
        free(s);
        return NULL;
    }
    return s;
}

wf_pmd_stream *wf_pmd_deflate_new(int window_bits, int level, int mem_level) {
    if (window_bits < 8 || window_bits > 15) {
        return NULL;
    }
    if (window_bits == 8) {
        window_bits = 9;
    }
    if (mem_level < 1 || mem_level > 9) {
        mem_level = 8;
    }
    if (level < -1 || level > 9) {
        level = Z_DEFAULT_COMPRESSION;
    }

    wf_pmd_stream *s = (wf_pmd_stream *) calloc(1, sizeof(*s));
    if (!s) {
        return NULL;
    }
    s->kind = 1;

    int rc = deflateInit2(&s->zs,
                          level,
                          Z_DEFLATED,
                          -window_bits,
                          mem_level,
                          Z_DEFAULT_STRATEGY);
    if (rc != Z_OK) {
        free(s);
        return NULL;
    }
    return s;
}

void wf_pmd_free(wf_pmd_stream *s) {
    if (!s) return;
    if (s->kind == 0) {
        (void) inflateEnd(&s->zs);
    } else {
        (void) deflateEnd(&s->zs);
    }
    free(s);
}

int wf_pmd_inflate(wf_pmd_stream *s,
                   const uint8_t *src, size_t src_len,
                   uint8_t *dst, size_t dst_cap,
                   size_t *out_produced) {
    if (out_produced) *out_produced = 0;
    if (!s || s->kind != 0) {
        return WF_PMD_BAD_INPUT;
    }

    s->zs.next_in   = (Bytef *) src;
    s->zs.avail_in  = (uInt) src_len;
    s->zs.next_out  = (Bytef *) dst;
    s->zs.avail_out = (uInt) dst_cap;

    int rc = inflate(&s->zs, Z_SYNC_FLUSH);

    /* On Z_OK with no remaining input we've consumed the full
     * frame; treat that as success.  Z_STREAM_END is not expected
     * on PMD because messages don't carry a BFINAL=1 block. */
    if (out_produced) {
        *out_produced = dst_cap - s->zs.avail_out;
    }

    if (rc == Z_OK || rc == Z_STREAM_END) {
        if (s->zs.avail_in > 0 && s->zs.avail_out == 0) {
            /* Ran out of output space mid-message.  Caller grows
             * the destination and calls again; the remaining input
             * is still queued on the stream so we don't need to
             * retain it on the Haskell side. */
            return WF_PMD_NEEDS_MORE_OUT;
        }
        return WF_PMD_OK;
    }
    if (rc == Z_BUF_ERROR) {
        /* Either no progress was possible OR we just ran out of
         * output space.  Distinguishing requires looking at
         * avail_in / avail_out: if avail_out == 0 we ran out of
         * room. */
        if (s->zs.avail_out == 0) {
            return WF_PMD_NEEDS_MORE_OUT;
        }
        /* Stalled with output room but no progress — malformed. */
        return WF_PMD_BAD_INPUT;
    }
    return WF_PMD_BAD_INPUT;
}

int wf_pmd_deflate(wf_pmd_stream *s,
                   const uint8_t *src, size_t src_len,
                   uint8_t *dst, size_t dst_cap,
                   size_t *out_produced) {
    if (out_produced) *out_produced = 0;
    if (!s || s->kind != 1) {
        return WF_PMD_BAD_INPUT;
    }

    s->zs.next_in   = (Bytef *) src;
    s->zs.avail_in  = (uInt) src_len;
    s->zs.next_out  = (Bytef *) dst;
    s->zs.avail_out = (uInt) dst_cap;

    int rc = deflate(&s->zs, Z_SYNC_FLUSH);

    if (out_produced) {
        *out_produced = dst_cap - s->zs.avail_out;
    }

    if (rc == Z_OK) {
        if (s->zs.avail_in > 0 || s->zs.avail_out == 0) {
            return WF_PMD_NEEDS_MORE_OUT;
        }
        return WF_PMD_OK;
    }
    if (rc == Z_BUF_ERROR) {
        if (s->zs.avail_out == 0) {
            return WF_PMD_NEEDS_MORE_OUT;
        }
        return WF_PMD_BAD_INPUT;
    }
    return WF_PMD_BAD_INPUT;
}

int wf_pmd_reset_inflate(wf_pmd_stream *s) {
    if (!s || s->kind != 0) return WF_PMD_BAD_INPUT;
    return inflateReset(&s->zs) == Z_OK ? WF_PMD_OK : WF_PMD_INIT_FAIL;
}

int wf_pmd_reset_deflate(wf_pmd_stream *s) {
    if (!s || s->kind != 1) return WF_PMD_BAD_INPUT;
    return deflateReset(&s->zs) == Z_OK ? WF_PMD_OK : WF_PMD_INIT_FAIL;
}
