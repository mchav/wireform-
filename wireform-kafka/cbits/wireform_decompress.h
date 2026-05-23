/*
 * wireform_decompress.h
 *
 * Direct-into-buffer decompression for Kafka codecs.  See the
 * companion .c file for design notes.
 */

#ifndef WIREFORM_DECOMPRESS_H
#define WIREFORM_DECOMPRESS_H

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Return codes used by all wrappers. */
#define WF_DECOMPRESS_OK              0
#define WF_DECOMPRESS_NEEDS_MORE_OUT -1
#define WF_DECOMPRESS_BAD_INPUT      -2
#define WF_DECOMPRESS_OOM            -3
#define WF_DECOMPRESS_INIT_FAIL      -4

/* Gzip / zlib (auto-detect): single-shot inflate into dst.
 * Returns one of the WF_DECOMPRESS_* codes; *out_produced is set
 * to the bytes actually written into dst on success / partial. */
int wf_gzip_inflate_into(const uint8_t *src, size_t src_len,
                         uint8_t *dst, size_t dst_cap,
                         size_t *out_produced);

/* LZ4 frame format: single-shot decompress.  *out_produced =
 * bytes written, *out_consumed = bytes read from src. */
int wf_lz4f_decompress_into(const uint8_t *src, size_t src_len,
                            uint8_t *dst, size_t dst_cap,
                            size_t *out_produced, size_t *out_consumed);

/* Zstd: returns the frame content size from the header.  Returns
 * ZSTD_CONTENTSIZE_UNKNOWN ((size_t)-1) when not embedded,
 * ZSTD_CONTENTSIZE_ERROR ((size_t)-2) on malformed input. */
size_t wf_zstd_get_frame_content_size(const uint8_t *src, size_t src_len);

/* Zstd: single-shot decompress into dst.  Returns one of the
 * WF_DECOMPRESS_* codes.  *out_produced is the bytes written. */
int wf_zstd_decompress_into(const uint8_t *src, size_t src_len,
                            uint8_t *dst, size_t dst_cap,
                            size_t *out_produced);

#ifdef __cplusplus
}
#endif

#endif /* WIREFORM_DECOMPRESS_H */
