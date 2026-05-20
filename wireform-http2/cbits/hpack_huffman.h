#ifndef WIREFORM_HPACK_HUFFMAN_H
#define WIREFORM_HPACK_HUFFMAN_H

#include <stdint.h>
#include <stddef.h>

size_t wireform_hpack_huffman_encode(
    const uint8_t *src, size_t len,
    uint8_t *dst);

size_t wireform_hpack_huffman_encode_len(
    const uint8_t *src, size_t len);

int wireform_hpack_huffman_decode(
    const uint8_t *src, size_t src_len,
    uint8_t *dst, size_t dst_cap,
    size_t *out_len);

/* Fast nibble-based decoder */
int wireform_hpack_huffman_decode_fast(
    const uint8_t *src, size_t src_len,
    uint8_t *dst, size_t dst_cap,
    size_t *out_len);

#endif
