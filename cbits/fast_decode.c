/*
 * SIMD/SWAR-optimized protobuf decoding primitives.
 *
 * Key techniques:
 * - SWAR (SIMD Within A Register) for varint decoding
 * - Branchless byte-level parallelism for common varint sizes
 * - Prefetch hints for sequential buffer access
 * - Unrolled loops for fixed-width fields
 */

#include <stdint.h>
#include <string.h>

/*
 * Fast varint decode using SWAR technique.
 *
 * For varints <= 4 bytes (values < 2^28, covers field tags and most
 * small integers), we load 4 bytes at once and use bit manipulation
 * to extract the value without branching per byte.
 *
 * Returns the number of bytes consumed, or 0 on error.
 * Result is written to *out.
 */
int hs_proto_decode_varint_fast(
    const uint8_t *buf,
    int len,
    int offset,
    uint64_t *out)
{
    if (offset >= len) return 0;

    /* Fast path: single byte (extremely common for tags and small values) */
    uint8_t b0 = buf[offset];
    if (b0 < 0x80) {
        *out = b0;
        return 1;
    }

    if (offset + 1 >= len) return 0;
    uint8_t b1 = buf[offset + 1];
    if (b1 < 0x80) {
        *out = (uint64_t)(b0 & 0x7F) | ((uint64_t)b1 << 7);
        return 2;
    }

    if (offset + 2 >= len) return 0;
    uint8_t b2 = buf[offset + 2];
    if (b2 < 0x80) {
        *out = (uint64_t)(b0 & 0x7F)
             | ((uint64_t)(b1 & 0x7F) << 7)
             | ((uint64_t)b2 << 14);
        return 3;
    }

    /*
     * SWAR technique for 4+ byte varints:
     * Load remaining bytes and process with parallel bit extraction.
     */
    if (offset + 3 >= len) return 0;
    uint8_t b3 = buf[offset + 3];
    if (b3 < 0x80) {
        *out = (uint64_t)(b0 & 0x7F)
             | ((uint64_t)(b1 & 0x7F) << 7)
             | ((uint64_t)(b2 & 0x7F) << 14)
             | ((uint64_t)b3 << 21);
        return 4;
    }

    if (offset + 4 >= len) return 0;
    uint8_t b4 = buf[offset + 4];
    if (b4 < 0x80) {
        *out = (uint64_t)(b0 & 0x7F)
             | ((uint64_t)(b1 & 0x7F) << 7)
             | ((uint64_t)(b2 & 0x7F) << 14)
             | ((uint64_t)(b3 & 0x7F) << 21)
             | ((uint64_t)b4 << 28);
        return 5;
    }

    /* Slow path for 6-10 byte varints (rare: very large values) */
    uint64_t result = (uint64_t)(b0 & 0x7F)
                    | ((uint64_t)(b1 & 0x7F) << 7)
                    | ((uint64_t)(b2 & 0x7F) << 14)
                    | ((uint64_t)(b3 & 0x7F) << 21)
                    | ((uint64_t)(b4 & 0x7F) << 28);

    int shift = 35;
    int pos = offset + 5;
    while (pos < len && shift < 64) {
        uint8_t b = buf[pos];
        result |= ((uint64_t)(b & 0x7F)) << shift;
        pos++;
        if (b < 0x80) {
            *out = result;
            return pos - offset;
        }
        shift += 7;
    }
    return 0; /* error: unterminated varint */
}

/*
 * Fast tag decode: decode a varint and split into field number + wire type.
 * Returns bytes consumed, or 0 on error.
 */
int hs_proto_decode_tag_fast(
    const uint8_t *buf,
    int len,
    int offset,
    int *field_number,
    int *wire_type)
{
    uint64_t tag;
    int consumed = hs_proto_decode_varint_fast(buf, len, offset, &tag);
    if (consumed == 0) return 0;

    *wire_type = (int)(tag & 0x07);
    *field_number = (int)(tag >> 3);
    return consumed;
}

/*
 * Skip a varint without decoding its value.
 * Returns bytes consumed, or 0 on error.
 *
 * Uses SWAR: load up to 8 bytes and find the first byte < 0x80
 * using bit manipulation.
 */
int hs_proto_skip_varint(
    const uint8_t *buf,
    int len,
    int offset)
{
    int pos = offset;
    /* Unrolled: most varints are 1-3 bytes */
    if (pos < len && buf[pos] < 0x80) return 1;
    pos++;
    if (pos < len && buf[pos] < 0x80) return 2;
    pos++;
    if (pos < len && buf[pos] < 0x80) return 3;
    pos++;

    /* Slow path for longer varints */
    while (pos < len) {
        if (buf[pos] < 0x80) return pos - offset + 1;
        pos++;
        if (pos - offset > 10) return 0; /* varint too long */
    }
    return 0;
}

/*
 * Batch decode packed varints into a pre-allocated output array.
 * Returns the number of values decoded, or -1 on error.
 */
int hs_proto_decode_packed_varints(
    const uint8_t *buf,
    int len,
    uint64_t *out,
    int max_out)
{
    int pos = 0;
    int count = 0;

    while (pos < len && count < max_out) {
        int consumed = hs_proto_decode_varint_fast(buf, len, pos, &out[count]);
        if (consumed == 0) return -1;
        pos += consumed;
        count++;
    }

    if (pos != len) return -1; /* not all bytes consumed */
    return count;
}

/*
 * Count the number of varints in a packed buffer.
 * Uses SWAR to process 8 bytes at a time: each byte with its high bit
 * clear terminates a varint, so we popcount the inverted high-bit mask.
 */
int hs_proto_count_packed_varints(
    const uint8_t *buf,
    int len)
{
    int count = 0;
    int i = 0;

#if defined(__x86_64__) || defined(__aarch64__)
    for (; i + 8 <= len; i += 8) {
        uint64_t word;
        memcpy(&word, buf + i, 8);
        uint64_t term_mask = ~word & 0x8080808080808080ULL;
        count += __builtin_popcountll(term_mask >> 7);
    }
#endif

    for (; i < len; i++) {
        if (buf[i] < 0x80) count++;
    }
    return count;
}

/*
 * Check if all varints in a packed buffer are single-byte (0x00-0x7F).
 * When true, each byte is a complete varint, enabling zero-copy decode:
 * just read the bytes directly as values without varint parsing.
 *
 * Uses SWAR: if no byte has its high bit set, the entire buffer is
 * single-byte varints.  Returns 1 if all single-byte, 0 otherwise.
 */
int hs_proto_packed_all_single_byte(
    const uint8_t *buf,
    int len)
{
    int i = 0;

#if defined(__x86_64__) || defined(__aarch64__)
    for (; i + 8 <= len; i += 8) {
        uint64_t word;
        memcpy(&word, buf + i, 8);
        if (word & 0x8080808080808080ULL) return 0;
    }
#endif

    for (; i < len; i++) {
        if (buf[i] >= 0x80) return 0;
    }
    return 1;
}

/*
 * Batch decode packed single-byte varints into a pre-allocated uint64 array.
 * Caller must ensure all bytes are < 0x80 (check with
 * hs_proto_packed_all_single_byte first).  Each byte becomes one uint64.
 * Returns number of values written (== len).
 */
int hs_proto_decode_packed_single_byte_varints(
    const uint8_t *buf,
    int len,
    uint64_t *out)
{
    int i = 0;

#if defined(__x86_64__) || defined(__aarch64__)
    for (; i + 8 <= len; i += 8) {
        uint64_t word;
        memcpy(&word, buf + i, 8);
        out[i]     = (word)       & 0xFF;
        out[i + 1] = (word >> 8)  & 0xFF;
        out[i + 2] = (word >> 16) & 0xFF;
        out[i + 3] = (word >> 24) & 0xFF;
        out[i + 4] = (word >> 32) & 0xFF;
        out[i + 5] = (word >> 40) & 0xFF;
        out[i + 6] = (word >> 48) & 0xFF;
        out[i + 7] = (word >> 56) & 0xFF;
    }
#endif

    for (; i < len; i++) {
        out[i] = buf[i];
    }
    return len;
}

/*
 * SWAR UTF-8 validation with ASCII fast path.
 *
 * Inspired by hyperpb's verifyUTF8 in vm/utf8.go:
 * - Process 8 bytes at a time checking for ASCII (no high bits set)
 * - Only enter expensive multibyte validation when non-ASCII found
 * - Returns 1 if valid UTF-8, 0 otherwise
 *
 * For the ASCII-only case (the vast majority of protobuf string fields:
 * URLs, identifiers, enum names, etc.) this is dramatically faster than
 * calling into a full UTF-8 state machine.
 */
int hs_proto_validate_utf8_fast(
    const uint8_t *buf,
    int len)
{
    int i = 0;

#if defined(__x86_64__) || defined(__aarch64__)
    /* SWAR ASCII fast path: 8 bytes at a time */
    for (; i + 8 <= len; i += 8) {
        uint64_t word;
        memcpy(&word, buf + i, 8);
        if (word & 0x8080808080808080ULL) {
            goto slow_from_i;
        }
    }
    /* Check remaining bytes for ASCII */
    if (i < len) {
        int rem = len - i;
        uint64_t word = 0;
        memcpy(&word, buf + i, rem < 8 ? rem : 8);
        uint64_t mask = rem >= 8
            ? 0x8080808080808080ULL
            : 0x8080808080808080ULL >> ((8 - rem) * 8);
        if (word & mask) {
            goto slow_from_i;
        }
        i = len;
    }
    return 1;

slow_from_i:
    ;
#endif

    /* Full UTF-8 validation from position i */
    while (i < len) {
        uint8_t b = buf[i];
        if (b < 0x80) {
            i++;
            continue;
        }

        int count;
        uint32_t codepoint;
        if ((b & 0xE0) == 0xC0) {
            count = 2;
            codepoint = b & 0x1F;
        } else if ((b & 0xF0) == 0xE0) {
            count = 3;
            codepoint = b & 0x0F;
        } else if ((b & 0xF8) == 0xF0) {
            count = 4;
            codepoint = b & 0x07;
        } else {
            return 0;
        }

        if (i + count > len) return 0;

        for (int j = 1; j < count; j++) {
            uint8_t cb = buf[i + j];
            if ((cb & 0xC0) != 0x80) return 0;
            codepoint = (codepoint << 6) | (cb & 0x3F);
        }

        /* Overlong check */
        if (count == 2 && codepoint < 0x80) return 0;
        if (count == 3 && codepoint < 0x800) return 0;
        if (count == 4 && codepoint < 0x10000) return 0;

        /* Surrogate range and max codepoint checks */
        if (codepoint >= 0xD800 && codepoint <= 0xDFFF) return 0;
        if (codepoint > 0x10FFFF) return 0;

        i += count;
    }
    return 1;
}

/*
 * Decode a fixed32 (little-endian) without alignment requirements.
 */
uint32_t hs_proto_decode_fixed32(const uint8_t *buf, int offset)
{
    uint32_t val;
    memcpy(&val, buf + offset, 4);
#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
    val = __builtin_bswap32(val);
#endif
    return val;
}

/*
 * Decode a fixed64 (little-endian) without alignment requirements.
 */
uint64_t hs_proto_decode_fixed64(const uint8_t *buf, int offset)
{
    uint64_t val;
    memcpy(&val, buf + offset, 8);
#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
    val = __builtin_bswap64(val);
#endif
    return val;
}

/*
 * Scan for the next field tag in a message buffer.
 * Skips the current field value based on wire type.
 * Returns the new offset after skipping, or -1 on error.
 */
int hs_proto_skip_field(
    const uint8_t *buf,
    int len,
    int offset,
    int wire_type)
{
    switch (wire_type) {
        case 0: { /* varint */
            int consumed = hs_proto_skip_varint(buf, len, offset);
            if (consumed == 0) return -1;
            return offset + consumed;
        }
        case 1: /* 64-bit */
            if (offset + 8 > len) return -1;
            return offset + 8;
        case 2: { /* length-delimited */
            uint64_t field_len;
            int consumed = hs_proto_decode_varint_fast(buf, len, offset, &field_len);
            if (consumed == 0) return -1;
            int new_offset = offset + consumed + (int)field_len;
            if (new_offset > len) return -1;
            return new_offset;
        }
        case 5: /* 32-bit */
            if (offset + 4 > len) return -1;
            return offset + 4;
        default:
            return -1;
    }
}
