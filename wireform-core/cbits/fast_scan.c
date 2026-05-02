/*
 * Format-agnostic SIMD byte-scanning primitives.
 *
 * These used to live in wireform-xml/cbits/fast_xml.c under
 * XML-specific names because that's where they were first
 * introduced. CSV, NDJSON, and a few other decoders reached
 * into that file to reuse hs_xml_find_byte; to keep the
 * dependency graph honest the generic helper lives here and is
 * exported under a neutral name. The XML-specific scanners
 * (find_lt, find_attr_end, find_cdata_end, etc.) remain in
 * wireform-xml since they encode XML-grammar knowledge.
 *
 * Uses SSE2 via simde for portable SIMD. Fallback to scalar
 * loops on architectures without simde support.
 */

#include <stdint.h>
#include <string.h>
#include <simde/x86/sse2.h>

/*
 * Scan buf[offset..len) for the first occurrence of target_byte.
 * Returns the absolute position of the match, or len if no
 * match was found. Used by CSV delimiter scanning, NDJSON
 * newline scanning, XML structural scanning, etc.
 */
int hs_find_byte(const uint8_t *buf, int offset, int len, uint8_t target_byte)
{
    int i = offset;
    simde__m128i target = simde_mm_set1_epi8((char)target_byte);

    /* Scalar prologue until 16-byte aligned. */
    while (i < len && ((uintptr_t)(buf + i) & 15) != 0) {
        if (buf[i] == target_byte) return i;
        i++;
    }

    /* SIMD main loop: 16 bytes at a time. */
    for (; i + 16 <= len; i += 16) {
        simde__m128i chunk = simde_mm_load_si128((const simde__m128i *)(buf + i));
        int mask = simde_mm_movemask_epi8(simde_mm_cmpeq_epi8(chunk, target));
        if (mask != 0) return i + __builtin_ctz(mask);
    }

    /* Scalar epilogue for the remaining <16 bytes. */
    for (; i < len; i++) {
        if (buf[i] == target_byte) return i;
    }

    return len;
}
