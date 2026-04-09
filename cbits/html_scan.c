#include <stdint.h>
#include <stddef.h>

#if defined(__aarch64__) || defined(_M_ARM64)
#include <arm_neon.h>
#define USE_NEON 1
#elif defined(__SSE2__)
#include <emmintrin.h>
#define USE_SSE2 1
#endif

/* Scan forward from buf+off looking for <, &, \0, or \r.
   Returns the byte offset of the first match, or 'end' if none found.
   Precondition: off <= end, end <= buflen. */
ptrdiff_t wireform_scan_text(const uint8_t *buf, ptrdiff_t off, ptrdiff_t end) {
    const uint8_t *p = buf + off;
    const uint8_t *pend = buf + end;

#if USE_NEON
    {
        const uint8x16_t v_lt = vdupq_n_u8(0x3C);   /* < */
        const uint8x16_t v_amp = vdupq_n_u8(0x26);   /* & */
        const uint8x16_t v_nul = vdupq_n_u8(0x00);   /* \0 */
        const uint8x16_t v_cr = vdupq_n_u8(0x0D);    /* \r */

        while (p + 16 <= pend) {
            uint8x16_t chunk = vld1q_u8(p);
            uint8x16_t hit = vorrq_u8(
                vorrq_u8(vceqq_u8(chunk, v_lt), vceqq_u8(chunk, v_amp)),
                vorrq_u8(vceqq_u8(chunk, v_nul), vceqq_u8(chunk, v_cr)));
            uint64x2_t hit64 = vreinterpretq_u64_u8(hit);
            uint64_t lo = vgetq_lane_u64(hit64, 0);
            uint64_t hi = vgetq_lane_u64(hit64, 1);
            if (lo) {
                return (ptrdiff_t)(p - buf) + (__builtin_ctzll(lo) >> 3);
            }
            if (hi) {
                return (ptrdiff_t)(p - buf) + 8 + (__builtin_ctzll(hi) >> 3);
            }
            p += 16;
        }
    }
#elif USE_SSE2
    {
        const __m128i v_lt = _mm_set1_epi8(0x3C);
        const __m128i v_amp = _mm_set1_epi8(0x26);
        const __m128i v_nul = _mm_setzero_si128();
        const __m128i v_cr = _mm_set1_epi8(0x0D);

        while (p + 16 <= pend) {
            __m128i chunk = _mm_loadu_si128((const __m128i *)p);
            __m128i hit = _mm_or_si128(
                _mm_or_si128(_mm_cmpeq_epi8(chunk, v_lt), _mm_cmpeq_epi8(chunk, v_amp)),
                _mm_or_si128(_mm_cmpeq_epi8(chunk, v_nul), _mm_cmpeq_epi8(chunk, v_cr)));
            int mask = _mm_movemask_epi8(hit);
            if (mask) {
                return (ptrdiff_t)(p - buf) + __builtin_ctz(mask);
            }
            p += 16;
        }
    }
#endif

    /* Scalar tail */
    while (p < pend) {
        uint8_t b = *p;
        if (b == 0x3C || b == 0x26 || b == 0x00 || b == 0x0D)
            return (ptrdiff_t)(p - buf);
        p++;
    }
    return end;
}

/* Scan for end of tag name. Returns offset of first non-tagname byte. */
ptrdiff_t wireform_scan_tagname(const uint8_t *buf, ptrdiff_t off, ptrdiff_t end) {
    const uint8_t *p = buf + off;
    const uint8_t *pend = buf + end;
    while (p < pend) {
        uint8_t b = *p;
        if ((b >= 'a' && b <= 'z') || (b >= 'A' && b <= 'Z')
            || (b >= '0' && b <= '9')
            || b == '-' || b == '_' || b == ':' || b == '.' || b == '<') {
            p++;
        } else {
            break;
        }
    }
    return (ptrdiff_t)(p - buf);
}

/* Check if all bytes in [off, end) are ASCII (< 0x80). */
int wireform_is_all_ascii(const uint8_t *buf, ptrdiff_t off, ptrdiff_t end) {
    const uint8_t *p = buf + off;
    const uint8_t *pend = buf + end;

#if USE_NEON
    {
        const uint8x16_t v_hi = vdupq_n_u8(0x80);
        while (p + 16 <= pend) {
            uint8x16_t chunk = vld1q_u8(p);
            if (vmaxvq_u8(vandq_u8(chunk, v_hi)) != 0) return 0;
            p += 16;
        }
    }
#elif USE_SSE2
    {
        while (p + 16 <= pend) {
            __m128i chunk = _mm_loadu_si128((const __m128i *)p);
            int mask = _mm_movemask_epi8(chunk);
            if (mask) return 0;
            p += 16;
        }
    }
#endif

    while (p < pend) {
        if (*p >= 0x80) return 0;
        p++;
    }
    return 1;
}
