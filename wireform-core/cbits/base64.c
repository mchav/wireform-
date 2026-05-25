/*
 * RFC 4648 \u00a74 base64 encode / decode.
 *
 * Two paths each, in line with the rest of wireform-core:
 *
 *   * A vectorised hot loop using SSSE3 (via simde, so the same
 *     code compiles on ARM NEON / WASM / scalar fallback).  Encodes
 *     12 input bytes -> 16 output chars per iteration; decodes 16
 *     input chars -> 12 output bytes per iteration.  Activates only
 *     when the remaining length is wide enough to amortise the
 *     register setup.
 *   * A scalar prologue / epilogue that handles the tail (and the
 *     entire payload for inputs under the SIMD threshold).
 *
 * The SIMD encode is the well-known Mu\u0142a / alfredklomp formula
 * (no LUT in memory; ASCII offset is derived from the 6-bit value
 * via a single PSHUFB on a 16-byte lookup).  The SIMD decode uses
 * a 16-entry PSHUFB classifier to map ASCII -> 6-bit value, then
 * a maddubs / madd / pack chain to repack 16 chars -> 12 bytes.
 *
 * The scalar paths are written in the most boring way possible so
 * the implementation is easy to audit against RFC 4648 \u00a74.
 */

#include <stdint.h>
#include <string.h>
#include <simde/x86/sse2.h>
#include <simde/x86/ssse3.h>

static const char b64_alphabet[64] =
  "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  "abcdefghijklmnopqrstuvwxyz"
  "0123456789+/";

/* Reverse-lookup table: ASCII -> 6-bit value, or 0xFF for any byte
 * outside the standard base64 alphabet (treat '=' as a special
 * terminator separately; here it is 0xFF too). */
static const uint8_t b64_dec_table[256] = {
  /* 0x00..0x2A */
  255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
  255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
  255,255,255,255,255,255,255,255,255,255,255,
  /* 0x2B '+' = 62, 0x2C ',' invalid, 0x2D '-' invalid,
   * 0x2E '.' invalid, 0x2F '/' = 63 */
  62, 255, 255, 255, 63,
  /* 0x30..0x39 '0'..'9' = 52..61 */
  52, 53, 54, 55, 56, 57, 58, 59, 60, 61,
  /* 0x3A..0x40 */
  255, 255, 255, 255, 255, 255, 255,
  /* 0x41..0x5A 'A'..'Z' = 0..25 */
  0,  1,  2,  3,  4,  5,  6,  7,  8,  9,
  10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
  20, 21, 22, 23, 24, 25,
  /* 0x5B..0x60 */
  255, 255, 255, 255, 255, 255,
  /* 0x61..0x7A 'a'..'z' = 26..51 */
  26, 27, 28, 29, 30, 31, 32, 33, 34, 35,
  36, 37, 38, 39, 40, 41, 42, 43, 44, 45,
  46, 47, 48, 49, 50, 51,
  /* 0x7B..0xFF */
  255, 255, 255, 255, 255,
  255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
  255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
  255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
  255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
  255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
  255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
  255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
  255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
};

/* ------------------------------------------------------------------
 * Length helpers (RFC 4648 \u00a74).
 *
 * Encode: 3-byte triplets pad up to the next multiple of 4 chars.
 * Decode (upper bound): every 4 chars produce up to 3 bytes;
 * trailing '=' chars subtract one byte each.  The exact decoded
 * length is reported by hs_base64_decode itself; this helper just
 * sizes the output buffer.
 * ------------------------------------------------------------------ */

int hs_base64_encoded_length(int in_len)
{
    if (in_len < 0) return 0;
    return ((in_len + 2) / 3) * 4;
}

int hs_base64_decoded_max_length(int in_len)
{
    if (in_len < 0) return 0;
    return (in_len / 4) * 3;
}

/* ------------------------------------------------------------------
 * Encode
 *
 * Returns the number of bytes written to @out.  Always equals
 * hs_base64_encoded_length(in_len) (padded form).
 * ------------------------------------------------------------------ */

static inline simde__m128i enc_reshuffle(simde__m128i in)
{
    /* Spread each 3-byte triplet into a 4-byte slot so the 6-bit
     * fields land in adjacent bytes.  Lane layout matches the
     * alfredklomp / Mu\u0142a SSSE3 encoder. */
    in = simde_mm_shuffle_epi8(in, simde_mm_setr_epi8(
         1,  0,  2,  1,
         4,  3,  5,  4,
         7,  6,  8,  7,
        10,  9, 11, 10));

    simde__m128i t0 = simde_mm_and_si128(in, simde_mm_set1_epi32(0x0fc0fc00));
    simde__m128i t1 = simde_mm_mulhi_epu16(t0, simde_mm_set1_epi32(0x04000040));
    simde__m128i t2 = simde_mm_and_si128(in, simde_mm_set1_epi32(0x003f03f0));
    simde__m128i t3 = simde_mm_mullo_epi16(t2, simde_mm_set1_epi32(0x01000010));
    return simde_mm_or_si128(t1, t3);
}

static inline simde__m128i enc_translate(simde__m128i in)
{
    /* 6-bit value v -> ASCII char via a 16-entry PSHUFB lookup of
     * the offset to add to v.  Branchless. */
    simde__m128i lut = simde_mm_setr_epi8(
        65, 71, -4, -4, -4, -4, -4, -4,
        -4, -4, -4, -4, -19, -16, 0, 0);
    simde__m128i indices = simde_mm_subs_epu8(in, simde_mm_set1_epi8(51));
    simde__m128i mask    = simde_mm_cmpgt_epi8(in, simde_mm_set1_epi8(25));
    indices = simde_mm_sub_epi8(indices, mask);
    simde__m128i offsets = simde_mm_shuffle_epi8(lut, indices);
    return simde_mm_add_epi8(in, offsets);
}

int hs_base64_encode(const uint8_t *in, int in_len, uint8_t *out)
{
    int i = 0, j = 0;

    /* SIMD: 12 in -> 16 out per iter.  Need 16 readable input bytes
     * (we load 16 then mask out the upper 4) so guard accordingly. */
    while (in_len - i >= 16) {
        simde__m128i chunk = simde_mm_loadu_si128((const simde__m128i *)(in + i));
        simde__m128i v = enc_reshuffle(chunk);
        simde__m128i c = enc_translate(v);
        simde_mm_storeu_si128((simde__m128i *)(out + j), c);
        i += 12;
        j += 16;
    }

    /* Scalar tail: 3 in -> 4 out. */
    while (in_len - i >= 3) {
        uint32_t v = ((uint32_t)in[i]   << 16)
                   | ((uint32_t)in[i+1] <<  8)
                   |  (uint32_t)in[i+2];
        out[j+0] = (uint8_t)b64_alphabet[(v >> 18) & 0x3F];
        out[j+1] = (uint8_t)b64_alphabet[(v >> 12) & 0x3F];
        out[j+2] = (uint8_t)b64_alphabet[(v >>  6) & 0x3F];
        out[j+3] = (uint8_t)b64_alphabet[ v        & 0x3F];
        i += 3;
        j += 4;
    }

    /* RFC 4648 \u00a74 padding. */
    int rem = in_len - i;
    if (rem == 1) {
        uint32_t v = (uint32_t)in[i] << 16;
        out[j+0] = (uint8_t)b64_alphabet[(v >> 18) & 0x3F];
        out[j+1] = (uint8_t)b64_alphabet[(v >> 12) & 0x3F];
        out[j+2] = (uint8_t)'=';
        out[j+3] = (uint8_t)'=';
        j += 4;
    } else if (rem == 2) {
        uint32_t v = ((uint32_t)in[i] << 16) | ((uint32_t)in[i+1] << 8);
        out[j+0] = (uint8_t)b64_alphabet[(v >> 18) & 0x3F];
        out[j+1] = (uint8_t)b64_alphabet[(v >> 12) & 0x3F];
        out[j+2] = (uint8_t)b64_alphabet[(v >>  6) & 0x3F];
        out[j+3] = (uint8_t)'=';
        j += 4;
    }
    return j;
}

/* ------------------------------------------------------------------
 * Decode
 *
 * Strict RFC 4648 decoder:
 *
 *   * Input length must be a multiple of 4.  Returns -1 otherwise.
 *   * Only the standard alphabet is accepted (no URL-safe variants;
 *     no whitespace tolerance).  Returns -1 on any out-of-alphabet
 *     byte that isn't '=' in the trailing position.
 *   * Returns the number of output bytes written on success
 *     (always <= hs_base64_decoded_max_length(in_len)).
 *
 * The SIMD body uses the Mu\u0142a SSSE3 decoder: classify 16
 * input chars in parallel, then a maddubs/madd/shuffle chain
 * repacks 16 sextets into 12 octets.
 * ------------------------------------------------------------------ */

static inline int sse_decode_block(const uint8_t *in, uint8_t *out)
{
    simde__m128i chunk = simde_mm_loadu_si128((const simde__m128i *)in);

    /* Classification: produce the 6-bit value for each input char,
     * or 0xFF if invalid.  We use Mu\u0142a's 16-entry hi-nibble
     * mask + lo-nibble mask approach, but for safety we still
     * fall back to a scalar re-check on any 0xFF.  Simpler and
     * portable. */
    simde__m128i hi_nibbles  = simde_mm_and_si128(simde_mm_srli_epi32(chunk, 4),
                                                  simde_mm_set1_epi8(0x0F));
    simde__m128i lo_nibbles  = simde_mm_and_si128(chunk, simde_mm_set1_epi8(0x0F));

    /* Mu\u0142a classifier tables. */
    const simde__m128i lut_hi = simde_mm_setr_epi8(
        0x10, 0x10, 0x01, 0x02, 0x04, 0x08, 0x04, 0x08,
        0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10);
    const simde__m128i lut_lo = simde_mm_setr_epi8(
        0x15, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
        0x11, 0x11, 0x13, 0x1A, 0x1B, 0x1B, 0x1B, 0x1A);
    const simde__m128i lut_roll = simde_mm_setr_epi8(
        0,   16,  19,   4,  -65, -65, -71, -71,
        0,    0,   0,    0,   0,   0,   0,   0);

    simde__m128i hi = simde_mm_shuffle_epi8(lut_hi, hi_nibbles);
    simde__m128i lo = simde_mm_shuffle_epi8(lut_lo, lo_nibbles);
    simde__m128i validate = simde_mm_and_si128(lo, hi);
    int err_mask = simde_mm_movemask_epi8(simde_mm_cmpeq_epi8(
        validate, simde_mm_setzero_si128()));
    if (err_mask != 0) return -1;

    simde__m128i eq_2f = simde_mm_cmpeq_epi8(chunk, simde_mm_set1_epi8(0x2F));
    simde__m128i roll  = simde_mm_shuffle_epi8(lut_roll,
                            simde_mm_add_epi8(eq_2f, hi_nibbles));
    simde__m128i values = simde_mm_add_epi8(chunk, roll);
    /* 'values' now contains the 6-bit value in each of the 16 lanes. */

    /* Pack 16 sextets into 12 octets. */
    simde__m128i merged = simde_mm_maddubs_epi16(
        values, simde_mm_set1_epi32(0x01400140));
    simde__m128i merged2 = simde_mm_madd_epi16(
        merged, simde_mm_set1_epi32(0x00011000));
    simde__m128i shuf = simde_mm_setr_epi8(
         2,  1,  0,  6,  5,  4, 10,  9,
         8, 14, 13, 12, -1, -1, -1, -1);
    simde__m128i packed = simde_mm_shuffle_epi8(merged2, shuf);

    simde_mm_storeu_si128((simde__m128i *)out, packed);
    return 0;
}

int hs_base64_decode(const uint8_t *in, int in_len, uint8_t *out)
{
    if (in_len < 0 || (in_len & 3) != 0) return -1;
    if (in_len == 0) return 0;

    int i = 0, j = 0;
    int main_len = in_len;

    /* The last 4-byte quartet may contain '=' padding, which the
     * SIMD validator would reject.  Handle it in the scalar tail. */
    int tail_len = 4;
    main_len -= tail_len;

    /* SIMD body: 16 in -> 12 out per iter.  Need at least 16 chars
     * available AND we mustn't enter the padded last quartet. */
    while (main_len - i >= 16) {
        if (sse_decode_block(in + i, out + j) != 0) {
            /* Bail to the scalar path: re-decode this 16-char
             * window the slow way so a single bad byte gives a
             * precise error rather than a false-positive on the
             * SSSE3 classifier. */
            int scalar_end = i + 16;
            while (i < scalar_end) {
                uint8_t a = b64_dec_table[in[i+0]];
                uint8_t b = b64_dec_table[in[i+1]];
                uint8_t c = b64_dec_table[in[i+2]];
                uint8_t d = b64_dec_table[in[i+3]];
                if ((a | b | c | d) >= 64) return -1;
                uint32_t v = ((uint32_t)a << 18)
                           | ((uint32_t)b << 12)
                           | ((uint32_t)c <<  6)
                           |  (uint32_t)d;
                out[j+0] = (uint8_t)(v >> 16);
                out[j+1] = (uint8_t)(v >>  8);
                out[j+2] = (uint8_t) v;
                i += 4;
                j += 3;
            }
            continue;
        }
        i += 16;
        j += 12;
    }

    /* Scalar middle: 4 in -> 3 out, no padding. */
    while (i < main_len) {
        uint8_t a = b64_dec_table[in[i+0]];
        uint8_t b = b64_dec_table[in[i+1]];
        uint8_t c = b64_dec_table[in[i+2]];
        uint8_t d = b64_dec_table[in[i+3]];
        if ((a | b | c | d) >= 64) return -1;
        uint32_t v = ((uint32_t)a << 18)
                   | ((uint32_t)b << 12)
                   | ((uint32_t)c <<  6)
                   |  (uint32_t)d;
        out[j+0] = (uint8_t)(v >> 16);
        out[j+1] = (uint8_t)(v >>  8);
        out[j+2] = (uint8_t) v;
        i += 4;
        j += 3;
    }

    /* Tail quartet: may contain '=' padding. */
    {
        uint8_t a = b64_dec_table[in[i+0]];
        uint8_t b = b64_dec_table[in[i+1]];
        if (a >= 64 || b >= 64) return -1;
        if (in[i+2] == (uint8_t)'=') {
            if (in[i+3] != (uint8_t)'=') return -1;
            uint32_t v = ((uint32_t)a << 18) | ((uint32_t)b << 12);
            out[j+0] = (uint8_t)(v >> 16);
            j += 1;
        } else if (in[i+3] == (uint8_t)'=') {
            uint8_t c = b64_dec_table[in[i+2]];
            if (c >= 64) return -1;
            uint32_t v = ((uint32_t)a << 18)
                       | ((uint32_t)b << 12)
                       | ((uint32_t)c <<  6);
            out[j+0] = (uint8_t)(v >> 16);
            out[j+1] = (uint8_t)(v >>  8);
            j += 2;
        } else {
            uint8_t c = b64_dec_table[in[i+2]];
            uint8_t d = b64_dec_table[in[i+3]];
            if (c >= 64 || d >= 64) return -1;
            uint32_t v = ((uint32_t)a << 18)
                       | ((uint32_t)b << 12)
                       | ((uint32_t)c <<  6)
                       |  (uint32_t)d;
            out[j+0] = (uint8_t)(v >> 16);
            out[j+1] = (uint8_t)(v >>  8);
            out[j+2] = (uint8_t) v;
            j += 3;
        }
        i += 4;
    }
    (void)i;
    return j;
}
