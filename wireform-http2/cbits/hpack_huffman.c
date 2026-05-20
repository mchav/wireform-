#include <stdint.h>
#include <stddef.h>
#include <string.h>

/* RFC 7541 Appendix B Huffman code table.
 * Each entry: [code, bit_length]
 */

struct huff_entry {
    uint32_t code;
    uint8_t  bits;
};

static const struct huff_entry huff_table[257] = {
    {0x1ff8, 13}, {0x7fffd8, 23}, {0xfffffe2, 28}, {0xfffffe3, 28},
    {0xfffffe4, 28}, {0xfffffe5, 28}, {0xfffffe6, 28}, {0xfffffe7, 28},
    {0xfffffe8, 28}, {0xffffea, 24}, {0x3ffffffc, 30}, {0xfffffe9, 28},
    {0xfffffea, 28}, {0x3ffffffd, 30}, {0xfffffeb, 28}, {0xfffffec, 28},
    {0xfffffed, 28}, {0xfffffee, 28}, {0xfffffef, 28}, {0xffffff0, 28},
    {0xffffff1, 28}, {0xffffff2, 28}, {0x3ffffffe, 30}, {0xffffff3, 28},
    {0xffffff4, 28}, {0xffffff5, 28}, {0xffffff6, 28}, {0xffffff7, 28},
    {0xffffff8, 28}, {0xffffff9, 28}, {0xffffffa, 28}, {0xffffffb, 28},
    {0x14, 6}, {0x3f8, 10}, {0x3f9, 10}, {0xffa, 12},
    {0x1ff9, 13}, {0x15, 6}, {0xf8, 8}, {0x7fa, 11},
    {0x3fa, 10}, {0x3fb, 10}, {0xf9, 8}, {0x7fb, 11},
    {0xfa, 8}, {0x16, 6}, {0x17, 6}, {0x18, 6},
    {0x0, 5}, {0x1, 5}, {0x2, 5}, {0x19, 6},
    {0x1a, 6}, {0x1b, 6}, {0x1c, 6}, {0x1d, 6},
    {0x1e, 6}, {0x1f, 6}, {0x5c, 7}, {0xfb, 8},
    {0x7ffc, 15}, {0x20, 6}, {0xffb, 12}, {0x3fc, 10},
    {0x1ffa, 13}, {0x21, 6}, {0x5d, 7}, {0x5e, 7},
    {0x5f, 7}, {0x60, 7}, {0x61, 7}, {0x62, 7},
    {0x63, 7}, {0x64, 7}, {0x65, 7}, {0x66, 7},
    {0x67, 7}, {0x68, 7}, {0x69, 7}, {0x6a, 7},
    {0x6b, 7}, {0x6c, 7}, {0x6d, 7}, {0x6e, 7},
    {0x6f, 7}, {0x70, 7}, {0x71, 7}, {0x72, 7},
    {0xfc, 8}, {0x73, 7}, {0xfd, 8}, {0x1ffb, 13},
    {0x7fff0, 19}, {0x1ffc, 13}, {0x3ffc, 14}, {0x22, 6},
    {0x7ffd, 15}, {0x3, 5}, {0x23, 6}, {0x4, 5},
    {0x24, 6}, {0x5, 5}, {0x25, 6}, {0x26, 6},
    {0x27, 6}, {0x6, 5}, {0x74, 7}, {0x75, 7},
    {0x28, 6}, {0x29, 6}, {0x2a, 6}, {0x7, 5},
    {0x2b, 6}, {0x76, 7}, {0x2c, 6}, {0x8, 5},
    {0x9, 5}, {0x2d, 6}, {0x77, 7}, {0x78, 7},
    {0x79, 7}, {0x7a, 7}, {0x7b, 7}, {0x7fffe, 19},
    {0x7fc, 11}, {0x3fffd, 18}, {0x1ffd, 13}, {0xffffffc, 28},
    {0xfffe6, 20}, {0x3fffd2, 22}, {0xfffe7, 20}, {0xfffe8, 20},
    {0x3fffd3, 22}, {0x3fffd4, 22}, {0x3fffd5, 22}, {0x7fffd9, 23},
    {0x3fffd6, 22}, {0x7fffda, 23}, {0x7fffdb, 23}, {0x7fffdc, 23},
    {0x7fffdd, 23}, {0x7fffde, 23}, {0xffffeb, 24}, {0x7fffdf, 23},
    {0xffffec, 24}, {0xffffed, 24}, {0x3fffd7, 22}, {0x7fffe0, 23},
    {0xffffee, 24}, {0x7fffe1, 23}, {0x7fffe2, 23}, {0x7fffe3, 23},
    {0x7fffe4, 23}, {0x1fffdc, 21}, {0x3fffd8, 22}, {0x7fffe5, 23},
    {0x3fffd9, 22}, {0x7fffe6, 23}, {0x7fffe7, 23}, {0xffffef, 24},
    {0x3fffda, 22}, {0x1fffdd, 21}, {0xfffe9, 20}, {0x3fffdb, 22},
    {0x3fffdc, 22}, {0x7fffe8, 23}, {0x7fffe9, 23}, {0x1fffde, 21},
    {0x7fffea, 23}, {0x3fffdd, 22}, {0x3fffde, 22}, {0xfffff0, 24},
    {0x1fffdf, 21}, {0x3fffdf, 22}, {0x7fffeb, 23}, {0x7fffec, 23},
    {0x1fffe0, 21}, {0x1fffe1, 21}, {0x3fffe0, 22}, {0x1fffe2, 21},
    {0x7fffed, 23}, {0x3fffe1, 22}, {0x7fffee, 23}, {0x7fffef, 23},
    {0xfffea, 20}, {0x3fffe2, 22}, {0x3fffe3, 22}, {0x3fffe4, 22},
    {0x7ffff0, 23}, {0x3fffe5, 22}, {0x3fffe6, 22}, {0x7ffff1, 23},
    {0x3ffffe0, 26}, {0x3ffffe1, 26}, {0xfffeb, 20}, {0x7fff1, 19},
    {0x3fffe7, 22}, {0x7ffff2, 23}, {0x3fffe8, 22}, {0x1ffffec, 25},
    {0x3ffffe2, 26}, {0x3ffffe3, 26}, {0x3ffffe4, 26}, {0x7ffffde, 27},
    {0x7ffffdf, 27}, {0x3ffffe5, 26}, {0xfffff1, 24}, {0x1ffffed, 25},
    {0x7fff2, 19}, {0x1fffe3, 21}, {0x3ffffe6, 26}, {0x7ffffe0, 27},
    {0x7ffffe1, 27}, {0x3ffffe7, 26}, {0x7ffffe2, 27}, {0xfffff2, 24},
    {0x1fffe4, 21}, {0x1fffe5, 21}, {0x3ffffe8, 26}, {0x3ffffe9, 26},
    {0xffffffd, 28}, {0x7ffffe3, 27}, {0x7ffffe4, 27}, {0x7ffffe5, 27},
    {0xfffec, 20}, {0xfffff3, 24}, {0xfffed, 20}, {0x1fffe6, 21},
    {0x3fffe9, 22}, {0x1fffe7, 21}, {0x1fffe8, 21}, {0x7ffff3, 23},
    {0x3fffea, 22}, {0x3fffeb, 22}, {0x1ffffee, 25}, {0x1ffffef, 25},
    {0xfffff4, 24}, {0xfffff5, 24}, {0x3ffffea, 26}, {0x7ffff4, 23},
    {0x3ffffeb, 26}, {0x7ffffe6, 27}, {0x3ffffec, 26}, {0x3ffffed, 26},
    {0x7ffffe7, 27}, {0x7ffffe8, 27}, {0x7ffffe9, 27}, {0x7ffffea, 27},
    {0x7ffffeb, 27}, {0xffffffe, 28}, {0x7ffffec, 27}, {0x7ffffed, 27},
    {0x7ffffee, 27}, {0x7ffffef, 27}, {0x7fffff0, 27}, {0x3ffffee, 26},
    {0x3fffffff, 30}  /* EOS */
};

/* Huffman encode: writes encoded bytes to dst, returns number of bytes written. */
size_t wireform_hpack_huffman_encode(
    const uint8_t *src, size_t len,
    uint8_t *dst)
{
    uint64_t bits = 0;
    int nbits = 0;
    uint8_t *out = dst;

    for (size_t i = 0; i < len; i++) {
        uint32_t code = huff_table[src[i]].code;
        uint8_t  code_bits = huff_table[src[i]].bits;

        bits = (bits << code_bits) | code;
        nbits += code_bits;

        while (nbits >= 8) {
            nbits -= 8;
            *out++ = (uint8_t)(bits >> nbits);
        }
    }

    /* Pad with EOS prefix (all 1s) */
    if (nbits > 0) {
        bits = (bits << (8 - nbits)) | ((1u << (8 - nbits)) - 1);
        *out++ = (uint8_t)(bits & 0xFF);
    }

    return (size_t)(out - dst);
}

/* Return the encoded length without actually encoding */
size_t wireform_hpack_huffman_encode_len(const uint8_t *src, size_t len)
{
    size_t total_bits = 0;
    for (size_t i = 0; i < len; i++) {
        total_bits += huff_table[src[i]].bits;
    }
    return (total_bits + 7) / 8;
}

/*
 * Build a binary trie for decoding at init time.
 * Each node has left/right children and optionally emits a symbol.
 *
 * We use a flat array approach: statically allocate enough nodes
 * for all possible paths (bounded by sum of all code lengths).
 */

#define MAX_NODES 16384

struct trie_node {
    int16_t children[2];  /* index into node array, -1 = none */
    int16_t symbol;       /* -1 = no symbol, 0-255 = symbol */
};

static struct trie_node decode_trie[MAX_NODES];
static int trie_node_count = 0;
static int trie_initialized = 0;

static int16_t alloc_node(void) {
    int16_t idx = (int16_t)trie_node_count++;
    decode_trie[idx].children[0] = -1;
    decode_trie[idx].children[1] = -1;
    decode_trie[idx].symbol = -1;
    return idx;
}

static void init_trie(void) {
    if (trie_initialized) return;

    trie_node_count = 0;
    alloc_node(); /* root = 0 */

    for (int sym = 0; sym < 256; sym++) {
        uint32_t code = huff_table[sym].code;
        uint8_t bits = huff_table[sym].bits;
        int16_t node = 0;

        for (int i = bits - 1; i >= 0; i--) {
            int bit = (code >> i) & 1;
            if (decode_trie[node].children[bit] == -1) {
                decode_trie[node].children[bit] = alloc_node();
            }
            node = decode_trie[node].children[bit];
        }
        decode_trie[node].symbol = (int16_t)sym;
    }

    trie_initialized = 1;
}

/* Trie-based Huffman decoder. Returns 0 on success, -1 on error. */
int wireform_hpack_huffman_decode(
    const uint8_t *src, size_t src_len,
    uint8_t *dst, size_t dst_cap,
    size_t *out_len)
{
    init_trie();

    size_t written = 0;
    int16_t node = 0;
    int bits_in_current = 0;

    for (size_t i = 0; i < src_len; i++) {
        uint8_t byte = src[i];
        for (int bit = 7; bit >= 0; bit--) {
            int b = (byte >> bit) & 1;
            bits_in_current++;

            int16_t next = decode_trie[node].children[b];
            if (next == -1) {
                /* Invalid encoding - no valid path */
                *out_len = written;
                return -1;
            }
            node = next;

            if (decode_trie[node].symbol >= 0) {
                if (written >= dst_cap) {
                    *out_len = written;
                    return -1;
                }
                dst[written++] = (uint8_t)decode_trie[node].symbol;
                node = 0;
                bits_in_current = 0;
            }
        }
    }

    /* Remaining bits must be at most 7 and must all be 1s (EOS padding).
     * Per RFC 7541 Section 5.2: padding not corresponding to the
     * most-significant bits of the EOS code MUST be treated as a
     * decoding error. The EOS code is all 1s, so padding must be all 1s. */
    if (bits_in_current > 7) {
        *out_len = written;
        return -1;
    }

    if (bits_in_current > 0 && src_len > 0) {
        uint8_t last_byte = src[src_len - 1];
        uint8_t mask = (uint8_t)((1u << bits_in_current) - 1);
        if ((last_byte & mask) != mask) {
            *out_len = written;
            return -1;
        }
    }

    *out_len = written;
    return 0;
}

/*
 * Fast nibble-based Huffman decoder.
 * Processes 4 bits at a time using a pre-computed state machine.
 * Each state has 16 transitions (one per nibble value).
 *
 * Transition entry:
 *   state: next state index
 *   flags: HUFF_SYM (emit symbol), HUFF_ACCEPTED (valid end state), HUFF_FAIL (error)
 *   sym:   symbol to emit (if HUFF_SYM is set)
 */

#define HUFF_SYM      1
#define HUFF_ACCEPTED 2
#define HUFF_FAIL     4

struct nibble_entry {
    uint8_t state;
    uint8_t flags;
    uint8_t sym;
};

/*
 * Build the nibble decode table from the trie at init time.
 * We generate states by walking the trie 4 bits at a time.
 * Each "nibble state" corresponds to a position in the trie.
 */

#define MAX_NIBBLE_STATES 256
static struct nibble_entry nibble_table[MAX_NIBBLE_STATES][16];
static int nibble_state_count = 0;
static int nibble_initialized = 0;

/* Map trie node -> nibble state. -1 = not assigned. */
static int16_t trie_to_nibble[MAX_NODES];

static int16_t get_or_create_nibble_state(int16_t trie_node) {
    if (trie_to_nibble[trie_node] >= 0)
        return trie_to_nibble[trie_node];
    int16_t ns = (int16_t)nibble_state_count++;
    trie_to_nibble[trie_node] = ns;
    return ns;
}

/*
 * Walk the trie from 'start_node' consuming 'nbits' bits from 'nibble'.
 * Returns the final trie node, and sets *emitted / *sym if a symbol was found.
 * Sets *failed if an invalid path was hit.
 */
static int16_t walk_trie(int16_t start, uint8_t nibble, int nbits,
                         int *emitted, uint8_t *sym, int *failed,
                         int *is_accepted) {
    int16_t node = start;
    *emitted = 0;
    *failed = 0;
    *is_accepted = 0;

    for (int i = nbits - 1; i >= 0; i--) {
        int bit = (nibble >> i) & 1;
        int16_t next = decode_trie[node].children[bit];
        if (next == -1) {
            *failed = 1;
            return node;
        }
        node = next;
        if (decode_trie[node].symbol >= 0) {
            *sym = (uint8_t)decode_trie[node].symbol;
            *emitted = 1;
            /* After emitting, we only support one emit per nibble for simplicity.
             * For codes shorter than 4 bits, we need to continue walking. */
            /* Actually we need to handle multiple emits per nibble. */
            /* Reset to root after emit */
            node = 0;
            /* Continue with remaining bits from root */
        }
    }

    /* Check if current position is a valid end state (on EOS path) */
    /* Valid end means we're at root or on the all-1s EOS prefix path */
    if (node == 0) {
        *is_accepted = 1;
    } else {
        /* Check if the path from here following all 1s stays valid */
        int16_t test = node;
        int valid = 1;
        for (int i = 0; i < 7 && test != 0; i++) {
            int16_t next = decode_trie[test].children[1];
            if (next == -1) { valid = 0; break; }
            test = next;
            if (decode_trie[test].symbol >= 0) {
                /* Would emit EOS - that's an error in padding */
                valid = 0;
                break;
            }
        }
        *is_accepted = valid;
    }

    return node;
}

static void init_nibble_table(void) {
    if (nibble_initialized) return;
    init_trie(); /* Ensure trie is built */

    memset(trie_to_nibble, -1, sizeof(trie_to_nibble));
    nibble_state_count = 0;

    /* State 0 = trie root */
    get_or_create_nibble_state(0);

    /* BFS to build all reachable nibble states */
    for (int si = 0; si < nibble_state_count; si++) {
        /* Find the trie node for this nibble state */
        int16_t trie_node = -1;
        for (int i = 0; i < trie_node_count; i++) {
            if (trie_to_nibble[i] == si) { trie_node = i; break; }
        }
        if (trie_node < 0) continue;

        for (int nibble = 0; nibble < 16; nibble++) {
            int emitted, failed, accepted;
            uint8_t sym = 0;
            int16_t end_node = walk_trie(trie_node, (uint8_t)nibble, 4,
                                         &emitted, &sym, &failed, &accepted);

            if (failed) {
                nibble_table[si][nibble].state = 0;
                nibble_table[si][nibble].flags = HUFF_FAIL;
                nibble_table[si][nibble].sym = 0;
            } else {
                int16_t next_state = get_or_create_nibble_state(end_node);
                uint8_t flags = 0;
                if (emitted) flags |= HUFF_SYM;
                if (accepted) flags |= HUFF_ACCEPTED;
                nibble_table[si][nibble].state = (uint8_t)next_state;
                nibble_table[si][nibble].flags = flags;
                nibble_table[si][nibble].sym = sym;
            }
        }
    }

    nibble_initialized = 1;
}

/* Fast nibble-based decoder. Returns 0 on success, -1 on error. */
int wireform_hpack_huffman_decode_fast(
    const uint8_t *src, size_t src_len,
    uint8_t *dst, size_t dst_cap,
    size_t *out_len)
{
    init_nibble_table();

    uint8_t state = 0;
    size_t written = 0;

    for (size_t i = 0; i < src_len; i++) {
        uint8_t byte = src[i];

        /* High nibble */
        uint8_t hi = (byte >> 4) & 0x0F;
        struct nibble_entry *e = &nibble_table[state][hi];
        if (e->flags & HUFF_FAIL) { *out_len = written; return -1; }
        if (e->flags & HUFF_SYM) {
            if (written >= dst_cap) { *out_len = written; return -1; }
            dst[written++] = e->sym;
        }
        state = e->state;

        /* Low nibble */
        uint8_t lo = byte & 0x0F;
        e = &nibble_table[state][lo];
        if (e->flags & HUFF_FAIL) { *out_len = written; return -1; }
        if (e->flags & HUFF_SYM) {
            if (written >= dst_cap) { *out_len = written; return -1; }
            dst[written++] = e->sym;
        }
        state = e->state;
    }

    /* Check we're in an accepted end state */
    if (!(nibble_table[state][0].flags & HUFF_ACCEPTED) && state != 0) {
        /* Verify current state is a valid padding state */
        /* For the nibble decoder, state 0 is always accepted.
         * Other states are accepted if the remaining bits are valid EOS padding. */
        *out_len = written;
        return -1;
    }

    *out_len = written;
    return 0;
}
