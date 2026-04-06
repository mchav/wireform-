/*
 * C decode VM: full protobuf decode loop in C.
 *
 * Inspired by hyperpb's vm/run.go loop. The entire tag-dispatch,
 * varint decode, field skip, and value extraction runs in C with
 * zero Haskell heap interaction. Results are written to a flat
 * output struct.
 *
 * This eliminates:
 * - ForeignPtr touch# calls per byte access
 * - Unboxed sum case splits per field
 * - Haskell function call overhead per field
 * - GC write barrier overhead
 */

#include <stdint.h>
#include <string.h>

/* --------------------------------------------------------
 * Internal varint decoder (branchless for 1-3 bytes)
 * -------------------------------------------------------- */
static inline int decode_varint(
    const uint8_t *buf, int len, int pos,
    uint64_t *out)
{
    if (pos >= len) return -1;
    uint8_t b0 = buf[pos];
    if (b0 < 0x80) { *out = b0; return 1; }

    if (pos + 1 >= len) return -1;
    uint8_t b1 = buf[pos + 1];
    if (b1 < 0x80) {
        *out = (uint64_t)(b0 & 0x7F) | ((uint64_t)b1 << 7);
        return 2;
    }

    if (pos + 2 >= len) return -1;
    uint8_t b2 = buf[pos + 2];
    if (b2 < 0x80) {
        *out = (uint64_t)(b0 & 0x7F) | ((uint64_t)(b1 & 0x7F) << 7)
             | ((uint64_t)b2 << 14);
        return 3;
    }

    if (pos + 3 >= len) return -1;
    uint8_t b3 = buf[pos + 3];
    if (b3 < 0x80) {
        *out = (uint64_t)(b0 & 0x7F) | ((uint64_t)(b1 & 0x7F) << 7)
             | ((uint64_t)(b2 & 0x7F) << 14) | ((uint64_t)b3 << 21);
        return 4;
    }

    if (pos + 4 >= len) return -1;
    uint8_t b4 = buf[pos + 4];
    if (b4 < 0x80) {
        *out = (uint64_t)(b0 & 0x7F) | ((uint64_t)(b1 & 0x7F) << 7)
             | ((uint64_t)(b2 & 0x7F) << 14) | ((uint64_t)(b3 & 0x7F) << 21)
             | ((uint64_t)b4 << 28);
        return 5;
    }

    /* Slow path for 6-10 byte varints */
    uint64_t result = (uint64_t)(b0 & 0x7F) | ((uint64_t)(b1 & 0x7F) << 7)
                    | ((uint64_t)(b2 & 0x7F) << 14) | ((uint64_t)(b3 & 0x7F) << 21)
                    | ((uint64_t)(b4 & 0x7F) << 28);
    int shift = 35;
    int p = pos + 5;
    while (p < len && shift < 64) {
        uint8_t b = buf[p];
        result |= ((uint64_t)(b & 0x7F)) << shift;
        p++;
        if (b < 0x80) { *out = result; return p - pos; }
        shift += 7;
    }
    return -1;
}

/* Skip a field value based on wire type. Returns new offset or -1. */
static inline int skip_field(const uint8_t *buf, int len, int pos, int wire_type)
{
    switch (wire_type) {
    case 0: { /* varint */
        uint64_t dummy;
        int n = decode_varint(buf, len, pos, &dummy);
        return n < 0 ? -1 : pos + n;
    }
    case 1: return (pos + 8 <= len) ? pos + 8 : -1;
    case 2: { /* length-delimited */
        uint64_t flen;
        int n = decode_varint(buf, len, pos, &flen);
        if (n < 0) return -1;
        int end = pos + n + (int)flen;
        return (end <= len) ? end : -1;
    }
    case 5: return (pos + 4 <= len) ? pos + 4 : -1;
    default: return -1;
    }
}

/* --------------------------------------------------------
 * Generic table-driven decode VM
 * --------------------------------------------------------
 *
 * Field descriptor for the VM:
 */
#define FIELD_TYPE_VARINT   0
#define FIELD_TYPE_FIXED32  1
#define FIELD_TYPE_FIXED64  2
#define FIELD_TYPE_BYTES    3  /* offset, length pair */
#define FIELD_TYPE_BOOL     4

struct field_desc {
    int field_number;
    int field_type;
    int offset;     /* byte offset in output struct for the value */
    int len_offset; /* byte offset for length (BYTES type only) */
};

/*
 * Run the generic decode VM.
 *
 * buf/len: input protobuf bytes
 * fields/nfields: field descriptor table
 * out: pointer to output struct (caller-allocated)
 *
 * Returns 0 on success, -1 on error.
 */
int hs_proto_vm_decode(
    const uint8_t *buf, int len,
    const struct field_desc *fields, int nfields,
    void *out)
{
    int pos = 0;

    while (pos < len) {
        uint64_t tag;
        int tn = decode_varint(buf, len, pos, &tag);
        if (tn < 0) return -1;
        pos += tn;

        int fn = (int)(tag >> 3);
        int wt = (int)(tag & 7);

        /* Linear scan for field (fast for small nfields, could use jump table) */
        int found = 0;
        for (int i = 0; i < nfields; i++) {
            if (fields[i].field_number == fn) {
                found = 1;
                switch (fields[i].field_type) {
                case FIELD_TYPE_VARINT: {
                    uint64_t val;
                    int vn = decode_varint(buf, len, pos, &val);
                    if (vn < 0) return -1;
                    pos += vn;
                    *(uint64_t *)((char *)out + fields[i].offset) = val;
                    break;
                }
                case FIELD_TYPE_BOOL: {
                    uint64_t val;
                    int vn = decode_varint(buf, len, pos, &val);
                    if (vn < 0) return -1;
                    pos += vn;
                    *(uint8_t *)((char *)out + fields[i].offset) = (val != 0) ? 1 : 0;
                    break;
                }
                case FIELD_TYPE_FIXED32: {
                    if (pos + 4 > len) return -1;
                    uint32_t val;
                    memcpy(&val, buf + pos, 4);
                    pos += 4;
                    *(uint32_t *)((char *)out + fields[i].offset) = val;
                    break;
                }
                case FIELD_TYPE_FIXED64: {
                    if (pos + 8 > len) return -1;
                    uint64_t val;
                    memcpy(&val, buf + pos, 8);
                    pos += 8;
                    *(uint64_t *)((char *)out + fields[i].offset) = val;
                    break;
                }
                case FIELD_TYPE_BYTES: {
                    uint64_t blen;
                    int bn = decode_varint(buf, len, pos, &blen);
                    if (bn < 0) return -1;
                    pos += bn;
                    if (pos + (int)blen > len) return -1;
                    /* Store offset + length into the output struct */
                    *(int *)((char *)out + fields[i].offset) = pos;
                    *(int *)((char *)out + fields[i].len_offset) = (int)blen;
                    pos += (int)blen;
                    break;
                }
                }
                break;
            }
        }
        if (!found) {
            pos = skip_field(buf, len, pos, wt);
            if (pos < 0) return -1;
        }
    }

    return (pos == len) ? 0 : -1;
}

/*
 * Specialized HSmall decoder: fields are id(1,varint), name(2,bytes), active(3,bool).
 *
 * Output layout:
 *   0-7:   int64  id
 *   8-11:  int32  name_offset (in buf)
 *   12-15: int32  name_length
 *   16:    uint8  active
 *
 * Returns 0 on success, -1 on decode error.
 */
int hs_proto_decode_small(
    const uint8_t *buf, int len,
    int64_t *out_id,
    int *out_name_off, int *out_name_len,
    uint8_t *out_active)
{
    int pos = 0;
    *out_id = 0;
    *out_name_off = 0;
    *out_name_len = 0;
    *out_active = 0;

    while (pos < len) {
        /* Decode tag - fast path for single-byte tags */
        uint8_t b = buf[pos];
        int fn, wt;
        if (b < 0x80) {
            fn = b >> 3;
            wt = b & 7;
            pos++;
        } else {
            uint64_t tag;
            int tn = decode_varint(buf, len, pos, &tag);
            if (tn < 0) return -1;
            pos += tn;
            fn = (int)(tag >> 3);
            wt = (int)(tag & 7);
        }

        switch (fn) {
        case 1: { /* id: varint */
            uint64_t val;
            int vn = decode_varint(buf, len, pos, &val);
            if (vn < 0) return -1;
            pos += vn;
            *out_id = (int64_t)val;
            break;
        }
        case 2: { /* name: length-delimited */
            uint64_t slen;
            int sn = decode_varint(buf, len, pos, &slen);
            if (sn < 0) return -1;
            pos += sn;
            if (pos + (int)slen > len) return -1;
            *out_name_off = pos;
            *out_name_len = (int)slen;
            pos += (int)slen;
            break;
        }
        case 3: { /* active: varint/bool */
            uint64_t val;
            int vn = decode_varint(buf, len, pos, &val);
            if (vn < 0) return -1;
            pos += vn;
            *out_active = (val != 0) ? 1 : 0;
            break;
        }
        default:
            pos = skip_field(buf, len, pos, wt);
            if (pos < 0) return -1;
            break;
        }
    }

    return (pos == len) ? 0 : -1;
}

/*
 * Specialized HMedium decoder.
 * Fields: title(1,bytes) count(2,varint) score(3,fixed64) payload(4,bytes)
 *         enabled(5,bool) timestamp(6,varint) description(7,bytes) ratio(8,fixed32)
 */
int hs_proto_decode_medium(
    const uint8_t *buf, int len,
    int *out_title_off, int *out_title_len,
    int32_t *out_count,
    double *out_score,
    int *out_payload_off, int *out_payload_len,
    uint8_t *out_enabled,
    int64_t *out_timestamp,
    int *out_desc_off, int *out_desc_len,
    float *out_ratio)
{
    int pos = 0;
    *out_title_off = 0; *out_title_len = 0;
    *out_count = 0; *out_score = 0.0;
    *out_payload_off = 0; *out_payload_len = 0;
    *out_enabled = 0; *out_timestamp = 0;
    *out_desc_off = 0; *out_desc_len = 0;
    *out_ratio = 0.0f;

    while (pos < len) {
        uint8_t b = buf[pos];
        int fn, wt;
        if (b < 0x80) { fn = b >> 3; wt = b & 7; pos++; }
        else {
            uint64_t tag;
            int tn = decode_varint(buf, len, pos, &tag);
            if (tn < 0) return -1;
            pos += tn; fn = (int)(tag >> 3); wt = (int)(tag & 7);
        }

        switch (fn) {
        case 1: { /* title */
            uint64_t slen; int sn = decode_varint(buf, len, pos, &slen);
            if (sn < 0) return -1; pos += sn;
            if (pos + (int)slen > len) return -1;
            *out_title_off = pos; *out_title_len = (int)slen; pos += (int)slen;
            break;
        }
        case 2: { /* count */
            uint64_t val; int vn = decode_varint(buf, len, pos, &val);
            if (vn < 0) return -1; pos += vn;
            *out_count = (int32_t)val;
            break;
        }
        case 3: { /* score: fixed64/double */
            if (pos + 8 > len) return -1;
            memcpy(out_score, buf + pos, 8); pos += 8;
            break;
        }
        case 4: { /* payload */
            uint64_t slen; int sn = decode_varint(buf, len, pos, &slen);
            if (sn < 0) return -1; pos += sn;
            if (pos + (int)slen > len) return -1;
            *out_payload_off = pos; *out_payload_len = (int)slen; pos += (int)slen;
            break;
        }
        case 5: { /* enabled */
            uint64_t val; int vn = decode_varint(buf, len, pos, &val);
            if (vn < 0) return -1; pos += vn;
            *out_enabled = (val != 0) ? 1 : 0;
            break;
        }
        case 6: { /* timestamp */
            uint64_t val; int vn = decode_varint(buf, len, pos, &val);
            if (vn < 0) return -1; pos += vn;
            *out_timestamp = (int64_t)val;
            break;
        }
        case 7: { /* description */
            uint64_t slen; int sn = decode_varint(buf, len, pos, &slen);
            if (sn < 0) return -1; pos += sn;
            if (pos + (int)slen > len) return -1;
            *out_desc_off = pos; *out_desc_len = (int)slen; pos += (int)slen;
            break;
        }
        case 8: { /* ratio: fixed32/float */
            if (pos + 4 > len) return -1;
            memcpy(out_ratio, buf + pos, 4); pos += 4;
            break;
        }
        default:
            pos = skip_field(buf, len, pos, wt);
            if (pos < 0) return -1;
            break;
        }
    }
    return (pos == len) ? 0 : -1;
}
