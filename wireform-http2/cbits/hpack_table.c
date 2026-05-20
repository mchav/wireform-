#include <stdint.h>
#include <stddef.h>
#include <string.h>

/*
 * HPACK static table: 61 entries.
 * We use a simple hash-to-bucket approach with open addressing.
 * Since entries are fixed, we can build a perfect hash at compile time.
 *
 * For name-only lookup: hash the name, probe the table.
 * For name+value lookup: hash both, probe.
 */

struct static_entry {
    const char *name;
    uint16_t name_len;
    const char *value;
    uint16_t value_len;
    uint8_t index;  /* 1-based HPACK index */
};

/* The 61 static table entries */
static const struct static_entry static_entries[61] = {
    {":authority", 10, "", 0, 1},
    {":method", 7, "GET", 3, 2},
    {":method", 7, "POST", 4, 3},
    {":path", 5, "/", 1, 4},
    {":path", 5, "/index.html", 11, 5},
    {":scheme", 7, "http", 4, 6},
    {":scheme", 7, "https", 5, 7},
    {":status", 7, "200", 3, 8},
    {":status", 7, "204", 3, 9},
    {":status", 7, "206", 3, 10},
    {":status", 7, "304", 3, 11},
    {":status", 7, "400", 3, 12},
    {":status", 7, "404", 3, 13},
    {":status", 7, "500", 3, 14},
    {"accept-charset", 14, "", 0, 15},
    {"accept-encoding", 15, "gzip, deflate", 13, 16},
    {"accept-language", 15, "", 0, 17},
    {"accept-ranges", 13, "", 0, 18},
    {"accept", 6, "", 0, 19},
    {"access-control-allow-origin", 27, "", 0, 20},
    {"age", 3, "", 0, 21},
    {"allow", 5, "", 0, 22},
    {"authorization", 13, "", 0, 23},
    {"cache-control", 13, "", 0, 24},
    {"content-disposition", 19, "", 0, 25},
    {"content-encoding", 16, "", 0, 26},
    {"content-language", 16, "", 0, 27},
    {"content-length", 14, "", 0, 28},
    {"content-location", 16, "", 0, 29},
    {"content-range", 13, "", 0, 30},
    {"content-type", 12, "", 0, 31},
    {"cookie", 6, "", 0, 32},
    {"date", 4, "", 0, 33},
    {"etag", 4, "", 0, 34},
    {"expect", 6, "", 0, 35},
    {"expires", 7, "", 0, 36},
    {"from", 4, "", 0, 37},
    {"host", 4, "", 0, 38},
    {"if-match", 8, "", 0, 39},
    {"if-modified-since", 17, "", 0, 40},
    {"if-none-match", 13, "", 0, 41},
    {"if-range", 8, "", 0, 42},
    {"if-unmodified-since", 19, "", 0, 43},
    {"last-modified", 13, "", 0, 44},
    {"link", 4, "", 0, 45},
    {"location", 8, "", 0, 46},
    {"max-forwards", 12, "", 0, 47},
    {"proxy-authenticate", 18, "", 0, 48},
    {"proxy-authorization", 19, "", 0, 49},
    {"range", 5, "", 0, 50},
    {"referer", 7, "", 0, 51},
    {"refresh", 7, "", 0, 52},
    {"retry-after", 11, "", 0, 53},
    {"server", 6, "", 0, 54},
    {"set-cookie", 10, "", 0, 55},
    {"strict-transport-security", 25, "", 0, 56},
    {"transfer-encoding", 17, "", 0, 57},
    {"user-agent", 10, "", 0, 58},
    {"vary", 4, "", 0, 59},
    {"via", 3, "", 0, 60},
    {"www-authenticate", 16, "", 0, 61},
};

/* Simple FNV-1a hash for ByteStrings */
static inline uint32_t fnv1a(const uint8_t *data, size_t len) {
    uint32_t hash = 2166136261u;
    for (size_t i = 0; i < len; i++) {
        hash ^= data[i];
        hash *= 16777619u;
    }
    return hash;
}

/* Hash table for name-only lookup. 128 buckets (power of 2 > 61*2). */
#define NAME_BUCKETS 128
static int8_t name_ht[NAME_BUCKETS]; /* index into static_entries, -1 = empty */
static int name_ht_initialized = 0;

static void init_name_ht(void) {
    if (name_ht_initialized) return;
    memset(name_ht, -1, sizeof(name_ht));
    for (int i = 0; i < 61; i++) {
        uint32_t h = fnv1a((const uint8_t*)static_entries[i].name,
                           static_entries[i].name_len) & (NAME_BUCKETS - 1);
        /* Linear probe — guaranteed to find a slot since load < 50% */
        while (name_ht[h] != -1) {
            /* Skip duplicates (same name, different value) — keep first */
            int existing = name_ht[h];
            if (static_entries[existing].name_len == static_entries[i].name_len &&
                memcmp(static_entries[existing].name, static_entries[i].name,
                       static_entries[i].name_len) == 0) {
                goto next_entry;
            }
            h = (h + 1) & (NAME_BUCKETS - 1);
        }
        name_ht[h] = (int8_t)i;
        next_entry:;
    }
    name_ht_initialized = 1;
}

/* Hash table for name+value lookup. 128 buckets. */
static int8_t namevalue_ht[NAME_BUCKETS];
static int namevalue_ht_initialized = 0;

static void init_namevalue_ht(void) {
    if (namevalue_ht_initialized) return;
    memset(namevalue_ht, -1, sizeof(namevalue_ht));
    for (int i = 0; i < 61; i++) {
        uint32_t h = fnv1a((const uint8_t*)static_entries[i].name,
                           static_entries[i].name_len);
        h = h * 16777619u;
        /* Mix in value hash */
        for (int j = 0; j < static_entries[i].value_len; j++) {
            h ^= (uint8_t)static_entries[i].value[j];
            h *= 16777619u;
        }
        h &= (NAME_BUCKETS - 1);
        while (namevalue_ht[h] != -1) h = (h + 1) & (NAME_BUCKETS - 1);
        namevalue_ht[h] = (int8_t)i;
    }
    namevalue_ht_initialized = 1;
}

/*
 * Lookup by name only. Returns 1-based index or 0 if not found.
 */
int wireform_hpack_static_find_name(const uint8_t *name, size_t name_len) {
    init_name_ht();
    uint32_t h = fnv1a(name, name_len) & (NAME_BUCKETS - 1);
    for (int probes = 0; probes < NAME_BUCKETS; probes++) {
        int8_t idx = name_ht[h];
        if (idx == -1) return 0;
        if (static_entries[idx].name_len == (uint16_t)name_len &&
            memcmp(static_entries[idx].name, name, name_len) == 0) {
            return static_entries[idx].index;
        }
        h = (h + 1) & (NAME_BUCKETS - 1);
    }
    return 0;
}

/*
 * Lookup by name+value. Returns 1-based index or 0 if not found.
 */
int wireform_hpack_static_find_name_value(
    const uint8_t *name, size_t name_len,
    const uint8_t *value, size_t value_len)
{
    init_namevalue_ht();
    uint32_t h = fnv1a(name, name_len);
    h = h * 16777619u;
    for (size_t j = 0; j < value_len; j++) {
        h ^= value[j];
        h *= 16777619u;
    }
    h &= (NAME_BUCKETS - 1);
    for (int probes = 0; probes < NAME_BUCKETS; probes++) {
        int8_t idx = namevalue_ht[h];
        if (idx == -1) return 0;
        if (static_entries[idx].name_len == (uint16_t)name_len &&
            static_entries[idx].value_len == (uint16_t)value_len &&
            memcmp(static_entries[idx].name, name, name_len) == 0 &&
            memcmp(static_entries[idx].value, value, value_len) == 0) {
            return static_entries[idx].index;
        }
        h = (h + 1) & (NAME_BUCKETS - 1);
    }
    return 0;
}
