/*
 * crc32c.c — CRC-32C (Castagnoli, polynomial 0x1EDC6F41) implementation
 * for Kafka record batches.
 *
 * The implementation is a thin wrapper over SIMDe's `_mm_crc32_u8/16/32/64`
 * intrinsics. SIMDe (https://github.com/simd-everywhere/simde) emits the
 * native hardware instruction on every architecture that has one:
 *
 *   - x86_64 with SSE4.2: a single CRC32 instruction per byte/word/qword.
 *   - AArch64 with the optional CRC extension (mandatory on ARMv8.1+,
 *     present on every Apple Silicon part): the ARM CRC32C instructions.
 *
 * On any architecture without a hardware CRC32C path (or when the build
 * is not configured for the host's vector ISA), SIMDe falls back to a
 * portable C reference implementation. There is therefore no need for
 * a hand-rolled lookup-table fallback or runtime CPUID dispatch in this
 * file — SIMDe is the dispatch.
 *
 * Performance note: when shipping binaries that must run on older CPUs
 * than the build host, compile with -msse4.2 (x86) / -march=armv8.1-a
 * (ARM) or higher to ensure SIMDe emits the hardware instruction.
 */

#include <stdint.h>
#include <stddef.h>
#include "crc32c.h"

#include <simde/x86/sse4.2.h>

uint32_t crc32c_init(void) {
    return 0xFFFFFFFFu;
}

uint32_t crc32c_finalize(uint32_t crc) {
    return crc ^ 0xFFFFFFFFu;
}

uint32_t crc32c_append(uint32_t crc, const uint8_t* data, size_t length) {
    if (data == NULL || length == 0) {
        return crc;
    }

    /* Process 8 bytes at a time as long as we have an aligned-or-better
     * 64-bit chunk available. simde_mm_crc32_u64 does an unaligned load
     * internally so we don't need to align by hand. */
    while (length >= 8) {
        uint64_t v;
        /* memcpy is the standards-compliant way to do an unaligned load
         * without UB; the compiler folds it into a single mov on every
         * mainstream target. */
        __builtin_memcpy(&v, data, 8);
        crc = simde_mm_crc32_u64(crc, v);
        data   += 8;
        length -= 8;
    }

    if (length >= 4) {
        uint32_t v;
        __builtin_memcpy(&v, data, 4);
        crc = simde_mm_crc32_u32(crc, v);
        data   += 4;
        length -= 4;
    }

    if (length >= 2) {
        uint16_t v;
        __builtin_memcpy(&v, data, 2);
        crc = simde_mm_crc32_u16(crc, v);
        data   += 2;
        length -= 2;
    }

    if (length > 0) {
        crc = simde_mm_crc32_u8(crc, *data);
    }

    return crc;
}

uint32_t crc32c(const uint8_t* data, size_t length) {
    uint32_t crc = crc32c_init();
    crc = crc32c_append(crc, data, length);
    return crc32c_finalize(crc);
}
