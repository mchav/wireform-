#ifdef _WIN32

#include <windows.h>
#include <memoryapi.h>
#include <stdint.h>
#include "magic_ring.h"

static long cached_page_size = 0;

long hs_page_size(void) {
    if (cached_page_size == 0) {
        SYSTEM_INFO si;
        GetSystemInfo(&si);
        cached_page_size = (long)si.dwAllocationGranularity;
    }
    return cached_page_size;
}

static size_t next_pow2(size_t val) {
    val--;
    val |= val >> 1;
    val |= val >> 2;
    val |= val >> 4;
    val |= val >> 8;
    val |= val >> 16;
#if defined(_WIN64)
    val |= val >> 32;
#endif
    val++;
    return val;
}

int hs_ring_create(size_t requested, struct hs_ring *out) {
    long ps = hs_page_size();
    size_t size;
    PVOID placeholder = NULL;
    HANDLE section = NULL;
    PVOID view1 = NULL, view2 = NULL;

    if (requested < (size_t)ps)
        requested = (size_t)ps;

    size = (requested + (size_t)ps - 1) & ~((size_t)ps - 1);
    size = next_pow2(size);

    placeholder = VirtualAlloc2(NULL, NULL, size * 2,
        MEM_RESERVE | MEM_RESERVE_PLACEHOLDER, PAGE_NOACCESS, NULL, 0);
    if (!placeholder)
        return -1;

    /* Split the placeholder into two halves */
    if (!VirtualFree(placeholder, size, MEM_RELEASE | MEM_PRESERVE_PLACEHOLDER)) {
        VirtualFree(placeholder, 0, MEM_RELEASE);
        return -1;
    }

    section = CreateFileMappingW(INVALID_HANDLE_VALUE, NULL, PAGE_READWRITE,
        (DWORD)(size >> 32), (DWORD)(size & 0xFFFFFFFF), NULL);
    if (!section) {
        VirtualFree(placeholder, 0, MEM_RELEASE);
        VirtualFree((char *)placeholder + size, 0, MEM_RELEASE);
        return -1;
    }

    view1 = MapViewOfFile3(section, NULL, placeholder, 0, size,
        MEM_REPLACE_PLACEHOLDER, PAGE_READWRITE, NULL, 0);
    if (!view1)
        goto fail;

    view2 = MapViewOfFile3(section, NULL, (char *)placeholder + size, 0, size,
        MEM_REPLACE_PLACEHOLDER, PAGE_READWRITE, NULL, 0);
    if (!view2) {
        UnmapViewOfFile(view1);
        goto fail;
    }

    /* Pre-fault */
    memset(view1, 0, size);

    out->base = view1;
    out->size = size;
    out->section = section;
    return 0;

fail:
    CloseHandle(section);
    VirtualFree(placeholder, 0, MEM_RELEASE);
    VirtualFree((char *)placeholder + size, 0, MEM_RELEASE);
    return -1;
}

void hs_ring_destroy(struct hs_ring *ring) {
    if (ring && ring->base) {
        UnmapViewOfFile(ring->base);
        UnmapViewOfFile((char *)ring->base + ring->size);
        CloseHandle(ring->section);
        ring->base = NULL;
        ring->size = 0;
        ring->section = NULL;
    }
}

#endif /* _WIN32 */
