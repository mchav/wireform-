#define _GNU_SOURCE
#include <sys/mman.h>
#include <sys/syscall.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include <stdint.h>
#include <stdlib.h>
#include "magic_ring.h"

static long cached_page_size = 0;

long hs_page_size(void) {
    if (cached_page_size == 0) {
        cached_page_size = sysconf(_SC_PAGESIZE);
    }
    return cached_page_size;
}

/* Round up to the next power of two >= val, with val >= page_size. */
static size_t next_pow2(size_t val) {
    val--;
    val |= val >> 1;
    val |= val >> 2;
    val |= val >> 4;
    val |= val >> 8;
    val |= val >> 16;
    val |= val >> 32;
    val++;
    return val;
}

/*
 * Allocate a double-mapped ring of at least `requested` bytes.
 * Returns 0 on success, -1 on failure (sets errno).
 * On success, out->base and out->size are populated.
 */
int hs_ring_create(size_t requested, struct hs_ring *out) {
    long ps = hs_page_size();
    size_t size;
    int fd;
    void *base, *m1, *m2;

    if (requested < (size_t)ps)
        requested = (size_t)ps;

    /* Round up to page-size multiple, then to power of two */
    size = (requested + (size_t)ps - 1) & ~((size_t)ps - 1);
    size = next_pow2(size);

    fd = (int)syscall(SYS_memfd_create, "wireform_ring", 1 /* MFD_CLOEXEC */);
    if (fd < 0)
        return -1;

    if (ftruncate(fd, (off_t)size) != 0) {
        close(fd);
        return -1;
    }

    /* Reserve 2N contiguous virtual addresses with no permissions */
    base = mmap(NULL, size * 2, PROT_NONE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (base == MAP_FAILED) {
        close(fd);
        return -1;
    }

    /* Map the first copy */
    m1 = mmap(base, size, PROT_READ | PROT_WRITE,
              MAP_SHARED | MAP_FIXED, fd, 0);
    if (m1 == MAP_FAILED) {
        munmap(base, size * 2);
        close(fd);
        return -1;
    }

    /* Map the second copy immediately after */
    m2 = mmap((char *)base + size, size, PROT_READ | PROT_WRITE,
              MAP_SHARED | MAP_FIXED, fd, 0);
    if (m2 == MAP_FAILED) {
        munmap(base, size * 2);
        close(fd);
        return -1;
    }

    close(fd); /* mappings keep the memfd alive */

    /* Pre-fault every page to avoid latency surprises on first touch */
    memset(base, 0, size);

    out->base = base;
    out->size = size;
    return 0;
}

void hs_ring_destroy(struct hs_ring *ring) {
    if (ring && ring->base) {
        munmap(ring->base, ring->size * 2);
        ring->base = NULL;
        ring->size = 0;
    }
}
