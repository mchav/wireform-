#include <sys/mman.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdatomic.h>
#include "magic_ring.h"

static long cached_page_size = 0;

long hs_page_size(void) {
    if (cached_page_size == 0) {
        cached_page_size = sysconf(_SC_PAGESIZE);
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
    val |= val >> 32;
    val++;
    return val;
}

#if !defined(__FreeBSD__) || !defined(SHM_ANON)
static _Atomic long shm_counter = 0;
#endif

int hs_ring_create(size_t requested, struct hs_ring *out) {
    long ps = hs_page_size();
    size_t size;
    int fd;
    void *base, *m1, *m2;
    int saved_errno;

    if (requested < (size_t)ps)
        requested = (size_t)ps;

    size = (requested + (size_t)ps - 1) & ~((size_t)ps - 1);
    size = next_pow2(size);

#if defined(__FreeBSD__) && defined(SHM_ANON)
    fd = shm_open(SHM_ANON, O_RDWR, 0600);
#else
    {
        char name[64];
        snprintf(name, sizeof(name), "/wfring.%d.%ld",
                 getpid(), atomic_fetch_add(&shm_counter, 1));
        fd = shm_open(name, O_RDWR | O_CREAT | O_EXCL, 0600);
        if (fd >= 0)
            shm_unlink(name);
    }
#endif

    if (fd < 0)
        return -1;

    if (ftruncate(fd, (off_t)size) != 0) {
        saved_errno = errno;
        close(fd);
        errno = saved_errno;
        return -1;
    }

    base = mmap(NULL, size * 2, PROT_NONE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (base == MAP_FAILED) {
        saved_errno = errno;
        close(fd);
        errno = saved_errno;
        return -1;
    }

    m1 = mmap(base, size, PROT_READ | PROT_WRITE,
              MAP_SHARED | MAP_FIXED, fd, 0);
    if (m1 == MAP_FAILED) {
        saved_errno = errno;
        munmap(base, size * 2);
        close(fd);
        errno = saved_errno;
        return -1;
    }

    m2 = mmap((char *)base + size, size, PROT_READ | PROT_WRITE,
              MAP_SHARED | MAP_FIXED, fd, 0);
    if (m2 == MAP_FAILED) {
        saved_errno = errno;
        munmap(base, size * 2);
        close(fd);
        errno = saved_errno;
        return -1;
    }

    close(fd);
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
