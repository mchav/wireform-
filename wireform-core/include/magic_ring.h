#ifndef WIREFORM_MAGIC_RING_H
#define WIREFORM_MAGIC_RING_H

#include <stddef.h>

struct hs_ring {
    void   *base;
    size_t  size;
#ifdef _WIN32
    void   *section; /* HANDLE */
#endif
};

long hs_page_size(void);
int  hs_ring_create(size_t requested, struct hs_ring *out);
void hs_ring_destroy(struct hs_ring *ring);

#endif
