#ifdef HAVE_IOURING

#include <liburing.h>
#include <sys/eventfd.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include <stdint.h>

struct hs_iouring {
    struct io_uring ring;
    int event_fd;
    int socket_fd;
    void *ring_buf;
    size_t ring_buf_size;
    size_t ring_buf_mask;
    uint64_t head;
    int closed;
};

int hs_iouring_create(int socket_fd, int queue_depth, int sqpoll_idle_ms,
                      void *ring_buf, size_t ring_buf_size,
                      struct hs_iouring *out) {
    struct io_uring_params params;
    memset(&params, 0, sizeof(params));

    if (sqpoll_idle_ms > 0) {
        params.flags = IORING_SETUP_SQPOLL;
        params.sq_thread_idle = (unsigned)sqpoll_idle_ms;
    }

    int ret = io_uring_queue_init_params((unsigned)queue_depth, &out->ring, &params);
    if (ret < 0) return ret;

    out->event_fd = eventfd(0, EFD_NONBLOCK | EFD_CLOEXEC);
    if (out->event_fd < 0) {
        io_uring_queue_exit(&out->ring);
        return -1;
    }

    ret = io_uring_register_eventfd(&out->ring, out->event_fd);
    if (ret < 0) {
        close(out->event_fd);
        io_uring_queue_exit(&out->ring);
        return ret;
    }

    out->socket_fd = socket_fd;
    out->ring_buf = ring_buf;
    out->ring_buf_size = ring_buf_size;
    out->ring_buf_mask = ring_buf_size - 1;
    out->head = 0;
    out->closed = 0;

    return 0;
}

/* Submit a recv SQE into the ring at the current head position. */
int hs_iouring_submit_recv(struct hs_iouring *uring, size_t max_bytes) {
    struct io_uring_sqe *sqe = io_uring_get_sqe(&uring->ring);
    if (!sqe) return -1;

    size_t write_off = uring->head & uring->ring_buf_mask;
    size_t available = max_bytes;
    if (available > uring->ring_buf_size - write_off)
        available = uring->ring_buf_size - write_off;

    io_uring_prep_recv(sqe, uring->socket_fd,
                       (char *)uring->ring_buf + write_off,
                       available, 0);
    sqe->user_data = uring->head;

    return io_uring_submit(&uring->ring);
}

/* Wait for a completion and process it.
 * Returns: >0 = bytes received (head advanced), 0 = EOF, <0 = error */
int hs_iouring_wait_cqe(struct hs_iouring *uring, uint64_t *new_head) {
    struct io_uring_cqe *cqe;
    int ret = io_uring_wait_cqe(&uring->ring, &cqe);
    if (ret < 0) return ret;

    int res = cqe->res;
    io_uring_cqe_seen(&uring->ring, cqe);

    if (res > 0) {
        uring->head += (uint64_t)res;
        *new_head = uring->head;
        return res;
    } else if (res == 0) {
        uring->closed = 1;
        *new_head = uring->head;
        return 0;
    } else {
        return res;
    }
}

/* Non-blocking check for completions. */
int hs_iouring_peek_cqe(struct hs_iouring *uring, uint64_t *new_head) {
    struct io_uring_cqe *cqe;
    int ret = io_uring_peek_cqe(&uring->ring, &cqe);
    if (ret < 0) return ret;

    int res = cqe->res;
    io_uring_cqe_seen(&uring->ring, cqe);

    if (res > 0) {
        uring->head += (uint64_t)res;
        *new_head = uring->head;
        return res;
    } else if (res == 0) {
        uring->closed = 1;
        *new_head = uring->head;
        return 0;
    } else {
        return res;
    }
}

int hs_iouring_get_eventfd(struct hs_iouring *uring) {
    return uring->event_fd;
}

uint64_t hs_iouring_get_head(struct hs_iouring *uring) {
    return uring->head;
}

void hs_iouring_destroy(struct hs_iouring *uring) {
    if (uring->event_fd >= 0) {
        close(uring->event_fd);
        uring->event_fd = -1;
    }
    io_uring_queue_exit(&uring->ring);
}

#endif /* HAVE_IOURING */
