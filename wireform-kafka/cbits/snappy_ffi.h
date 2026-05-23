/*
 * snappy_ffi.h - FFI bindings for Snappy compression
 */

#ifndef SNAPPY_FFI_H
#define SNAPPY_FFI_H

#include <stddef.h>

/*
 * Compress data using Snappy.
 * Returns the compressed size, or 0 on error.
 * The caller must free the output buffer.
 */
size_t snappy_compress_wrapper(const char* input, size_t input_length,
                                char** output, size_t* output_length);

/*
 * Decompress Snappy-compressed data.
 * Returns the decompressed size, or 0 on error.
 * The caller must free the output buffer.
 */
size_t snappy_decompress_wrapper(const char* input, size_t input_length,
                                  char** output, size_t* output_length);

/*
 * Sized-output decompress: caller supplies a destination buffer
 * (e.g. a pinned Haskell ByteString of the size returned by
 * snappy_get_uncompressed_length_wrapper).  No intermediate malloc.
 * Returns 1 on success, 0 on snappy error.
 */
int snappy_decompress_into(const char* input, size_t input_length,
                            char* dst, size_t dst_capacity);

/*
 * Read the uncompressed length from a snappy block header without
 * decompressing.  Returns the length on success, or (size_t)-1 on
 * malformed input.
 */
size_t snappy_get_uncompressed_length_wrapper(const char* input,
                                               size_t input_length);

#endif /* SNAPPY_FFI_H */

