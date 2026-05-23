/*
 * snappy_ffi.c - FFI bindings for Snappy compression
 *
 * Provides a simple C interface to the Snappy compression library
 * for use from Haskell.
 */

#include <snappy-c.h>
#include <stdlib.h>
#include <string.h>

/*
 * Compress data using Snappy.
 * 
 * Returns the compressed size, or 0 on error.
 * The caller must free the output buffer.
 */
size_t snappy_compress_wrapper(const char* input, size_t input_length,
                                char** output, size_t* output_length) {
    if (!input || !output || !output_length) {
        return 0;
    }
    
    // Get the maximum compressed length
    size_t max_compressed_length = snappy_max_compressed_length(input_length);

    // Allocate output buffer
    *output = (char*)malloc(max_compressed_length);
    if (!*output) {
        return 0;
    }

    // snappy_compress expects *output_length to be initialised to the
    // capacity of the output buffer; on success it overwrites it with
    // the actual compressed length. Without this initialisation snappy
    // sees output_length = 0 and refuses to write anything (returns
    // SNAPPY_BUFFER_TOO_SMALL).
    *output_length = max_compressed_length;

    // Compress the data
    snappy_status status = snappy_compress(input, input_length,
                                           *output, output_length);
    
    if (status != SNAPPY_OK) {
        free(*output);
        *output = NULL;
        return 0;
    }
    
    return *output_length;
}

/*
 * Decompress Snappy-compressed data.
 * 
 * Returns the decompressed size, or 0 on error.
 * The caller must free the output buffer.
 */
size_t snappy_decompress_wrapper(const char* input, size_t input_length,
                                  char** output, size_t* output_length) {
    if (!input || !output || !output_length) {
        return 0;
    }
    
    // Get the uncompressed length
    snappy_status status = snappy_uncompressed_length(input, input_length,
                                                      output_length);
    if (status != SNAPPY_OK) {
        return 0;
    }
    
    // Allocate output buffer
    *output = (char*)malloc(*output_length);
    if (!*output) {
        return 0;
    }
    
    // Decompress the data
    status = snappy_uncompress(input, input_length, *output, output_length);
    
    if (status != SNAPPY_OK) {
        free(*output);
        *output = NULL;
        return 0;
    }
    
    return *output_length;
}

/*
 * Sized-output decompress: caller pre-allocates the destination
 * buffer (e.g. a pinned Haskell ForeignPtr sized to the result of
 * snappy_get_uncompressed_length_wrapper) and snappy writes
 * directly into it.  No intermediate malloc, no copy out, no free.
 *
 * Returns 1 on success, 0 on snappy error.
 */
int snappy_decompress_into(const char* input, size_t input_length,
                            char* dst, size_t dst_capacity) {
    if (!input || !dst) {
        return 0;
    }
    size_t out = dst_capacity;
    snappy_status status = snappy_uncompress(input, input_length, dst, &out);
    return status == SNAPPY_OK ? 1 : 0;
}

/*
 * Read the uncompressed length of a snappy block from its header,
 * without decompressing.  Used by the Haskell side to size the
 * destination ForeignPtr for snappy_decompress_into.
 *
 * Returns the uncompressed length on success, or (size_t)-1 on
 * malformed input.
 */
size_t snappy_get_uncompressed_length_wrapper(const char* input,
                                               size_t input_length) {
    if (!input) {
        return (size_t)-1;
    }
    size_t out;
    snappy_status status = snappy_uncompressed_length(input, input_length,
                                                       &out);
    return status == SNAPPY_OK ? out : (size_t)-1;
}

