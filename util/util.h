#ifndef _UTIL_H
#define _UTIL_H

typedef enum util_status {
    /**
     * No error occurred.
     */
    UTIL_SUCCESS = 0,
    /**
     * Out of memory.
     */
    UTIL_OUT_OF_MEMORY,
    /**
     * End of stream reached while attempting to read.
     */
    UTIL_EOF,
    /**
     * Low level error occured, consult errno.
     */
    UTIL_ERRNO,
    /**
     * Zlib error occured, call gzerror for details.
     */
    UTIL_GZERROR
} util_status_t;

const char *
util_error_string (util_status_t status);
#endif
