#ifndef _HEX_ESCAPE_H
#define _HEX_ESCAPE_H

typedef enum hex_status {
    HEX_SUCCESS = 0,
    HEX_SYNTAX_ERROR,
    HEX_OUT_OF_MEMORY
} hex_status_t;

/*
 * The API for hex_encode() and hex_decode() is modelled on that for
 * getline.
 *
 * If 'out' points to a NULL pointer a char array of the appropriate
 * size is allocated using talloc, and out_size is updated.
 *
 * If 'out' points to a non-NULL pointer, it assumed to describe an
 * existing char array, with the size given in *out_size.  This array
 * may be resized by talloc_realloc if needed; in this case *out_size
 * will also be updated.
 *
 * Note that it is an error to pass a NULL pointer for any parameter
 * of these routines.
 */

hex_status_t
hex_encode (void *talloc_ctx, const char *in, char **out,
            size_t *out_size);

hex_status_t
hex_decode (void *talloc_ctx, const char *in, char **out,
            size_t *out_size);

/*
 * Non-allocating hex decode to decode 's' in-place. The length of the
 * result is always equal to or shorter than the length of the
 * original.
 */
hex_status_t
hex_decode_inplace (char *s);
#endif
