/* zlib-extra.c -  Extra or enhanced routines for compressed I/O.
 *
 * Copyright (c) 2014 David Bremner
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see https://www.gnu.org/licenses/ .
 *
 * Author: David Bremner <david@tethera.net>
 */

#include "zlib-extra.h"
#include <talloc.h>
#include <stdio.h>
#include <string.h>

/* mimic POSIX/glibc getline, but on a zlib gzFile stream, and using talloc */
util_status_t
gz_getline (void *talloc_ctx, char **bufptr, ssize_t *bytes_read, gzFile stream)
{
    char *buf = *bufptr;
    unsigned int len;
    size_t offset = 0;

    if (buf) {
	len = talloc_array_length (buf);
    } else {
	/* same as getdelim from gnulib */
	len = 120;
	buf = talloc_array (talloc_ctx, char, len);
	if (buf == NULL)
	    return UTIL_OUT_OF_MEMORY;
    }

    while (1) {
	if (! gzgets (stream, buf + offset, len - offset)) {
	    /* Null indicates EOF or error */
	    int zlib_status = 0;
	    (void) gzerror (stream, &zlib_status);
	    switch (zlib_status) {
	    case Z_OK:
		/* no data read before EOF */
		if (offset == 0)
		    return UTIL_EOF;
		else
		    goto SUCCESS;
	    case Z_ERRNO:
		return UTIL_ERRNO;
	    default:
		return UTIL_GZERROR;
	    }
	}

	offset += strlen (buf + offset);

	if (buf[offset - 1] == '\n')
	    goto SUCCESS;

	len *= 2;
	buf = talloc_realloc (talloc_ctx, buf, char, len);
	if (buf == NULL)
	    return UTIL_OUT_OF_MEMORY;
    }
 SUCCESS:
    *bufptr = buf;
    *bytes_read = offset;
    return UTIL_SUCCESS;
}

const char *gz_error_string (util_status_t status, gzFile file)
{
    if (status == UTIL_GZERROR)
	return gzerror (file, NULL);
    else
	return util_error_string (status);
}
