/* sha1.c - Interfaces to SHA-1 hash for the notmuch mail system
 *
 * Copyright © 2009 Carl Worth
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
 * along with this program.  If not, see http://www.gnu.org/licenses/ .
 *
 * Author: Carl Worth <cworth@cworth.org>
 */

#include "notmuch-private.h"

#include "libsha1.h"

/* Just some simple interfaces on top of libsha1 so that we can leave
 * libsha1 as untouched as possible. */

static char *
_hex_of_sha1_digest (const unsigned char digest[SHA1_DIGEST_SIZE])
{
    char *result, *r;
    int i;

    result = xcalloc (SHA1_DIGEST_SIZE * 2 + 1, 1);

    for (r = result, i = 0;
	 i < SHA1_DIGEST_SIZE;
	 r += 2, i++)
    {
	sprintf (r, "%02x", digest[i]);
    }

    return result;
}

/* Create a hexadecimal string version of the SHA-1 digest of 'str'
 * (including its null terminating character).
 *
 * This function returns a newly allocated string which the caller
 * should free() when finished.
 */
char *
_notmuch_sha1_of_string (const char *str)
{
    sha1_ctx sha1;
    unsigned char digest[SHA1_DIGEST_SIZE];

    sha1_begin (&sha1);

    sha1_hash ((unsigned char *) str, strlen (str) + 1, &sha1);

    sha1_end (digest, &sha1);

    return _hex_of_sha1_digest (digest);
}

/* Create a hexadecimal string version of the SHA-1 digest of the
 * contents of the named file.
 *
 * This function returns a newly allocated string which the caller
 * should free() when finished.
 *
 * If any error occurs while reading the file, (permission denied,
 * file not found, etc.), this function returns NULL.
 */
char *
_notmuch_sha1_of_file (const char *filename)
{
    FILE *file;
#define BLOCK_SIZE 4096
    unsigned char block[BLOCK_SIZE];
    size_t bytes_read;
    sha1_ctx sha1;
    unsigned char digest[SHA1_DIGEST_SIZE];
    char *result;

    file = fopen (filename, "r");
    if (file == NULL)
	return NULL;

    sha1_begin (&sha1);

    while (1) {
	bytes_read = fread (block, 1, 4096, file);
	if (bytes_read == 0) {
	    if (feof (file)) {
		break;
	    } else if (ferror (file)) {
		fclose (file);
		return NULL;
	    }
	} else {
	    sha1_hash (block, bytes_read, &sha1);
	}
    }

    sha1_end (digest, &sha1);

    result = _hex_of_sha1_digest (digest);

    fclose (file);

    return result;
}

