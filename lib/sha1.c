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
 * along with this program.  If not, see https://www.gnu.org/licenses/ .
 *
 * Author: Carl Worth <cworth@cworth.org>
 */

#include "notmuch-private.h"

#include <glib.h>

/* Create a hexadecimal string version of the SHA-1 digest of 'str'
 * (including its null terminating character).
 *
 * This function returns a newly allocated string which the caller
 * should free() when finished.
 */
char *
_notmuch_sha1_of_string (const char *str)
{
    GChecksum *sha1;
    char *digest;

    sha1 = g_checksum_new (G_CHECKSUM_SHA1);
    g_checksum_update (sha1, (const guchar *) str, strlen (str) + 1);
    digest = xstrdup (g_checksum_get_string (sha1));
    g_checksum_free (sha1);

    return digest;
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
    GChecksum *sha1;
    char *digest = NULL;

    file = fopen (filename, "r");
    if (file == NULL)
	return NULL;

    sha1 = g_checksum_new (G_CHECKSUM_SHA1);
    if (sha1 == NULL)
	goto DONE;

    while (1) {
	bytes_read = fread (block, 1, 4096, file);
	if (bytes_read == 0) {
	    if (feof (file))
		break;
	    else if (ferror (file))
		goto DONE;
	} else {
	    g_checksum_update (sha1, block, bytes_read);
	}
    }

    digest = xstrdup (g_checksum_get_string (sha1));

  DONE:
    if (sha1)
	g_checksum_free (sha1);
    if (file)
	fclose (file);

    return digest;
}
