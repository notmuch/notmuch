/*
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

#define _GNU_SOURCE

#include "path-util.h"

#include <limits.h>
#include <stdlib.h>


char *
notmuch_canonicalize_file_name (const char *path)
{
#if HAVE_CANONICALIZE_FILE_NAME
    return canonicalize_file_name (path);
#elif defined(PATH_MAX)
    char *resolved_path =  malloc (PATH_MAX + 1);
    if (resolved_path == NULL)
	return NULL;

    return realpath (path, resolved_path);
#else
#error undefined PATH_MAX _and_ missing canonicalize_file_name not supported
#endif
}
