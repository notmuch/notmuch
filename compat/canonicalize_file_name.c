#include "compat.h"
#include <limits.h>
#undef _GNU_SOURCE
#include <stdlib.h>

char *
canonicalize_file_name (const char * path)
{
#ifdef PATH_MAX
    char *resolved_path =  malloc (PATH_MAX+1);
    if (resolved_path == NULL)
	return NULL;

    return realpath (path, resolved_path);
#else
#error undefined PATH_MAX _and_ missing canonicalize_file_name not supported
#endif
}
