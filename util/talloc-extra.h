#ifndef _TALLOC_EXTRA_H
#define _TALLOC_EXTRA_H

#include <talloc.h>

/* Like talloc_strndup, but take an extra parameter for the internal talloc
 * name (for debugging) */

char *
talloc_strndup_named_const (void *ctx, const char *str,
			    size_t len, const char *name);

/* use the __location__ macro from talloc.h to name a string according to its
 * source location */

#define talloc_strndup_debug(ctx, str, len) talloc_strndup_named_const (ctx, str, len, __location__)

#endif
