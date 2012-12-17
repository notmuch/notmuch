#include <string.h>
#include "talloc-extra.h"

char *
talloc_strndup_named_const (void *ctx, const char *str,
			    size_t len, const char *name)
{
    char *ptr = talloc_strndup (ctx, str, len);

    if (ptr)
	talloc_set_name_const (ptr, name);

    return ptr;
}
