#include "util.h"
#include "error_util.h"
#include <string.h>
#include <errno.h>

const char *
util_error_string (util_status_t errnum)
{
    switch (errnum) {
    case UTIL_SUCCESS:
	return "success";
    case UTIL_OUT_OF_MEMORY:
	return "out of memory";
    case UTIL_EOF:
	return "end of file";
    case UTIL_ERRNO:
	return strerror (errno);
    case UTIL_GZERROR:
	/* we lack context to be more informative here */
	return "zlib error";
    default:
	INTERNAL_ERROR("unexpected error status %d", errnum);
    }
}
