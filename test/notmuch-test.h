#ifndef _NOTMUCH_TEST_H
#define _NOTMUCH_TEST_H
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include <assert.h>
#include <dlfcn.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <talloc.h>
#include <unistd.h>

#include <notmuch.h>

inline static void
expect0 (int line, notmuch_status_t ret)
{
    if (ret) {
	fprintf (stderr, "line %d: %d\n", line, ret);
	exit (1);
    }
}

#define EXPECT0(v)  expect0 (__LINE__, v);

inline static void *
dlsym_next (const char *symbol)
{
    void *sym = dlsym (RTLD_NEXT, symbol);
    char *str = dlerror ();

    if (str != NULL) {
	fprintf (stderr, "finding symbol '%s' failed: %s", symbol, str);
	exit (77);
    }
    return sym;
}

#define WRAP_DLFUNC(_rtype, _func, _args)                               \
    _rtype _func _args;                                                 \
    _rtype _func _args {                                                \
	static _rtype (*_func##_orig) _args = NULL;                         \
	if (! _func##_orig ) *(void **) (&_func##_orig) = dlsym_next (#_func);
#endif
