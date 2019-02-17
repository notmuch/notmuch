#ifndef _NOTMUCH_TEST_H
#define _NOTMUCH_TEST_H
#include <stdio.h>
#include <notmuch.h>

inline static void
expect0(int line, notmuch_status_t ret)
{
   if (ret) {
	fprintf (stderr, "line %d: %d\n", line, ret);
	exit (1);
   }
}

#define EXPECT0(v)  expect0(__LINE__, v);
#endif
