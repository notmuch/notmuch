/* debugger.c - Some debugger utilities for the notmuch mail library
 *
 * Copyright © 2009 Chris Wilson
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
 * Author: Chris Wilson <chris@chris-wilson.co.uk>
 */

#include "notmuch-client.h"

#include <libgen.h>

#if HAVE_VALGRIND
#include <valgrind.h>
#else
#define RUNNING_ON_VALGRIND 0
#endif

notmuch_bool_t
debugger_is_active (void)
{
    char buf[1024];

    if (RUNNING_ON_VALGRIND)
	return TRUE;

    sprintf (buf, "/proc/%d/exe", getppid ());
    if (readlink (buf, buf, sizeof (buf)) != -1 &&
	strncmp (basename (buf), "gdb", 3) == 0)
    {
	return TRUE;
    }

    return FALSE;
}
