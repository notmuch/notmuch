/* error_util.c - internal error utilities for notmuch.
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

#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>

#include "error_util.h"

void
_internal_error (const char *format, ...)
{
    va_list va_args;

    va_start (va_args, format);

    fprintf (stderr, "Internal error: ");
    vfprintf (stderr, format, va_args);

    exit (1);
}

