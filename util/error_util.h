/* error_util.h - Provide the INTERNAL_ERROR macro
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

#ifndef ERROR_UTIL_H
#define ERROR_UTIL_H

#include <talloc.h>

#include "function-attributes.h"

/* There's no point in continuing when we've detected that we've done
 * something wrong internally (as opposed to the user passing in a
 * bogus value).
 *
 * Note that PRINTF_ATTRIBUTE comes from talloc.h
 */
void
_internal_error (const char *format, ...) PRINTF_ATTRIBUTE (1, 2) NORETURN_ATTRIBUTE;

/* There's no point in continuing when we've detected that we've done
 * something wrong internally (as opposed to the user passing in a
 * bogus value).
 *
 * Note that __location__ comes from talloc.h.
 */
#define INTERNAL_ERROR(format, ...)			\
    _internal_error (format " (%s).\n",			\
		     ##__VA_ARGS__, __location__)

#endif
