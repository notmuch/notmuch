/* notmuch - Not much of an email program, (just index and search)
 *
 * Copyright © 2009 Scott Robinson
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
 * along with this program.  If not, see http://www.gnu.org/licenses/ .
 *
 * Authors: Scott Robinson <scott@quadhome.com>
 *
 */

#include "notmuch-client.h"

/*
 * json_quote_str derived from cJSON's print_string_ptr,
 * Copyright (c) 2009 Dave Gamble
 */

char *
json_quote_str(const void *ctx, const char *str)
{
    const char *ptr;
    char *ptr2;
    char *out;
    int len = 0;

    if (!str)
	return NULL;

    for (ptr = str; *ptr; len++, ptr++) {
	if (*ptr < 32 || *ptr == '\"' || *ptr == '\\')
	    len++;
    }

    out = talloc_array (ctx, char, len + 3);

    ptr = str;
    ptr2 = out;

    *ptr2++ = '\"';
    while (*ptr) {
	    if (*ptr > 31 && *ptr != '\"' && *ptr != '\\') {
		*ptr2++ = *ptr++;
	    } else {
		*ptr2++ = '\\';
		switch (*ptr++) {
		    case '\"':	*ptr2++ = '\"';	break;
		    case '\\':	*ptr2++ = '\\';	break;
		    case '\b':	*ptr2++ = 'b';	break;
		    case '\f':	*ptr2++ = 'f';	break;
		    case '\n':	*ptr2++ = 'n';	break;
		    case '\r':	*ptr2++ = 'r';	break;
		    case '\t':	*ptr2++ = 't';	break;
		    default:	 ptr2--;	break;
		}
	    }
    }
    *ptr2++ = '\"';
    *ptr2++ = '\0';

    return out;
}
