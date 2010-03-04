/* notmuch - Not much of an email program, (just index and search)
 *
 * Copyright © 2009 Dave Gamble
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
 * Authors: Dave Gamble
 *          Scott Robinson <scott@quadhome.com>
 *
 */

#include "notmuch-client.h"

/* This function was derived from the print_string_ptr function of
 * cJSON (http://cjson.sourceforge.net/) and is used by permission of
 * the following license:
 *
 * Copyright (c) 2009 Dave Gamble
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

char *
json_quote_chararray(const void *ctx, const char *str, const size_t len)
{
    const char *ptr;
    char *ptr2;
    char *out;
    size_t loop;
    size_t required;

    if (len == 0)
	return (char *)"\"\"";

    for (loop = 0, required = 0, ptr = str;
	 loop < len;
	 loop++, required++, ptr++) {
	if ((unsigned char)(*ptr) < 32 || *ptr == '\"' || *ptr == '\\')
	    required++;
    }

    /*
     * + 3 for:
     * - leading quotation mark,
     * - trailing quotation mark,
     * - trailing NULL.
     */
    out = talloc_array (ctx, char, required + 3);

    ptr = str;
    ptr2 = out;

    *ptr2++ = '\"';
    for (loop = 0; loop < len; loop++) {
	if ((unsigned char)(*ptr) > 31 && *ptr != '\"' && *ptr != '\\') {
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

char *
json_quote_str(const void *ctx, const char *str)
{
    return (json_quote_chararray (ctx, str, strlen (str)));
}
