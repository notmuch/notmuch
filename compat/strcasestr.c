/*
 * slow simplistic reimplementation of strcasestr for systems that
 * don't include it in their library
 *
 * based on a GPL implementation in OpenTTD found under GPL v2

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation, version 2.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
   02110-1301, USA.  */

/* Imported into notmuch by Dirk Hohndel - original author unknown. */

#include <string.h>

#include "compat.h"

char *strcasestr(const char *haystack, const char *needle)
{
	size_t hay_len = strlen(haystack);
	size_t needle_len = strlen(needle);
	while (hay_len >= needle_len) {
		if (strncasecmp(haystack, needle, needle_len) == 0)
		    return (char *) haystack;

		haystack++;
		hay_len--;
	}

	return NULL;
}
