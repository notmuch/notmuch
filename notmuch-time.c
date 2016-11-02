/* notmuch - Not much of an email program, (just index and search)
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

#include "notmuch-client.h"

/* Format a nice representation of 'time' relative to the current time.
 *
 * Examples include:
 *
 *	5 mins. ago	(For times less than 60 minutes ago)
 *	Today 12:30	(For times >60 minutes but still today)
 *	Yest. 12:30
 *	Mon.  12:30	(Before yesterday but fewer than 7 days ago)
 *	October 12	(Between 7 and 180 days ago (about 6 months))
 *	2008-06-30	(More than 180 days ago)
 *
 * The returned string is either static data (a string literal) or
 * newly talloced data belonging to 'ctx'. That is, the caller should
 * not modify nor free the returned value. But when the caller
 * arranges for 'ctx' to be talloc_freed, then memory allocated here
 * (if any) will be reclaimed.
 *
 */
#define MINUTE (60)
#define HOUR (60 * MINUTE)
#define DAY (24 * HOUR)
#define RELATIVE_DATE_MAX 20
const char *
notmuch_time_relative_date (const void *ctx, time_t then)
{
    struct tm tm_now, tm_then;
    time_t now = time(NULL);
    time_t delta;
    char *result;

    localtime_r (&now, &tm_now);
    localtime_r (&then, &tm_then);

    result = talloc_zero_size (ctx, RELATIVE_DATE_MAX);
    if (result == NULL)
	return "when?";

    if (then > now)
	return "the future";

    delta = now - then;

    if (delta > 180 * DAY) {
	strftime (result, RELATIVE_DATE_MAX,
		  "%F", &tm_then); /* 2008-06-30 */
	return result;
    }

    if (delta < 3600) {
	snprintf (result, RELATIVE_DATE_MAX,
		  "%d mins. ago", (int) (delta / 60));
	return result;
    }

    if (delta <= 7 * DAY) {
	if (tm_then.tm_wday == tm_now.tm_wday &&
	    delta < DAY)
	{
	    strftime (result, RELATIVE_DATE_MAX,
		      "Today %R", &tm_then); /* Today 12:30 */
	    return result;
	} else if ((tm_now.tm_wday + 7 - tm_then.tm_wday) % 7 == 1) {
	    strftime (result, RELATIVE_DATE_MAX,
		      "Yest. %R", &tm_then); /* Yest. 12:30 */
	    return result;
	} else {
	    if (tm_then.tm_wday != tm_now.tm_wday) {
		strftime (result, RELATIVE_DATE_MAX,
			  "%a. %R", &tm_then); /* Mon. 12:30 */
		return result;
	    }
	}
    }

    strftime (result, RELATIVE_DATE_MAX,
	      "%B %d", &tm_then); /* October 12 */
    return result;
}
#undef MINUTE
#undef HOUR
#undef DAY

void
notmuch_time_print_formatted_seconds (double seconds)
{
    int hours;
    int minutes;

    if (seconds < 1) {
	printf ("almost no time");
	return;
    }

    if (seconds > 3600) {
	hours = (int) seconds / 3600;
	printf ("%dh ", hours);
	seconds -= hours * 3600;
    }

    if (seconds > 60) {
	minutes = (int) seconds / 60;
	printf ("%dm ", minutes);
	seconds -= minutes * 60;
    }

    printf ("%ds", (int) seconds);
}

/* Compute the number of seconds elapsed from start to end. */
double
notmuch_time_elapsed (struct timeval start, struct timeval end)
{
    return ((end.tv_sec - start.tv_sec) +
	    (end.tv_usec - start.tv_usec) / 1e6);
}
