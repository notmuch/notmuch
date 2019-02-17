/* timegm.c --- Implementation of replacement timegm function.

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 3, or (at
   your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
   02110-1301, USA.  */

/* Copyright 2013 Blake Jones. */

#include <time.h>
#include "compat.h"

static int
leapyear (int year)
{
    return ((year % 4) == 0 && ((year % 100) != 0 || (year % 400) == 0));
}

/*
 * This is a simple implementation of timegm() which does what is needed
 * by create_output() -- just turns the "struct tm" into a GMT time_t.
 * It does not normalize any of the fields of the "struct tm", nor does
 * it set tm_wday or tm_yday.
 */
time_t
timegm (struct tm *tm)
{
    int	monthlen[2][12] = {
	{ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 },
	{ 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 },
    };
    int	year, month, days;

    days = 365 * (tm->tm_year - 70);
    for (year = 70; year < tm->tm_year; year++) {
	if (leapyear(1900 + year)) {
	    days++;
	}
    }
    for (month = 0; month < tm->tm_mon; month++) {
	days += monthlen[leapyear(1900 + year)][month];
    }
    days += tm->tm_mday - 1;

    return ((((days * 24) + tm->tm_hour) * 60 + tm->tm_min) * 60 + tm->tm_sec);
}
