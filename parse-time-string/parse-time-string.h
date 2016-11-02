/*
 * parse time string - user friendly date and time parser
 * Copyright © 2012 Jani Nikula
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 *
 * Author: Jani Nikula <jani@nikula.org>
 */

#ifndef PARSE_TIME_STRING_H
#define PARSE_TIME_STRING_H

#ifdef __cplusplus
extern "C" {
#endif

#include <time.h>

/* return values for parse_time_string() */
enum {
    PARSE_TIME_OK = 0,
    PARSE_TIME_ERR,		/* unspecified error */
    PARSE_TIME_ERR_LIB,		/* library call failed */
    PARSE_TIME_ERR_ALREADYSET,	/* attempt to set unit twice */
    PARSE_TIME_ERR_FORMAT,	/* generic date/time format error */
    PARSE_TIME_ERR_DATEFORMAT,	/* date format error */
    PARSE_TIME_ERR_TIMEFORMAT,	/* time format error */
    PARSE_TIME_ERR_INVALIDDATE,	/* date value error */
    PARSE_TIME_ERR_INVALIDTIME,	/* time value error */
    PARSE_TIME_ERR_KEYWORD,	/* unknown keyword */
};

/* round values for parse_time_string() */
enum {
    PARSE_TIME_ROUND_DOWN = -1,
    PARSE_TIME_NO_ROUND = 0,
    PARSE_TIME_ROUND_UP = 1,
    PARSE_TIME_ROUND_UP_INCLUSIVE = 2,
};

/**
 * parse_time_string() - user friendly date and time parser
 * @s:		string to parse
 * @t:		pointer to time_t to store parsed time in
 * @ref:	pointer to time_t containing reference date/time, or NULL
 * @round:	PARSE_TIME_NO_ROUND, PARSE_TIME_ROUND_DOWN, or
 *		PARSE_TIME_ROUND_UP
 *
 * Parse a date/time string 's' and store the parsed date/time result
 * in 't'.
 *
 * A reference date/time is used for determining the "date/time units"
 * (roughly equivalent to struct tm members) not specified by 's'. If
 * 'ref' is non-NULL, it must contain a pointer to a time_t to be used
 * as reference date/time. Otherwise, the current time is used.
 *
 * If 's' does not specify a full date/time, the 'round' parameter
 * specifies if and how the result should be rounded as follows:
 *
 *   PARSE_TIME_NO_ROUND: All date/time units that are not specified
 *   by 's' are set to the corresponding unit derived from the
 *   reference date/time.
 *
 *   PARSE_TIME_ROUND_DOWN: All date/time units that are more accurate
 *   than the most accurate unit specified by 's' are set to the
 *   smallest valid value for that unit. Rest of the unspecified units
 *   are set as in PARSE_TIME_NO_ROUND.
 *
 *   PARSE_TIME_ROUND_UP: All date/time units that are more accurate
 *   than the most accurate unit specified by 's' are set to the
 *   smallest valid value for that unit. The most accurate unit
 *   specified by 's' is incremented by one (and this is rolled over
 *   to the less accurate units as necessary), unless the most
 *   accurate unit is seconds. Rest of the unspecified units are set
 *   as in PARSE_TIME_NO_ROUND.
 *
 *   PARSE_TIME_ROUND_UP_INCLUSIVE: Same as PARSE_TIME_ROUND_UP, minus
 *   one second, unless the most accurate unit specified by 's' is
 *   seconds. This is useful for callers that require a value for
 *   inclusive comparison of the result.
 *
 * Return 0 (PARSE_TIME_OK) for succesfully parsed date/time, or one
 * of PARSE_TIME_ERR_* on error. 't' is not modified on error.
 */
int parse_time_string (const char *s, time_t *t, const time_t *ref, int round);

#ifdef __cplusplus
}
#endif

#endif /* PARSE_TIME_STRING_H */
