/* date.c - Date-parsing utility for the notmuch mail system.
 *
 *  Copyright © 2000-2009 Jeffrey Stedfast
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
 * along with this program.  If not, see http://www.gnu.org/licenses/ 
 */

/* This code was originally written by from Jeffrey Stedfast
 * as part of his GMime library (http://spruce.sourceforge.net/gmime/)
 *
 * Carl Worth <cworth@cworth.org> imported it into notmuch and removed
 * some glib-isms.
 */

#include "notmuch-private.h"

#include <time.h>

#include <glib.h> /* For g_ascii_strncasecmp only. */

#ifndef FALSE
#define FALSE 0
#endif

#ifndef TRUE
#define TRUE 1
#endif

#define d(x)

#define GMIME_FOLD_PREENCODED  (GMIME_FOLD_LEN / 2)

/* date parser macros */
#define NUMERIC_CHARS          "1234567890"
#define WEEKDAY_CHARS          "SundayMondayTuesdayWednesdayThursdayFridaySaturday"
#define MONTH_CHARS            "JanuaryFebruaryMarchAprilMayJuneJulyAugustSeptemberOctoberNovemberDecember"
#define TIMEZONE_ALPHA_CHARS   "UTCGMTESTEDTCSTCDTMSTPSTPDTZAMNY()"
#define TIMEZONE_NUMERIC_CHARS "-+1234567890"
#define TIME_CHARS             "1234567890:"

#define DATE_TOKEN_NON_NUMERIC          (1 << 0)
#define DATE_TOKEN_NON_WEEKDAY          (1 << 1)
#define DATE_TOKEN_NON_MONTH            (1 << 2)
#define DATE_TOKEN_NON_TIME             (1 << 3)
#define DATE_TOKEN_HAS_COLON            (1 << 4)
#define DATE_TOKEN_NON_TIMEZONE_ALPHA   (1 << 5)
#define DATE_TOKEN_NON_TIMEZONE_NUMERIC (1 << 6)
#define DATE_TOKEN_HAS_SIGN             (1 << 7)

static unsigned char gmime_datetok_table[256] = {
	128,111,111,111,111,111,111,111,111,111,111,111,111,111,111,111,
	111,111,111,111,111,111,111,111,111,111,111,111,111,111,111,111,
	111,111,111,111,111,111,111,111, 79, 79,111,175,111,175,111,111,
	 38, 38, 38, 38, 38, 38, 38, 38, 38, 38,119,111,111,111,111,111,
	111, 75,111, 79, 75, 79,105, 79,111,111,107,111,111, 73, 75,107,
	 79,111,111, 73, 77, 79,111,109,111, 79, 79,111,111,111,111,111,
	111,105,107,107,109,105,111,107,105,105,111,111,107,107,105,105,
	107,111,105,105,105,105,107,111,111,105,111,111,111,111,111,111,
	111,111,111,111,111,111,111,111,111,111,111,111,111,111,111,111,
	111,111,111,111,111,111,111,111,111,111,111,111,111,111,111,111,
	111,111,111,111,111,111,111,111,111,111,111,111,111,111,111,111,
	111,111,111,111,111,111,111,111,111,111,111,111,111,111,111,111,
	111,111,111,111,111,111,111,111,111,111,111,111,111,111,111,111,
	111,111,111,111,111,111,111,111,111,111,111,111,111,111,111,111,
	111,111,111,111,111,111,111,111,111,111,111,111,111,111,111,111,
	111,111,111,111,111,111,111,111,111,111,111,111,111,111,111,111,
};

/* hrm, is there a library for this stuff? */
static struct {
	const char *name;
	int offset;
} tz_offsets [] = {
	{ "UT", 0 },
	{ "GMT", 0 },
	{ "EST", -500 },	/* these are all US timezones.  bloody yanks */
	{ "EDT", -400 },
	{ "CST", -600 },
	{ "CDT", -500 },
	{ "MST", -700 },
	{ "MDT", -600 },
	{ "PST", -800 },
	{ "PDT", -700 },
	{ "Z", 0 },
	{ "A", -100 },
	{ "M", -1200 },
	{ "N", 100 },
	{ "Y", 1200 },
};

static const char *tm_months[] = {
	"Jan", "Feb", "Mar", "Apr", "May", "Jun",
	"Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
};

static const char *tm_days[] = {
	"Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"
};

/* This is where it gets ugly... */

typedef struct _date_token {
	struct _date_token *next;
	unsigned char mask;
	const char *start;
	size_t len;
} date_token;

#define date_token_free(tok) free (tok)
#define date_token_new() malloc (sizeof (date_token))

static date_token *
datetok (const char *date)
{
	date_token *tokens = NULL, *token, *tail = (date_token *) &tokens;
	const char *start, *end;
        unsigned char mask;
	
	start = date;
	while (*start) {
		/* kill leading whitespace */
		while (*start == ' ' || *start == '\t')
			start++;
		
		if (*start == '\0')
			break;
		
		mask = gmime_datetok_table[(unsigned char) *start];
		
		/* find the end of this token */
		end = start + 1;
		while (*end && !strchr ("-/,\t\r\n ", *end))
			mask |= gmime_datetok_table[(unsigned char) *end++];
		
		if (end != start) {
			token = date_token_new ();
			token->next = NULL;
			token->start = start;
			token->len = end - start;
			token->mask = mask;
			
			tail->next = token;
			tail = token;
		}
		
		if (*end)
			start = end + 1;
		else
			break;
	}
	
	return tokens;
}

static int
decode_int (const char *in, size_t inlen)
{
	register const char *inptr;
	int sign = 1, val = 0;
	const char *inend;
	
	inptr = in;
	inend = in + inlen;
	
	if (*inptr == '-') {
		sign = -1;
		inptr++;
	} else if (*inptr == '+')
		inptr++;
	
	for ( ; inptr < inend; inptr++) {
		if (!(*inptr >= '0' && *inptr <= '9'))
			return -1;
		else
			val = (val * 10) + (*inptr - '0');
	}
	
	val *= sign;
	
	return val;
}

#if 0
static int
get_days_in_month (int month, int year)
{
        switch (month) {
	case 1:
	case 3:
	case 5:
	case 7:
	case 8:
	case 10:
	case 12:
	        return 31;
	case 4:
	case 6:
	case 9:
	case 11:
	        return 30;
	case 2:
	        if (g_date_is_leap_year (year))
		        return 29;
		else
		        return 28;
	default:
	        return 0;
	}
}
#endif

static int
get_wday (const char *in, size_t inlen)
{
	int wday;

	if (in == NULL)
		return -1;
	
	if (inlen < 3)
		return -1;
	
	for (wday = 0; wday < 7; wday++) {
		if (!g_ascii_strncasecmp (in, tm_days[wday], 3))
			return wday;
	}
	
	return -1;  /* unknown week day */
}

static int
get_mday (const char *in, size_t inlen)
{
	int mday;
	
	if (in == NULL)
		return -1;
	
	mday = decode_int (in, inlen);
	
	if (mday < 0 || mday > 31)
		mday = -1;
	
	return mday;
}

static int
get_month (const char *in, size_t inlen)
{
	int i;
	
	if (in == NULL)
		return -1;
	
	if (inlen < 3)
		return -1;
	
	for (i = 0; i < 12; i++) {
		if (!g_ascii_strncasecmp (in, tm_months[i], 3))
			return i;
	}
	
	return -1;  /* unknown month */
}

static int
get_year (const char *in, size_t inlen)
{
	int year;
	
	if (in == NULL)
		return -1;
	
	if ((year = decode_int (in, inlen)) == -1)
		return -1;
	
	if (year < 100)
		year += (year < 70) ? 2000 : 1900;
	
	if (year < 1969)
		return -1;
	
	return year;
}

static int
get_time (const char *in, size_t inlen, int *hour, int *min, int *sec)
{
	register const char *inptr;
	int *val, colons = 0;
	const char *inend;
	
	*hour = *min = *sec = 0;
	
	inend = in + inlen;
	val = hour;
	for (inptr = in; inptr < inend; inptr++) {
		if (*inptr == ':') {
			colons++;
			switch (colons) {
			case 1:
				val = min;
				break;
			case 2:
				val = sec;
				break;
			default:
				return FALSE;
			}
		} else if (!(*inptr >= '0' && *inptr <= '9'))
			return FALSE;
		else
			*val = (*val * 10) + (*inptr - '0');
	}
	
	return TRUE;
}

static int
get_tzone (date_token **token)
{
	const char *inptr, *inend;
	size_t inlen;
	int i, t;
	
	for (i = 0; *token && i < 2; *token = (*token)->next, i++) {
		inptr = (*token)->start;
		inlen = (*token)->len;
		inend = inptr + inlen;
		
		if (*inptr == '+' || *inptr == '-') {
			return decode_int (inptr, inlen);
		} else {
			if (*inptr == '(') {
				inptr++;
				if (*(inend - 1) == ')')
					inlen -= 2;
				else
					inlen--;
			}
			
			for (t = 0; t < 15; t++) {
				size_t len = strlen (tz_offsets[t].name);
				
				if (len != inlen)
					continue;
				
				if (!strncmp (inptr, tz_offsets[t].name, len))
					return tz_offsets[t].offset;
			}
		}
	}
	
	return -1;
}

#define HAVE_TIMEZONE

static time_t
mktime_utc (struct tm *tm)
{
	time_t tt;
	long tz;
	
	tm->tm_isdst = -1;
	tt = mktime (tm);
	
#if defined (G_OS_WIN32)
	_get_timezone (&tz);
	if (tm->tm_isdst > 0) {
		int dst;
		
		_get_dstbias (&dst);
		tz += dst;
	}
#elif defined (HAVE_TM_GMTOFF)
	tz = -tm->tm_gmtoff;
#elif defined (HAVE_TIMEZONE)
	if (tm->tm_isdst > 0) {
#if defined (HAVE_ALTZONE)
		tz = altzone;
#else /* !defined (HAVE_ALTZONE) */
		tz = (timezone - 3600);
#endif
	} else {
		tz = timezone;
	}
#elif defined (HAVE__TIMEZONE)
	tz = _timezone;
#else
#error Neither HAVE_TIMEZONE nor HAVE_TM_GMTOFF defined. Rerun autoheader, autoconf, etc.
#endif
	
	return tt - tz;
}

static time_t
parse_rfc822_date (date_token *tokens, int *tzone)
{
	int hour, min, sec, offset, n;
	date_token *token;
	struct tm tm;
	time_t t;
	
	if (tokens == NULL)
		return 0;
	
	token = tokens;
	
	memset ((void *) &tm, 0, sizeof (struct tm));
	
	if ((n = get_wday (token->start, token->len)) != -1) {
		/* not all dates may have this... */
		tm.tm_wday = n;
		token = token->next;
	}
	
	/* get the mday */
	if (!token || (n = get_mday (token->start, token->len)) == -1)
		return (time_t) 0;
	
	tm.tm_mday = n;
	token = token->next;
	
	/* get the month */
	if (!token || (n = get_month (token->start, token->len)) == -1)
		return (time_t) 0;
	
	tm.tm_mon = n;
	token = token->next;
	
	/* get the year */
	if (!token || (n = get_year (token->start, token->len)) == -1)
		return (time_t) 0;
	
	tm.tm_year = n - 1900;
	token = token->next;
	
	/* get the hour/min/sec */
	if (!token || !get_time (token->start, token->len, &hour, &min, &sec))
		return (time_t) 0;
	
	tm.tm_hour = hour;
	tm.tm_min = min;
	tm.tm_sec = sec;
	token = token->next;
	
	/* get the timezone */
	if (!token || (n = get_tzone (&token)) == -1) {
		/* I guess we assume tz is GMT? */
		offset = 0;
	} else {
		offset = n;
	}
	
	t = mktime_utc (&tm);
	
	/* t is now GMT of the time we want, but not offset by the timezone ... */
	
	/* this should convert the time to the GMT equiv time */
	t -= ((offset / 100) * 60 * 60) + (offset % 100) * 60;
	
	if (tzone)
		*tzone = offset;
	
	return t;
}


#define date_token_mask(t)  (((date_token *) t)->mask)
#define is_numeric(t)       ((date_token_mask (t) & DATE_TOKEN_NON_NUMERIC) == 0)
#define is_weekday(t)       ((date_token_mask (t) & DATE_TOKEN_NON_WEEKDAY) == 0)
#define is_month(t)         ((date_token_mask (t) & DATE_TOKEN_NON_MONTH) == 0)
#define is_time(t)          (((date_token_mask (t) & DATE_TOKEN_NON_TIME) == 0) && (date_token_mask (t) & DATE_TOKEN_HAS_COLON))
#define is_tzone_alpha(t)   ((date_token_mask (t) & DATE_TOKEN_NON_TIMEZONE_ALPHA) == 0)
#define is_tzone_numeric(t) (((date_token_mask (t) & DATE_TOKEN_NON_TIMEZONE_NUMERIC) == 0) && (date_token_mask (t) & DATE_TOKEN_HAS_SIGN))
#define is_tzone(t)         (is_tzone_alpha (t) || is_tzone_numeric (t))

static time_t
parse_broken_date (date_token *tokens, int *tzone)
{
	int got_wday, got_month, got_tzone;
	int hour, min, sec, offset, n;
	date_token *token;
	struct tm tm;
	time_t t;
	
	memset ((void *) &tm, 0, sizeof (struct tm));
	got_wday = got_month = got_tzone = FALSE;
	offset = 0;
	
	token = tokens;
	while (token) {
		if (is_weekday (token) && !got_wday) {
			if ((n = get_wday (token->start, token->len)) != -1) {
				d(printf ("weekday; "));
				got_wday = TRUE;
				tm.tm_wday = n;
				goto next;
			}
		}
		
		if (is_month (token) && !got_month) {
			if ((n = get_month (token->start, token->len)) != -1) {
				d(printf ("month; "));
				got_month = TRUE;
				tm.tm_mon = n;
				goto next;
			}
		}
		
		if (is_time (token) && !tm.tm_hour && !tm.tm_min && !tm.tm_sec) {
			if (get_time (token->start, token->len, &hour, &min, &sec)) {
				d(printf ("time; "));
				tm.tm_hour = hour;
				tm.tm_min = min;
				tm.tm_sec = sec;
				goto next;
			}
		}
		
		if (is_tzone (token) && !got_tzone) {
			date_token *t = token;
			
			if ((n = get_tzone (&t)) != -1) {
				d(printf ("tzone; "));
				got_tzone = TRUE;
				offset = n;
				goto next;
			}
		}
		
		if (is_numeric (token)) {
			if (token->len == 4 && !tm.tm_year) {
				if ((n = get_year (token->start, token->len)) != -1) {
					d(printf ("year; "));
					tm.tm_year = n - 1900;
					goto next;
				}
			} else {
				/* Note: assumes MM-DD-YY ordering if '0 < MM < 12' holds true */
				if (!got_month && token->next && is_numeric (token->next)) {
					if ((n = decode_int (token->start, token->len)) > 12) {
						goto mday;
					} else if (n > 0) {
						d(printf ("mon; "));
						got_month = TRUE;
						tm.tm_mon = n - 1;
					}
					goto next;
				} else if (!tm.tm_mday && (n = get_mday (token->start, token->len)) != -1) {
				mday:
					d(printf ("mday; "));
					tm.tm_mday = n;
					goto next;
				} else if (!tm.tm_year) {
					if ((n = get_year (token->start, token->len)) != -1) {
						d(printf ("2-digit year; "));
						tm.tm_year = n - 1900;
					}
					goto next;
				}
			}
		}
		
		d(printf ("???; "));
		
	next:
		
		token = token->next;
	}
	
	d(printf ("\n"));
	
	t = mktime_utc (&tm);
	
	/* t is now GMT of the time we want, but not offset by the timezone ... */
	
	/* this should convert the time to the GMT equiv time */
	t -= ((offset / 100) * 60 * 60) + (offset % 100) * 60;
	
	if (tzone)
		*tzone = offset;
	
	return t;
}

#if 0
static void
gmime_datetok_table_init (void)
{
	int i;
	
	memset (gmime_datetok_table, 0, sizeof (gmime_datetok_table));
	
	for (i = 0; i < 256; i++) {
		if (!strchr (NUMERIC_CHARS, i))
			gmime_datetok_table[i] |= DATE_TOKEN_NON_NUMERIC;
		
		if (!strchr (WEEKDAY_CHARS, i))
			gmime_datetok_table[i] |= DATE_TOKEN_NON_WEEKDAY;
		
		if (!strchr (MONTH_CHARS, i))
			gmime_datetok_table[i] |= DATE_TOKEN_NON_MONTH;
		
		if (!strchr (TIME_CHARS, i))
			gmime_datetok_table[i] |= DATE_TOKEN_NON_TIME;
		
		if (!strchr (TIMEZONE_ALPHA_CHARS, i))
			gmime_datetok_table[i] |= DATE_TOKEN_NON_TIMEZONE_ALPHA;
		
		if (!strchr (TIMEZONE_NUMERIC_CHARS, i))
			gmime_datetok_table[i] |= DATE_TOKEN_NON_TIMEZONE_NUMERIC;
		
		if (((char) i) == ':')
			gmime_datetok_table[i] |= DATE_TOKEN_HAS_COLON;
		
		if (strchr ("+-", i))
			gmime_datetok_table[i] |= DATE_TOKEN_HAS_SIGN;
	}
	
	printf ("static unsigned char gmime_datetok_table[256] = {");
	for (i = 0; i < 256; i++) {
		if (i % 16 == 0)
			printf ("\n\t");
		printf ("%3d,", gmime_datetok_table[i]);
	}
	printf ("\n};\n");
}
#endif

time_t
notmuch_parse_date (const char *str, int *tz_offset)
{
	date_token *token, *tokens;
	time_t date;

	if (str == NULL)
		return 0;

	if (!(tokens = datetok (str))) {
		if (tz_offset)
			*tz_offset = 0;
		
		return 0;
	}
	
	if (!(date = parse_rfc822_date (tokens, tz_offset)))
		date = parse_broken_date (tokens, tz_offset);
	
	/* cleanup */
	while (tokens) {
		token = tokens;
		tokens = tokens->next;
		date_token_free (token);
	}
	
	return date;
}
