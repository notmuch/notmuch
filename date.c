/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*- */
/*  GMime
 *  Copyright (C) 2000-2009 Jeffrey Stedfast
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public License
 *  as published by the Free Software Foundation; either version 2.1
 *  of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free
 *  Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
 *  02110-1301, USA.
 */


#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#define _GNU_SOURCE

#include <glib.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>      /* for MAXHOSTNAMELEN */
#else
#define MAXHOSTNAMELEN 64
#endif
#ifdef HAVE_UTSNAME_DOMAINNAME
#include <sys/utsname.h>    /* for uname() */
#endif
#include <sys/types.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>         /* Unix header for getpid() */
#endif
#ifdef G_OS_WIN32
#include <winsock2.h>
#include <ws2tcpip.h>
#include <process.h>
#define getpid() _getpid()
#endif
#ifdef HAVE_NETDB_H
#include <netdb.h>
#endif
#include <ctype.h>
#include <errno.h>

#include "gmime-utils.h"
#include "gmime-table-private.h"
#include "gmime-parse-utils.h"
#include "gmime-part.h"
#include "gmime-charset.h"
#include "gmime-iconv.h"
#include "gmime-iconv-utils.h"

#ifdef ENABLE_WARNINGS
#define w(x) x
#else
#define w(x)
#endif /* ENABLE_WARNINGS */

#define d(x)


/**
 * SECTION: gmime-utils
 * @title: gmime-utils
 * @short_description: MIME utility functions
 * @see_also:
 *
 * Utility functions to parse, encode and decode various MIME tokens
 * and encodings.
 **/

extern gboolean _g_mime_enable_rfc2047_workarounds (void);

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

static unsigned char tohex[16] = {
	'0', '1', '2', '3', '4', '5', '6', '7',
	'8', '9', 'A', 'B', 'C', 'D', 'E', 'F'
};

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

/* hrm, is there a library for this shit? */
static struct {
	char *name;
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

static char *tm_months[] = {
	"Jan", "Feb", "Mar", "Apr", "May", "Jun",
	"Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
};

static char *tm_days[] = {
	"Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"
};


/**
 * g_mime_utils_header_format_date:
 * @date: time_t date representation
 * @tz_offset: Timezone offset
 *
 * Allocates a string buffer containing the rfc822 formatted date
 * string represented by @time and @tz_offset.
 *
 * Returns: a valid string representation of the date.
 **/
char *
g_mime_utils_header_format_date (time_t date, int tz_offset)
{
	struct tm tm;
	
	date += ((tz_offset / 100) * (60 * 60)) + (tz_offset % 100) * 60;
	
#if defined (HAVE_GMTIME_R)
	gmtime_r (&date, &tm);
#elif defined (HAVE_GMTIME_S)
	gmtime_s (&tm, &date);
#else
	memcpy (&tm, gmtime (&date), sizeof (tm));
#endif
	
	return g_strdup_printf ("%s, %02d %s %04d %02d:%02d:%02d %+05d",
				tm_days[tm.tm_wday], tm.tm_mday,
				tm_months[tm.tm_mon],
				tm.tm_year + 1900,
				tm.tm_hour, tm.tm_min, tm.tm_sec,
				tz_offset);
}

/* This is where it gets ugly... */

typedef struct _date_token {
	struct _date_token *next;
	unsigned char mask;
	const char *start;
	size_t len;
} date_token;

#define date_token_free(tok) g_slice_free (date_token, tok)
#define date_token_new() g_slice_new (date_token)

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
	
	g_return_val_if_fail (in != NULL, -1);
	
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
	
	g_return_val_if_fail (in != NULL, -1);
	
	mday = decode_int (in, inlen);
	
	if (mday < 0 || mday > 31)
		mday = -1;
	
	return mday;
}

static int
get_month (const char *in, size_t inlen)
{
	int i;
	
	g_return_val_if_fail (in != NULL, -1);
	
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
	
	g_return_val_if_fail (in != NULL, -1);
	
	if ((year = decode_int (in, inlen)) == -1)
		return -1;
	
	if (year < 100)
		year += (year < 70) ? 2000 : 1900;
	
	if (year < 1969)
		return -1;
	
	return year;
}

static gboolean
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
	
	g_return_val_if_fail (tokens != NULL, (time_t) 0);
	
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
	gboolean got_wday, got_month, got_tzone;
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


/**
 * g_mime_utils_header_decode_date:
 * @str: input date string
 * @tz_offset: timezone offset
 *
 * Decodes the rfc822 date string and saves the GMT offset into
 * @tz_offset if non-NULL.
 *
 * Returns: the time_t representation of the date string specified by
 * @str or (time_t) %0 on error. If @tz_offset is non-NULL, the value
 * of the timezone offset will be stored.
 **/
time_t
g_mime_utils_header_decode_date (const char *str, int *tz_offset)
{
	date_token *token, *tokens;
	time_t date;
	
	if (!(tokens = datetok (str))) {
		if (tz_offset)
			*tz_offset = 0;
		
		return (time_t) 0;
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


/**
 * g_mime_utils_generate_message_id:
 * @fqdn: Fully qualified domain name
 *
 * Generates a unique Message-Id.
 *
 * Returns: a unique string in an addr-spec format suitable for use as
 * a Message-Id.
 **/
char *
g_mime_utils_generate_message_id (const char *fqdn)
{
#ifdef G_THREADS_ENABLED
	static GStaticMutex mutex = G_STATIC_MUTEX_INIT;
#define MUTEX_LOCK()   g_static_mutex_lock (&mutex)
#define MUTEX_UNLOCK() g_static_mutex_unlock (&mutex)
#else
#define MUTEX_LOCK()
#define MUTEX_UNLOCK()
#endif
	static unsigned long int count = 0;
	const char *hostname = NULL;
	char *name = NULL;
	char *msgid;
	
	if (!fqdn) {
#ifdef HAVE_UTSNAME_DOMAINNAME
		struct utsname unam;
		
		uname (&unam);
		
		hostname = unam.nodename;
		
		if (unam.domainname[0])
			name = g_strdup_printf ("%s.%s", hostname, unam.domainname);
#else /* ! HAVE_UTSNAME_DOMAINNAME */
		char host[MAXHOSTNAMELEN + 1];
		
#ifdef HAVE_GETHOSTNAME
		host[MAXHOSTNAMELEN] = '\0';
		if (gethostname (host, MAXHOSTNAMELEN) == 0) {
#ifdef HAVE_GETDOMAINNAME
			size_t domainlen = MAXHOSTNAMELEN;
			char *domain;
			int rv;
			
			domain = g_malloc (domainlen);
			
			while ((rv = getdomainname (domain, domainlen)) == -1 && errno == EINVAL) {
				domainlen += MAXHOSTNAMELEN;
				domain = g_realloc (domain, domainlen);
			}
			
			if (rv == 0 && domain[0]) {
				if (host[0]) {
					name = g_strdup_printf ("%s.%s", host, domain);
					g_free (domain);
				} else {
					name = domain;
				}
			}
#endif /* HAVE_GETDOMAINNAME */
		} else {
			host[0] = '\0';
		}
#endif /* HAVE_GETHOSTNAME */
		hostname = host;
#endif /* HAVE_UTSNAME_DOMAINNAME */
		
#ifdef HAVE_GETADDRINFO
		if (!name && hostname[0]) {
			/* we weren't able to get a domain name */
			struct addrinfo hints, *res;
			
			memset (&hints, 0, sizeof (hints));
			hints.ai_flags = AI_CANONNAME;
			
			if (getaddrinfo (hostname, NULL, &hints, &res) == 0) {
				name = g_strdup (res->ai_canonname);
				freeaddrinfo (res);
			}
		}
#endif /* HAVE_GETADDRINFO */
		
		fqdn = name != NULL ? name : (hostname[0] ? hostname : "localhost.localdomain");
	}
	
	MUTEX_LOCK ();
	msgid = g_strdup_printf ("%lu.%lu.%lu@%s", (unsigned long int) time (NULL),
				 (unsigned long int) getpid (), count++, fqdn);
	MUTEX_UNLOCK ();
	
	g_free (name);
	
	return msgid;
}

static char *
decode_addrspec (const char **in)
{
	const char *word, *inptr;
	GString *addrspec;
	char *str;
	
	decode_lwsp (in);
	inptr = *in;
	
	if (!(word = decode_word (&inptr))) {
		w(g_warning ("No local-part in addr-spec: %s", *in));
		return NULL;
	}
	
	addrspec = g_string_new ("");
	g_string_append_len (addrspec, word, (size_t) (inptr - word));
	
	/* get the rest of the local-part */
	decode_lwsp (&inptr);
	while (*inptr == '.') {
		g_string_append_c (addrspec, *inptr++);
		if ((word = decode_word (&inptr))) {
			g_string_append_len (addrspec, word, (size_t) (inptr - word));
			decode_lwsp (&inptr);
		} else {
			w(g_warning ("Invalid local-part in addr-spec: %s", *in));
			goto exception;
		}
	}
	
	/* we should be at the '@' now... */
	if (*inptr++ != '@') {
		w(g_warning ("Invalid addr-spec; missing '@': %s", *in));
		goto exception;
	}
	
	g_string_append_c (addrspec, '@');
	if (!decode_domain (&inptr, addrspec)) {
		w(g_warning ("No domain in addr-spec: %s", *in));
		goto exception;
	}
	
	str = addrspec->str;
	g_string_free (addrspec, FALSE);
	
	*in = inptr;
	
	return str;
	
 exception:
	
	g_string_free (addrspec, TRUE);
	
	return NULL;
}

static char *
decode_msgid (const char **in)
{
	const char *inptr = *in;
	char *msgid = NULL;
	
	decode_lwsp (&inptr);
	if (*inptr != '<') {
		w(g_warning ("Invalid msg-id; missing '<': %s", *in));
	} else {
		inptr++;
	}
	
	decode_lwsp (&inptr);
	if ((msgid = decode_addrspec (&inptr))) {
		decode_lwsp (&inptr);
		if (*inptr != '>') {
			w(g_warning ("Invalid msg-id; missing '>': %s", *in));
		} else {
			inptr++;
		}
		
		*in = inptr;
	} else {
		w(g_warning ("Invalid msg-id; missing addr-spec: %s", *in));
		*in = inptr;
		while (*inptr && *inptr != '>')
			inptr++;
		
		msgid = g_strndup (*in, (size_t) (inptr - *in));
		*in = inptr;
	}
	
	return msgid;
}


/**
 * g_mime_utils_decode_message_id:
 * @message_id: string containing a message-id
 *
 * Decodes a msg-id as defined by rfc822.
 *
 * Returns: the addr-spec portion of the msg-id.
 **/
char *
g_mime_utils_decode_message_id (const char *message_id)
{
	g_return_val_if_fail (message_id != NULL, NULL);
	
	return decode_msgid (&message_id);
}


/**
 * g_mime_references_decode:
 * @text: string containing a list of msg-ids
 *
 * Decodes a list of msg-ids as in the References and/or In-Reply-To
 * headers defined in rfc822.
 *
 * Returns: a list of referenced msg-ids.
 **/
GMimeReferences *
g_mime_references_decode (const char *text)
{
	GMimeReferences *refs, *tail, *ref;
	const char *word, *inptr = text;
	char *msgid;
	
	g_return_val_if_fail (text != NULL, NULL);
	
	refs = NULL;
	tail = (GMimeReferences *) &refs;
	
	while (*inptr) {
		decode_lwsp (&inptr);
		if (*inptr == '<') {
			/* looks like a msg-id */
			if ((msgid = decode_msgid (&inptr))) {
				ref = g_new (GMimeReferences, 1);
				ref->next = NULL;
				ref->msgid = msgid;
				tail->next = ref;
				tail = ref;
			} else {
				w(g_warning ("Invalid References header: %s", inptr));
				break;
			}
		} else if (*inptr) {
			/* looks like part of a phrase */
			if (!(word = decode_word (&inptr))) {
				w(g_warning ("Invalid References header: %s", inptr));
				break;
			}
		}
	}
	
	return refs;
}


/**
 * g_mime_references_append:
 * @refs: the address of a #GMimeReferences list
 * @msgid: a message-id string
 *
 * Appends a reference to msgid to the list of references.
 **/
void
g_mime_references_append (GMimeReferences **refs, const char *msgid)
{
	GMimeReferences *ref;
	
	g_return_if_fail (refs != NULL);
	g_return_if_fail (msgid != NULL);
	
	ref = (GMimeReferences *) refs;
	while (ref->next)
		ref = ref->next;
	
	ref->next = g_new (GMimeReferences, 1);
	ref->next->msgid = g_strdup (msgid);
	ref->next->next = NULL;
}


/**
 * g_mime_references_free:
 * @refs: a #GMimeReferences list
 *
 * Frees the #GMimeReferences list.
 **/
void
g_mime_references_free (GMimeReferences *refs)
{
	GMimeReferences *ref, *next;
	
	ref = refs;
	while (ref) {
		next = ref->next;
		g_free (ref->msgid);
		g_free (ref);
		ref = next;
	}
}


/**
 * g_mime_references_clear:
 * @refs: address of a #GMimeReferences list
 *
 * Clears the #GMimeReferences list and resets it to %NULL.
 **/
void
g_mime_references_clear (GMimeReferences **refs)
{
	g_return_if_fail (refs != NULL);
	
	g_mime_references_free (*refs);
	*refs = NULL;
}


/**
 * g_mime_references_get_next:
 * @ref: a #GMimeReferences list
 *
 * Advances to the next reference node in the #GMimeReferences list.
 *
 * Returns: the next reference node in the #GMimeReferences list.
 **/
const GMimeReferences *
g_mime_references_get_next (const GMimeReferences *ref)
{
	return ref ? ref->next : NULL;
}


/**
 * g_mime_references_get_message_id:
 * @ref: a #GMimeReferences list
 *
 * Gets the Message-Id reference from the #GMimeReferences node.
 *
 * Returns: the Message-Id reference from the #GMimeReferences node.
 **/
const char *
g_mime_references_get_message_id (const GMimeReferences *ref)
{
	return ref ? ref->msgid : NULL;
}


static gboolean
is_rfc2047_token (const char *inptr, size_t len)
{
	if (len < 8 || strncmp (inptr, "=?", 2) != 0 || strncmp (inptr + len - 2, "?=", 2) != 0)
		return FALSE;
	
	inptr += 2;
	len -= 2;
	
	/* skip past the charset */
	while (*inptr != '?' && len > 0) {
		inptr++;
		len--;
	}
	
	if (*inptr != '?' || len < 4)
		return FALSE;
	
	if (inptr[1] != 'q' && inptr[1] != 'Q' && inptr[1] != 'b' && inptr[1] != 'B')
		return FALSE;
	
	inptr += 2;
	len -= 2;
	
	if (*inptr != '?')
		return FALSE;
	
	return TRUE;
}

static char *
header_fold (const char *in, gboolean structured)
{
	gboolean last_was_lwsp = FALSE;
	register const char *inptr;
	size_t len, outlen, i;
	size_t fieldlen;
	GString *out;
	char *ret;
	
	inptr = in;
	len = strlen (in);
	if (len <= GMIME_FOLD_LEN + 1)
		return g_strdup (in);
	
	out = g_string_new ("");
	fieldlen = strcspn (inptr, ": \t\n");
	g_string_append_len (out, inptr, fieldlen);
	outlen = fieldlen;
	inptr += fieldlen;
	
	while (*inptr && *inptr != '\n') {
		len = strcspn (inptr, " \t\n");
		
		if (len > 1 && outlen + len > GMIME_FOLD_LEN) {
			if (outlen > 1 && out->len > fieldlen + 2) {
				if (last_was_lwsp) {
					if (structured)
						out->str[out->len - 1] = '\t';
					
					g_string_insert_c (out, out->len - 1, '\n');
				} else
					g_string_append (out, "\n\t");
				outlen = 1;
			}
			
			if (!structured && !is_rfc2047_token (inptr, len)) {
				/* check for very long words, just cut them up */
				while (outlen + len > GMIME_FOLD_LEN) {
					for (i = 0; i < GMIME_FOLD_LEN - outlen; i++)
						g_string_append_c (out, inptr[i]);
					inptr += GMIME_FOLD_LEN - outlen;
					len -= GMIME_FOLD_LEN - outlen;
					g_string_append (out, "\n\t");
					outlen = 1;
				}
			} else {
				g_string_append_len (out, inptr, len);
				outlen += len;
				inptr += len;
			}
			last_was_lwsp = FALSE;
		} else if (len > 0) {
			g_string_append_len (out, inptr, len);
			outlen += len;
			inptr += len;
			last_was_lwsp = FALSE;
		} else {
			last_was_lwsp = TRUE;
			if (*inptr == '\t') {
				/* tabs are a good place to fold, odds
				   are that this is where the previous
				   mailer folded it */
				g_string_append (out, "\n\t");
				outlen = 1;
				while (is_blank (*inptr))
					inptr++;
			} else {
				g_string_append_c (out, *inptr++);
				outlen++;
			}
		}
	}
	
	if (*inptr == '\n' && out->str[out->len - 1] != '\n')
		g_string_append_c (out, '\n');
	
	ret = out->str;
	g_string_free (out, FALSE);
	
	return ret;
}


/**
 * g_mime_utils_structured_header_fold:
 * @str: input string
 *
 * Folds a structured header according to the rules in rfc822.
 *
 * Returns: an allocated string containing the folded header.
 **/
char *
g_mime_utils_structured_header_fold (const char *str)
{
	return header_fold (str, TRUE);
}


/**
 * g_mime_utils_unstructured_header_fold:
 * @str: input string
 *
 * Folds an unstructured header according to the rules in rfc822.
 *
 * Returns: an allocated string containing the folded header.
 **/
char *
g_mime_utils_unstructured_header_fold (const char *str)
{
	return header_fold (str, FALSE);
}


/**
 * g_mime_utils_header_fold:
 * @str: input string
 *
 * Folds a structured header according to the rules in rfc822.
 *
 * Returns: an allocated string containing the folded header.
 **/
char *
g_mime_utils_header_fold (const char *str)
{
	return header_fold (str, TRUE);
}


/**
 * g_mime_utils_header_printf:
 * @format: string format
 * @Varargs: arguments
 *
 * Allocates a buffer containing a formatted header specified by the
 * @Varargs.
 *
 * Returns: an allocated string containing the folded header specified
 * by @format and the following arguments.
 **/
char *
g_mime_utils_header_printf (const char *format, ...)
{
	char *buf, *ret;
	va_list ap;
	
	va_start (ap, format);
	buf = g_strdup_vprintf (format, ap);
	va_end (ap);
	
	ret = header_fold (buf, TRUE);
	g_free (buf);
	
	return ret;
}

static gboolean
need_quotes (const char *string)
{
	gboolean quoted = FALSE;
	const char *inptr;
	
	inptr = string;
	
	while (*inptr) {
		if (*inptr == '\\')
			inptr++;
		else if (*inptr == '"')
			quoted = !quoted;
		else if (!quoted && (is_tspecial (*inptr) || *inptr == '.'))
			return TRUE;
		
		if (*inptr)
			inptr++;
	}
	
	return FALSE;
}

/**
 * g_mime_utils_quote_string:
 * @str: input string
 *
 * Quotes @string as needed according to the rules in rfc2045.
 * 
 * Returns: an allocated string containing the escaped and quoted (if
 * needed to be) input string. The decision to quote the string is
 * based on whether or not the input string contains any 'tspecials'
 * as defined by rfc2045.
 **/
char *
g_mime_utils_quote_string (const char *str)
{
	gboolean quote;
	const char *c;
	char *qstring;
	GString *out;
	
	out = g_string_new ("");
	
	if ((quote = need_quotes (str)))
		g_string_append_c (out, '"');
	
	for (c = str; *c; c++) {
		if ((*c == '"' && quote) || *c == '\\')
			g_string_append_c (out, '\\');
		
		g_string_append_c (out, *c);
	}
	
	if (quote)
		g_string_append_c (out, '"');
	
	qstring = out->str;
	g_string_free (out, FALSE);
	
	return qstring;
}


/**
 * g_mime_utils_unquote_string:
 * @str: input string
 * 
 * Unquotes and unescapes a string.
 **/
void
g_mime_utils_unquote_string (char *str)
{
	/* if the string is quoted, unquote it */
	register char *inptr = str;
	int escaped = FALSE;
	int quoted = FALSE;
	
	if (!str)
		return;
	
	while (*inptr) {
		if (*inptr == '\\') {
			if (escaped)
				*str++ = *inptr++;
			else
				inptr++;
			escaped = !escaped;
		} else if (*inptr == '"') {
			if (escaped) {
				*str++ = *inptr++;
				escaped = FALSE;
			} else {
				quoted = !quoted;
				inptr++;
			}
		} else {
			*str++ = *inptr++;
			escaped = FALSE;
		}
	}
	
	*str = '\0';
}


/**
 * g_mime_utils_text_is_8bit:
 * @text: text to check for 8bit chars
 * @len: text length
 *
 * Determines if @text contains 8bit characters within the first @len
 * bytes.
 *
 * Returns: %TRUE if the text contains 8bit characters or %FALSE
 * otherwise.
 **/
gboolean
g_mime_utils_text_is_8bit (const unsigned char *text, size_t len)
{
	register const unsigned char *inptr;
	const unsigned char *inend;
	
	g_return_val_if_fail (text != NULL, FALSE);
	
	inend = text + len;
	for (inptr = text; *inptr && inptr < inend; inptr++)
		if (*inptr > (unsigned char) 127)
			return TRUE;
	
	return FALSE;
}


/**
 * g_mime_utils_best_encoding:
 * @text: text to encode
 * @len: text length
 *
 * Determines the best content encoding for the first @len bytes of
 * @text.
 *
 * Returns: a #GMimeContentEncoding that is determined to be the best
 * encoding type for the specified block of text. ("best" in this
 * particular case means smallest output size)
 **/
GMimeContentEncoding
g_mime_utils_best_encoding (const unsigned char *text, size_t len)
{
	const unsigned char *ch, *inend;
	size_t count = 0;
	
	inend = text + len;
	for (ch = text; ch < inend; ch++)
		if (*ch > (unsigned char) 127)
			count++;
	
	if ((float) count <= len * 0.17)
		return GMIME_CONTENT_ENCODING_QUOTEDPRINTABLE;
	else
		return GMIME_CONTENT_ENCODING_BASE64;
}


/**
 * charset_convert:
 * @cd: iconv converter
 * @inbuf: input text buffer to convert
 * @inleft: length of the input buffer
 * @outp: pointer to output buffer
 * @outlenp: pointer to output buffer length
 * @ninval: the number of invalid bytes in @inbuf
 *
 * Converts the input buffer from one charset to another using the
 * @cd. On completion, @outp will point to the output buffer
 * containing the converted text (nul-terminated), @outlenp will be
 * the size of the @outp buffer (note: not the strlen() of @outp) and
 * @ninval will contain the number of bytes which could not be
 * converted.
 *
 * Bytes which cannot be converted from @inbuf will appear as '?'
 * characters in the output buffer.
 *
 * If *@outp is non-NULL, then it is assumed that it points to a
 * pre-allocated buffer of length *@outlenp. This is done so that the
 * same output buffer can be reused multiple times.
 *
 * Returns: the string length of the output buffer.
 **/
static size_t
charset_convert (iconv_t cd, const char *inbuf, size_t inleft, char **outp, size_t *outlenp, size_t *ninval)
{
	size_t outlen, outleft, rc, n = 0;
	char *outbuf, *out;
	
	if (*outp == NULL) {
		outleft = outlen = (inleft * 2) + 16;
		outbuf = out = g_malloc (outlen + 1);
	} else {
		outleft = outlen = *outlenp;
		outbuf = out = *outp;
	}
	
	do {
		rc = iconv (cd, (char **) &inbuf, &inleft, &outbuf, &outleft);
		if (rc == (size_t) -1) {
			if (errno == EINVAL) {
				/* incomplete sequence at the end of the input buffer */
				n += inleft;
				break;
			}
			
#ifdef G_OS_WIN32
			/* seems that GnuWin32's libiconv 1.9 does not set errno in
			 * the E2BIG case, so we have to fake it */
			if (outleft <= inleft)
				errno = E2BIG;
#endif
			
			if (errno == E2BIG) {
				/* need to grow the output buffer */
				outlen += (inleft * 2) + 16;
				rc = (size_t) (outbuf - out);
				out = g_realloc (out, outlen + 1);
				outleft = outlen - rc;
				outbuf = out + rc;
			} else {
				/* invalid byte(-sequence) in the input buffer */
				*outbuf++ = '?';
				outleft--;
				inleft--;
				inbuf++;
				n++;
			}
		}
	} while (inleft > 0);
	
	iconv (cd, NULL, NULL, &outbuf, &outleft);
	*outbuf++ = '\0';
	
	*outlenp = outlen;
	*outp = out;
	*ninval = n;
	
	return (outbuf - out);
}


#define USER_CHARSETS_INCLUDE_UTF8    (1 << 0)
#define USER_CHARSETS_INCLUDE_LOCALE  (1 << 1)


/**
 * g_mime_utils_decode_8bit:
 * @text: input text in unknown 8bit/multibyte character set
 * @len: input text length
 *
 * Attempts to convert text in an unknown 8bit/multibyte charset into
 * UTF-8 by finding the charset which will convert the most bytes into
 * valid UTF-8 characters as possible. If no exact match can be found,
 * it will choose the best match and convert invalid byte sequences
 * into question-marks (?) in the returned string buffer.
 *
 * Returns: a UTF-8 string representation of @text.
 **/
char *
g_mime_utils_decode_8bit (const char *text, size_t len)
{
	const char **charsets, **user_charsets, *locale, *best;
	size_t outleft, outlen, min, ninval;
	unsigned int included = 0;
	iconv_t cd;
	char *out;
	int i = 0;
	
	g_return_val_if_fail (text != NULL, NULL);
	
	locale = g_mime_locale_charset ();
	if (locale && !g_ascii_strcasecmp (locale, "UTF-8"))
		included |= USER_CHARSETS_INCLUDE_LOCALE;
	
	if ((user_charsets = g_mime_user_charsets ())) {
		while (user_charsets[i])
			i++;
	}
	
	charsets = g_alloca (sizeof (char *) * (i + 3));
	i = 0;
	
	if (user_charsets) {
		while (user_charsets[i]) {
			/* keep a record of whether or not the user-supplied
			 * charsets include UTF-8 and/or the default fallback
			 * charset so that we avoid doubling our efforts for
			 * these 2 charsets. We could have used a hash table
			 * to keep track of unique charsets, but we can
			 * (hopefully) assume that user_charsets is a unique
			 * list of charsets with no duplicates. */
			if (!g_ascii_strcasecmp (user_charsets[i], "UTF-8"))
				included |= USER_CHARSETS_INCLUDE_UTF8;
			
			if (locale && !g_ascii_strcasecmp (user_charsets[i], locale))
				included |= USER_CHARSETS_INCLUDE_LOCALE;
			
			charsets[i] = user_charsets[i];
			i++;
		}
	}
	
	if (!(included & USER_CHARSETS_INCLUDE_UTF8))
		charsets[i++] = "UTF-8";
	
	if (!(included & USER_CHARSETS_INCLUDE_LOCALE))
		charsets[i++] = locale;
	
	charsets[i] = NULL;
	
	min = len;
	best = charsets[0];
	
	outleft = (len * 2) + 16;
	out = g_malloc (outleft + 1);
	
	for (i = 0; charsets[i]; i++) {
		if ((cd = g_mime_iconv_open ("UTF-8", charsets[i])) == (iconv_t) -1)
			continue;
		
		outlen = charset_convert (cd, text, len, &out, &outleft, &ninval);
		
		g_mime_iconv_close (cd);
		
		if (ninval == 0)
			return g_realloc (out, outlen + 1);
		
		if (ninval < min) {
			best = charsets[i];
			min = ninval;
		}
	}
	
	/* if we get here, then none of the charsets fit the 8bit text flawlessly...
	 * try to find the one that fit the best and use that to convert what we can,
	 * replacing any byte we can't convert with a '?' */
	
	if ((cd = g_mime_iconv_open ("UTF-8", best)) == (iconv_t) -1) {
		/* this shouldn't happen... but if we are here, then
		 * it did...  the only thing we can do at this point
		 * is replace the 8bit garbage and pray */
		register const char *inptr = text;
		const char *inend = inptr + len;
		char *outbuf = out;
		
		while (inptr < inend) {
			if (is_ascii (*inptr))
				*outbuf++ = *inptr++;
			else
				*outbuf++ = '?';
		}
		
		*outbuf++ = '\0';
		
		return g_realloc (out, (size_t) (outbuf - out));
	}
	
	outlen = charset_convert (cd, text, len, &out, &outleft, &ninval);
	
	g_mime_iconv_close (cd);
	
	return g_realloc (out, outlen + 1);
}


/* this decodes rfc2047's version of quoted-printable */
static ssize_t
quoted_decode (const unsigned char *in, size_t len, unsigned char *out)
{
	register const unsigned char *inptr;
	register unsigned char *outptr;
	const unsigned char *inend;
	unsigned char c, c1;
	
	inend = in + len;
	outptr = out;
	
	inptr = in;
	while (inptr < inend) {
		c = *inptr++;
		if (c == '=') {
			if (inend - inptr >= 2) {
				c = toupper (*inptr++);
				c1 = toupper (*inptr++);
				*outptr++ = (((c >= 'A' ? c - 'A' + 10 : c - '0') & 0x0f) << 4)
					| ((c1 >= 'A' ? c1 - 'A' + 10 : c1 - '0') & 0x0f);
			} else {
				/* data was truncated */
				return -1;
			}
		} else if (c == '_') {
			/* _'s are an rfc2047 shortcut for encoding spaces */
			*outptr++ = ' ';
		} else {
			*outptr++ = c;
		}
	}
	
	return (ssize_t) (outptr - out);
}

#define is_rfc2047_encoded_word(atom, len) (len >= 7 && !strncmp (atom, "=?", 2) && !strncmp (atom + len - 2, "?=", 2))

static char *
rfc2047_decode_word (const char *in, size_t inlen)
{
	const unsigned char *instart = (const unsigned char *) in;
	const register unsigned char *inptr = instart + 2;
	const unsigned char *inend = instart + inlen - 2;
	unsigned char *decoded;
	const char *charset;
	size_t len, ninval;
	char *charenc, *p;
	guint32 save = 0;
	ssize_t declen;
	int state = 0;
	iconv_t cd;
	char *buf;
	
	/* skip over the charset */
	if (!(inptr = memchr (inptr, '?', inend - inptr)) || inptr[2] != '?')
		return NULL;
	
	inptr++;
	
	switch (*inptr) {
	case 'B':
	case 'b':
		inptr += 2;
		len = (size_t) (inend - inptr);
		decoded = g_alloca (len);
		declen = g_mime_encoding_base64_decode_step (inptr, len, decoded, &state, &save);
		
		if (declen == -1) {
			d(fprintf (stderr, "encountered broken 'Q' encoding\n"));
			return NULL;
		}
		break;
	case 'Q':
	case 'q':
		inptr += 2;
		len = (size_t) (inend - inptr);
		decoded = g_alloca (len);
		declen = quoted_decode (inptr, len, decoded);
		
		if (declen == -1) {
			d(fprintf (stderr, "encountered broken 'Q' encoding\n"));
			return NULL;
		}
		break;
	default:
		d(fprintf (stderr, "unknown encoding\n"));
		return NULL;
	}
	
	len = (inptr - 3) - (instart + 2);
	charenc = g_alloca (len + 1);
	memcpy (charenc, in + 2, len);
	charenc[len] = '\0';
	charset = charenc;
	
	/* rfc2231 updates rfc2047 encoded words...
	 * The ABNF given in RFC 2047 for encoded-words is:
	 *   encoded-word := "=?" charset "?" encoding "?" encoded-text "?="
	 * This specification changes this ABNF to:
	 *   encoded-word := "=?" charset ["*" language] "?" encoding "?" encoded-text "?="
	 */
	
	/* trim off the 'language' part if it's there... */
	if ((p = strchr (charset, '*')))
		*p = '\0';
	
	/* slight optimization? */
	if (!g_ascii_strcasecmp (charset, "UTF-8")) {
		p = (char *) decoded;
		len = declen;
		
		//while (!g_utf8_validate (p, len, (const char **) &p)) {
		//	len = declen - (p - (char *) decoded);
		//	*p = '?';
		//}
		
		return g_strndup ((char *) decoded, declen);
	}
	
	if (!charset[0] || (cd = g_mime_iconv_open ("UTF-8", charset)) == (iconv_t) -1) {
		w(g_warning ("Cannot convert from %s to UTF-8, header display may "
			     "be corrupt: %s", charset[0] ? charset : "unspecified charset",
			     g_strerror (errno)));
		
		return g_mime_utils_decode_8bit ((char *) decoded, declen);
	}
	
	len = declen;
	buf = g_malloc (len + 1);
	
	charset_convert (cd, (char *) decoded, declen, &buf, &len, &ninval);
	
	g_mime_iconv_close (cd);
	
#if w(!)0
	if (ninval > 0) {
		g_warning ("Failed to completely convert \"%.*s\" to UTF-8, display may be "
			   "corrupt: %s", declen, decoded, g_strerror (errno));
	}
#endif
	
	return buf;
}


/**
 * g_mime_utils_header_decode_text:
 * @text: header text to decode
 *
 * Decodes an rfc2047 encoded 'text' header.
 *
 * Note: See g_mime_set_user_charsets() for details on how charset
 * conversion is handled for unencoded 8bit text and/or wrongly
 * specified rfc2047 encoded-word tokens.
 *
 * Returns: a newly allocated UTF-8 string representing the the decoded
 * header.
 **/
char *
g_mime_utils_header_decode_text (const char *text)
{
	gboolean enable_rfc2047_workarounds = _g_mime_enable_rfc2047_workarounds ();
	register const char *inptr = text;
	gboolean encoded = FALSE;
	const char *lwsp, *word;
	size_t nlwsp, n;
	gboolean ascii;
	char *decoded;
	GString *out;
	
	if (text == NULL)
		return g_strdup ("");
	
	out = g_string_sized_new (strlen (text) + 1);
	
	while (*inptr != '\0') {
		lwsp = inptr;
		while (is_lwsp (*inptr))
			inptr++;
		
		nlwsp = (size_t) (inptr - lwsp);
		
		if (*inptr != '\0') {
			word = inptr;
			ascii = TRUE;
			
			if (enable_rfc2047_workarounds) {
				if (!strncmp (inptr, "=?", 2)) {
					inptr += 2;
					
					/* skip past the charset (if one is even declared, sigh) */
					while (*inptr && *inptr != '?') {
						ascii = ascii && is_ascii (*inptr);
						inptr++;
					}
					
					/* sanity check encoding type */
					if (inptr[0] != '?' || !strchr ("BbQq", inptr[1]) || inptr[2] != '?')
						goto non_rfc2047;
					
					inptr += 3;
					
					/* find the end of the rfc2047 encoded word token */
					while (*inptr && strncmp (inptr, "?=", 2) != 0) {
						ascii = ascii && is_ascii (*inptr);
						inptr++;
					}
					
					if (!strncmp (inptr, "?=", 2))
						inptr += 2;
				} else {
				non_rfc2047:
					/* stop if we encounter a possible rfc2047 encoded
					 * token even if it's inside another word, sigh. */
					while (*inptr && !is_lwsp (*inptr) &&
					       strncmp (inptr, "=?", 2) != 0) {
						ascii = ascii && is_ascii (*inptr);
						inptr++;
					}
				}
			} else {
				while (*inptr && !is_lwsp (*inptr)) {
					ascii = ascii && is_ascii (*inptr);
					inptr++;
				}
			}
			
			n = (size_t) (inptr - word);
			if (is_rfc2047_encoded_word (word, n)) {
				if ((decoded = rfc2047_decode_word (word, n))) {
					/* rfc2047 states that you must ignore all
					 * whitespace between encoded words */
					if (!encoded)
						g_string_append_len (out, lwsp, nlwsp);
					
					g_string_append (out, decoded);
					g_free (decoded);
					
					encoded = TRUE;
				} else {
					/* append lwsp and invalid rfc2047 encoded-word token */
					g_string_append_len (out, lwsp, nlwsp + n);
					encoded = FALSE;
				}
			} else {
				/* append lwsp */
				g_string_append_len (out, lwsp, nlwsp);
				
				/* append word token */
				if (!ascii) {
					/* *sigh* I hate broken mailers... */
					decoded = g_mime_utils_decode_8bit (word, n);
					g_string_append (out, decoded);
					g_free (decoded);
				} else {
					g_string_append_len (out, word, n);
				}
				
				encoded = FALSE;
			}
		} else {
			/* appending trailing lwsp */
			g_string_append_len (out, lwsp, nlwsp);
			break;
		}
	}
	
	decoded = out->str;
	g_string_free (out, FALSE);
	
	return decoded;
}


/**
 * g_mime_utils_header_decode_phrase:
 * @phrase: header to decode
 *
 * Decodes an rfc2047 encoded 'phrase' header.
 *
 * Note: See g_mime_set_user_charsets() for details on how charset
 * conversion is handled for unencoded 8bit text and/or wrongly
 * specified rfc2047 encoded-word tokens.
 *
 * Returns: a newly allocated UTF-8 string representing the the decoded
 * header.
 **/
char *
g_mime_utils_header_decode_phrase (const char *phrase)
{
	register const char *inptr = phrase;
	gboolean encoded = FALSE;
	const char *lwsp, *text;
	size_t nlwsp, n;
	gboolean ascii;
	char *decoded;
	GString *out;
	
	if (phrase == NULL)
		return g_strdup ("");
	
	out = g_string_sized_new (strlen (phrase) + 1);
	
	while (*inptr != '\0') {
		lwsp = inptr;
		while (is_lwsp (*inptr))
			inptr++;
		
		nlwsp = (size_t) (inptr - lwsp);
		
		text = inptr;
		if (is_atom (*inptr)) {
			while (is_atom (*inptr))
				inptr++;
			
			n = (size_t) (inptr - text);
			if (is_rfc2047_encoded_word (text, n)) {
				if ((decoded = rfc2047_decode_word (text, n))) {
					/* rfc2047 states that you must ignore all
					 * whitespace between encoded words */
					if (!encoded)
						g_string_append_len (out, lwsp, nlwsp);
					
					g_string_append (out, decoded);
					g_free (decoded);
					
					encoded = TRUE;
				} else {
					/* append lwsp and invalid rfc2047 encoded-word token */
					g_string_append_len (out, lwsp, nlwsp + n);
					encoded = FALSE;
				}
			} else {
				/* append lwsp and atom token */
				g_string_append_len (out, lwsp, nlwsp + n);
				encoded = FALSE;
			}
		} else {
			g_string_append_len (out, lwsp, nlwsp);
			
			ascii = TRUE;
			while (*inptr && !is_lwsp (*inptr)) {
				ascii = ascii && is_ascii (*inptr);
				inptr++;
			}
			
			n = (size_t) (inptr - text);
			
			if (!ascii) {
				/* *sigh* I hate broken mailers... */
				decoded = g_mime_utils_decode_8bit (text, n);
				g_string_append (out, decoded);
				g_free (decoded);
			} else {
				g_string_append_len (out, text, n);
			}
			
			encoded = FALSE;
		}
	}
	
	decoded = out->str;
	g_string_free (out, FALSE);
	
	return decoded;
}


/* rfc2047 version of quoted-printable */
static size_t
quoted_encode (const char *in, size_t len, unsigned char *out, gushort safemask)
{
	register const unsigned char *inptr = (const unsigned char *) in;
	const unsigned char *inend = inptr + len;
	register unsigned char *outptr = out;
	unsigned char c;
	
	while (inptr < inend) {
		c = *inptr++;
		if (c == ' ') {
			*outptr++ = '_';
		} else if (c != '_' && gmime_special_table[c] & safemask) {
			*outptr++ = c;
		} else {
			*outptr++ = '=';
			*outptr++ = tohex[(c >> 4) & 0xf];
			*outptr++ = tohex[c & 0xf];
		}
	}
	
	return (outptr - out);
}

static void
rfc2047_encode_word (GString *string, const char *word, size_t len,
		     const char *charset, gushort safemask)
{
	register char *inptr, *outptr;
	iconv_t cd = (iconv_t) -1;
	unsigned char *encoded;
	size_t enclen, pos;
	char *uword = NULL;
	guint32 save = 0;
	int state = 0;
	char encoding;
	
	if (g_ascii_strcasecmp (charset, "UTF-8") != 0)
		cd = g_mime_iconv_open (charset, "UTF-8");
	
	if (cd != (iconv_t) -1) {
		uword = g_mime_iconv_strndup (cd, (char *) word, len);
		g_mime_iconv_close (cd);
	}
	
	if (uword) {
		len = strlen (uword);
		word = uword;
	} else {
		charset = "UTF-8";
	}
	
	switch (g_mime_utils_best_encoding ((const unsigned char *) word, len)) {
	case GMIME_CONTENT_ENCODING_BASE64:
		enclen = GMIME_BASE64_ENCODE_LEN (len);
		encoded = g_alloca (enclen + 1);
		
		encoding = 'b';
		
		pos = g_mime_encoding_base64_encode_close ((const unsigned char *) word, len, encoded, &state, &save);
		encoded[pos] = '\0';
		
		/* remove \n chars as headers need to be wrapped differently */
		if (G_UNLIKELY ((inptr = strchr ((char *) encoded, '\n')))) {
			outptr = inptr++;
			while (G_LIKELY (*inptr)) {
				if (G_LIKELY (*inptr != '\n'))
					*outptr++ = *inptr;
				
				inptr++;
			}
			
			*outptr = '\0';
		}
		
		break;
	case GMIME_CONTENT_ENCODING_QUOTEDPRINTABLE:
		enclen = GMIME_QP_ENCODE_LEN (len);
		encoded = g_alloca (enclen + 1);
		
		encoding = 'q';
		
		pos = quoted_encode (word, len, encoded, safemask);
		encoded[pos] = '\0';
		
		break;
	default:
		encoded = NULL;
		encoding = '\0';
		g_assert_not_reached ();
	}
	
	g_free (uword);
	
	g_string_append_printf (string, "=?%s?%c?%s?=", charset, encoding, encoded);
}


typedef enum {
	WORD_ATOM,
	WORD_QSTRING,
	WORD_2047
} rfc822_word_t;

typedef struct _rfc822_word {
	struct _rfc822_word *next;
	const char *start, *end;
	rfc822_word_t type;
	int encoding;
} rfc822_word;

#define rfc822_word_free(word) g_slice_free (rfc822_word, word)
#define rfc822_word_new() g_slice_new (rfc822_word)

/* okay, so 'unstructured text' fields don't actually contain 'word'
 * tokens, but we can group stuff similarly... */
static rfc822_word *
rfc2047_encode_get_rfc822_words (const char *in, gboolean phrase)
{
	rfc822_word *words, *tail, *word;
	rfc822_word_t type = WORD_ATOM;
	const char *inptr, *start, *last;
	int count = 0, encoding = 0;
	
	words = NULL;
	tail = (rfc822_word *) &words;
	
	last = start = inptr = in;
	while (inptr && *inptr) {
		const char *newinptr;
		gunichar c;
		
		newinptr = g_utf8_next_char (inptr);
		c = g_utf8_get_char (inptr);
		if (newinptr == NULL || !g_unichar_validate (c)) {
			w(g_warning ("Invalid UTF-8 sequence encountered"));
			inptr++;
			continue;
		}
		
		inptr = newinptr;
		
		if (c < 256 && is_lwsp (c)) {
			if (count > 0) {
				word = rfc822_word_new ();
				word->next = NULL;
				word->start = start;
				word->end = last;
				word->type = type;
				word->encoding = encoding;
				
				tail->next = word;
				tail = word;
				count = 0;
			}
			
			start = inptr;
			type = WORD_ATOM;
			encoding = 0;
		} else {
			count++;
			if (phrase && c < 128) {
				/* phrases can have qstring words */
				if (!is_atom (c))
					type = MAX (type, WORD_QSTRING);
			} else if (c > 127 && c < 256) {
				type = WORD_2047;
				encoding = MAX (encoding, 1);
			} else if (c >= 256) {
				type = WORD_2047;
				encoding = 2;
			}
			
			if (count >= GMIME_FOLD_PREENCODED) {
				word = rfc822_word_new ();
				word->next = NULL;
				word->start = start;
				word->end = inptr;
				word->type = type;
				word->encoding = encoding;
				
				tail->next = word;
				tail = word;
				count = 0;
				
				/* Note: don't reset 'type' as it
				 * needs to be preserved when breaking
				 * long words */
				start = inptr;
				encoding = 0;
			}
		}
		
		last = inptr;
	}
	
	if (count > 0) {
		word = rfc822_word_new ();
		word->next = NULL;
		word->start = start;
		word->end = last;
		word->type = type;
		word->encoding = encoding;
		
		tail->next = word;
		tail = word;
	}
	
#if d(!)0
	printf ("rfc822 word tokens:\n");
	word = words;
	while (word) {
		printf ("\t'%.*s'; type=%d, encoding=%d\n",
			word->end - word->start, word->start,
			word->type, word->encoding);
		
		word = word->next;
	}
#endif
	
	return words;
}

#define MERGED_WORD_LT_FOLDLEN(wlen, type) ((type) == WORD_2047 ? (wlen) < GMIME_FOLD_PREENCODED : (wlen) < (GMIME_FOLD_LEN - 8))

static gboolean
should_merge_words (rfc822_word *word, rfc822_word *next)
{
	switch (word->type) {
	case WORD_ATOM:
		if (next->type == WORD_2047)
			return FALSE;
		
		return (MERGED_WORD_LT_FOLDLEN (next->end - word->start, next->type));
	case WORD_QSTRING:
		/* avoid merging with words that need to be rfc2047 encoded */
		if (next->type == WORD_2047)
			return FALSE;
		
		return (MERGED_WORD_LT_FOLDLEN (next->end - word->start, WORD_QSTRING));
	case WORD_2047:
		if (next->type == WORD_ATOM) {
			/* whether we merge or not is dependent upon:
			 * 1. the number of atoms in a row after 'word'
			 * 2. if there is another encword after the string of atoms.
			 */
			int natoms = 0;
			
			while (next && next->type == WORD_ATOM) {
				next = next->next;
				natoms++;
			}
			
			/* if all the words after the encword are atoms, don't merge */
			if (!next || natoms > 3)
				return FALSE;
		}
		
		/* avoid merging with qstrings */
		if (next->type == WORD_QSTRING)
			return FALSE;
		
		return (MERGED_WORD_LT_FOLDLEN (next->end - word->start, WORD_2047));
	default:
		return FALSE;
	}
}

static void
rfc2047_encode_merge_rfc822_words (rfc822_word **wordsp)
{
	rfc822_word *word, *next, *words = *wordsp;
	
	/* first pass: merge qstrings with adjacent qstrings and encwords with adjacent encwords */
	word = words;
	while (word && word->next) {
		next = word->next;
		
		if (word->type != WORD_ATOM && word->type == next->type &&
		    MERGED_WORD_LT_FOLDLEN (next->end - word->start, word->type)) {
			/* merge the words */
			word->encoding = MAX (word->encoding, next->encoding);
			
			word->end = next->end;
			word->next = next->next;
			
			rfc822_word_free (next);
			
			next = word;
		}
		
		word = next;
	}
	
	/* second pass: now merge atoms with the other words */
	word = words;
	while (word && word->next) {
		next = word->next;
		
		if (should_merge_words (word, next)) {
			/* the resulting word type is the MAX of the 2 types */
			word->type = MAX (word->type, next->type);
			
			word->encoding = MAX (word->encoding, next->encoding);
			
			word->end = next->end;
			word->next = next->next;
			
			rfc822_word_free (next);
			
			continue;
		}
		
		word = next;
	}
	
	*wordsp = words;
}

static void
g_string_append_len_quoted (GString *out, const char *in, size_t len)
{
	register const char *inptr;
	const char *inend;
	
	g_string_append_c (out, '"');
	
	inptr = in;
	inend = in + len;
	
	while (inptr < inend) {
		if (*inptr == '"' || *inptr == '\\')
			g_string_append_c (out, '\\');
		
		g_string_append_c (out, *inptr);
		
		inptr++;
	}
	
	g_string_append_c (out, '"');
}

static char *
rfc2047_encode (const char *in, gushort safemask)
{
	rfc822_word *words, *word, *prev = NULL;
	const char **charsets, *charset;
	const char *start;
	GMimeCharset mask;
	GString *out;
	char *outstr;
	size_t len;
	int i;
	
	if (!(words = rfc2047_encode_get_rfc822_words (in, safemask & IS_PSAFE)))
		return g_strdup (in);
	
	rfc2047_encode_merge_rfc822_words (&words);
	
	charsets = g_mime_user_charsets ();
	
	out = g_string_new ("");
	
	/* output words now with spaces between them */
	word = words;
	while (word) {
		/* append correct number of spaces between words */
		if (prev && !(prev->type == WORD_2047 && word->type == WORD_2047)) {
			/* one or both of the words are not encoded so we write the spaces out untouched */
			len = word->start - prev->end;
			g_string_append_len (out, prev->end, len);
		}
		
		switch (word->type) {
		case WORD_ATOM:
			g_string_append_len (out, word->start, (size_t) (word->end - word->start));
			break;
		case WORD_QSTRING:
			g_assert (safemask & IS_PSAFE);
			g_string_append_len_quoted (out, word->start, (size_t) (word->end - word->start));
			break;
		case WORD_2047:
			if (prev && prev->type == WORD_2047) {
				/* include the whitespace chars between these 2 words in the
				   resulting rfc2047 encoded word. */
				len = word->end - prev->end;
				start = prev->end;
				
				/* encoded words need to be separated by linear whitespace */
				g_string_append_c (out, ' ');
			} else {
				len = word->end - word->start;
				start = word->start;
			}
			
			switch (word->encoding) {
			case 0: /* us-ascii */
				rfc2047_encode_word (out, start, len, "us-ascii", safemask);
				break;
			case 1: /* iso-8859-1 */
				rfc2047_encode_word (out, start, len, "iso-8859-1", safemask);
				break;
			default:
				charset = NULL;
				g_mime_charset_init (&mask);
				g_mime_charset_step (&mask, start, len);
				
				for (i = 0; charsets && charsets[i]; i++) {
					if (g_mime_charset_can_encode (&mask, charsets[i], start, len)) {
						charset = charsets[i];
						break;
					}
				}
				
				if (!charset)
					charset = g_mime_charset_best_name (&mask);
				
				rfc2047_encode_word (out, start, len, charset, safemask);
				break;
			}
			
			break;
		}
		
		rfc822_word_free (prev);
		
		prev = word;
		word = word->next;
	}
	
	rfc822_word_free (prev);
	
	outstr = out->str;
	g_string_free (out, FALSE);
	
	return outstr;
}


/**
 * g_mime_utils_header_encode_phrase:
 * @phrase: phrase to encode
 *
 * Encodes a 'phrase' header according to the rules in rfc2047.
 *
 * Returns: the encoded 'phrase'. Useful for encoding internet
 * addresses.
 **/
char *
g_mime_utils_header_encode_phrase (const char *phrase)
{
	if (phrase == NULL)
		return NULL;
	
	return rfc2047_encode (phrase, IS_PSAFE);
}


/**
 * g_mime_utils_header_encode_text:
 * @text: text to encode
 *
 * Encodes a 'text' header according to the rules in rfc2047.
 *
 * Returns: the encoded header. Useful for encoding
 * headers like "Subject".
 **/
char *
g_mime_utils_header_encode_text (const char *text)
{
	if (text == NULL)
		return NULL;
	
	return rfc2047_encode (text, IS_ESAFE);
}
