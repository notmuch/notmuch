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

#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <time.h>
#include <sys/time.h>
#include <sys/types.h>

#include "compat.h"
#include "parse-time-string.h"

/*
 * IMPLEMENTATION DETAILS
 *
 * At a high level, the parsing is done in two phases: 1) actual
 * parsing of the input string and storing the parsed data into
 * 'struct state', and 2) processing of the data in 'struct state'
 * according to current time (or provided reference time) and
 * rounding. This is evident in the main entry point function
 * parse_time_string().
 *
 * 1) The parsing phase - parse_input()
 *
 * Parsing is greedy and happens from left to right. The parsing is as
 * unambiguous as possible; only unambiguous date/time formats are
 * accepted. Redundant or contradictory absolute date/time in the
 * input (e.g. date specified multiple times/ways) is not
 * accepted. Relative date/time on the other hand just accumulates if
 * present multiple times (e.g. "5 days 5 days" just turns into 10
 * days).
 *
 * Parsing decisions are made on the input format, not value. For
 * example, "20/5/2005" fails because the recognized format here is
 * MM/D/YYYY, even though the values would suggest DD/M/YYYY.
 *
 * Parsing is mostly stateless in the sense that parsing decisions are
 * not made based on the values of previously parsed data, or whether
 * certain data is present in the first place. (There are a few
 * exceptions to the latter part, though, such as parsing of time zone
 * that would otherwise look like plain time.)
 *
 * When the parser encounters a number that is not greedily parsed as
 * part of a format, the interpretation is postponed until the next
 * token is parsed. The parser for the next token may consume the
 * previously postponed number. For example, when parsing "20 May" the
 * meaning of "20" is not known until "May" is parsed. If the parser
 * for the next token does not consume the postponed number, the
 * number is handled as a "lone" number before parser for the next
 * token finishes.
 *
 * 2) The processing phase - create_output()
 *
 * Once the parser in phase 1 has finished, 'struct state' contains
 * all the information from the input string, and it's no longer
 * needed. Since the parser does not even handle the concept of "now",
 * the processing initializes the fields referring to the current
 * date/time.
 *
 * If requested, the result is rounded towards past or future. The
 * idea behind rounding is to support parsing date/time ranges in an
 * obvious way. For example, for a range defined as two dates (without
 * time), one would typically want to have an inclusive range from the
 * beginning of start date to the end of the end date. The caller
 * would use rounding towards past in the start date, and towards
 * future in the end date.
 *
 * The absolute date and time is shifted by the relative date and
 * time, and time zone adjustments are made. Daylight saving time
 * (DST) is specifically *not* handled at all.
 *
 * Finally, the result is stored to time_t.
 */

#define unused(x) x __attribute__ ((unused))

/* XXX: Redefine these to add i18n support. The keyword table uses
 * N_() to mark strings to be translated; they are accessed
 * dynamically using _(). */
#define _(s) (s)	/* i18n: define as gettext (s) */
#define N_(s) (s)	/* i18n: define as gettext_noop (s) */

#define ARRAY_SIZE(a) (sizeof (a) / sizeof (a[0]))

/*
 * Field indices in the tm and set arrays of struct state.
 *
 * NOTE: There's some code that depends on the ordering of this enum.
 */
enum field {
    /* Keep SEC...YEAR in this order. */
    TM_ABS_SEC,		/* seconds */
    TM_ABS_MIN,		/* minutes */
    TM_ABS_HOUR,	/* hours */
    TM_ABS_MDAY,	/* day of the month */
    TM_ABS_MON,		/* month */
    TM_ABS_YEAR,	/* year */

    TM_WDAY,		/* day of the week. special: may be relative */
    TM_ABS_ISDST,	/* daylight saving time */

    TM_AMPM,		/* am vs. pm */
    TM_TZ,		/* timezone in minutes */

    /* Keep SEC...YEAR in this order. */
    TM_REL_SEC,		/* seconds relative to absolute or reference time */
    TM_REL_MIN,		/* minutes ... */
    TM_REL_HOUR,	/* hours ... */
    TM_REL_DAY,		/* days ... */
    TM_REL_MON,		/* months ... */
    TM_REL_YEAR,	/* years ... */
    TM_REL_WEEK,	/* weeks ... */

    TM_NONE,		/* not a field */

    TM_SIZE = TM_NONE,
    TM_FIRST_ABS = TM_ABS_SEC,
    TM_FIRST_REL = TM_REL_SEC,
};

/* Values for the set array of struct state. */
enum field_set {
    FIELD_UNSET,	/* The field has not been touched by parser. */
    FIELD_SET,		/* The field has been set by parser. */
    FIELD_NOW,		/* The field will be set to reference time. */
};

static enum field
next_abs_field (enum field field)
{
    /* NOTE: Depends on the enum ordering. */
    return field < TM_ABS_YEAR ? field + 1 : TM_NONE;
}

static enum field
abs_to_rel_field (enum field field)
{
    assert (field <= TM_ABS_YEAR);

    /* NOTE: Depends on the enum ordering. */
    return field + (TM_FIRST_REL - TM_FIRST_ABS);
}

/* Get the smallest acceptable value for field. */
static int
get_field_epoch_value (enum field field)
{
    if (field == TM_ABS_MDAY || field == TM_ABS_MON)
	return 1;
    else if (field == TM_ABS_YEAR)
	return 1970;
    else
	return 0;
}

/* The parsing state. */
struct state {
    int tm[TM_SIZE];			/* parsed date and time */
    enum field_set set[TM_SIZE];	/* set status of tm */

    enum field last_field;	/* Previously set field. */
    char delim;

    int postponed_length;	/* Number of digits in postponed value. */
    int postponed_value;
    char postponed_delim;	/* The delimiter preceding postponed number. */
};

/*
 * Helpers for postponed numbers.
 *
 * postponed_length is the number of digits in postponed value. 0
 * means there is no postponed number. -1 means there is a postponed
 * number, but it comes from a keyword, and it doesn't have digits.
 */
static int
get_postponed_length (struct state *state)
{
    return state->postponed_length;
}

/*
 * Consume a previously postponed number. Return true if a number was
 * in fact postponed, false otherwise. Store the postponed number's
 * value in *v, length in the input string in *n (or -1 if the number
 * was written out and parsed as a keyword), and the preceding
 * delimiter to *d. If a number was not postponed, *v, *n and *d are
 * unchanged.
 */
static bool
consume_postponed_number (struct state *state, int *v, int *n, char *d)
{
    if (!state->postponed_length)
	return false;

    if (n)
	*n = state->postponed_length;

    if (v)
	*v = state->postponed_value;

    if (d)
	*d = state->postponed_delim;

    state->postponed_length = 0;
    state->postponed_value = 0;
    state->postponed_delim = 0;

    return true;
}

static int parse_postponed_number (struct state *state, enum field next_field);

/*
 * Postpone a number to be handled later. If one exists already,
 * handle it first. n may be -1 to indicate a keyword that has no
 * number length.
 */
static int
set_postponed_number (struct state *state, int v, int n)
{
    int r;
    char d = state->delim;

    /* Parse a previously postponed number, if any. */
    r = parse_postponed_number (state, TM_NONE);
    if (r)
	return r;

    state->postponed_length = n;
    state->postponed_value = v;
    state->postponed_delim = d;

    return 0;
}

static void
set_delim (struct state *state, char delim)
{
    state->delim = delim;
}

static void
unset_delim (struct state *state)
{
    state->delim = 0;
}

/*
 * Field set/get/mod helpers.
 */

/* Return true if field has been set. */
static bool
is_field_set (struct state *state, enum field field)
{
    assert (field < ARRAY_SIZE (state->tm));

    return state->set[field] != FIELD_UNSET;
}

static void
unset_field (struct state *state, enum field field)
{
    assert (field < ARRAY_SIZE (state->tm));

    state->set[field] = FIELD_UNSET;
    state->tm[field] = 0;
}

/*
 * Set field to value. A field can only be set once to ensure the
 * input does not contain redundant and potentially conflicting data.
 */
static int
set_field (struct state *state, enum field field, int value)
{
    int r;

    /* Fields can only be set once. */
    if (is_field_set (state, field))
	return -PARSE_TIME_ERR_ALREADYSET;

    state->set[field] = FIELD_SET;

    /* Parse a previously postponed number, if any. */
    r = parse_postponed_number (state, field);
    if (r)
	return r;

    unset_delim (state);

    state->tm[field] = value;
    state->last_field = field;

    return 0;
}

/*
 * Mark n fields in fields to be set to the reference date/time in the
 * specified time zone, or local timezone if not specified. The fields
 * will be initialized after parsing is complete and timezone is
 * known.
 */
static int
set_fields_to_now (struct state *state, enum field *fields, size_t n)
{
    size_t i;
    int r;

    for (i = 0; i < n; i++) {
	r = set_field (state, fields[i], 0);
	if (r)
	    return r;
	state->set[fields[i]] = FIELD_NOW;
    }

    return 0;
}

/* Modify field by adding value to it. To be used on relative fields,
 * which can be modified multiple times (to accumulate). */
static int
add_to_field (struct state *state, enum field field, int value)
{
    int r;

    assert (field < ARRAY_SIZE (state->tm));

    state->set[field] = FIELD_SET;

    /* Parse a previously postponed number, if any. */
    r = parse_postponed_number (state, field);
    if (r)
	return r;

    unset_delim (state);

    state->tm[field] += value;
    state->last_field = field;

    return 0;
}

/*
 * Get field value. Make sure the field is set before query. It's most
 * likely an error to call this while parsing (for example fields set
 * as FIELD_NOW will only be set to some value after parsing).
 */
static int
get_field (struct state *state, enum field field)
{
    assert (field < ARRAY_SIZE (state->tm));

    return state->tm[field];
}

/*
 * Validity checkers.
 */
static bool is_valid_12hour (int h)
{
    return h >= 1 && h <= 12;
}

static bool is_valid_time (int h, int m, int s)
{
    /* Allow 24:00:00 to denote end of day. */
    if (h == 24 && m == 0 && s == 0)
	return true;

    return h >= 0 && h <= 23 && m >= 0 && m <= 59 && s >= 0 && s <= 59;
}

static bool is_valid_mday (int mday)
{
    return mday >= 1 && mday <= 31;
}

static bool is_valid_mon (int mon)
{
    return mon >= 1 && mon <= 12;
}

static bool is_valid_year (int year)
{
    return year >= 1970;
}

static bool is_valid_date (int year, int mon, int mday)
{
    return is_valid_year (year) && is_valid_mon (mon) && is_valid_mday (mday);
}

/* Unset indicator for time and date set helpers. */
#define UNSET -1

/* Time set helper. No input checking. Use UNSET (-1) to leave unset. */
static int
set_abs_time (struct state *state, int hour, int min, int sec)
{
    int r;

    if (hour != UNSET) {
	if ((r = set_field (state, TM_ABS_HOUR, hour)))
	    return r;
    }

    if (min != UNSET) {
	if ((r = set_field (state, TM_ABS_MIN, min)))
	    return r;
    }

    if (sec != UNSET) {
	if ((r = set_field (state, TM_ABS_SEC, sec)))
	    return r;
    }

    return 0;
}

/* Date set helper. No input checking. Use UNSET (-1) to leave unset. */
static int
set_abs_date (struct state *state, int year, int mon, int mday)
{
    int r;

    if (year != UNSET) {
	if ((r = set_field (state, TM_ABS_YEAR, year)))
	    return r;
    }

    if (mon != UNSET) {
	if ((r = set_field (state, TM_ABS_MON, mon)))
	    return r;
    }

    if (mday != UNSET) {
	if ((r = set_field (state, TM_ABS_MDAY, mday)))
	    return r;
    }

    return 0;
}

/*
 * Keyword parsing and handling.
 */
struct keyword;
typedef int (*setter_t)(struct state *state, struct keyword *kw);

struct keyword {
    const char *name;	/* keyword */
    enum field field;	/* field to set, or FIELD_NONE if N/A */
    int value;		/* value to set, or 0 if N/A */
    setter_t set;	/* function to use for setting, if non-NULL */
};

/*
 * Setter callback functions for keywords.
 */
static int
kw_set_rel (struct state *state, struct keyword *kw)
{
    int multiplier = 1;

    /* Get a previously set multiplier, if any. */
    consume_postponed_number (state, &multiplier, NULL, NULL);

    /* Accumulate relative field values. */
    return add_to_field (state, kw->field, multiplier * kw->value);
}

static int
kw_set_number (struct state *state, struct keyword *kw)
{
    /* -1 = no length, from keyword. */
    return set_postponed_number (state, kw->value, -1);
}

static int
kw_set_month (struct state *state, struct keyword *kw)
{
    int n = get_postponed_length (state);

    /* Consume postponed number if it could be mday. This handles "20
     * January". */
    if (n == 1 || n == 2) {
	int r, v;

	consume_postponed_number (state, &v, NULL, NULL);

	if (!is_valid_mday (v))
	    return -PARSE_TIME_ERR_INVALIDDATE;

	r = set_field (state, TM_ABS_MDAY, v);
	if (r)
	    return r;
    }

    return set_field (state, kw->field, kw->value);
}

static int
kw_set_ampm (struct state *state, struct keyword *kw)
{
    int n = get_postponed_length (state);

    /* Consume postponed number if it could be hour. This handles
     * "5pm". */
    if (n == 1 || n == 2) {
	int r, v;

	consume_postponed_number (state, &v, NULL, NULL);

	if (!is_valid_12hour (v))
	    return -PARSE_TIME_ERR_INVALIDTIME;

	r = set_abs_time (state, v, 0, 0);
	if (r)
	    return r;
    }

    return set_field (state, kw->field, kw->value);
}

static int
kw_set_timeofday (struct state *state, struct keyword *kw)
{
    return set_abs_time (state, kw->value, 0, 0);
}

static int
kw_set_today (struct state *state, unused (struct keyword *kw))
{
    enum field fields[] = { TM_ABS_YEAR, TM_ABS_MON, TM_ABS_MDAY };

    return set_fields_to_now (state, fields, ARRAY_SIZE (fields));
}

static int
kw_set_now (struct state *state, unused (struct keyword *kw))
{
    enum field fields[] = { TM_ABS_HOUR, TM_ABS_MIN, TM_ABS_SEC };

    return set_fields_to_now (state, fields, ARRAY_SIZE (fields));
}

static int
kw_set_ordinal (struct state *state, struct keyword *kw)
{
    int n, v;

    /* Require a postponed number. */
    if (!consume_postponed_number (state, &v, &n, NULL))
	return -PARSE_TIME_ERR_DATEFORMAT;

    /* Ordinals are mday. */
    if (n != 1 && n != 2)
	return -PARSE_TIME_ERR_DATEFORMAT;

    /* Be strict about st, nd, rd, and lax about th. */
    if (strcasecmp (kw->name, "st") == 0 && v != 1 && v != 21 && v != 31)
	return -PARSE_TIME_ERR_INVALIDDATE;
    else if (strcasecmp (kw->name, "nd") == 0 && v != 2 && v != 22)
	return -PARSE_TIME_ERR_INVALIDDATE;
    else if (strcasecmp (kw->name, "rd") == 0 && v != 3 && v != 23)
	return -PARSE_TIME_ERR_INVALIDDATE;
    else if (strcasecmp (kw->name, "th") == 0 && !is_valid_mday (v))
	return -PARSE_TIME_ERR_INVALIDDATE;

    return set_field (state, TM_ABS_MDAY, v);
}

static int
kw_ignore (unused (struct state *state), unused (struct keyword *kw))
{
    return 0;
}

/*
 * Accepted keywords.
 *
 * A keyword may optionally contain a '|' to indicate the minimum
 * match length. Without one, full match is required. It's advisable
 * to keep the minimum match parts unique across all keywords. If
 * they're not, the first match wins.
 *
 * If keyword begins with '*', then the matching will be case
 * sensitive. Otherwise the matching is case insensitive.
 *
 * If .set is NULL, the field specified by .field will be set to
 * .value.
 *
 * Note: Observe how "m" and "mi" match minutes, "M" and "mo" and
 * "mont" match months, but "mon" matches Monday.
 */
static struct keyword keywords[] = {
    /* Weekdays. */
    { N_("sun|day"),	TM_WDAY,	0,	NULL },
    { N_("mon|day"),	TM_WDAY,	1,	NULL },
    { N_("tue|sday"),	TM_WDAY,	2,	NULL },
    { N_("wed|nesday"),	TM_WDAY,	3,	NULL },
    { N_("thu|rsday"),	TM_WDAY,	4,	NULL },
    { N_("fri|day"),	TM_WDAY,	5,	NULL },
    { N_("sat|urday"),	TM_WDAY,	6,	NULL },

    /* Months. */
    { N_("jan|uary"),	TM_ABS_MON,	1,	kw_set_month },
    { N_("feb|ruary"),	TM_ABS_MON,	2,	kw_set_month },
    { N_("mar|ch"),	TM_ABS_MON,	3,	kw_set_month },
    { N_("apr|il"),	TM_ABS_MON,	4,	kw_set_month },
    { N_("may"),	TM_ABS_MON,	5,	kw_set_month },
    { N_("jun|e"),	TM_ABS_MON,	6,	kw_set_month },
    { N_("jul|y"),	TM_ABS_MON,	7,	kw_set_month },
    { N_("aug|ust"),	TM_ABS_MON,	8,	kw_set_month },
    { N_("sep|tember"),	TM_ABS_MON,	9,	kw_set_month },
    { N_("oct|ober"),	TM_ABS_MON,	10,	kw_set_month },
    { N_("nov|ember"),	TM_ABS_MON,	11,	kw_set_month },
    { N_("dec|ember"),	TM_ABS_MON,	12,	kw_set_month },

    /* Durations. */
    { N_("y|ears"),	TM_REL_YEAR,	1,	kw_set_rel },
    { N_("mo|nths"),	TM_REL_MON,	1,	kw_set_rel },
    { N_("*M"),		TM_REL_MON,	1,	kw_set_rel },
    { N_("w|eeks"),	TM_REL_WEEK,	1,	kw_set_rel },
    { N_("d|ays"),	TM_REL_DAY,	1,	kw_set_rel },
    { N_("h|ours"),	TM_REL_HOUR,	1,	kw_set_rel },
    { N_("hr|s"),	TM_REL_HOUR,	1,	kw_set_rel },
    { N_("mi|nutes"),	TM_REL_MIN,	1,	kw_set_rel },
    { N_("mins"),	TM_REL_MIN,	1,	kw_set_rel },
    { N_("*m"),		TM_REL_MIN,	1,	kw_set_rel },
    { N_("s|econds"),	TM_REL_SEC,	1,	kw_set_rel },
    { N_("secs"),	TM_REL_SEC,	1,	kw_set_rel },

    /* Numbers. */
    { N_("one"),	TM_NONE,	1,	kw_set_number },
    { N_("two"),	TM_NONE,	2,	kw_set_number },
    { N_("three"),	TM_NONE,	3,	kw_set_number },
    { N_("four"),	TM_NONE,	4,	kw_set_number },
    { N_("five"),	TM_NONE,	5,	kw_set_number },
    { N_("six"),	TM_NONE,	6,	kw_set_number },
    { N_("seven"),	TM_NONE,	7,	kw_set_number },
    { N_("eight"),	TM_NONE,	8,	kw_set_number },
    { N_("nine"),	TM_NONE,	9,	kw_set_number },
    { N_("ten"),	TM_NONE,	10,	kw_set_number },
    { N_("dozen"),	TM_NONE,	12,	kw_set_number },
    { N_("hundred"),	TM_NONE,	100,	kw_set_number },

    /* Special number forms. */
    { N_("this"),	TM_NONE,	0,	kw_set_number },
    { N_("last"),	TM_NONE,	1,	kw_set_number },

    /* Other special keywords. */
    { N_("yesterday"),	TM_REL_DAY,	1,	kw_set_rel },
    { N_("today"),	TM_NONE,	0,	kw_set_today },
    { N_("now"),	TM_NONE,	0,	kw_set_now },
    { N_("noon"),	TM_NONE,	12,	kw_set_timeofday },
    { N_("midnight"),	TM_NONE,	0,	kw_set_timeofday },
    { N_("am"),		TM_AMPM,	0,	kw_set_ampm },
    { N_("a.m."),	TM_AMPM,	0,	kw_set_ampm },
    { N_("pm"),		TM_AMPM,	1,	kw_set_ampm },
    { N_("p.m."),	TM_AMPM,	1,	kw_set_ampm },
    { N_("st"),		TM_NONE,	0,	kw_set_ordinal },
    { N_("nd"),		TM_NONE,	0,	kw_set_ordinal },
    { N_("rd"),		TM_NONE,	0,	kw_set_ordinal },
    { N_("th"),		TM_NONE,	0,	kw_set_ordinal },
    { N_("ago"),       	TM_NONE,	0,	kw_ignore },

    /* Timezone codes: offset in minutes. XXX: Add more codes. */
    { N_("pst"),	TM_TZ,		-8*60,	NULL },
    { N_("mst"),	TM_TZ,		-7*60,	NULL },
    { N_("cst"),	TM_TZ,		-6*60,	NULL },
    { N_("est"),	TM_TZ,		-5*60,	NULL },
    { N_("ast"),	TM_TZ,		-4*60,	NULL },
    { N_("nst"),	TM_TZ,		-(3*60+30),	NULL },

    { N_("gmt"),	TM_TZ,		0,	NULL },
    { N_("utc"),	TM_TZ,		0,	NULL },

    { N_("wet"),	TM_TZ,		0,	NULL },
    { N_("cet"),	TM_TZ,		1*60,	NULL },
    { N_("eet"),	TM_TZ,		2*60,	NULL },
    { N_("fet"),	TM_TZ,		3*60,	NULL },

    { N_("wat"),	TM_TZ,		1*60,	NULL },
    { N_("cat"),	TM_TZ,		2*60,	NULL },
    { N_("eat"),	TM_TZ,		3*60,	NULL },
};

/*
 * Compare strings str and keyword. Return the number of matching
 * chars on match, 0 for no match.
 *
 * All of the alphabetic characters (isalpha) in str up to the first
 * non-alpha character (or end of string) must match the
 * keyword. Consequently, the value returned on match is the number of
 * consecutive alphabetic characters in str.
 *
 * Abbreviated match is accepted if the keyword contains a '|'
 * character, and str matches keyword up to that character. Any alpha
 * characters after that in str must still match the keyword following
 * the '|' character. If no '|' is present, all of keyword must match.
 *
 * Excessive, consecutive, and misplaced (at the beginning or end) '|'
 * characters in keyword are handled gracefully. Only the first one
 * matters.
 *
 * If match_case is true, the matching is case sensitive.
 */
static size_t
match_keyword (const char *str, const char *keyword, bool match_case)
{
    const char *s = str;
    bool prefix_matched = false;

    for (;;) {
	while (*keyword == '|') {
	    prefix_matched = true;
	    keyword++;
	}

	if (!*s || !isalpha ((unsigned char) *s) || !*keyword)
	    break;

	if (match_case) {
	    if (*s != *keyword)
		return 0;
	} else {
	    if (tolower ((unsigned char) *s) !=
		tolower ((unsigned char) *keyword))
		return 0;
	}
	s++;
	keyword++;
    }

    /* did not match all of the keyword in input string */
    if (*s && isalpha ((unsigned char) *s))
	return 0;

    /* did not match enough of keyword */
    if (*keyword && !prefix_matched)
	return 0;

    return s - str;
}

/*
 * Parse a keyword. Return < 0 on error, number of parsed chars on
 * success.
 */
static ssize_t
parse_keyword (struct state *state, const char *s)
{
    unsigned int i;
    size_t n = 0;
    struct keyword *kw = NULL;
    int r;

    for (i = 0; i < ARRAY_SIZE (keywords); i++) {
	const char *keyword = _(keywords[i].name);
	bool mcase = false;

	/* Match case if keyword begins with '*'. */
	if (*keyword == '*') {
	    mcase = true;
	    keyword++;
	}

	n = match_keyword (s, keyword, mcase);
	if (n) {
	    kw = &keywords[i];
	    break;
	}
    }

    if (!kw)
	return -PARSE_TIME_ERR_KEYWORD;

    if (kw->set)
	r = kw->set (state, kw);
    else
	r = set_field (state, kw->field, kw->value);

    if (r < 0)
	return r;

    return n;
}

/*
 * Non-keyword parsers and their helpers.
 */

static int
set_user_tz (struct state *state, char sign, int hour, int min)
{
    int tz = hour * 60 + min;

    assert (sign == '+' || sign == '-');

    if (hour < 0 || hour > 14 || min < 0 || min > 59 || min % 15)
	return -PARSE_TIME_ERR_INVALIDTIME;

    if (sign == '-')
	tz = -tz;

    return set_field (state, TM_TZ, tz);
}

/*
 * Parse a previously postponed number if one exists. Independent
 * parsing of a postponed number when it wasn't consumed during
 * parsing of the following token.
 */
static int
parse_postponed_number (struct state *state, unused (enum field next_field))
{
    int v, n;
    char d;

    /* Bail out if there's no postponed number. */
    if (!consume_postponed_number (state, &v, &n, &d))
	return 0;

    if (n == 1 || n == 2) {
	/* Notable exception: Previous field affects parsing. This
	 * handles "January 20". */
	if (state->last_field == TM_ABS_MON) {
	    /* D[D] */
	    if (!is_valid_mday (v))
		return -PARSE_TIME_ERR_INVALIDDATE;

	    return set_field (state, TM_ABS_MDAY, v);
	} else if (n == 2) {
	    /* XXX: Only allow if last field is hour, min, or sec? */
	    if (d == '+' || d == '-') {
		/* +/-HH */
		return set_user_tz (state, d, v, 0);
	    }
	}
    } else if (n == 4) {
	/* Notable exception: Value affects parsing. Time zones are
	 * always at most 1400 and we don't understand years before
	 * 1970. */
	if (!is_valid_year (v)) {
	    if (d == '+' || d == '-') {
		/* +/-HHMM */
		return set_user_tz (state, d, v / 100, v % 100);
	    }
	} else {
	    /* YYYY */
	    return set_field (state, TM_ABS_YEAR, v);
	}
    } else if (n == 6) {
	/* HHMMSS */
	int hour = v / 10000;
	int min = (v / 100) % 100;
	int sec = v % 100;

	if (!is_valid_time (hour, min, sec))
	    return -PARSE_TIME_ERR_INVALIDTIME;

	return set_abs_time (state, hour, min, sec);
    } else if (n == 8) {
	/* YYYYMMDD */
	int year = v / 10000;
	int mon = (v / 100) % 100;
	int mday = v % 100;

	if (!is_valid_date (year, mon, mday))
	    return -PARSE_TIME_ERR_INVALIDDATE;

	return set_abs_date (state, year, mon, mday);
    }

    return -PARSE_TIME_ERR_FORMAT;
}

static int tm_get_field (const struct tm *tm, enum field field);

static int
set_timestamp (struct state *state, time_t t)
{
    struct tm tm;
    enum field f;
    int r;

    if (gmtime_r (&t, &tm) == NULL)
	return -PARSE_TIME_ERR_LIB;

    for (f = TM_ABS_SEC; f != TM_NONE; f = next_abs_field (f)) {
	r = set_field (state, f, tm_get_field (&tm, f));
	if (r)
	    return r;
    }

    r = set_field (state, TM_TZ, 0);
    if (r)
	return r;

    /* XXX: Prevent TM_AMPM with timestamp, e.g. "@123456 pm" */

    return 0;
}

/* Parse a single number. Typically postpone parsing until later. */
static int
parse_single_number (struct state *state, unsigned long v,
		     unsigned long n)
{
    assert (n);

    if (state->delim == '@')
	return set_timestamp (state, (time_t) v);

    if (v > INT_MAX)
	return -PARSE_TIME_ERR_FORMAT;

    return set_postponed_number (state, v, n);
}

static bool
is_time_sep (char c)
{
    return c == ':';
}

static bool
is_date_sep (char c)
{
    return c == '/' || c == '-' || c == '.';
}

static bool
is_sep (char c)
{
    return is_time_sep (c) || is_date_sep (c);
}

/* Two-digit year: 00...69 is 2000s, 70...99 1900s, if n == 0 keep
 * unset. */
static int
expand_year (unsigned long year, size_t n)
{
    if (n == 2) {
	return (year < 70 ? 2000 : 1900) + year;
    } else if (n == 4) {
	return year;
    } else {
	return UNSET;
    }
}

/* Parse a date number triplet. */
static int
parse_date (struct state *state, char sep,
	    unsigned long v1, unsigned long v2, unsigned long v3,
	    size_t n1, size_t n2, size_t n3)
{
    int year = UNSET, mon = UNSET, mday = UNSET;

    assert (is_date_sep (sep));

    switch (sep) {
    case '/': /* Date: M[M]/D[D][/YY[YY]] or M[M]/YYYY */
	if (n1 != 1 && n1 != 2)
	    return -PARSE_TIME_ERR_DATEFORMAT;

	if ((n2 == 1 || n2 == 2) && (n3 == 0 || n3 == 2 || n3 == 4)) {
	    /* M[M]/D[D][/YY[YY]] */
	    year = expand_year (v3, n3);
	    mon = v1;
	    mday = v2;
	} else if (n2 == 4 && n3 == 0) {
	    /* M[M]/YYYY */
	    year = v2;
	    mon = v1;
	} else {
	    return -PARSE_TIME_ERR_DATEFORMAT;
	}
	break;

    case '-': /* Date: YYYY-MM[-DD] or DD-MM[-YY[YY]] or MM-YYYY */
	if (n1 == 4 && n2 == 2 && (n3 == 0 || n3 == 2)) {
	    /* YYYY-MM[-DD] */
	    year = v1;
	    mon = v2;
	    if (n3)
		mday = v3;
	} else if (n1 == 2 && n2 == 2 && (n3 == 0 || n3 == 2 || n3 == 4)) {
	    /* DD-MM[-YY[YY]] */
	    year = expand_year (v3, n3);
	    mon = v2;
	    mday = v1;
	} else if (n1 == 2 && n2 == 4 && n3 == 0) {
	    /* MM-YYYY */
	    year = v2;
	    mon = v1;
	} else {
	    return -PARSE_TIME_ERR_DATEFORMAT;
	}
	break;

    case '.': /* Date: D[D].M[M][.[YY[YY]]] */
	if ((n1 != 1 && n1 != 2) || (n2 != 1 && n2 != 2) ||
	    (n3 != 0 && n3 != 2 && n3 != 4))
	    return -PARSE_TIME_ERR_DATEFORMAT;

	year = expand_year (v3, n3);
	mon = v2;
	mday = v1;
	break;
    }

    if (year != UNSET && !is_valid_year (year))
	return -PARSE_TIME_ERR_INVALIDDATE;

    if (mon != UNSET && !is_valid_mon (mon))
	return -PARSE_TIME_ERR_INVALIDDATE;

    if (mday != UNSET && !is_valid_mday (mday))
	return -PARSE_TIME_ERR_INVALIDDATE;

    return set_abs_date (state, year, mon, mday);
}

/* Parse a time number triplet. */
static int
parse_time (struct state *state, char sep,
	    unsigned long v1, unsigned long v2, unsigned long v3,
	    size_t n1, size_t n2, size_t n3)
{
    assert (is_time_sep (sep));

    if ((n1 != 1 && n1 != 2) || n2 != 2 || (n3 != 0 && n3 != 2))
	return -PARSE_TIME_ERR_TIMEFORMAT;

    /*
     * Notable exception: Previously set fields affect
     * parsing. Interpret (+|-)HH:MM as time zone only if hour and
     * minute have been set.
     *
     * XXX: This could be fixed by restricting the delimiters
     * preceding time. For '+' it would be justified, but for '-' it
     * might be inconvenient. However prefer to allow '-' as an
     * insignificant delimiter preceding time for convenience, and
     * handle '+' the same way for consistency between positive and
     * negative time zones.
     */
    if (is_field_set (state, TM_ABS_HOUR) &&
	is_field_set (state, TM_ABS_MIN) &&
	n1 == 2 && n2 == 2 && n3 == 0 &&
	(state->delim == '+' || state->delim == '-')) {
	return set_user_tz (state, state->delim, v1, v2);
    }

    if (!is_valid_time (v1, v2, n3 ? v3 : 0))
	return -PARSE_TIME_ERR_INVALIDTIME;

    return set_abs_time (state, v1, v2, n3 ? (int) v3 : UNSET);
}

/* strtoul helper that assigns length. */
static unsigned long
strtoul_len (const char *s, const char **endp, size_t *len)
{
    unsigned long val = strtoul (s, (char **) endp, 10);

    *len = *endp - s;
    return val;
}

/*
 * Parse a (group of) number(s). Return < 0 on error, number of parsed
 * chars on success.
 */
static ssize_t
parse_number (struct state *state, const char *s)
{
    int r;
    unsigned long v1, v2, v3 = 0;
    size_t n1, n2, n3 = 0;
    const char *p = s;
    char sep;

    v1 = strtoul_len (p, &p, &n1);

    if (!is_sep (*p) || !isdigit ((unsigned char) *(p + 1))) {
	/* A single number. */
	r = parse_single_number (state, v1, n1);
	if (r)
	    return r;

	return p - s;
    }

    sep = *p;
    v2 = strtoul_len (p + 1, &p, &n2);

    /* A group of two or three numbers? */
    if (*p == sep && isdigit ((unsigned char) *(p + 1)))
	v3 = strtoul_len (p + 1, &p, &n3);

    if (is_time_sep (sep))
	r = parse_time (state, sep, v1, v2, v3, n1, n2, n3);
    else
	r = parse_date (state, sep, v1, v2, v3, n1, n2, n3);

    if (r)
	return r;

    return p - s;
}

/*
 * Parse delimiter(s). Throw away all except the last one, which is
 * stored for parsing the next non-delimiter. Return < 0 on error,
 * number of parsed chars on success.
 *
 * XXX: We might want to be more strict here.
 */
static ssize_t
parse_delim (struct state *state, const char *s)
{
    const char *p = s;

    /*
     * Skip non-alpha and non-digit, and store the last for further
     * processing.
     */
    while (*p && !isalnum ((unsigned char) *p)) {
	set_delim (state, *p);
	p++;
    }

    return p - s;
}

/*
 * Parse a date/time string. Return < 0 on error, number of parsed
 * chars on success.
 */
static ssize_t
parse_input (struct state *state, const char *s)
{
    const char *p = s;
    ssize_t n;
    int r;

    while (*p) {
	if (isalpha ((unsigned char) *p)) {
	    n = parse_keyword (state, p);
	} else if (isdigit ((unsigned char) *p)) {
	    n = parse_number (state, p);
	} else {
	    n = parse_delim (state, p);
	}

	if (n <= 0) {
	    if (n == 0)
		n = -PARSE_TIME_ERR;

	    return n;
	}

	p += n;
    }

    /* Parse a previously postponed number, if any. */
    r = parse_postponed_number (state, TM_NONE);
    if (r < 0)
	return r;

    return p - s;
}

/*
 * Processing the parsed input.
 */

/*
 * Initialize reference time to tm. Use time zone in state if
 * specified, otherwise local time. Use now for reference time if
 * non-NULL, otherwise current time.
 */
static int
initialize_now (struct state *state, const time_t *ref, struct tm *tm)
{
    time_t t;

    if (ref) {
	t = *ref;
    } else {
	if (time (&t) == (time_t) -1)
	    return -PARSE_TIME_ERR_LIB;
    }

    if (is_field_set (state, TM_TZ)) {
	/* Some other time zone. */

	/* Adjust now according to the TZ. */
	t += get_field (state, TM_TZ) * 60;

	/* It's not gm, but this doesn't mess with the TZ. */
	if (gmtime_r (&t, tm) == NULL)
	    return -PARSE_TIME_ERR_LIB;
    } else {
	/* Local time. */
	if (localtime_r (&t, tm) == NULL)
	    return -PARSE_TIME_ERR_LIB;
    }

    return 0;
}

/*
 * Normalize tm according to mktime(3); if structure members are
 * outside their valid interval, they will be normalized (so that, for
 * example, 40 October is changed into 9 November), and tm_wday and
 * tm_yday are set to values determined from the contents of the other
 * fields.
 *
 * Both mktime(3) and localtime_r(3) use local time, but they cancel
 * each other out here, making this function agnostic to time zone.
 */
static int
normalize_tm (struct tm *tm)
{
    time_t t = mktime (tm);

    if (t == (time_t) -1)
	return -PARSE_TIME_ERR_LIB;

    if (!localtime_r (&t, tm))
	return -PARSE_TIME_ERR_LIB;

    return 0;
}

/* Get field out of a struct tm. */
static int
tm_get_field (const struct tm *tm, enum field field)
{
    switch (field) {
    case TM_ABS_SEC:	return tm->tm_sec;
    case TM_ABS_MIN:	return tm->tm_min;
    case TM_ABS_HOUR:	return tm->tm_hour;
    case TM_ABS_MDAY:	return tm->tm_mday;
    case TM_ABS_MON:	return tm->tm_mon + 1; /* 0- to 1-based */
    case TM_ABS_YEAR:	return 1900 + tm->tm_year;
    case TM_WDAY:	return tm->tm_wday;
    case TM_ABS_ISDST:	return tm->tm_isdst;
    default:
	assert (false);
	break;
    }

    return 0;
}

/* Modify hour according to am/pm setting. */
static int
fixup_ampm (struct state *state)
{
    int hour, hdiff = 0;

    if (!is_field_set (state, TM_AMPM))
	return 0;

    if (!is_field_set (state, TM_ABS_HOUR))
	return -PARSE_TIME_ERR_TIMEFORMAT;

    hour = get_field (state, TM_ABS_HOUR);
    if (!is_valid_12hour (hour))
	return -PARSE_TIME_ERR_INVALIDTIME;

    if (get_field (state, TM_AMPM)) {
	/* 12pm is noon. */
	if (hour != 12)
	    hdiff = 12;
    } else {
	/* 12am is midnight, beginning of day. */
	if (hour == 12)
	    hdiff = -12;
    }

    add_to_field (state, TM_REL_HOUR, -hdiff);

    return 0;
}

/* Combine absolute and relative fields, and round. */
static int
create_output (struct state *state, time_t *t_out, const time_t *ref,
	       int round)
{
    struct tm tm = { .tm_isdst = -1 };
    struct tm now;
    time_t t;
    enum field f;
    int r;
    int week_round = PARSE_TIME_NO_ROUND;

    r = initialize_now (state, ref, &now);
    if (r)
	return r;

    /* Initialize fields flagged as "now" to reference time. */
    for (f = TM_ABS_SEC; f != TM_NONE; f = next_abs_field (f)) {
	if (state->set[f] == FIELD_NOW) {
	    state->tm[f] = tm_get_field (&now, f);
	    state->set[f] = FIELD_SET;
	}
    }

    /*
     * If WDAY is set but MDAY is not, we consider WDAY relative
     *
     * XXX: This fails on stuff like "two months monday" because two
     * months ago wasn't the same day as today. Postpone until we know
     * date?
     */
    if (is_field_set (state, TM_WDAY) &&
	!is_field_set (state, TM_ABS_MDAY)) {
	int wday = get_field (state, TM_WDAY);
	int today = tm_get_field (&now, TM_WDAY);
	int rel_days;

	if (today > wday)
	    rel_days = today - wday;
	else
	    rel_days = today + 7 - wday;

	/* This also prevents special week rounding from happening. */
	add_to_field (state, TM_REL_DAY, rel_days);

	unset_field (state, TM_WDAY);
    }

    r = fixup_ampm (state);
    if (r)
	return r;

    /*
     * Iterate fields from most accurate to least accurate, and set
     * unset fields according to requested rounding.
     */
    for (f = TM_ABS_SEC; f != TM_NONE; f = next_abs_field (f)) {
	if (round != PARSE_TIME_NO_ROUND) {
	    enum field r = abs_to_rel_field (f);

	    if (is_field_set (state, f) || is_field_set (state, r)) {
		if (round >= PARSE_TIME_ROUND_UP && f != TM_ABS_SEC) {
		    /*
		     * This is the most accurate field
		     * specified. Round up adjusting it towards
		     * future.
		     */
		    add_to_field (state, r, -1);

		    /*
		     * Go back a second if the result is to be used
		     * for inclusive comparisons.
		     */
		    if (round == PARSE_TIME_ROUND_UP_INCLUSIVE)
			add_to_field (state, TM_REL_SEC, 1);
		}
		round = PARSE_TIME_NO_ROUND; /* No more rounding. */
	    } else {
		if (f == TM_ABS_MDAY &&
		    is_field_set (state, TM_REL_WEEK)) {
		    /* Week is most accurate. */
		    week_round = round;
		    round = PARSE_TIME_NO_ROUND;
		} else {
		    set_field (state, f, get_field_epoch_value (f));
		}
	    }
	}

	if (!is_field_set (state, f))
	    set_field (state, f, tm_get_field (&now, f));
    }

    /* Special case: rounding with week accuracy. */
    if (week_round != PARSE_TIME_NO_ROUND) {
	/* Temporarily set more accurate fields to now. */
	set_field (state, TM_ABS_SEC, tm_get_field (&now, TM_ABS_SEC));
	set_field (state, TM_ABS_MIN, tm_get_field (&now, TM_ABS_MIN));
	set_field (state, TM_ABS_HOUR, tm_get_field (&now, TM_ABS_HOUR));
	set_field (state, TM_ABS_MDAY, tm_get_field (&now, TM_ABS_MDAY));
    }

    /*
     * Set all fields. They may contain out of range values before
     * normalization by mktime(3).
     */
    tm.tm_sec = get_field (state, TM_ABS_SEC) - get_field (state, TM_REL_SEC);
    tm.tm_min = get_field (state, TM_ABS_MIN) - get_field (state, TM_REL_MIN);
    tm.tm_hour = get_field (state, TM_ABS_HOUR) - get_field (state, TM_REL_HOUR);
    tm.tm_mday = get_field (state, TM_ABS_MDAY) -
		 get_field (state, TM_REL_DAY) - 7 * get_field (state, TM_REL_WEEK);
    tm.tm_mon = get_field (state, TM_ABS_MON) - get_field (state, TM_REL_MON);
    tm.tm_mon--; /* 1- to 0-based */
    tm.tm_year = get_field (state, TM_ABS_YEAR) - get_field (state, TM_REL_YEAR) - 1900;

    /*
     * It's always normal time.
     *
     * XXX: This is probably not a solution that universally
     * works. Just make sure DST is not taken into account. We don't
     * want rounding to be affected by DST.
     */
    tm.tm_isdst = -1;

    /* Special case: rounding with week accuracy. */
    if (week_round != PARSE_TIME_NO_ROUND) {
	/* Normalize to get proper tm.wday. */
	r = normalize_tm (&tm);
	if (r < 0)
	    return r;

	/* Set more accurate fields back to zero. */
	tm.tm_sec = 0;
	tm.tm_min = 0;
	tm.tm_hour = 0;
	tm.tm_isdst = -1;

	/* Monday is the true 1st day of week, but this is easier. */
	if (week_round >= PARSE_TIME_ROUND_UP) {
	    tm.tm_mday += 7 - tm.tm_wday;
	    if (week_round == PARSE_TIME_ROUND_UP_INCLUSIVE)
		tm.tm_sec--;
	} else {
	    tm.tm_mday -= tm.tm_wday;
	}
    }

    if (is_field_set (state, TM_TZ)) {
	/* tm is in specified TZ, convert to UTC for timegm(3). */
	tm.tm_min -= get_field (state, TM_TZ);
	t = timegm (&tm);
    } else {
	/* tm is in local time. */
	t = mktime (&tm);
    }

    if (t == (time_t) -1)
	return -PARSE_TIME_ERR_LIB;

    *t_out = t;

    return 0;
}

/* Internally, all errors are < 0. parse_time_string() returns errors > 0. */
#define EXTERNAL_ERR(r) (-r)

int
parse_time_string (const char *s, time_t *t, const time_t *ref, int round)
{
    struct state state = { .last_field = TM_NONE };
    int r;

    if (!s || !t)
	return EXTERNAL_ERR (-PARSE_TIME_ERR);

    r = parse_input (&state, s);
    if (r < 0)
	return EXTERNAL_ERR (r);

    r = create_output (&state, t, ref, round);
    if (r < 0)
	return EXTERNAL_ERR (r);

    return 0;
}
