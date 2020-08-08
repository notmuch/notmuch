/* database-private.h - For peeking into the internals of notmuch_database_t
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

#ifndef NOTMUCH_DATABASE_PRIVATE_H
#define NOTMUCH_DATABASE_PRIVATE_H

/* According to WG14/N1124, a C++ implementation won't provide us a
 * macro like PRIx64 (which gives a printf format string for
 * formatting a uint64_t as hexadecimal) unless we define
 * __STDC_FORMAT_MACROS before including inttypes.h. That's annoying,
 * but there it is.
 */
#define __STDC_FORMAT_MACROS
#include <inttypes.h>

#include "notmuch-private.h"

#define ARRAY_SIZE(arr) (sizeof (arr) / sizeof (arr[0]))

#ifdef SILENCE_XAPIAN_DEPRECATION_WARNINGS
#define XAPIAN_DEPRECATED(D) D
#endif

#include <xapian.h>

/* Bit masks for _notmuch_database::features.  Features are named,
 * independent aspects of the database schema.
 *
 * A database stores the set of features that it "uses" (implicitly
 * before database version 3 and explicitly as of version 3).
 *
 * A given library version will "recognize" a particular set of
 * features; if a database uses a feature that the library does not
 * recognize, the library will refuse to open it.  It is assumed the
 * set of recognized features grows monotonically over time.  A
 * library version will "implement" some subset of the recognized
 * features: some operations may require that the database use (or not
 * use) some feature, while other operations may support both
 * databases that use and that don't use some feature.
 *
 * On disk, the database stores string names for these features (see
 * the feature_names array).  These enum bit values are never
 * persisted to disk and may change freely.
 */
enum _notmuch_features {
    /* If set, file names are stored in "file-direntry" terms.  If
     * unset, file names are stored in document data.
     *
     * Introduced: version 1. */
    NOTMUCH_FEATURE_FILE_TERMS			= 1 << 0,

    /* If set, directory timestamps are stored in documents with
     * XDIRECTORY terms and relative paths.  If unset, directory
     * timestamps are stored in documents with XTIMESTAMP terms and
     * absolute paths.
     *
     * Introduced: version 1. */
    NOTMUCH_FEATURE_DIRECTORY_DOCS		= 1 << 1,

    /* If set, the from, subject, and message-id headers are stored in
     * message document values.  If unset, message documents *may*
     * have these values, but if the value is empty, it must be
     * retrieved from the message file.
     *
     * Introduced: optional in version 1, required as of version 3.
     */
    NOTMUCH_FEATURE_FROM_SUBJECT_ID_VALUES	= 1 << 2,

    /* If set, folder terms are boolean and path terms exist.  If
     * unset, folder terms are probabilistic and stemmed and path
     * terms do not exist.
     *
     * Introduced: version 2. */
    NOTMUCH_FEATURE_BOOL_FOLDER			= 1 << 3,

    /* If set, missing messages are stored in ghost mail documents.
     * If unset, thread IDs of ghost messages are stored as database
     * metadata instead of in ghost documents.
     *
     * Introduced: version 3. */
    NOTMUCH_FEATURE_GHOSTS			= 1 << 4,


    /* If set, then the database was created after the introduction of
     * indexed mime types. If unset, then the database may contain a
     * mixture of messages with indexed and non-indexed mime types.
     *
     * Introduced: version 3. */
    NOTMUCH_FEATURE_INDEXED_MIMETYPES		= 1 << 5,

    /* If set, messages store the revision number of the last
     * modification in NOTMUCH_VALUE_LAST_MOD.
     *
     * Introduced: version 3. */
    NOTMUCH_FEATURE_LAST_MOD			= 1 << 6,

    /* If set, unprefixed terms are stored only for the message body,
     * not for headers.
     *
     * Introduced: version 3. */
    NOTMUCH_FEATURE_UNPREFIX_BODY_ONLY		= 1 << 7,
};

/* In C++, a named enum is its own type, so define bitwise operators
 * on _notmuch_features. */
inline _notmuch_features
operator| (_notmuch_features a, _notmuch_features b)
{
    return static_cast<_notmuch_features>(
	static_cast<unsigned>(a) | static_cast<unsigned>(b));
}

inline _notmuch_features
operator& (_notmuch_features a, _notmuch_features b)
{
    return static_cast<_notmuch_features>(
	static_cast<unsigned>(a) & static_cast<unsigned>(b));
}

inline _notmuch_features
operator~ (_notmuch_features a)
{
    return static_cast<_notmuch_features>(~static_cast<unsigned>(a));
}

inline _notmuch_features&
operator|= (_notmuch_features &a, _notmuch_features b)
{
    a = a | b;
    return a;
}

inline _notmuch_features&
operator&= (_notmuch_features &a, _notmuch_features b)
{
    a = a & b;
    return a;
}

/*
 * Configuration options for xapian database fields */
typedef enum notmuch_field_flags {
    NOTMUCH_FIELD_NO_FLAGS	= 0,
    NOTMUCH_FIELD_EXTERNAL	= 1 << 0,
    NOTMUCH_FIELD_PROBABILISTIC = 1 << 1,
    NOTMUCH_FIELD_PROCESSOR	= 1 << 2,
} notmuch_field_flag_t;

/*
 * define bitwise operators to hide casts */
inline notmuch_field_flag_t
operator| (notmuch_field_flag_t a, notmuch_field_flag_t b)
{
    return static_cast<notmuch_field_flag_t>(
	static_cast<unsigned>(a) | static_cast<unsigned>(b));
}

inline notmuch_field_flag_t
operator& (notmuch_field_flag_t a, notmuch_field_flag_t b)
{
    return static_cast<notmuch_field_flag_t>(
	static_cast<unsigned>(a) & static_cast<unsigned>(b));
}

#define NOTMUCH_QUERY_PARSER_FLAGS (Xapian::QueryParser::FLAG_BOOLEAN | \
				    Xapian::QueryParser::FLAG_PHRASE | \
				    Xapian::QueryParser::FLAG_LOVEHATE | \
				    Xapian::QueryParser::FLAG_BOOLEAN_ANY_CASE | \
				    Xapian::QueryParser::FLAG_WILDCARD | \
				    Xapian::QueryParser::FLAG_PURE_NOT)

struct _notmuch_database {
    bool exception_reported;

    char *path;

    int atomic_nesting;
    /* true if changes have been made in this atomic section */
    bool atomic_dirty;
    Xapian::Database *xapian_db;
    Xapian::WritableDatabase *writable_xapian_db;
    bool open;
    /* Bit mask of features used by this database.  This is a
     * bitwise-OR of NOTMUCH_FEATURE_* values (above). */
    enum _notmuch_features features;

    unsigned int last_doc_id;
    uint64_t last_thread_id;

    /* error reporting; this value persists only until the
     * next library call. May be NULL */
    char *status_string;

    /* Highest committed revision number.  Modifications are recorded
     * under a higher revision number, which can be generated with
     * notmuch_database_new_revision. */
    unsigned long revision;
    const char *uuid;

    /* Keep track of the number of times the database has been re-opened
     * (or other global invalidations of notmuch's caching)
     */
    unsigned long view;
    Xapian::QueryParser *query_parser;
    Xapian::TermGenerator *term_gen;
    Xapian::RangeProcessor *value_range_processor;
    Xapian::RangeProcessor *date_range_processor;
    Xapian::RangeProcessor *last_mod_range_processor;

    /* XXX it's slightly gross to use two parallel string->string maps
     * here, but at least they are small */
    notmuch_string_map_t *user_prefix;
    notmuch_string_map_t *user_header;

    /* Cached and possibly overridden configuration */
    notmuch_string_map_t *config;
};

/* Prior to database version 3, features were implied by the database
 * version number, so hard-code them for earlier versions. */
#define NOTMUCH_FEATURES_V0 ((enum _notmuch_features) 0)
#define NOTMUCH_FEATURES_V1 (NOTMUCH_FEATURES_V0 | NOTMUCH_FEATURE_FILE_TERMS | \
			     NOTMUCH_FEATURE_DIRECTORY_DOCS)
#define NOTMUCH_FEATURES_V2 (NOTMUCH_FEATURES_V1 | NOTMUCH_FEATURE_BOOL_FOLDER)

/* Current database features.  If any of these are missing from a
 * database, request an upgrade.
 * NOTMUCH_FEATURE_FROM_SUBJECT_ID_VALUES and
 * NOTMUCH_FEATURE_INDEXED_MIMETYPES are not included because upgrade
 * doesn't currently introduce the features (though brand new databases
 * will have it). */
#define NOTMUCH_FEATURES_CURRENT \
    (NOTMUCH_FEATURE_FILE_TERMS | NOTMUCH_FEATURE_DIRECTORY_DOCS | \
     NOTMUCH_FEATURE_BOOL_FOLDER | NOTMUCH_FEATURE_GHOSTS | \
     NOTMUCH_FEATURE_LAST_MOD)

/* Return the list of terms from the given iterator matching a prefix.
 * The prefix will be stripped from the strings in the returned list.
 * The list will be allocated using ctx as the talloc context.
 *
 * The function returns NULL on failure.
 */
notmuch_string_list_t *
_notmuch_database_get_terms_with_prefix (void *ctx, Xapian::TermIterator &i,
					 Xapian::TermIterator &end,
					 const char *prefix);

void
_notmuch_database_find_doc_ids (notmuch_database_t *notmuch,
				const char *prefix_name,
				const char *value,
				Xapian::PostingIterator *begin,
				Xapian::PostingIterator *end);

#define NOTMUCH_DATABASE_VERSION 3

/* features.cc */

_notmuch_features
_notmuch_database_parse_features (const void *ctx, const char *features, unsigned int version,
				  char mode, char **incompat_out);

char *
_notmuch_database_print_features (const void *ctx, unsigned int features);

/* prefix.cc */
notmuch_status_t
_notmuch_database_setup_standard_query_fields (notmuch_database_t *notmuch);

notmuch_status_t
_notmuch_database_setup_user_query_fields (notmuch_database_t *notmuch);

#endif
