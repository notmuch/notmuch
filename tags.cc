/* tags.cc - Iterator for tags returned from message or thread
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
 * along with this program.  If not, see http://www.gnu.org/licenses/ .
 *
 * Author: Carl Worth <cworth@cworth.org>
 */

#include "notmuch-private-cxx.h"
#include "database-private.h"

#include <xapian.h>

/* We end up having to call the destructors explicitly because we had
 * to use "placement new" in order to initialize C++ objects within a
 * block that we allocated with talloc. So C++ is making talloc
 * slightly less simple to use, (we wouldn't need
 * talloc_set_destructor at all otherwise).
 */
static int
_notmuch_terms_destructor (notmuch_terms_t *terms)
{
    terms->iterator.~TermIterator ();
    terms->iterator_end.~TermIterator ();

    return 0;
}

notmuch_terms_t *
_notmuch_terms_create (void *ctx,
		       Xapian::Document doc,
		       const char *prefix_name)
{
    notmuch_terms_t *terms;
    const char *prefix = _find_prefix (prefix_name);

    /* Currently, notmuch_terms_t is written with the assumption that
     * any prefix its derivatives use will be only a single
     * character. */
    assert (strlen (prefix) == 1);

    terms = talloc (ctx, notmuch_terms_t);
    if (unlikely (terms == NULL))
	return NULL;

    terms->prefix_char = *prefix;
    new (&terms->iterator) Xapian::TermIterator;
    new (&terms->iterator_end) Xapian::TermIterator;

    talloc_set_destructor (terms, _notmuch_terms_destructor);

    terms->iterator = doc.termlist_begin ();
    terms->iterator.skip_to (prefix);
    terms->iterator_end = doc.termlist_end ();

    return terms;
}

static notmuch_bool_t
_notmuch_terms_has_more (notmuch_terms_t *terms)
{
    std::string s;

    if (terms->iterator == terms->iterator_end)
	return FALSE;

    s = *terms->iterator;
    if (! s.empty () && s[0] == terms->prefix_char)
	return TRUE;
    else
	return FALSE;
}

static const char *
_notmuch_terms_get (notmuch_terms_t *terms)
{
    return talloc_strdup (terms, (*terms->iterator).c_str () + 1);
}

static void
_notmuch_terms_advance (notmuch_terms_t *terms)
{
    terms->iterator++;
}

static void
_notmuch_terms_destroy (notmuch_terms_t *terms)
{
    talloc_free (terms);
}

notmuch_bool_t
notmuch_tags_has_more (notmuch_tags_t *tags)
{
    return _notmuch_terms_has_more (&tags->terms);
}

const char *
notmuch_tags_get (notmuch_tags_t *tags)
{
    return _notmuch_terms_get (&tags->terms);
}

void
notmuch_tags_advance (notmuch_tags_t *tags)
{
    return _notmuch_terms_advance (&tags->terms);
}

void
notmuch_tags_destroy (notmuch_tags_t *tags)
{
    return _notmuch_terms_destroy (&tags->terms);
}
