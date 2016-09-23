/* string-map.c - associative arrays of strings
 *
 *
 * Copyright © 2016 David Bremner
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
 * Author: David Bremner <david@tethera.net>
 */

#include "notmuch-private.h"

/* Create a new notmuch_string_map_t object, with 'ctx' as its
 * talloc owner.
 *
 * This function can return NULL in case of out-of-memory.
 */

typedef struct _notmuch_string_pair_t {
    char *key;
    char *value;
} notmuch_string_pair_t;

struct _notmuch_string_map {
    notmuch_bool_t sorted;
    size_t length;
    notmuch_string_pair_t *pairs;
};

struct _notmuch_string_map_iterator {
    notmuch_string_pair_t *current;
    notmuch_bool_t exact;
    const char *key;
};

notmuch_string_map_t *
_notmuch_string_map_create (const void *ctx)
{
    notmuch_string_map_t *map;

    map = talloc (ctx, notmuch_string_map_t);
    if (unlikely (map == NULL))
	return NULL;

    map->length = 0;
    map->pairs = NULL;
    map->sorted = TRUE;

    return map;
}

void
_notmuch_string_map_append (notmuch_string_map_t *map,
			    const char *key,
			    const char *value)
{

    map->length++;
    map->sorted = FALSE;

    if (map->pairs)
	map->pairs = talloc_realloc (map, map->pairs, notmuch_string_pair_t, map->length + 1);
    else
	map->pairs = talloc_array (map, notmuch_string_pair_t, map->length + 1);

    map->pairs[map->length - 1].key = talloc_strdup (map, key);
    map->pairs[map->length - 1].value = talloc_strdup (map, value);

    /* Add sentinel */
    map->pairs[map->length].key = NULL;
    map->pairs[map->length].value = NULL;

}

static int
cmppair (const void *pa, const void *pb)
{
    notmuch_string_pair_t *a = (notmuch_string_pair_t *) pa;
    notmuch_string_pair_t *b = (notmuch_string_pair_t *) pb;

    return strcmp (a->key, b->key);
}

static void
_notmuch_string_map_sort (notmuch_string_map_t *map)
{
    if (map->length == 0)
	return;

    if (map->sorted)
	return;

    qsort (map->pairs, map->length, sizeof (notmuch_string_pair_t), cmppair);

    map->sorted = TRUE;
}

static notmuch_bool_t
string_cmp (const char *a, const char *b, notmuch_bool_t exact)
{
    if (exact)
	return (strcmp (a, b));
    else
	return (strncmp (a, b, strlen (a)));
}

static notmuch_string_pair_t *
bsearch_first (notmuch_string_pair_t *array, size_t len, const char *key, notmuch_bool_t exact)
{
    size_t first = 0;
    size_t last = len - 1;
    size_t mid;

    if (len <= 0)
	return NULL;

    while (last > first) {
	mid = (first + last) / 2;
	int sign = string_cmp (key, array[mid].key, exact);

	if (sign <= 0)
	    last = mid;
	else
	    first = mid + 1;
    }


    if (string_cmp (key, array[first].key, exact) == 0)
	return array + first;
    else
	return NULL;

}

const char *
_notmuch_string_map_get (notmuch_string_map_t *map, const char *key)
{
    notmuch_string_pair_t *pair;

    /* this means that calling append invalidates iterators */
    _notmuch_string_map_sort (map);

    pair = bsearch_first (map->pairs, map->length, key, TRUE);
    if (! pair)
	return NULL;

    return pair->value;
}

notmuch_string_map_iterator_t *
_notmuch_string_map_iterator_create (notmuch_string_map_t *map, const char *key,
				     notmuch_bool_t exact)
{
    notmuch_string_map_iterator_t *iter;

    _notmuch_string_map_sort (map);

    iter = talloc (map, notmuch_string_map_iterator_t);
    if (unlikely (iter == NULL))
	return NULL;

    if (unlikely (talloc_reference (iter, map) == NULL))
	return NULL;

    iter->key = talloc_strdup (iter, key);
    iter->exact = exact;
    iter->current = bsearch_first (map->pairs, map->length, key, exact);
    return iter;
}

notmuch_bool_t
_notmuch_string_map_iterator_valid (notmuch_string_map_iterator_t *iterator)
{
    if (iterator->current == NULL)
	return FALSE;

    /* sentinel */
    if (iterator->current->key == NULL)
	return FALSE;

    return (0 == string_cmp (iterator->key, iterator->current->key, iterator->exact));

}

void
_notmuch_string_map_iterator_move_to_next (notmuch_string_map_iterator_t *iterator)
{

    if (! _notmuch_string_map_iterator_valid (iterator))
	return;

    (iterator->current)++;
}

const char *
_notmuch_string_map_iterator_key (notmuch_string_map_iterator_t *iterator)
{
    if (! _notmuch_string_map_iterator_valid (iterator))
	return NULL;

    return iterator->current->key;
}

const char *
_notmuch_string_map_iterator_value (notmuch_string_map_iterator_t *iterator)
{
    if (! _notmuch_string_map_iterator_valid (iterator))
	return NULL;

    return iterator->current->value;
}

void
_notmuch_string_map_iterator_destroy (notmuch_string_map_iterator_t *iterator)
{
    talloc_free (iterator);
}
