/* indexopts.c - options for indexing messages (currently a stub)
 *
 * Copyright © 2017 Daniel Kahn Gillmor
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
 * Author: Daniel Kahn Gillmor <dkg@fifthhorseman.net>
 */

#include "notmuch-private.h"

notmuch_indexopts_t *
notmuch_database_get_default_indexopts (notmuch_database_t unused (*db))
{
    return NULL;
}

void
notmuch_indexopts_destroy (notmuch_indexopts_t *indexopts)
{
    talloc_free (indexopts);
}
