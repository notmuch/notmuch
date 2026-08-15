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

struct _notmuch_indexopts {
    _notmuch_crypto_t crypto;

    char *filter_cmd;
};

notmuch_indexopts_t *
notmuch_database_get_default_indexopts (notmuch_database_t *db)
{
    notmuch_indexopts_t *ret = talloc_zero (db, notmuch_indexopts_t);

    if (! ret)
	return ret;
    ret->crypto.decrypt = NOTMUCH_DECRYPT_AUTO;

    char *decrypt_policy;
    notmuch_status_t err = notmuch_database_get_config (db, "index.decrypt", &decrypt_policy);

    if (err)
	goto FAIL;

    if (decrypt_policy) {
	if ((! (strcasecmp (decrypt_policy, "true"))) ||
	    (! (strcasecmp (decrypt_policy, "yes"))) ||
	    (! (strcasecmp (decrypt_policy, "1"))))
	    notmuch_indexopts_set_decrypt_policy (ret, NOTMUCH_DECRYPT_TRUE);
	else if ((! (strcasecmp (decrypt_policy, "false"))) ||
		 (! (strcasecmp (decrypt_policy, "no"))) ||
		 (! (strcasecmp (decrypt_policy, "0"))))
	    notmuch_indexopts_set_decrypt_policy (ret, NOTMUCH_DECRYPT_FALSE);
	else if (! strcasecmp (decrypt_policy, "nostash"))
	    notmuch_indexopts_set_decrypt_policy (ret, NOTMUCH_DECRYPT_NOSTASH);
    }

    free (decrypt_policy);

    char *filter_cmd;

    err = notmuch_database_get_config (db, "index.filter", &filter_cmd);
    if (err)
	goto FAIL;

    if (filter_cmd && *filter_cmd) {
	ret->filter_cmd = talloc_strdup (ret, filter_cmd);
	free (filter_cmd);
	if (! ret->filter_cmd)
	    goto FAIL;
    } else
	free (filter_cmd);

    return ret;

  FAIL:
    talloc_free (ret);
    return NULL;
}

notmuch_status_t
notmuch_indexopts_set_decrypt_policy (notmuch_indexopts_t *indexopts,
				      notmuch_decryption_policy_t decrypt_policy)
{
    if (! indexopts)
	return NOTMUCH_STATUS_NULL_POINTER;
    indexopts->crypto.decrypt = decrypt_policy;
    return NOTMUCH_STATUS_SUCCESS;
}

notmuch_decryption_policy_t
notmuch_indexopts_get_decrypt_policy (const notmuch_indexopts_t *indexopts)
{
    if (! indexopts)
	return false;
    return indexopts->crypto.decrypt;
}

notmuch_status_t
notmuch_indexopts_set_filter (notmuch_indexopts_t *indexopts,
			      const char *filter_cmd)
{
    talloc_free (indexopts->filter_cmd);
    indexopts->filter_cmd = talloc_strdup (indexopts, filter_cmd);
    if (! indexopts->filter_cmd)
	return NOTMUCH_STATUS_OUT_OF_MEMORY;
    return NOTMUCH_STATUS_SUCCESS;
}

const char *
notmuch_indexopts_get_filter (const notmuch_indexopts_t *indexopts)
{
    return indexopts ? indexopts->filter_cmd : NULL;
}

void
notmuch_indexopts_destroy (notmuch_indexopts_t *indexopts)
{
    talloc_free (indexopts);
}
