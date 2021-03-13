/* notmuch - Not much of an email program, (just index and search)
 *
 * Copyright © 2019 Daniel Kahn Gillmor
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
 * Authors: Daniel Kahn Gillmor <dkg@fifthhorseman.net>
 */

#include <stdbool.h>
#include "repair.h"


static bool
_notmuch_crypto_payload_has_legacy_display (GMimeObject *payload)
{
    GMimeMultipart *mpayload;
    const char *protected_header_parameter;
    GMimeObject *first;

    if (! g_mime_content_type_is_type (g_mime_object_get_content_type (payload),
				       "multipart", "mixed"))
	return false;
    protected_header_parameter = g_mime_object_get_content_type_parameter (payload,
									   "protected-headers");
    if ((! protected_header_parameter) || strcmp (protected_header_parameter, "v1"))
	return false;
    if (! GMIME_IS_MULTIPART (payload))
	return false;
    mpayload = GMIME_MULTIPART (payload);
    if (mpayload == NULL)
	return false;
    if (g_mime_multipart_get_count (mpayload) != 2)
	return false;
    first = g_mime_multipart_get_part (mpayload, 0);
    /* Early implementations that generated "Legacy Display" parts used
     * Content-Type: text/rfc822-headers, but text/plain is more widely
     * rendered, so it is now the standard choice.  We accept either as a
     * Legacy Display part. */
    if (! (g_mime_content_type_is_type (g_mime_object_get_content_type (first),
					"text", "plain") ||
	   g_mime_content_type_is_type (g_mime_object_get_content_type (first),
					"text", "rfc822-headers")))
	return false;
    protected_header_parameter = g_mime_object_get_content_type_parameter (first,
									   "protected-headers");
    if ((! protected_header_parameter) || strcmp (protected_header_parameter, "v1"))
	return false;
    if (! GMIME_IS_TEXT_PART (first))
	return false;

    return true;
}

GMimeObject *
_notmuch_repair_crypto_payload_skip_legacy_display (GMimeObject *payload)
{
    if (_notmuch_crypto_payload_has_legacy_display (payload)) {
	return g_mime_multipart_get_part (GMIME_MULTIPART (payload), 1);
    } else {
	return payload;
    }
}

/* see
 * https://tools.ietf.org/html/draft-dkg-openpgp-pgpmime-message-mangling-00#section-4.1.1 */
static bool
_notmuch_is_mixed_up_mangled (GMimeObject *part)
{
    GMimeMultipart *mpart = NULL;
    GMimeObject *parts[3] = { NULL, NULL, NULL };
    GMimeContentType *type = NULL;
    char *prelude_string = NULL;
    bool prelude_is_empty;

    if (part == NULL)
	return false;
    type = g_mime_object_get_content_type (part);
    if (type == NULL)
	return false;
    if (! g_mime_content_type_is_type (type, "multipart", "mixed"))
	return false;
    if (! GMIME_IS_MULTIPART (part)) /* probably impossible */
	return false;
    mpart = GMIME_MULTIPART (part);
    if (mpart == NULL)
	return false;
    if (g_mime_multipart_get_count (mpart) != 3)
	return false;
    parts[0] = g_mime_multipart_get_part (mpart, 0);
    if (! g_mime_content_type_is_type (g_mime_object_get_content_type (parts[0]),
				       "text", "plain"))
	return false;
    if (! GMIME_IS_TEXT_PART (parts[0]))
	return false;
    parts[1] = g_mime_multipart_get_part (mpart, 1);
    if (! g_mime_content_type_is_type (g_mime_object_get_content_type (parts[1]),
				       "application", "pgp-encrypted"))
	return false;
    parts[2] = g_mime_multipart_get_part (mpart, 2);
    if (! g_mime_content_type_is_type (g_mime_object_get_content_type (parts[2]),
				       "application", "octet-stream"))
	return false;

    /* Is parts[0] length 0? */
    prelude_string = g_mime_text_part_get_text (GMIME_TEXT_PART (parts[0]));
    prelude_is_empty = (prelude_string[0] == '\0');
    g_free (prelude_string);
    if (! prelude_is_empty)
	return false;

    /* FIXME: after decoding and stripping whitespace, is parts[1]
     * subpart just "Version: 1" ? */

    /* FIXME: can we determine that parts[2] subpart is *only* PGP
     * encrypted data?  I tried g_mime_part_get_openpgp_data () but
     * found https://github.com/jstedfast/gmime/issues/60 */

    return true;
}


/* see
 * https://tools.ietf.org/html/draft-dkg-openpgp-pgpmime-message-mangling-00#section-4.1.2 */
GMimeObject *
_notmuch_repair_mixed_up_mangled (GMimeObject *part)
{
    GMimeMultipart *mpart = NULL, *mpart_ret = NULL;
    GMimeObject *ret = NULL;

    if (! _notmuch_is_mixed_up_mangled (part))
	return NULL;
    mpart = GMIME_MULTIPART (part);
    ret = GMIME_OBJECT (g_mime_multipart_encrypted_new ());
    if (ret == NULL)
	return NULL;
    mpart_ret = GMIME_MULTIPART (ret);
    if (mpart_ret == NULL) {
	g_object_unref (ret);
	return NULL;
    }
    g_mime_object_set_content_type_parameter (ret, "protocol", "application/pgp-encrypted");

    g_mime_multipart_insert (mpart_ret, 0, g_mime_multipart_get_part (mpart, 1));
    g_mime_multipart_insert (mpart_ret, 1, g_mime_multipart_get_part (mpart, 2));
    return ret;
}
