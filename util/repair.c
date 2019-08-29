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
    GMimeTextPart *legacy_display;
    char *legacy_display_header_text = NULL;
    GMimeStream *stream = NULL;
    GMimeParser *parser = NULL;
    GMimeObject *legacy_header_object = NULL, *first;
    GMimeHeaderList *legacy_display_headers = NULL, *protected_headers = NULL;
    bool ret = false;

    if (! g_mime_content_type_is_type (g_mime_object_get_content_type (payload),
				       "multipart", "mixed"))
	return false;
    protected_header_parameter = g_mime_object_get_content_type_parameter (payload, "protected-headers");
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
    if (! g_mime_content_type_is_type (g_mime_object_get_content_type (first),
				       "text", "rfc822-headers"))
	return false;
    protected_header_parameter = g_mime_object_get_content_type_parameter (first, "protected-headers");
    if ((! protected_header_parameter) || strcmp (protected_header_parameter, "v1"))
	return false;
    if (! GMIME_IS_TEXT_PART (first))
	return false;

    /* ensure that the headers in the first part all match the values
     * found in the payload's own protected headers!  if they don't,
     * we should not treat this as a valid "legacy-display" part.
     *
     * Crafting a GMimeHeaderList object from the content of the
     * text/rfc822-headers part is pretty clumsy; we should probably
     * push something into GMime that makes this a one-shot
     * operation. */
    if ((protected_headers = g_mime_object_get_header_list (payload), protected_headers) &&
	(legacy_display = GMIME_TEXT_PART (first), legacy_display) &&
	(legacy_display_header_text = g_mime_text_part_get_text (legacy_display), legacy_display_header_text) &&
	(stream = g_mime_stream_mem_new_with_buffer (legacy_display_header_text, strlen (legacy_display_header_text)), stream) &&
	(g_mime_stream_write (stream, "\r\n\r\n", 4) == 4) &&
	(g_mime_stream_seek (stream, 0, GMIME_STREAM_SEEK_SET) == 0) &&
	(parser = g_mime_parser_new_with_stream (stream), parser) &&
	(legacy_header_object = g_mime_parser_construct_part (parser, NULL), legacy_header_object) &&
	(legacy_display_headers = g_mime_object_get_header_list (legacy_header_object), legacy_display_headers)) {
	/* walk through legacy_display_headers, comparing them against
	 * their values in the protected_headers: */
	ret = true;
	for (int i = 0; i < g_mime_header_list_get_count (legacy_display_headers); i++) {
	    GMimeHeader *dh = g_mime_header_list_get_header_at (legacy_display_headers, i);
	    if (dh == NULL) {
		ret = false;
		goto DONE;
	    }
	    GMimeHeader *ph = g_mime_header_list_get_header (protected_headers, g_mime_header_get_name (dh));
	    if (ph == NULL) {
		ret = false;
		goto DONE;
	    }
	    const char *dhv = g_mime_header_get_value (dh);
	    const char *phv = g_mime_header_get_value (ph);
	    if (dhv == NULL || phv == NULL || strcmp (dhv, phv)) {
		ret = false;
		goto DONE;
	    }
	}
    }

 DONE:
    if (legacy_display_header_text)
	g_free (legacy_display_header_text);
    if (stream)
	g_object_unref (stream);
    if (parser)
	g_object_unref (parser);
    if (legacy_header_object)
	g_object_unref (legacy_header_object);

    return ret;
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
