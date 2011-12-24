/* notmuch - Not much of an email program, (just index and search)
 *
 * Copyright © 2009 Carl Worth
 * Copyright © 2009 Keith Packard
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
 * Authors: Carl Worth <cworth@cworth.org>
 *	    Keith Packard <keithp@keithp.com>
 */

#include "notmuch-client.h"

typedef struct show_message_state {
    int part_count;
    int in_zone;
} show_message_state_t;

static void
show_message_part (GMimeObject *part,
		   show_message_state_t *state,
		   const notmuch_show_format_t *format,
		   notmuch_show_params_t *params,
		   int first)
{
    GMimeObject *decryptedpart = NULL;
    int selected;
    state->part_count += 1;

    if (! (GMIME_IS_PART (part) || GMIME_IS_MULTIPART (part) || GMIME_IS_MESSAGE_PART (part))) {
	fprintf (stderr, "Warning: Not displaying unknown mime part: %s.\n",
		 g_type_name (G_OBJECT_TYPE (part)));
	return;
    }

    selected = (params->part <= 0 || state->part_count == params->part);

    if (selected || state->in_zone) {
	if (!first && (params->part <= 0 || state->in_zone))
	    fputs (format->part_sep, stdout);

	if (format->part_start)
	    format->part_start (part, &(state->part_count));
    }

    /* handle PGP/MIME parts */
    if (GMIME_IS_MULTIPART (part) && params->cryptoctx) {
	GMimeMultipart *multipart = GMIME_MULTIPART (part);
	GError* err = NULL;

	if (GMIME_IS_MULTIPART_ENCRYPTED (part) && params->decrypt)
	{
	    if ( g_mime_multipart_get_count (multipart) != 2 ) {
		/* this violates RFC 3156 section 4, so we won't bother with it. */
		fprintf (stderr,
			 "Error: %d part(s) for a multipart/encrypted message (should be exactly 2)\n",
			 g_mime_multipart_get_count (multipart));
	    } else {
		GMimeMultipartEncrypted *encrypteddata = GMIME_MULTIPART_ENCRYPTED (part);
		decryptedpart = g_mime_multipart_encrypted_decrypt (encrypteddata, params->cryptoctx, &err);
		if (decryptedpart) {
		    if ((selected || state->in_zone) && format->part_encstatus)
			format->part_encstatus (1);
		    const GMimeSignatureValidity *sigvalidity = g_mime_multipart_encrypted_get_signature_validity (encrypteddata);
		    if (!sigvalidity)
			fprintf (stderr, "Failed to verify signed part: %s\n", (err ? err->message : "no error explanation given"));
		    if ((selected || state->in_zone) && format->part_sigstatus)
			format->part_sigstatus (sigvalidity);
		} else {
		    fprintf (stderr, "Failed to decrypt part: %s\n", (err ? err->message : "no error explanation given"));
		    if ((selected || state->in_zone) && format->part_encstatus)
			format->part_encstatus (0);
		}
	    }
	}
	else if (GMIME_IS_MULTIPART_SIGNED (part))
	{
	    if ( g_mime_multipart_get_count (multipart) != 2 ) {
		/* this violates RFC 3156 section 5, so we won't bother with it. */
		fprintf (stderr,
			 "Error: %d part(s) for a multipart/signed message (should be exactly 2)\n",
			 g_mime_multipart_get_count (multipart));
	    } else {
		/* For some reason the GMimeSignatureValidity returned
		 * here is not a const (inconsistent with that
		 * returned by
		 * g_mime_multipart_encrypted_get_signature_validity,
		 * and therefore needs to be properly disposed of.
		 * Hopefully the API will become more consistent. */
		GMimeSignatureValidity *sigvalidity = g_mime_multipart_signed_verify (GMIME_MULTIPART_SIGNED (part), params->cryptoctx, &err);
		if (!sigvalidity) {
		    fprintf (stderr, "Failed to verify signed part: %s\n", (err ? err->message : "no error explanation given"));
		}
		if ((selected || state->in_zone) && format->part_sigstatus)
		    format->part_sigstatus (sigvalidity);
		if (sigvalidity)
		    g_mime_signature_validity_free (sigvalidity);
	    }
	}

	if (err)
	    g_error_free (err);
    }
    /* end handle PGP/MIME parts */

    if (selected || state->in_zone)
	format->part_content (part);

    if (GMIME_IS_MULTIPART (part)) {
	GMimeMultipart *multipart = GMIME_MULTIPART (part);
	int i;

	if (selected)
	    state->in_zone = 1;

	if (decryptedpart) {
	    /* We emit the useless application/pgp-encrypted version
	     * part here only to keep the emitted output as consistent
	     * as possible between decrypted output and the
	     * unprocessed multipart/mime. For some strange reason,
	     * the actual encrypted data is the second part of the
	     * multipart. */
	    show_message_part (g_mime_multipart_get_part (multipart, 0), state, format, params, TRUE);
	    show_message_part (decryptedpart, state, format, params, FALSE);
	} else {
	    for (i = 0; i < g_mime_multipart_get_count (multipart); i++) {
		show_message_part (g_mime_multipart_get_part (multipart, i),
				   state, format, params, i == 0);
	    }
	}

	if (selected)
	    state->in_zone = 0;

    } else if (GMIME_IS_MESSAGE_PART (part)) {
	GMimeMessage *mime_message = g_mime_message_part_get_message (GMIME_MESSAGE_PART (part));

	if (selected)
	    state->in_zone = 1;

	if (selected || (!selected && state->in_zone)) {
	    fputs (format->header_start, stdout);
	    if (format->header_message_part)
		format->header_message_part (mime_message);
	    fputs (format->header_end, stdout);

	    fputs (format->body_start, stdout);
	}

	show_message_part (g_mime_message_get_mime_part (mime_message),
			   state, format, params, TRUE);

	if (selected || (!selected && state->in_zone))
	    fputs (format->body_end, stdout);

	if (selected)
	    state->in_zone = 0;
    }

    if (selected || state->in_zone) {
	if (format->part_end)
	    format->part_end (part);
    }
}

notmuch_status_t
show_message_body (notmuch_message_t *message,
		   const notmuch_show_format_t *format,
		   notmuch_show_params_t *params)
{
    GMimeStream *stream = NULL;
    GMimeParser *parser = NULL;
    GMimeMessage *mime_message = NULL;
    notmuch_status_t ret = NOTMUCH_STATUS_SUCCESS;
    const char *filename = notmuch_message_get_filename (message);
    FILE *file = NULL;
    show_message_state_t state;

    state.part_count = 0;
    state.in_zone = 0;

    file = fopen (filename, "r");
    if (! file) {
	fprintf (stderr, "Error opening %s: %s\n", filename, strerror (errno));
	ret = NOTMUCH_STATUS_FILE_ERROR;
	goto DONE;
    }

    stream = g_mime_stream_file_new (file);
    g_mime_stream_file_set_owner (GMIME_STREAM_FILE (stream), FALSE);

    parser = g_mime_parser_new_with_stream (stream);

    mime_message = g_mime_parser_construct_message (parser);

    show_message_part (g_mime_message_get_mime_part (mime_message),
		       &state,
		       format,
		       params,
		       TRUE);

  DONE:
    if (mime_message)
	g_object_unref (mime_message);

    if (parser)
	g_object_unref (parser);

    if (stream)
	g_object_unref (stream);

    if (file)
	fclose (file);

    return ret;
}
