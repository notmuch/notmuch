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
} show_message_state_t;

static void
show_message_part (mime_node_t *node,
		   show_message_state_t *state,
		   const notmuch_show_format_t *format,
		   int first)
{
    /* Formatters expect the envelope for embedded message parts */
    GMimeObject *part = node->envelope_part ?
	GMIME_OBJECT (node->envelope_part) : node->part;
    int i;

    if (!first)
	fputs (format->part_sep, stdout);

    /* Format this part */
    if (format->part_start)
	format->part_start (part, &(state->part_count));

    if (node->decrypt_attempted && format->part_encstatus)
	format->part_encstatus (node->decrypt_success);

    if (node->verify_attempted && format->part_sigstatus)
	format->part_sigstatus (node->sig_validity);

    format->part_content (part);

    if (node->envelope_part) {
	fputs (format->header_start, stdout);
	if (format->header_message_part)
	    format->header_message_part (GMIME_MESSAGE (node->part));
	fputs (format->header_end, stdout);

	fputs (format->body_start, stdout);
    }

    /* Recurse over the children */
    state->part_count += 1;
    for (i = 0; i < node->nchildren; i++)
	show_message_part (mime_node_child (node, i), state, format, i == 0);

    /* Finish this part */
    if (node->envelope_part)
	fputs (format->body_end, stdout);

    if (format->part_end)
	format->part_end (part);
}

notmuch_status_t
show_message_body (notmuch_message_t *message,
		   const notmuch_show_format_t *format,
		   notmuch_show_params_t *params)
{
    notmuch_status_t ret;
    show_message_state_t state;
    mime_node_t *root, *part;

    ret = mime_node_open (NULL, message, params->cryptoctx, params->decrypt,
			  &root);
    if (ret)
	return ret;

    /* The caller of show_message_body has already handled the
     * outermost envelope, so skip it. */
    state.part_count = MAX (params->part, 1);

    part = mime_node_seek_dfs (root, state.part_count);
    if (part)
	show_message_part (part, &state, format, TRUE);

    talloc_free (root);

    return NOTMUCH_STATUS_SUCCESS;
}
