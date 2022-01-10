/*
 * Generate a random corpus of stub messages.
 *
 * Initial use case is testing dump and restore, so we only have
 * message-ids and tags.
 *
 * Generated message-id's and tags are intentionally nasty.
 *
 * Copyright (c) 2012 David Bremner
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
 * Author: David Bremner <david@tethera.net>
 */

#include <stdlib.h>
#include <assert.h>
#include <talloc.h>
#include <string.h>
#include <glib.h>
#include <math.h>

#include "notmuch-client.h"
#include "command-line-arguments.h"
#include "database-test.h"

/* Current largest Unicode value defined. Note that most of these will
 * be printed as boxes in most fonts.
 */

#define GLYPH_MAX 0x10FFFE


typedef struct {
    int weight;
    int start;
    int stop;
} char_class_t;

/*
 *  Choose about half ascii as test characters, as ascii
 *  punctuation and whitespace is the main cause of problems for
 *  the (old) restore parser.
 *
 *  We then favour code points with 2 byte encodings. Note that
 *  code points 0xD800-0xDFFF are forbidden in UTF-8.
 */

static const
char_class_t char_class[] = { { 0.50 * GLYPH_MAX, 0x0001, 0x007f },
			      { 0.75 * GLYPH_MAX, 0x0080, 0x07ff },
			      { 0.88 * GLYPH_MAX, 0x0800, 0xd7ff },
			      { 0.90 * GLYPH_MAX, 0xE000, 0xffff },
			      {        GLYPH_MAX, 0x10000, GLYPH_MAX } };

static gunichar
random_unichar ()
{
    int i;
    int class = random () % GLYPH_MAX;
    int size;

    for (i = 0; char_class[i].weight < class; i++) /* nothing */;

    size = char_class[i].stop - char_class[i].start + 1;

    return char_class[i].start + (random () % size);
}

static char *
random_utf8_string (void *ctx, size_t char_count)
{
    size_t offset = 0;
    size_t i;
    gchar *buf = NULL;
    size_t buf_size = 0;

    for (i = 0; i < char_count; i++) {
	gunichar randomchar;
	size_t written;

	/* 6 for one glyph, one for null, one for luck */
	while (buf_size <= offset + 8) {
	    buf_size = 2 * buf_size + 8;
	    buf = talloc_realloc (ctx, buf, gchar, buf_size);
	}

	do {
	    randomchar = random_unichar ();
	} while (randomchar == '\n');

	written = g_unichar_to_utf8 (randomchar, buf + offset);

	if (written <= 0) {
	    fprintf (stderr, "error converting to utf8\n");
	    exit (1);
	}

	offset += written;

    }
    buf[offset] = 0;
    return buf;
}

/* stubs since we cannot link with notmuch.o */
const notmuch_opt_desc_t notmuch_shared_options[] = {
    { }
};

const char *notmuch_requested_db_uuid = NULL;

void
notmuch_process_shared_options (unused (notmuch_database_t *notmuch),
				unused (const char *dummy))
{
}

int
notmuch_minimal_options (unused (const char *subcommand),
			 unused (int argc),
			 unused (char **argv))
{
    return 0;
}

int
main (int argc, char **argv)
{

    void *ctx = talloc_new (NULL);

    const char *config_path = NULL;
    notmuch_database_t *notmuch;

    int num_messages = 500;
    int max_tags = 10;
    // leave room for UTF-8 encoding.
    int tag_len = NOTMUCH_TAG_MAX / 6;
    // NOTMUCH_MESSAGE_ID_MAX is not exported, so we make a
    // conservative guess.
    int message_id_len = (NOTMUCH_TAG_MAX - 20) / 6;

    int seed = 734569;

    notmuch_opt_desc_t options[] = {
	{ .opt_string = &config_path, .name = "config-path" },
	{ .opt_int = &num_messages, .name = "num-messages" },
	{ .opt_int = &max_tags, .name = "max-tags" },
	{ .opt_int = &message_id_len, .name = "message-id-len" },
	{ .opt_int = &tag_len, .name = "tag-len" },
	{ .opt_int = &seed, .name = "seed" },
	{ }
    };

    int opt_index = parse_arguments (argc, argv, options, 1);

    if (opt_index < 0)
	exit (1);

    if (message_id_len < 1) {
	fprintf (stderr, "message id's must be least length 1\n");
	exit (1);
    }

    if (config_path == NULL) {
	fprintf (stderr, "configuration path must be specified");
	exit (1);
    }

    if (notmuch_database_open_with_config (NULL,
					   NOTMUCH_DATABASE_MODE_READ_WRITE,
					   config_path,
					   NULL,
					   &notmuch,
					   NULL))
	return 1;

    srandom (seed);

    int count;

    for (count = 0; count < num_messages; count++) {
	int j;
	/* explicitly allow zero tags */
	int num_tags = random () % (max_tags + 1);
	/* message ids should be non-empty */
	int this_mid_len = (random () % message_id_len) + 1;
	const char **tag_list;
	char *mid;
	notmuch_status_t status;

	do {
	    mid = random_utf8_string (ctx, this_mid_len);

	    tag_list = talloc_realloc (ctx, NULL, const char *, num_tags + 1);

	    for (j = 0; j < num_tags; j++) {
		int this_tag_len = random () % tag_len + 1;

		tag_list[j] = random_utf8_string (ctx, this_tag_len);
	    }

	    tag_list[j] = NULL;

	    status = notmuch_database_add_stub_message (notmuch, mid, tag_list);
	} while (status == NOTMUCH_STATUS_DUPLICATE_MESSAGE_ID);

	if (status != NOTMUCH_STATUS_SUCCESS) {
	    fprintf (stderr, "error %d adding message", status);
	    exit (status);
	}
    }

    notmuch_database_destroy (notmuch);

    talloc_free (ctx);

    return 0;
}
