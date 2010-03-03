/*
 * Copyright © 2009 Keith Packard <keithp@keithp.com>
 * Copyright © 2010 Michal Sojka <sojkam1@fel.cvut.cz>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 */

#include "gmime-filter-headers.h"
#include <string.h>
#include <gmime/gmime-utils.h>
#include <glib/gprintf.h>
#include <stdlib.h>
#include <xutil.h>

/**
 * SECTION: gmime-filter-headers
 * @title: GMimeFilterHeaders
 * @short_description: Add/remove headers markers
 *
 * A #GMimeFilter for decoding rfc2047 encoded headers to UTF-8
 **/


static void g_mime_filter_headers_class_init (GMimeFilterHeadersClass *klass);
static void g_mime_filter_headers_init (GMimeFilterHeaders *filter, GMimeFilterHeadersClass *klass);
static void g_mime_filter_headers_finalize (GObject *object);

static GMimeFilter *filter_copy (GMimeFilter *filter);
static void filter_filter (GMimeFilter *filter, char *in, size_t len, size_t prespace,
			   char **out, size_t *outlen, size_t *outprespace);
static void filter_complete (GMimeFilter *filter, char *in, size_t len, size_t prespace,
			     char **out, size_t *outlen, size_t *outprespace);
static void filter_reset (GMimeFilter *filter);


static GMimeFilterClass *parent_class = NULL;

GType
g_mime_filter_headers_get_type (void)
{
	static GType type = 0;

	if (!type) {
		static const GTypeInfo info = {
			sizeof (GMimeFilterHeadersClass),
			NULL, /* base_class_init */
			NULL, /* base_class_finalize */
			(GClassInitFunc) g_mime_filter_headers_class_init,
			NULL, /* class_finalize */
			NULL, /* class_data */
			sizeof (GMimeFilterHeaders),
			0,    /* n_preallocs */
			(GInstanceInitFunc) g_mime_filter_headers_init,
			NULL	/* value_table */
		};

		type = g_type_register_static (GMIME_TYPE_FILTER, "GMimeFilterHeaders", &info, (GTypeFlags) 0);
	}

	return type;
}


static void
g_mime_filter_headers_class_init (GMimeFilterHeadersClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS (klass);
	GMimeFilterClass *filter_class = GMIME_FILTER_CLASS (klass);

	parent_class = (GMimeFilterClass *) g_type_class_ref (GMIME_TYPE_FILTER);

	object_class->finalize = g_mime_filter_headers_finalize;

	filter_class->copy = filter_copy;
	filter_class->filter = filter_filter;
	filter_class->complete = filter_complete;
	filter_class->reset = filter_reset;
}

static void
g_mime_filter_headers_init (GMimeFilterHeaders *filter, GMimeFilterHeadersClass *klass)
{
	(void) klass;
	filter->saw_nl = TRUE;
	filter->line = NULL;
	filter->line_size = 0;
	filter->lineptr = NULL;
}

static void
g_mime_filter_headers_finalize (GObject *object)
{
	free (GMIME_FILTER_HEADERS (object)->line);
	G_OBJECT_CLASS (parent_class)->finalize (object);
}


static GMimeFilter *
filter_copy (GMimeFilter *filter)
{
	(void) filter;
	return g_mime_filter_headers_new ();
}

static void
output_decoded_header (GMimeFilterHeaders *headers, char **outptr)
{
	char *colon, *name, *s, *decoded_value;
	size_t offset;
	gint ret;

	colon = strchr (headers->line, ':');
	if (colon == NULL)
		return;

	name = headers->line;
	*colon = '\0';
	s = colon + 1;
	while (*s == ' ' || *s == '\t')
		s++;
	decoded_value = g_mime_utils_header_decode_text(s);
	if (decoded_value == NULL)
		return;
	offset = *outptr - GMIME_FILTER (headers)->outbuf;
	g_mime_filter_set_size (GMIME_FILTER (headers), strlen(name) + 2 +
			       strlen(decoded_value) + 2, TRUE);
	*outptr = GMIME_FILTER (headers)->outbuf + offset;
	ret = g_sprintf (*outptr, "%s: %s\n", name, decoded_value);
	if (ret > 0)
		*outptr += ret;
	free (decoded_value);
}

static void
output_final_newline (GMimeFilterHeaders *headers, char **outptr)
{
	size_t offset;

	offset = *outptr - GMIME_FILTER (headers)->outbuf;
	g_mime_filter_set_size (GMIME_FILTER (headers), 1, TRUE);
	*outptr = GMIME_FILTER (headers)->outbuf + offset;
	*(*outptr)++ = '\n';
}

static void
filter_filter (GMimeFilter *filter, char *inbuf, size_t inlen, size_t prespace,
	       char **outbuf, size_t *outlen, size_t *outprespace)
{
	GMimeFilterHeaders *headers = (GMimeFilterHeaders *) filter;
	register const char *inptr = inbuf;
	const char *inend = inbuf + inlen;
	char *lineptr, *lineend, *outptr;

	(void) prespace;
	if (headers->line == NULL) {
		headers->line_size = 200;
		headers->lineptr = headers->line = malloc (headers->line_size);
	}
	lineptr = headers->lineptr;
	lineend = headers->line + headers->line_size;
	if (lineptr == NULL)
		return;
	outptr = filter->outbuf;
	while (inptr < inend) {
		if (*inptr == '\n') {
			if (headers->saw_nl)
				output_final_newline(headers, &outptr);
			headers->saw_nl = TRUE;
			inptr++;
			continue;
		}

		if (lineptr == lineend) {
			headers->line_size *= 2;
			headers->line = xrealloc (headers->line, headers->line_size);
			lineptr = headers->line + headers->line_size / 2;
			lineend = headers->line + headers->line_size;
		}

		if (headers->saw_nl && *inptr != ' ' && *inptr != '\t') {
			*lineptr = '\0';
			output_decoded_header (headers, &outptr);
			lineptr = headers->line;
		}
		if (headers->saw_nl && (*inptr == ' ' || *inptr == '\t')) {
			*lineptr = ' ';
			lineptr++;
			while (inptr < inend && (*inptr == ' ' || *inptr == '\t'))
				inptr++;
			headers->saw_nl = FALSE;
			continue;
		}
		headers->saw_nl = FALSE;

		if (*inptr != '\r')
			*lineptr++ = *inptr;
		inptr++;
	}
	if (headers->saw_nl) {
		*lineptr = '\0';
		output_decoded_header (headers, &outptr);
		lineptr = headers->line;
	}
	headers->lineptr = lineptr;
	*outlen = outptr - filter->outbuf;
	*outprespace = filter->outpre;
	*outbuf = filter->outbuf;
}

static void
filter_complete (GMimeFilter *filter, char *inbuf, size_t inlen, size_t prespace,
		 char **outbuf, size_t *outlen, size_t *outprespace)
{
	if (inbuf && inlen)
		filter_filter (filter, inbuf, inlen, prespace, outbuf, outlen, outprespace);
}

static void
filter_reset (GMimeFilter *filter)
{
	GMimeFilterHeaders *headers = (GMimeFilterHeaders *) filter;

	headers->saw_nl = TRUE;
	free(headers->line);
	headers->line = NULL;
	headers->line_size = 0;
}


/**
 * g_mime_filter_headers_new:
 * @encode: %TRUE if the filter should encode or %FALSE otherwise
 * @dots: encode/decode dots (as for SMTP)
 *
 * Creates a new #GMimeFilterHeaders filter.
 *
 * If @encode is %TRUE, then all lines will be prefixed by "> ",
 * otherwise any lines starting with "> " will have that removed
 *
 * Returns: a new #GMimeFilterHeaders filter.
 **/
GMimeFilter *
g_mime_filter_headers_new (void)
{
	GMimeFilterHeaders *new_headers;

	new_headers = (GMimeFilterHeaders *) g_object_newv (GMIME_TYPE_FILTER_HEADERS, 0, NULL);

	return (GMimeFilter *) new_headers;
}

