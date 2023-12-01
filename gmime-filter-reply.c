/*
 * Copyright © 2009 Keith Packard <keithp@keithp.com>
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

#include <stdbool.h>

#include "gmime-filter-reply.h"
#include "notmuch-client.h"

/**
 * SECTION: gmime-filter-reply
 * @title: GMimeFilterReply
 * @short_description: Add/remove reply markers
 *
 * A #GMimeFilter for adding or removing reply markers
 **/


static void g_mime_filter_reply_class_init (GMimeFilterReplyClass *klass, void *class_data);
static void g_mime_filter_reply_init (GMimeFilterReply *filter, GMimeFilterReplyClass *klass);
static void g_mime_filter_reply_finalize (GObject *object);

static GMimeFilter *filter_copy (GMimeFilter *filter);
static void filter_filter (GMimeFilter *filter, char *in, size_t len, size_t prespace,
			   char **out, size_t *outlen, size_t *outprespace);
static void filter_complete (GMimeFilter *filter, char *in, size_t len, size_t prespace,
			     char **out, size_t *outlen, size_t *outprespace);
static void filter_reset (GMimeFilter *filter);


static GMimeFilterClass *parent_class = NULL;
static GType type = 0;
static const GTypeInfo info = {
    .class_size = sizeof (GMimeFilterReplyClass),
    .base_init = NULL,
    .base_finalize = NULL,
    .class_init = (GClassInitFunc) g_mime_filter_reply_class_init,
    .class_finalize = NULL,
    .class_data = NULL,
    .instance_size = sizeof (GMimeFilterReply),
    .n_preallocs = 0,
    .instance_init = (GInstanceInitFunc) g_mime_filter_reply_init,
    .value_table = NULL,
};


void
g_mime_filter_reply_module_init (void)
{
    type = g_type_register_static (GMIME_TYPE_FILTER, "GMimeFilterReply", &info, (GTypeFlags) 0);
    parent_class = (GMimeFilterClass *) g_type_class_ref (GMIME_TYPE_FILTER);
}

GType
g_mime_filter_reply_get_type (void)
{
    return type;
}


static void
g_mime_filter_reply_class_init (GMimeFilterReplyClass *klass, unused (void *class_data))
{
    GObjectClass *object_class = G_OBJECT_CLASS (klass);
    GMimeFilterClass *filter_class = GMIME_FILTER_CLASS (klass);

    object_class->finalize = g_mime_filter_reply_finalize;

    filter_class->copy = filter_copy;
    filter_class->filter = filter_filter;
    filter_class->complete = filter_complete;
    filter_class->reset = filter_reset;
}

static void
g_mime_filter_reply_init (GMimeFilterReply *filter, GMimeFilterReplyClass *klass)
{
    (void) klass;
    filter->saw_nl = true;
    filter->saw_angle = false;
}

static void
g_mime_filter_reply_finalize (GObject *object)
{
    G_OBJECT_CLASS (parent_class)->finalize (object);
}


static GMimeFilter *
filter_copy (GMimeFilter *filter)
{
    GMimeFilterReply *reply = (GMimeFilterReply *) filter;

    return g_mime_filter_reply_new (reply->encode);
}

static void
filter_filter (GMimeFilter *filter, char *inbuf, size_t inlen, size_t prespace,
	       char **outbuf, size_t *outlen, size_t *outprespace)
{
    GMimeFilterReply *reply = (GMimeFilterReply *) filter;
    const char *inptr = inbuf;
    const char *inend = inbuf + inlen;
    char *outptr;

    (void) prespace;
    if (reply->encode) {
	g_mime_filter_set_size (filter, 3 * inlen, false);

	outptr = filter->outbuf;
	while (inptr < inend) {
	    if (reply->saw_nl) {
		*outptr++ = '>';
		*outptr++ = ' ';
		reply->saw_nl = false;
	    }
	    if (*inptr == '\n')
		reply->saw_nl = true;
	    else
		reply->saw_nl = false;
	    if (*inptr != '\r')
		*outptr++ = *inptr;
	    inptr++;
	}
    } else {
	g_mime_filter_set_size (filter, inlen + 1, false);

	outptr = filter->outbuf;
	while (inptr < inend) {
	    if (reply->saw_nl) {
		if (*inptr == '>')
		    reply->saw_angle = true;
		else
		    *outptr++ = *inptr;
		reply->saw_nl = false;
	    } else if (reply->saw_angle) {
		if (*inptr == ' ')
		    ;
		else
		    *outptr++ = *inptr;
		reply->saw_angle = false;
	    } else if (*inptr != '\r') {
		if (*inptr == '\n')
		    reply->saw_nl = true;
		*outptr++ = *inptr;
	    }

	    inptr++;
	}
    }

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
    GMimeFilterReply *reply = (GMimeFilterReply *) filter;

    reply->saw_nl = true;
    reply->saw_angle = false;
}


/**
 * g_mime_filter_reply_new:
 * @encode: %true if the filter should encode or %false otherwise
 * @dots: encode/decode dots (as for SMTP)
 *
 * Creates a new #GMimeFilterReply filter.
 *
 * If @encode is %true, then all lines will be prefixed by "> ",
 * otherwise any lines starting with "> " will have that removed
 *
 * Returns: a new #GMimeFilterReply filter.
 **/
GMimeFilter *
g_mime_filter_reply_new (gboolean encode)
{
    GMimeFilterReply *new_reply;

    new_reply = (GMimeFilterReply *) g_object_new (GMIME_TYPE_FILTER_REPLY, NULL);
    new_reply->encode = encode;

    return (GMimeFilter *) new_reply;
}

