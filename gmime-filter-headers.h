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

#ifndef _GMIME_FILTER_HEADERS_H_
#define _GMIME_FILTER_HEADERS_H_

#include <gmime/gmime-filter.h>

G_BEGIN_DECLS

#define GMIME_TYPE_FILTER_HEADERS            (g_mime_filter_headers_get_type ())
#define GMIME_FILTER_HEADERS(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GMIME_TYPE_FILTER_HEADERS, GMimeFilterHeaders))
#define GMIME_FILTER_HEADERS_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GMIME_TYPE_FILTER_HEADERS, GMimeFilterHeadersClass))
#define GMIME_IS_FILTER_HEADERS(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GMIME_TYPE_FILTER_HEADERS))
#define GMIME_IS_FILTER_HEADERS_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GMIME_TYPE_FILTER_HEADERS))
#define GMIME_FILTER_HEADERS_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GMIME_TYPE_FILTER_HEADERS, GMimeFilterHeadersClass))

typedef struct _GMimeFilterHeaders GMimeFilterHeaders;
typedef struct _GMimeFilterHeadersClass GMimeFilterHeadersClass;

/**
 * GMimeFilterHeaders:
 * @parent_object: parent #GMimeFilter
 * @saw_nl: previous char was a \n
 * @line: temporary buffer for line unfolding
 * @line_size: size of currently allocated nemory for @line
 * @lineptr: pointer to the first unused character in @line
 *
 * A filter to decode rfc2047 encoded headers
 **/
struct _GMimeFilterHeaders {
	GMimeFilter parent_object;

	gboolean saw_nl;
	char *line;
	size_t line_size;
	char *lineptr;
};

struct _GMimeFilterHeadersClass {
	GMimeFilterClass parent_class;

};


GType g_mime_filter_headers_get_type (void);

GMimeFilter *g_mime_filter_headers_new (void);

G_END_DECLS


#endif /* _GMIME_FILTER_HEADERS_H_ */
