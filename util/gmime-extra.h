#ifndef _GMIME_EXTRA_H
#define _GMIME_EXTRA_H
#include <gmime/gmime.h>

GMimeStream *g_mime_stream_stdout_new(void);

#include <talloc.h>

/**
 * return talloc allocated date string
 */
char *g_mime_message_get_date_string (void *ctx, GMimeMessage *message);

#endif
