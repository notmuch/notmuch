#ifndef _GMIME_EXTRA_H
#define _GMIME_EXTRA_H
#include <gmime/gmime.h>

GMimeStream *g_mime_stream_stdout_new(void);

#include <talloc.h>

/**
 * return talloc allocated date string
 */
char *g_mime_message_get_date_string (void *ctx, GMimeMessage *message);
InternetAddressList * g_mime_message_get_reply_to_list (GMimeMessage *message);

/**
 * return talloc allocated reply-to string
 */
char * g_mime_message_get_reply_to_string (void *ctx, GMimeMessage *message);


/**
 * Return glib allocated reply-to list
 */
InternetAddressList * g_mime_message_get_reply_to_list (GMimeMessage *message);

#endif
