#ifndef _GMIME_EXTRA_H
#define _GMIME_EXTRA_H
#include <gmime/gmime.h>

GMimeStream *g_mime_stream_stdout_new(void);

#include <talloc.h>

/**
 * return talloc allocated date string
 */

char *g_mime_message_get_date_string (void *ctx, GMimeMessage *message);

/**
 * glib allocated list of From: addresses
 */

InternetAddressList * g_mime_message_get_from (GMimeMessage *message);

/**
 * return string for From: address
 * (owned by gmime)
 */
const char * g_mime_message_get_from_string (GMimeMessage *message);

InternetAddressList * g_mime_message_get_reply_to_list (GMimeMessage *message);

/**
 * return talloc allocated reply-to string
 */
char * g_mime_message_get_reply_to_string (void *ctx, GMimeMessage *message);


#endif
