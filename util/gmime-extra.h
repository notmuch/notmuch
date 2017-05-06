#ifndef _GMIME_EXTRA_H
#define _GMIME_EXTRA_H
#include <gmime/gmime.h>

GMimeStream *g_mime_stream_stdout_new(void);

#include <talloc.h>


#if (GMIME_MAJOR_VERSION < 3)

#define GMIME_ADDRESS_TYPE_TO GMIME_RECIPIENT_TYPE_TO
#define GMIME_ADDRESS_TYPE_CC GMIME_RECIPIENT_TYPE_CC
#define GMIME_ADDRESS_TYPE_BCC GMIME_RECIPIENT_TYPE_BCC


#else /* GMime >= 3.0 */
typedef GMimeAddressType GMimeRecipientType;
#endif

/**
 * Return the contents of the appropriate address header as a string
 * Should be freed using g_free
 */
char *g_mime_message_get_address_string (GMimeMessage *message, GMimeRecipientType type);

InternetAddressList * g_mime_message_get_addresses (GMimeMessage *message, GMimeRecipientType type);

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
