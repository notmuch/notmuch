#include "gmime-extra.h"
#include <string.h>

GMimeStream *
g_mime_stream_stdout_new()
{
    GMimeStream *stream_stdout = NULL;
    GMimeStream *stream_buffered = NULL;

    stream_stdout = g_mime_stream_pipe_new (STDOUT_FILENO);
    if (!stream_stdout)
	return NULL;

    g_mime_stream_pipe_set_owner (GMIME_STREAM_PIPE (stream_stdout), FALSE);

    stream_buffered = g_mime_stream_buffer_new (stream_stdout, GMIME_STREAM_BUFFER_BLOCK_WRITE);

    g_object_unref (stream_stdout);

    return stream_buffered;
}

/**
 * copy a glib string into a talloc context, and free it.
 */
static char*
g_string_talloc_strdup (void *ctx, char *g_string)
{
    char *new_str = talloc_strdup (ctx, g_string);
    g_free (g_string);
    return new_str;
}

#if (GMIME_MAJOR_VERSION < 3)

char *
g_mime_message_get_address_string (GMimeMessage *message, GMimeRecipientType type)
{
    InternetAddressList *list = g_mime_message_get_recipients (message, type);
    return internet_address_list_to_string (list, 0);
}

inline InternetAddressList *
g_mime_message_get_addresses (GMimeMessage *message, GMimeRecipientType type)
{
    return g_mime_message_get_recipients (message, type);
}

char *
g_mime_message_get_date_string (void *ctx, GMimeMessage *message)
{
    char *date = g_mime_message_get_date_as_string (message);
    return g_string_talloc_strdup (ctx, date);
}

InternetAddressList *
g_mime_message_get_from (GMimeMessage *message)
{
    return internet_address_list_parse_string (g_mime_message_get_sender (message));
}

const char *
g_mime_message_get_from_string (GMimeMessage *message) {
    return  g_mime_message_get_sender (message);
}

InternetAddressList *
g_mime_message_get_reply_to_list (GMimeMessage *message)
{
    const char *reply_to;

    reply_to = g_mime_message_get_reply_to (message);
    if (reply_to && *reply_to)
	return internet_address_list_parse_string (reply_to);
    else
	return NULL;
}

/**
 * return talloc allocated reply-to string
 */
char *
g_mime_message_get_reply_to_string (void *ctx, GMimeMessage *message)
{
    return talloc_strdup(ctx, g_mime_message_get_reply_to (message));
}

gboolean
g_mime_signature_status_good (GMimeSignatureStatus status) {
    return (status == GMIME_SIGNATURE_STATUS_GOOD);
}

gboolean
g_mime_signature_status_bad (GMimeSignatureStatus status) {
    return (status == GMIME_SIGNATURE_STATUS_BAD);
}

gboolean
g_mime_signature_status_error (GMimeSignatureError error) {
    return (error != GMIME_SIGNATURE_ERROR_NONE);
}

gint64
g_mime_utils_header_decode_date_unix (const char *date) {
    return (gint64) g_mime_utils_header_decode_date (date, NULL);
}

#else /* GMime >= 3.0 */

const char*
g_mime_certificate_get_fpr16 (GMimeCertificate *cert) {
    const char *fpr = g_mime_certificate_get_fingerprint (cert);
    if (!fpr || strlen (fpr) < 16)
	return fpr;

    return fpr + (strlen (fpr) - 16);
}

char *
g_mime_message_get_address_string (GMimeMessage *message, GMimeAddressType type)
{
    InternetAddressList *list = g_mime_message_get_addresses (message, type);
    return internet_address_list_to_string (list, NULL, 0);
}

char *
g_mime_message_get_date_string (void *ctx, GMimeMessage *message)
{
    GDateTime* parsed_date = g_mime_message_get_date (message);
    if (parsed_date) {
	char *date = g_mime_utils_header_format_date (parsed_date);
	return g_string_talloc_strdup (ctx, date);
    } else {
	return talloc_strdup(ctx, "Thu, 01 Jan 1970 00:00:00 +0000");
    }
}

InternetAddressList *
g_mime_message_get_reply_to_list(GMimeMessage *message)
{
    return g_mime_message_get_reply_to (message);
}

const char *
g_mime_message_get_from_string (GMimeMessage *message)
{
    return g_mime_object_get_header (GMIME_OBJECT (message), "From");
}

char *
g_mime_message_get_reply_to_string (void *ctx, GMimeMessage *message)
{
    InternetAddressList *list = g_mime_message_get_reply_to (message);
    return g_string_talloc_strdup (ctx, internet_address_list_to_string (list, NULL, 0));
}

void
g_mime_parser_set_scan_from (GMimeParser *parser, gboolean flag)
{
    g_mime_parser_set_format (parser, flag ? GMIME_FORMAT_MBOX : GMIME_FORMAT_MESSAGE);
}

/* In GMime 3.0, status GOOD and VALID both imply something about the
 * validity of the UIDs attached to the signing key. This forces us to
 * use following somewhat relaxed definition of a "good" signature to
 * preserve current notmuch semantics.
 */

gboolean
g_mime_signature_status_good (GMimeSignatureStatus status) {
    return ((status  & (GMIME_SIGNATURE_STATUS_RED | GMIME_SIGNATURE_STATUS_ERROR_MASK)) == 0);
}

gboolean
g_mime_signature_status_bad (GMimeSignatureStatus status) {
    return (status & GMIME_SIGNATURE_STATUS_RED);
}

gboolean
g_mime_signature_status_error (GMimeSignatureStatus status) {
    return (status & GMIME_SIGNATURE_STATUS_ERROR_MASK);
}

gint64
g_mime_utils_header_decode_date_unix (const char *date) {
    GDateTime* parsed_date = g_mime_utils_header_decode_date (date);
    time_t ret;

    if (parsed_date) {
	ret = g_date_time_to_unix (parsed_date);
	g_date_time_unref (parsed_date);
    } else {
	ret = 0;
    }

    return ret;
}

#endif
