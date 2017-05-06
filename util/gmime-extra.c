#include "gmime-extra.h"

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


#else /* GMime >= 3.0 */

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


#endif
