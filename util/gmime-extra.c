#include "gmime-extra.h"
#include <string.h>

static
GMimeStream *
_gzfile_maybe_filter (GMimeStream *file_stream)
{
    char buf[4];
    int bytes_read;

    if ((bytes_read = g_mime_stream_read (file_stream, buf, sizeof (buf))) < 0)
	return NULL;

    if (g_mime_stream_reset (file_stream))
	return NULL;

    /* check for gzipped input */
    if (bytes_read >= 2 && buf[0] == 0x1f && (unsigned char) buf[1] == 0x8b) {
	GMimeStream *gzstream;
	GMimeFilter *gzfilter;

	gzfilter = g_mime_filter_gzip_new (GMIME_FILTER_GZIP_MODE_UNZIP, 0);
	if (! gzfilter)
	    return NULL;

	gzstream = g_mime_stream_filter_new (file_stream);
	if (! gzstream)
	    return NULL;

	/* ignore filter id */
	(void) g_mime_stream_filter_add ((GMimeStreamFilter *) gzstream, gzfilter);
	g_object_unref (gzfilter);
	g_object_unref (file_stream);
	return gzstream;
    } else {
	return file_stream;
    }
}

GMimeStream *
g_mime_stream_gzfile_new (int fd)
{
    GMimeStream *file_stream;

    file_stream = g_mime_stream_fs_new (fd);
    if (! file_stream)
	return NULL;

    return _gzfile_maybe_filter (file_stream);
}

GMimeStream *
g_mime_stream_gzfile_open (const char *filename)
{
    GMimeStream *file_stream;

    file_stream = g_mime_stream_fs_open (filename, 0, 0, NULL);
    if (! file_stream)
	return NULL;

    return _gzfile_maybe_filter (file_stream);
}

GMimeStream *
g_mime_stream_stdout_new ()
{
    GMimeStream *stream_stdout = NULL;
    GMimeStream *stream_buffered = NULL;

    stream_stdout = g_mime_stream_pipe_new (STDOUT_FILENO);
    if (! stream_stdout)
	return NULL;

    g_mime_stream_pipe_set_owner (GMIME_STREAM_PIPE (stream_stdout), FALSE);

    stream_buffered = g_mime_stream_buffer_new (stream_stdout, GMIME_STREAM_BUFFER_BLOCK_WRITE);

    g_object_unref (stream_stdout);

    return stream_buffered;
}

/**
 * copy a glib string into a talloc context, and free it.
 */
static char *
g_string_talloc_strdup (void *ctx, char *g_string)
{
    char *new_str = talloc_strdup (ctx, g_string);

    g_free (g_string);
    return new_str;
}

const char *
g_mime_certificate_get_valid_userid (GMimeCertificate *cert)
{
    /* output user id only if validity is FULL or ULTIMATE. */
    const char *uid = g_mime_certificate_get_user_id (cert);

    if (uid == NULL)
	return uid;
    GMimeValidity validity = g_mime_certificate_get_id_validity (cert);

    if (validity == GMIME_VALIDITY_FULL || validity == GMIME_VALIDITY_ULTIMATE)
	return uid;
    return NULL;
}

const char *
g_mime_certificate_get_valid_email (GMimeCertificate *cert)
{
    /* output e-mail address only if validity is FULL or ULTIMATE. */
    const char *email = g_mime_certificate_get_email(cert);

    if (email == NULL)
	return email;
    GMimeValidity validity = g_mime_certificate_get_id_validity (cert);

    if (validity == GMIME_VALIDITY_FULL || validity == GMIME_VALIDITY_ULTIMATE)
	return email;
    return NULL;
}

const char *
g_mime_certificate_get_fpr16 (GMimeCertificate *cert)
{
    const char *fpr = g_mime_certificate_get_fingerprint (cert);

    if (! fpr || strlen (fpr) < 16)
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
    GDateTime *parsed_date = g_mime_message_get_date (message);

    if (parsed_date) {
	char *date = g_mime_utils_header_format_date (parsed_date);
	return g_string_talloc_strdup (ctx, date);
    } else {
	return talloc_strdup (ctx, "Thu, 01 Jan 1970 00:00:00 +0000");
    }
}

InternetAddressList *
g_mime_message_get_reply_to_list (GMimeMessage *message)
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
g_mime_signature_status_good (GMimeSignatureStatus status)
{
    return ((status & (GMIME_SIGNATURE_STATUS_RED | GMIME_SIGNATURE_STATUS_ERROR_MASK)) == 0);
}

gboolean
g_mime_signature_status_bad (GMimeSignatureStatus status)
{
    return (status & GMIME_SIGNATURE_STATUS_RED);
}

gboolean
g_mime_signature_status_error (GMimeSignatureStatus status)
{
    return (status & GMIME_SIGNATURE_STATUS_ERROR_MASK);
}

gint64
g_mime_utils_header_decode_date_unix (const char *date)
{
    GDateTime *parsed_date = g_mime_utils_header_decode_date (date);
    time_t ret;

    if (parsed_date) {
	ret = g_date_time_to_unix (parsed_date);
	g_date_time_unref (parsed_date);
    } else {
	ret = 0;
    }

    return ret;
}
