#ifndef _GMIME_EXTRA_H
#define _GMIME_EXTRA_H
#include <gmime/gmime.h>
#include <talloc.h>

#ifdef __cplusplus
extern "C" {
#endif

GMimeStream *g_mime_stream_stdout_new(void);

#define GMIME_ENABLE_RFC_2047_WORKAROUNDS 0xdeadbeef
#define g_mime_content_type_to_string(c) g_mime_content_type_get_mime_type (c)
#define g_mime_filter_crlf_new(encode,dots) g_mime_filter_dos2unix_new (FALSE)
#define g_mime_gpg_context_new(func,path) g_mime_gpg_context_new ()
#define g_mime_gpg_context_set_use_agent(ctx,val) /*ignore*/
#define g_mime_gpg_context_set_always_trust(ctx,val) /*ignore*/
#define g_mime_init(flags) g_mime_init()
#define g_mime_message_add_recipient(m,t,n,a) g_mime_message_add_mailbox (m,t,n,a)
#define g_mime_message_set_subject(m,s) g_mime_message_set_subject(m,s,NULL)
#define g_mime_multipart_signed_verify(mps,ctx,err) g_mime_multipart_signed_verify(mps, GMIME_ENCRYPT_NONE, err)
#define g_mime_object_write_to_stream(o,s) g_mime_object_write_to_stream (o,NULL,s)
#define g_mime_object_set_header(o,h,v) g_mime_object_set_header (o,h,v,NULL)
#define g_mime_parser_construct_message(p) g_mime_parser_construct_message (p, g_mime_parser_options_get_default ())
#define g_mime_part_get_content_object(p) g_mime_part_get_content (p)
#define g_mime_pkcs7_context_new(arg) g_mime_pkcs7_context_new()
#define g_mime_pkcs7_context_set_always_trust(ctx,val) /*ignore*/
#define g_mime_signature_get_errors(sig) g_mime_signature_get_status (sig)
#define g_mime_utils_header_decode_text(txt) g_mime_utils_header_decode_text (NULL, txt)
#define internet_address_to_string(ia,encode) internet_address_to_string (ia,NULL,encode)
#define internet_address_list_parse_string(str) internet_address_list_parse (NULL,str)

typedef GMimeAddressType GMimeRecipientType;

typedef GMimeSignatureStatus GMimeSignatureError;

/**
 * Get last 16 hex digits of fingerprint ("keyid")
 */
const char *g_mime_certificate_get_fpr16 (GMimeCertificate *cert);
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

void g_mime_parser_set_scan_from (GMimeParser *parser, gboolean flag);

gboolean g_mime_signature_status_good (GMimeSignatureStatus status);

gboolean g_mime_signature_status_bad (GMimeSignatureStatus status);

gboolean g_mime_signature_status_error (GMimeSignatureError status);

gint64 g_mime_utils_header_decode_date_unix (const char *date);

/**
 * Return string for valid User ID (or NULL if no valid User ID exists)
 */
const char * g_mime_certificate_get_valid_userid (GMimeCertificate *cert);

#ifdef __cplusplus
}
#endif

#endif
