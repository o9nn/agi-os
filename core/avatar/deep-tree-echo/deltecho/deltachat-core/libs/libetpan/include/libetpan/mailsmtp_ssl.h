#ifndef MAILSMTP_SSL_H
#define MAILSMTP_SSL_H
#ifdef __cplusplus
extern "C" {
#endif
#ifdef HAVE_INTTYPES_H
#	include <inttypes.h>
#endif
#include <libetpan/mailsmtp_types.h>
LIBETPAN_EXPORT
int mailsmtp_ssl_connect(mailsmtp * session,
const char * server, uint16_t port);
LIBETPAN_EXPORT
int mailsmtp_ssl_connect_with_callback(mailsmtp * session,
const char * server, uint16_t port,
void (* callback)(struct mailstream_ssl_context * ssl_context, void * data), void * data);
#ifdef __cplusplus
}
#endif
#endif