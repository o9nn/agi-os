#ifndef MAILPOP3_SSL_H
#define MAILPOP3_SSL_H
#ifdef __cplusplus
extern "C" {
#endif
#ifdef HAVE_INTTYPES_H
#	include <inttypes.h>
#endif
#include <libetpan/mailpop3_types.h>
LIBETPAN_EXPORT
int mailpop3_ssl_connect(mailpop3 * f, const char * server, uint16_t port);
LIBETPAN_EXPORT
int mailpop3_ssl_connect_with_callback(mailpop3 * f, const char * server, uint16_t port,
void (* callback)(struct mailstream_ssl_context * ssl_context, void * data), void * data);
#ifdef __cplusplus
}
#endif
#endif