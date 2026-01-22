#ifndef MAILPOP3_SOCKET_H
#define MAILPOP3_SOCKET_H
#ifdef __cplusplus
extern "C" {
#endif
#ifdef HAVE_INTTYPES_H
#	include <inttypes.h>
#endif
#include <libetpan/mailpop3_types.h>
LIBETPAN_EXPORT
int mailpop3_socket_connect(mailpop3 * f, const char * server, uint16_t port);
LIBETPAN_EXPORT
int mailpop3_socket_starttls(mailpop3 * f);
LIBETPAN_EXPORT
int mailpop3_socket_starttls_with_callback(mailpop3 * f,
void (* callback)(struct mailstream_ssl_context * ssl_context, void * data), void * data);
#ifdef __cplusplus
}
#endif
#endif