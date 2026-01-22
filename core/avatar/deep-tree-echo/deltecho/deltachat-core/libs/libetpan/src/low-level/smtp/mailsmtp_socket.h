#ifndef MAILSMTP_SOCKET_H
#define MAILSMTP_SOCKET_H
#ifdef __cplusplus
extern "C" {
#endif
#include <libetpan/libetpan-config.h>
#include <libetpan/mailsmtp_types.h>
LIBETPAN_EXPORT
int mailsmtp_socket_connect(mailsmtp * session,
const char * server, uint16_t port);
LIBETPAN_EXPORT
int mailsmtp_socket_starttls(mailsmtp * session);
LIBETPAN_EXPORT
int mailsmtp_socket_starttls_with_callback(mailsmtp * session,
void (* callback)(struct mailstream_ssl_context * ssl_context, void * data), void * data);
#ifdef __cplusplus
}
#endif
#endif