#ifndef MAILIMAP_SOCKET_H
#define MAILIMAP_SOCKET_H
#ifdef __cplusplus
extern "C" {
#endif
#ifdef HAVE_INTTYPES_H
#	include <inttypes.h>
#endif
#include <libetpan/mailimap_types.h>
LIBETPAN_EXPORT
int mailimap_socket_connect_voip(mailimap * f, const char * server, uint16_t port, int voip_enabled);
LIBETPAN_EXPORT
int mailimap_socket_connect(mailimap * f, const char * server, uint16_t port);
LIBETPAN_EXPORT
int mailimap_socket_starttls(mailimap * f);
LIBETPAN_EXPORT
int mailimap_socket_starttls_with_callback(mailimap * f,
void (* callback)(struct mailstream_ssl_context * ssl_context, void * data), void * data);
#ifdef __cplusplus
}
#endif
#endif