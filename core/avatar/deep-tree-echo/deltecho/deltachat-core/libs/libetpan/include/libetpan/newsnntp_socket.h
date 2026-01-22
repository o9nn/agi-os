#ifndef NEWSNNTP_SOCKET_H
#define NEWSNNTP_SOCKET_H
#ifdef __cplusplus
extern "C" {
#endif
#include <sys/types.h>
#ifdef HAVE_INTTYPES_H
#	include <inttypes.h>
#endif
#include <libetpan/newsnntp_types.h>
LIBETPAN_EXPORT
int newsnntp_socket_connect(newsnntp * f, const char * server, uint16_t port);
#ifdef __cplusplus
}
#endif
#endif