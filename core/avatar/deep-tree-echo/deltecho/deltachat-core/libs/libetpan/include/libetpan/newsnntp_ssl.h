#ifndef NEWSNNTP_SSL_H
#define NEWSNNTP_SSL_H
#ifdef __cplusplus
extern "C" {
#endif
#include <sys/types.h>
#ifdef HAVE_INTTYPES_H
#	include <inttypes.h>
#endif
#include <libetpan/newsnntp_types.h>
LIBETPAN_EXPORT
int newsnntp_ssl_connect(newsnntp * f, const char * server, uint16_t port);
LIBETPAN_EXPORT
int newsnntp_ssl_connect_with_callback(newsnntp * f, const char * server, uint16_t port,
void (* callback)(struct mailstream_ssl_context * ssl_context, void * data), void * data);
#ifdef __cplusplus
}
#endif
#endif