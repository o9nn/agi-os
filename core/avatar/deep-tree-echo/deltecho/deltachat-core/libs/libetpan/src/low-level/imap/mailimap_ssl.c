#ifdef HAVE_CONFIG_H
#	include <config.h>
#endif
#include "mailimap_ssl.h"
#include "mailimap.h"
#include "connect.h"
#include <stdlib.h>
#ifdef HAVE_NETINET_IN_H
#	include <netinet/in.h>
#endif
#ifdef HAVE_UNISTD_H
#	include <unistd.h>
#endif
#include "mailstream_cfstream.h"
#define DEFAULT_IMAPS_PORT 993
#define SERVICE_NAME_IMAPS "imaps"
#define SERVICE_TYPE_TCP "tcp"
#if HAVE_CFNETWORK
static int mailimap_cfssl_connect_voip(mailimap * f, const char * server, uint16_t port, int voip_enabled);
#endif
int mailimap_ssl_connect_with_callback(mailimap * f, const char * server, uint16_t port,
void (* callback)(struct mailstream_ssl_context * ssl_context, void * data), void * data)
{
return mailimap_ssl_connect_voip_with_callback(f, server, port, mailstream_cfstream_voip_enabled, callback, data);
}
int mailimap_ssl_connect_voip_with_callback(mailimap * f, const char * server, uint16_t port, int voip_enabled,
void (* callback)(struct mailstream_ssl_context * ssl_context, void * data), void * data)
{
int s;
mailstream * stream;
#if HAVE_CFNETWORK
if (mailstream_cfstream_enabled) {
if (callback == NULL) {
return mailimap_cfssl_connect_voip(f, server, port, voip_enabled);
}
}
#endif
if (port == 0) {
port = mail_get_service_port(SERVICE_NAME_IMAPS, SERVICE_TYPE_TCP);
if (port == 0)
port = DEFAULT_IMAPS_PORT;
}
s = mail_tcp_connect_timeout(server, port, f->imap_timeout);
if (s == -1)
return MAILIMAP_ERROR_CONNECTION_REFUSED;
stream = mailstream_ssl_open_with_callback_timeout(s, f->imap_timeout, callback, data);
if (stream == NULL) {
#ifdef WIN32
closesocket(s);
#else
close(s);
#endif
return MAILIMAP_ERROR_SSL;
}
return mailimap_connect(f, stream);
}
int mailimap_ssl_connect(mailimap * f, const char * server, uint16_t port)
{
return mailimap_ssl_connect_voip(f, server, port, mailstream_cfstream_voip_enabled);
}
int mailimap_ssl_connect_voip(mailimap * f, const char * server, uint16_t port, int voip_enabled)
{
return mailimap_ssl_connect_voip_with_callback(f, server, port, voip_enabled,
NULL, NULL);
}
#if HAVE_CFNETWORK
static int mailimap_cfssl_connect_voip_ssl_level(mailimap * f, const char * server, uint16_t port, int voip_enabled, int ssl_level)
{
mailstream * stream;
int r;
stream = mailstream_cfstream_open_voip_timeout(server, port, voip_enabled, f->imap_timeout);
if (stream == NULL) {
return MAILIMAP_ERROR_CONNECTION_REFUSED;
}
mailstream_cfstream_set_ssl_level(stream, ssl_level);
mailstream_cfstream_set_ssl_verification_mask(stream, MAILSTREAM_CFSTREAM_SSL_NO_VERIFICATION);
r = mailstream_cfstream_set_ssl_enabled(stream, 1);
if (r < 0) {
mailstream_close(stream);
return MAILIMAP_ERROR_SSL;
}
return mailimap_connect(f, stream);
}
static int mailimap_cfssl_connect_voip(mailimap * f, const char * server, uint16_t port, int voip_enabled)
{
return mailimap_cfssl_connect_voip_ssl_level(f, server, port, voip_enabled, MAILSTREAM_CFSTREAM_SSL_LEVEL_NEGOCIATED_SSL);
}
#endif