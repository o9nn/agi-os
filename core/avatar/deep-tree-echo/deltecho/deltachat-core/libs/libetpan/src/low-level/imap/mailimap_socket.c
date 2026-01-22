#ifdef HAVE_CONFIG_H
#	include <config.h>
#endif
#include "mailimap_socket.h"
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
#define DEFAULT_IMAP_PORT 143
#define SERVICE_NAME_IMAP "imap2"
#define SERVICE_TYPE_TCP "tcp"
#if HAVE_CFNETWORK
static int mailimap_cfsocket_connect_voip(mailimap * f, const char * server, uint16_t port, int voip_enabled);
#endif
LIBETPAN_EXPORT
int mailimap_socket_connect_voip(mailimap * f, const char * server, uint16_t port, int voip_enabled)
{
int s;
mailstream * stream;
#if HAVE_CFNETWORK
if (mailstream_cfstream_enabled) {
return mailimap_cfsocket_connect_voip(f, server, port, voip_enabled);
}
#endif
if (port == 0) {
port = mail_get_service_port(SERVICE_NAME_IMAP, SERVICE_TYPE_TCP);
if (port == 0)
port = DEFAULT_IMAP_PORT;
}
s = mail_tcp_connect_timeout(server, port, f->imap_timeout);
if (s == -1)
return MAILIMAP_ERROR_CONNECTION_REFUSED;
stream = mailstream_socket_open_timeout(s, f->imap_timeout);
if (stream == NULL) {
#ifdef WIN32
closesocket(s);
#else
close(s);
#endif
return MAILIMAP_ERROR_MEMORY;
}
return mailimap_connect(f, stream);
}
LIBETPAN_EXPORT
int mailimap_socket_connect(mailimap * f, const char * server, uint16_t port)
{
return mailimap_socket_connect_voip(f, server, port, mailstream_cfstream_voip_enabled);
}
int mailimap_socket_starttls(mailimap * f)
{
return mailimap_socket_starttls_with_callback(f, NULL, NULL);
}
static int mailimap_cfsocket_starttls(mailimap * f);
int mailimap_socket_starttls_with_callback(mailimap * f,
void (* callback)(struct mailstream_ssl_context * ssl_context, void * data), void * data)
{
mailstream_low * low;
mailstream_low * new_low;
int r;
int fd;
low = mailstream_get_low(f->imap_stream);
if (low->driver == mailstream_cfstream_driver) {
return mailimap_cfsocket_starttls(f);
}
r = mailimap_starttls(f);
switch (r) {
case MAILIMAP_NO_ERROR:
break;
default:
return r;
}
fd = mailstream_low_get_fd(low);
if (fd == -1)
return MAILIMAP_ERROR_STREAM;
new_low = mailstream_low_tls_open_with_callback_timeout(fd, f->imap_timeout,
callback, data);
if (new_low == NULL)
return MAILIMAP_ERROR_STREAM;
mailstream_low_free(low);
mailstream_set_low(f->imap_stream, new_low);
return MAILIMAP_NO_ERROR;
}
#if HAVE_CFNETWORK
static int mailimap_cfsocket_connect_voip(mailimap * f, const char * server, uint16_t port, int voip_enabled)
{
mailstream * stream;
stream = mailstream_cfstream_open_voip_timeout(server, port, voip_enabled, f->imap_timeout);
if (stream == NULL) {
return MAILIMAP_ERROR_CONNECTION_REFUSED;
}
return mailimap_connect(f, stream);
}
#endif
static int mailimap_cfsocket_starttls(mailimap * f)
{
int r;
r = mailimap_starttls(f);
switch (r) {
case MAILIMAP_NO_ERROR:
break;
default:
return r;
}
mailstream_cfstream_set_ssl_verification_mask(f->imap_stream, MAILSTREAM_CFSTREAM_SSL_NO_VERIFICATION);
r = mailstream_cfstream_set_ssl_enabled(f->imap_stream, 1);
if (r < 0) {
return MAILIMAP_ERROR_SSL;
}
return MAILIMAP_NO_ERROR;
}