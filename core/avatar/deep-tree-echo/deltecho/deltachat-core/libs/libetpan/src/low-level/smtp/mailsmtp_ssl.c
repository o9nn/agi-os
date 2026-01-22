#ifdef HAVE_CONFIG_H
#	include <config.h>
#endif
#include "mailsmtp_socket.h"
#include "mailsmtp.h"
#include "mailstream_cfstream.h"
#include "connect.h"
#include <stdlib.h>
#ifdef HAVE_NETINET_IN_H
#	include <netinet/in.h>
#endif
#ifdef HAVE_UNISTD_H
#	include <unistd.h>
#endif
#define DEFAULT_SMTPS_PORT 465
#define SERVICE_NAME_SMTPS "smtps"
#define SERVICE_TYPE_TCP "tcp"
#if HAVE_CFNETWORK
static int mailsmtp_cfssl_connect(mailsmtp * session,
const char * server, uint16_t port);
#endif
int mailsmtp_ssl_connect(mailsmtp * session,
const char * server, uint16_t port)
{
return mailsmtp_ssl_connect_with_callback(session, server, port,
NULL, NULL);
}
int mailsmtp_ssl_connect_with_callback(mailsmtp * session,
const char * server, uint16_t port,
void (* callback)(struct mailstream_ssl_context * ssl_context, void * data), void * data)
{
int s;
mailstream * stream;
#if HAVE_CFNETWORK
if (mailstream_cfstream_enabled) {
if (callback == NULL) {
return mailsmtp_cfssl_connect(session, server, port);
}
}
#endif
if (port == 0) {
port = mail_get_service_port(SERVICE_NAME_SMTPS, SERVICE_TYPE_TCP);
if (port == 0)
port = DEFAULT_SMTPS_PORT;
}
s = mail_tcp_connect_timeout(server, port, session->smtp_timeout);
if (s == -1)
return MAILSMTP_ERROR_CONNECTION_REFUSED;
stream = mailstream_ssl_open_with_callback_timeout(s, session->smtp_timeout, callback, data);
if (stream == NULL) {
#ifdef WIN32
closesocket(s);
#else
close(s);
#endif
return MAILSMTP_ERROR_SSL;
}
return mailsmtp_connect(session, stream);
}
#if HAVE_CFNETWORK
static int mailsmtp_cfssl_connect_ssl_level(mailsmtp * session,
const char * server, uint16_t port, int ssl_level)
{
mailstream * stream;
int r;
stream = mailstream_cfstream_open_timeout(server, port, session->smtp_timeout);
if (stream == NULL) {
return MAILSMTP_ERROR_CONNECTION_REFUSED;
}
mailstream_cfstream_set_ssl_level(stream, ssl_level);
mailstream_cfstream_set_ssl_verification_mask(stream, MAILSTREAM_CFSTREAM_SSL_NO_VERIFICATION);
r = mailstream_cfstream_set_ssl_enabled(stream, 1);
if (r < 0) {
mailstream_close(stream);
return MAILSMTP_ERROR_SSL;
}
return mailsmtp_connect(session, stream);
}
static int mailsmtp_cfssl_connect(mailsmtp * session,
const char * server, uint16_t port)
{
return mailsmtp_cfssl_connect_ssl_level(session, server, port, MAILSTREAM_CFSTREAM_SSL_LEVEL_NEGOCIATED_SSL);
}
#endif