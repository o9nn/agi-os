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
#define DEFAULT_SMTP_PORT 25
#define SERVICE_NAME_SMTP "smtp"
#define SERVICE_TYPE_TCP "tcp"
#if HAVE_CFNETWORK
static int mailsmtp_cfsocket_connect(mailsmtp * session,
const char * server, uint16_t port);
#endif
int mailsmtp_socket_connect(mailsmtp * session,
const char * server, uint16_t port)
{
int s;
mailstream * stream;
#if HAVE_CFNETWORK
if (mailstream_cfstream_enabled) {
return mailsmtp_cfsocket_connect(session, server, port);
}
#endif
if (port == 0) {
port = mail_get_service_port(SERVICE_NAME_SMTP, SERVICE_TYPE_TCP);
if (port == 0)
port = DEFAULT_SMTP_PORT;
}
s = mail_tcp_connect_timeout(server, port, session->smtp_timeout);
if (s == -1)
return MAILSMTP_ERROR_CONNECTION_REFUSED;
stream = mailstream_socket_open(s);
if (stream == NULL) {
#ifdef WIN32
closesocket(s);
#else
close(s);
#endif
return MAILSMTP_ERROR_MEMORY;
}
return mailsmtp_connect(session, stream);
}
static int mailsmtp_cfsocket_starttls(mailsmtp * session);
int mailsmtp_socket_starttls(mailsmtp * session)
{
return mailsmtp_socket_starttls_with_callback(session, NULL, NULL);
}
int mailsmtp_socket_starttls_with_callback(mailsmtp * session,
void (* callback)(struct mailstream_ssl_context * ssl_context, void * data), void * data)
{
int r;
int fd;
mailstream_low * low;
mailstream_low * new_low;
low = mailstream_get_low(session->stream);
if (low->driver == mailstream_cfstream_driver) {
return mailsmtp_cfsocket_starttls(session);
}
r = mailesmtp_starttls(session);
if (r != MAILSMTP_NO_ERROR)
return r;
fd = mailstream_low_get_fd(low);
if (fd == -1)
return MAILSMTP_ERROR_STREAM;
new_low = mailstream_low_tls_open_with_callback_timeout(fd, session->smtp_timeout, callback, data);
if (new_low == NULL)
return MAILSMTP_ERROR_SSL;
mailstream_low_free(low);
mailstream_set_low(session->stream, new_low);
return MAILSMTP_NO_ERROR;
}
#if HAVE_CFNETWORK
static int mailsmtp_cfsocket_connect(mailsmtp * session,
const char * server, uint16_t port)
{
mailstream * stream;
stream = mailstream_cfstream_open_timeout(server, port, session->smtp_timeout);
if (stream == NULL) {
return MAILSMTP_ERROR_CONNECTION_REFUSED;
}
return mailsmtp_connect(session, stream);
}
#endif
static int mailsmtp_cfsocket_starttls(mailsmtp * session)
{
int r;
r = mailesmtp_starttls(session);
if (r != MAILSMTP_NO_ERROR)
return r;
mailstream_cfstream_set_ssl_verification_mask(session->stream, MAILSTREAM_CFSTREAM_SSL_NO_VERIFICATION);
r = mailstream_cfstream_set_ssl_enabled(session->stream, 1);
if (r < 0) {
return MAILSMTP_ERROR_SSL;
}
return MAILSMTP_NO_ERROR;
}