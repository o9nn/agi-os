#ifdef HAVE_CONFIG_H
#	include <config.h>
#endif
#include "mailstorage_tools.h"
#include "libetpan-config.h"
#include <sys/types.h>
#include <stdlib.h>
#ifdef WIN32
#	include "win_etpan.h"
#else
#	include <netdb.h>
#	include <netinet/in.h>
#	include <sys/socket.h>
#	include <unistd.h>
#	include <sys/wait.h>
#	include <sys/ioctl.h>
#endif
#include <fcntl.h>
#include <string.h>
#include <stdio.h>
#include "mail.h"
#include "mailmessage.h"
#include "maildriver.h"
#include "connect.h"
#ifndef WIN32
#define ENV_BUFFER_SIZE 512
static void do_exec_command(int fd, const char *command,
char *servername, uint16_t port)
{
long i, maxopen;
#ifndef HAVE_SETENV
char env_buffer[ENV_BUFFER_SIZE];
#endif
if (fork() > 0) {
exit(0);
}
#ifndef HAVE_SETENV
if (servername)
snprintf(env_buffer, ENV_BUFFER_SIZE, "ETPANSERVER=%s", servername);
else
snprintf(env_buffer, ENV_BUFFER_SIZE, "ETPANSERVER=");
putenv(env_buffer);
#else
if (servername)
setenv("ETPANSERVER", servername, 1);
else
unsetenv("ETPANSERVER");
#endif
#ifndef HAVE_SETENV
if (port)
snprintf(env_buffer, ENV_BUFFER_SIZE, "ETPANPORT=%d", port);
else
snprintf(env_buffer, ENV_BUFFER_SIZE, "ETPANPORT=");
putenv(env_buffer);
#else
if (port) {
char porttext[20];
snprintf(porttext, sizeof(porttext), "%d", port);
setenv("ETPANPORT", porttext, 1);
}
else {
unsetenv("ETPANPORT");
}
#endif
if (dup2(fd, 0) == -1)
exit(1);
if (dup2(fd, 1) == -1)
exit(1);
maxopen = sysconf(_SC_OPEN_MAX);
for (i=3; i < maxopen; i++)
close((int) i);
#ifdef TIOCNOTTY
fd = open("/dev/tty", O_RDONLY);
if (fd != -1) {
ioctl(fd, TIOCNOTTY, NULL);
close(fd);
}
#endif
execl("/bin/sh", "/bin/sh", "-c", command, NULL);
exit(1);
}
#endif
static int subcommand_connect(char *command, char *servername, uint16_t port)
{
#ifdef WIN32
return -1;
#else
int sockfds[2];
pid_t childpid;
if (socketpair(AF_UNIX, SOCK_STREAM, 0, sockfds))
return -1;
childpid = fork();
if (!childpid) {
do_exec_command(sockfds[1], command, servername, port);
}
else if (childpid == -1) {
close(sockfds[0]);
close(sockfds[1]);
return -1;
}
close(sockfds[1]);
waitpid(childpid, NULL, 0);
return sockfds[0];
#endif
}
int mailstorage_generic_connect(mailsession_driver * driver,
char * servername,
uint16_t port,
char * command,
int connection_type,
int cache_function_id,
char * cache_directory,
int flags_function_id,
char * flags_directory,
mailsession ** result)
{
return mailstorage_generic_connect_with_local_address(driver,
servername,
port,
NULL,
0,
command,
connection_type,
cache_function_id,
cache_directory,
flags_function_id,
flags_directory,
result);
}
int mailstorage_generic_connect_with_local_address(mailsession_driver * driver,
char * servername,
uint16_t port,
char * local_address,
uint16_t local_port,
char * command,
int connection_type,
int cache_function_id,
char * cache_directory,
int flags_function_id,
char * flags_directory,
mailsession ** result)
{
int r;
int res;
mailstream * stream = NULL;
int fd = -1;
mailsession * session;
int connect_result;
switch (connection_type) {
case CONNECTION_TYPE_PLAIN:
case CONNECTION_TYPE_TRY_STARTTLS:
case CONNECTION_TYPE_STARTTLS:
case CONNECTION_TYPE_TLS:
#if HAVE_CFNETWORK
if (mailstream_cfstream_enabled) {
stream = mailstream_cfstream_open_voip(servername, port, mailstream_cfstream_voip_enabled);
if (stream == NULL) {
res = MAIL_ERROR_CONNECT;
goto err;
}
}
#endif
if (stream == NULL) {
fd = mail_tcp_connect_with_local_address(servername, port,
local_address, local_port);
if (fd == -1) {
res = MAIL_ERROR_CONNECT;
goto err;
}
}
break;
case CONNECTION_TYPE_COMMAND:
case CONNECTION_TYPE_COMMAND_TRY_STARTTLS:
case CONNECTION_TYPE_COMMAND_STARTTLS:
case CONNECTION_TYPE_COMMAND_TLS:
fd = subcommand_connect(command, servername, port);
break;
default:
fd = -1;
break;
}
if (fd == -1 && stream == NULL) {
res = MAIL_ERROR_INVAL;
goto err;
}
switch (connection_type) {
case CONNECTION_TYPE_PLAIN:
case CONNECTION_TYPE_TRY_STARTTLS:
case CONNECTION_TYPE_STARTTLS:
case CONNECTION_TYPE_COMMAND:
case CONNECTION_TYPE_COMMAND_TRY_STARTTLS:
case CONNECTION_TYPE_COMMAND_STARTTLS:
if (stream == NULL) {
stream = mailstream_socket_open(fd);
}
break;
case CONNECTION_TYPE_TLS:
case CONNECTION_TYPE_COMMAND_TLS:
#if HAVE_CFNETWORK
if (mailstream_cfstream_enabled) {
int ssl_level = MAILSTREAM_CFSTREAM_SSL_LEVEL_NEGOCIATED_SSL;
mailstream_cfstream_set_ssl_level(stream, ssl_level);
mailstream_cfstream_set_ssl_verification_mask(stream, MAILSTREAM_CFSTREAM_SSL_NO_VERIFICATION);
r = mailstream_cfstream_set_ssl_enabled(stream, 1);
if (r < 0) {
mailstream_close(stream);
return MAIL_ERROR_SSL;
}
}
#endif
if (stream == NULL) {
stream = mailstream_ssl_open(fd);
}
break;
default:
stream = NULL;
break;
}
if (stream == NULL) {
res = MAIL_ERROR_STREAM;
#ifdef WIN32
closesocket(fd);
#else
close(fd);
#endif
goto err;
}
session = mailsession_new(driver);
if (session == NULL) {
res = MAIL_ERROR_MEMORY;
goto close_stream;
}
if (cache_directory != NULL) {
char cache_directory_server[PATH_MAX];
snprintf(cache_directory_server, PATH_MAX, "%s/%s",
cache_directory, servername);
r = mailsession_parameters(session,
cache_function_id,
cache_directory_server);
if (r != MAIL_NO_ERROR) {
res = r;
goto close_stream;
}
}
if (flags_directory != NULL) {
char flags_directory_server[PATH_MAX];
snprintf(flags_directory_server, PATH_MAX, "%s/%s",
flags_directory, servername);
r = mailsession_parameters(session,
flags_function_id,
flags_directory_server);
if (r != MAIL_NO_ERROR) {
res = r;
goto close_stream;
}
}
r = mailsession_connect_stream(session, stream);
switch (r) {
case MAIL_NO_ERROR_NON_AUTHENTICATED:
case MAIL_NO_ERROR_AUTHENTICATED:
case MAIL_NO_ERROR:
break;
default:
res = r;
goto free;
}
connect_result = r;
switch (connection_type) {
case CONNECTION_TYPE_TRY_STARTTLS:
case CONNECTION_TYPE_COMMAND_TRY_STARTTLS:
r = mailsession_starttls(session);
if ((r != MAIL_NO_ERROR) && (r != MAIL_ERROR_NO_TLS)) {
res = r;
goto free;
}
break;
case CONNECTION_TYPE_STARTTLS:
case CONNECTION_TYPE_COMMAND_STARTTLS:
r = mailsession_starttls(session);
if (r != MAIL_NO_ERROR) {
res = r;
goto free;
}
}
* result = session;
return connect_result;
close_stream:
mailstream_close(stream);
free:
mailsession_free(session);
err:
return res;
}
int mailstorage_generic_auth(mailsession * session,
int connect_result,
int auth_type,
char * login,
char * password)
{
return mailstorage_generic_auth_sasl(session,
connect_result,
NULL, NULL, NULL, NULL,
login, login,
password, NULL);
}
int mailstorage_generic_auth_sasl(mailsession * session,
int connect_result,
const char * auth_type,
const char * server_fqdn,
const char * local_ip_port,
const char * remote_ip_port,
const char * login, const char * auth_name,
const char * password, const char * realm)
{
int must_auth;
int r;
int res;
r = connect_result;
must_auth = FALSE;
switch (r) {
case MAIL_NO_ERROR_NON_AUTHENTICATED:
must_auth = TRUE;
break;
case MAIL_NO_ERROR_AUTHENTICATED:
case MAIL_NO_ERROR:
break;
default:
res = r;
goto err;
}
if (must_auth) {
if (auth_type != NULL) {
r = mailsession_login_sasl(session, auth_type,
server_fqdn,
local_ip_port,
remote_ip_port,
login, auth_name,
password, realm);
}
else {
if ((login == NULL) || (password == NULL)) {
r = MAIL_NO_ERROR;
}
else {
r = mailsession_login(session, login, password);
}
}
if (r != MAIL_NO_ERROR) {
mailsession_logout(session);
res = r;
goto err;
}
}
return MAIL_NO_ERROR;
err:
return res;
}