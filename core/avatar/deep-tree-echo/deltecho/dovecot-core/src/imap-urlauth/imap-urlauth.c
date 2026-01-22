#include "imap-urlauth-common.h"
#include "lib-signals.h"
#include "ioloop.h"
#include "buffer.h"
#include "array.h"
#include "istream.h"
#include "ostream.h"
#include "path-util.h"
#include "base64.h"
#include "str.h"
#include "process-title.h"
#include "settings.h"
#include "auth-master.h"
#include "master-service.h"
#include "master-service-settings.h"
#include "login-server.h"
#include "master-interface.h"
#include "var-expand.h"
#include <stdio.h>
#include <unistd.h>
#define IS_STANDALONE() \
(getenv(MASTER_IS_PARENT_ENV) == NULL)
bool verbose_proctitle = FALSE;
static struct login_server *login_server = NULL;
static const struct imap_urlauth_settings *imap_urlauth_settings;
void imap_urlauth_refresh_proctitle(void)
{
struct client *client;
string_t *title = t_str_new(128);
if (!verbose_proctitle)
return;
str_append_c(title, '[');
switch (imap_urlauth_clist->connections_count) {
case 0:
str_append(title, "idling");
break;
case 1:
client = container_of(imap_urlauth_clist->connections,
struct client, conn);
str_append(title, client->username);
break;
default:
str_printfa(title, "%u connections",
imap_urlauth_clist->connections_count);
break;
}
str_append_c(title, ']');
process_title_set(str_c(title));
}
static void imap_urlauth_die(void)
{
}
static int
client_create_from_input(const char *service, const char *username,
int fd_in, int fd_out)
{
struct client *client;
if (client_create(service, username, fd_in, fd_out,
imap_urlauth_settings, &client) < 0)
return -1;
if (!IS_STANDALONE())
client_send_line(client, "OK");
return 0;
}
static void main_stdio_run(const char *username)
{
username = username != NULL ? username : getenv("USER");
if (username == NULL && IS_STANDALONE())
username = getlogin();
if (username == NULL)
i_fatal("USER environment missing");
(void)client_create_from_input("", username, STDIN_FILENO, STDOUT_FILENO);
}
static void
login_request_finished(const struct login_server_request *request,
const char *username, const char *const *extra_fields)
{
const char *msg = "NO\n";
struct auth_user_reply reply;
struct net_unix_cred cred;
const char *const *fields;
const char *service = NULL;
unsigned int count, i;
const char *error;
if (auth_user_fields_parse(extra_fields, pool_datastack_create(),
&reply, &error) < 0) {
e_error(request->conn->event,
"Invalid settings in userdb: %s", error);
if (write(request->fd, msg, strlen(msg)) < 0) {
}
net_disconnect(request->fd);
return;
}
if (reply.uid != (uid_t)-1 && net_getunixcred(request->fd, &cred) == 0 &&
reply.uid != cred.uid) {
e_error(request->conn->event,
"Peer's credentials (uid=%ld) do not match "
"the user that logged in (uid=%ld).",
(long)cred.uid, (long)reply.uid);
if (write(request->fd, msg, strlen(msg)) < 0) {
}
net_disconnect(request->fd);
return;
}
fields = array_get(&reply.extra_fields, &count);
for (i = 0; i < count; i++) {
if (str_begins(fields[i], "client_service=", &service))
break;
}
if (service == NULL) {
e_error(request->conn->event,
"Auth did not yield required client_service field (BUG).");
if (write(request->fd, msg, strlen(msg)) < 0) {
}
net_disconnect(request->fd);
return;
}
if (reply.anonymous)
username = NULL;
if (client_create_from_input(service, username, request->fd,
request->fd) < 0)
net_disconnect(request->fd);
}
static void login_request_failed(const struct login_server_request *request,
const char *errormsg ATTR_UNUSED)
{
const char *msg = "NO\n";
if (write(request->fd, msg, strlen(msg)) < 0) {
}
}
static void client_connected(struct master_service_connection *conn)
{
i_assert(login_server != NULL);
master_service_client_connection_accept(conn);
login_server_add(login_server, conn->fd);
}
int main(int argc, char *argv[])
{
struct login_server_settings login_set;
enum master_service_flags service_flags = 0;
const char *error = NULL, *username = NULL;
const char *auth_socket_path = "auth-master";
int c;
i_zero(&login_set);
login_set.postlogin_timeout_secs =
LOGIN_SERVER_POSTLOGIN_TIMEOUT_DEFAULT;
if (IS_STANDALONE() && getuid() == 0 &&
net_getpeername(1, NULL, NULL) == 0) {
printf("NO imap_urlauth binary must not be started from "
"inetd, use imap-urlauth-login instead.\n");
return 1;
}
if (IS_STANDALONE()) {
service_flags |= MASTER_SERVICE_FLAG_STANDALONE |
MASTER_SERVICE_FLAG_STD_CLIENT;
}
master_service = master_service_init("imap-urlauth", service_flags,
&argc, &argv, "a:");
while ((c = master_getopt(master_service)) > 0) {
switch (c) {
case 'a':
auth_socket_path = optarg;
break;
default:
return FATAL_DEFAULT;
}
}
master_service_init_log(master_service);
if (master_service_settings_read_simple(master_service, &error) < 0)
i_fatal("%s", error);
imap_urlauth_settings =
settings_get_or_fatal(master_service_get_event(master_service),
&imap_urlauth_setting_parser_info);
if (imap_urlauth_settings->verbose_proctitle)
verbose_proctitle = TRUE;
if (t_abspath(auth_socket_path, &login_set.auth_socket_path, &error) < 0) {
i_fatal("t_abspath(%s) failed: %s", auth_socket_path, error);
}
login_set.callback = login_request_finished;
login_set.failure_callback = login_request_failed;
login_set.update_proctitle = verbose_proctitle &&
master_service_get_client_limit(master_service) == 1;
clients_init();
master_service_set_die_callback(master_service, imap_urlauth_die);
if (!IS_STANDALONE())
login_server = login_server_init(master_service, &login_set);
master_service_init_finish(master_service);
io_loop_set_running(current_ioloop);
if (IS_STANDALONE()) {
T_BEGIN {
main_stdio_run(username);
} T_END;
} else {
io_loop_set_running(current_ioloop);
}
if (io_loop_is_running(current_ioloop))
master_service_run(master_service, client_connected);
clients_deinit();
if (login_server != NULL)
login_server_deinit(&login_server);
settings_free(imap_urlauth_settings);
master_service_deinit(&master_service);
return 0;
}