#include "login-common.h"
#include "base64.h"
#include "buffer.h"
#include "hex-binary.h"
#include "ioloop.h"
#include "istream.h"
#include "ostream.h"
#include "safe-memset.h"
#include "str.h"
#include "str-sanitize.h"
#include "auth-client.h"
#include "master-service-ssl-settings.h"
#include "client.h"
#include "client-authenticate.h"
#include "submission-proxy.h"
#include "submission-login-settings.h"
static void cmd_helo_reply(struct submission_client *subm_client,
struct smtp_server_cmd_ctx *cmd,
struct smtp_server_cmd_helo *data)
{
struct client *client = &subm_client->common;
const struct submission_login_settings *set = subm_client->set;
enum smtp_capability backend_caps = subm_client->backend_capabilities;
bool exotic_backend =
HAS_ALL_BITS(set->parsed_workarounds,
SUBMISSION_LOGIN_WORKAROUND_EXOTIC_BACKEND);
struct smtp_server_reply *reply;
reply = smtp_server_reply_create_ehlo(cmd->cmd);
if (!data->helo.old_smtp) {
if ((backend_caps & SMTP_CAPABILITY_8BITMIME) != 0)
smtp_server_reply_ehlo_add(reply, "8BITMIME");
if (client->connection_secured ||
strcmp(client->ssl_set->ssl, "required") != 0) {
const struct auth_mech_desc *mechs;
unsigned int count, i;
string_t *param = t_str_new(128);
mechs = sasl_server_get_advertised_mechs(client,
&count);
for (i = 0; i < count; i++) {
if (i > 0)
str_append_c(param, ' ');
str_append(param, mechs[i].name);
}
smtp_server_reply_ehlo_add_param(reply,
"AUTH", "%s", str_c(param));
}
if ((backend_caps & SMTP_CAPABILITY_BINARYMIME) != 0 &&
(backend_caps & SMTP_CAPABILITY_CHUNKING) != 0)
smtp_server_reply_ehlo_add(reply, "BINARYMIME");
if (!exotic_backend ||
(backend_caps & SMTP_CAPABILITY_BURL) != 0) {
smtp_server_reply_ehlo_add_param(reply, "BURL", "imap");
}
if (!exotic_backend ||
(backend_caps & SMTP_CAPABILITY_CHUNKING) != 0) {
smtp_server_reply_ehlo_add(reply,
"CHUNKING");
}
if ((backend_caps & SMTP_CAPABILITY_DSN) != 0)
smtp_server_reply_ehlo_add(reply, "DSN");
if (!exotic_backend ||
(backend_caps & SMTP_CAPABILITY_ENHANCEDSTATUSCODES) != 0) {
smtp_server_reply_ehlo_add(
reply, "ENHANCEDSTATUSCODES");
}
if (subm_client->set->submission_max_mail_size > 0) {
smtp_server_reply_ehlo_add_param(reply,
"SIZE", "%"PRIuUOFF_T,
subm_client->set->submission_max_mail_size);
} else if (!exotic_backend ||
(backend_caps & SMTP_CAPABILITY_SIZE) != 0) {
smtp_server_reply_ehlo_add(reply, "SIZE");
}
if (client_is_tls_enabled(client) &&
!client->connection_tls_secured &&
!client->haproxy_terminated_tls)
smtp_server_reply_ehlo_add(reply, "STARTTLS");
if (!exotic_backend ||
(backend_caps & SMTP_CAPABILITY_PIPELINING) != 0)
smtp_server_reply_ehlo_add(reply, "PIPELINING");
if ((backend_caps & SMTP_CAPABILITY_VRFY) != 0)
smtp_server_reply_ehlo_add(reply, "VRFY");
smtp_server_reply_ehlo_add_xclient(reply);
}
smtp_server_reply_submit(reply);
}
int cmd_helo(void *conn_ctx, struct smtp_server_cmd_ctx *cmd,
struct smtp_server_cmd_helo *data)
{
struct submission_client *subm_client = conn_ctx;
T_BEGIN {
cmd_helo_reply(subm_client, cmd, data);
} T_END;
return 1;
}
void submission_client_auth_result(struct client *client,
enum client_auth_result result,
const struct client_auth_reply *reply ATTR_UNUSED,
const char *text)
{
struct submission_client *subm_client =
container_of(client, struct submission_client, common);
struct smtp_server_cmd_ctx *cmd = subm_client->pending_auth;
if (subm_client->conn == NULL)
return;
subm_client->pending_auth = NULL;
i_assert(cmd != NULL);
switch (result) {
case CLIENT_AUTH_RESULT_SUCCESS:
if (client->login_proxy != NULL)
subm_client->pending_auth = cmd;
break;
case CLIENT_AUTH_RESULT_REFERRAL_NOLOGIN: {
const struct smtp_proxy_redirect predir = {
.username = reply->proxy.username,
.host = reply->proxy.host,
.host_ip = reply->proxy.host_ip,
.port = reply->proxy.port,
};
smtp_server_reply_redirect(cmd, login_binary->default_port,
&predir);
break;
}
case CLIENT_AUTH_RESULT_TEMPFAIL:
smtp_server_reply(cmd, 454, "4.7.0", "%s", text);
break;
case CLIENT_AUTH_RESULT_ABORTED:
smtp_server_reply(cmd, 501, "5.5.2", "%s", text);
break;
case CLIENT_AUTH_RESULT_INVALID_BASE64:
smtp_server_reply(cmd, 501, "5.5.2", "%s", text);
break;
case CLIENT_AUTH_RESULT_SSL_REQUIRED:
smtp_server_reply(cmd, 530, "5.7.0", "%s", text);
break;
case CLIENT_AUTH_RESULT_MECH_SSL_REQUIRED:
smtp_server_reply(cmd, 523, "5.7.10", "%s", text);
break;
case CLIENT_AUTH_RESULT_MECH_INVALID:
smtp_server_reply(cmd, 504, "5.5.4", "%s", text);
break;
case CLIENT_AUTH_RESULT_LOGIN_DISABLED:
case CLIENT_AUTH_RESULT_ANONYMOUS_DENIED:
smtp_server_reply(cmd, 525, "5.7.13", "%s", text);
break;
case CLIENT_AUTH_RESULT_PASS_EXPIRED:
default:
smtp_server_reply(cmd, 535, "5.7.8", "%s", text);
break;
}
}
int cmd_auth_continue(void *conn_ctx,
struct smtp_server_cmd_ctx *cmd ATTR_UNUSED,
const char *response)
{
struct submission_client *subm_client = conn_ctx;
struct client *client = &subm_client->common;
client_auth_respond(client, response);
return 0;
}
void submission_client_auth_send_challenge(struct client *client,
const char *data)
{
struct submission_client *subm_client =
container_of(client, struct submission_client, common);
struct smtp_server_cmd_ctx *cmd = subm_client->pending_auth;
i_assert(cmd != NULL);
smtp_server_cmd_auth_send_challenge(cmd, data);
}
static void
cmd_auth_set_master_data_prefix(struct submission_client *subm_client,
const char *mail_params) ATTR_NULL(2)
{
struct client *client = &subm_client->common;
struct smtp_server_helo_data *helo;
struct smtp_proxy_data proxy;
buffer_t *buf = buffer_create_dynamic(default_pool, 2048);
helo = smtp_server_connection_get_helo_data(subm_client->conn);
if (helo->domain_valid) {
i_assert(helo->domain != NULL);
buffer_append(buf, helo->domain, strlen(helo->domain));
}
buffer_append_c(buf, '\0');
smtp_server_connection_get_proxy_data(subm_client->conn, &proxy);
if (proxy.helo != NULL)
buffer_append(buf, proxy.helo, strlen(proxy.helo));
buffer_append_c(buf, '\0');
if (mail_params != NULL) {
buffer_append(buf, "MAIL ", 5);
buffer_append(buf, mail_params, strlen(mail_params));
buffer_append(buf, "\r\n", 2);
}
i_free(client->master_data_prefix);
client->master_data_prefix_len = buf->used;
client->master_data_prefix = buffer_free_without_data(&buf);
}
int cmd_auth(void *conn_ctx, struct smtp_server_cmd_ctx *cmd,
struct smtp_server_cmd_auth *data)
{
struct submission_client *subm_client = conn_ctx;
struct client *client = &subm_client->common;
cmd_auth_set_master_data_prefix(subm_client, NULL);
i_assert(subm_client->pending_auth == NULL);
subm_client->pending_auth = cmd;
(void)client_auth_begin(client, data->sasl_mech, data->initial_response);
return 0;
}
void cmd_mail(struct smtp_server_cmd_ctx *cmd, const char *params)
{
struct smtp_server_connection *conn = cmd->conn;
struct submission_client *subm_client =
smtp_server_connection_get_context(conn);
struct client *client = &subm_client->common;
enum submission_login_client_workarounds workarounds =
subm_client->set->parsed_workarounds;
if (HAS_NO_BITS(workarounds,
SUBMISSION_LOGIN_WORKAROUND_IMPLICIT_AUTH_EXTERNAL) ||
sasl_server_find_available_mech(client, "EXTERNAL") == NULL) {
smtp_server_command_fail(cmd->cmd, 530, "5.7.0",
"Authentication required.");
return;
}
e_debug(cmd->event,
"Performing implicit EXTERNAL authentication");
smtp_server_command_input_lock(cmd);
cmd_auth_set_master_data_prefix(subm_client, params);
i_assert(subm_client->pending_auth == NULL);
subm_client->pending_auth = cmd;
(void)client_auth_begin_implicit(client, "EXTERNAL", "=");
}