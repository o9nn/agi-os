#include "lib.h"
#include "smtp-syntax.h"
#include "smtp-server-private.h"
void smtp_server_cmd_vrfy(struct smtp_server_cmd_ctx *cmd,
const char *params)
{
struct smtp_server_connection *conn = cmd->conn;
struct smtp_server_command *command = cmd->cmd;
const struct smtp_server_callbacks *callbacks = conn->callbacks;
const char *param, *error;
int ret;
ret = smtp_string_parse(params, &param, &error);
if (ret < 0) {
smtp_server_reply(cmd, 501, "5.5.4",
"Invalid string parameter: %s", error);
return;
} else if (ret == 0) {
smtp_server_reply(cmd, 501, "5.5.4", "Invalid parameters");
return;
}
smtp_server_command_ref(command);
if (callbacks != NULL && callbacks->conn_cmd_vrfy != NULL) {
ret = callbacks->conn_cmd_vrfy(conn->context, cmd, param);
if (ret <= 0) {
i_assert(ret == 0 ||
smtp_server_command_is_replied(command));
smtp_server_command_unref(&command);
return;
}
}
if (!smtp_server_command_is_replied(command))
smtp_server_cmd_vrfy_reply_default(cmd);
smtp_server_command_unref(&command);
}
void smtp_server_cmd_vrfy_reply_default(struct smtp_server_cmd_ctx *cmd)
{
i_assert(cmd->cmd->reg->func == smtp_server_cmd_vrfy);
smtp_server_reply(cmd, 252, "2.3.3", "Try RCPT instead");
}