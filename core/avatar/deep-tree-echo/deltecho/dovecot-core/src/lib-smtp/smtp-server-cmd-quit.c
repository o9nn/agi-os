#include "lib.h"
#include "smtp-server-private.h"
void smtp_server_cmd_quit(struct smtp_server_cmd_ctx *cmd,
const char *params)
{
struct smtp_server_connection *conn = cmd->conn;
struct smtp_server_command *command = cmd->cmd;
const struct smtp_server_callbacks *callbacks = conn->callbacks;
int ret;
if (*params != '\0') {
smtp_server_reply(cmd,
501, "5.5.4", "Invalid parameters");
return;
}
smtp_server_connection_input_halt(conn);
smtp_server_command_ref(command);
if (callbacks != NULL && callbacks->conn_cmd_quit != NULL) {
if ((ret = callbacks->conn_cmd_quit(conn->context, cmd)) <= 0) {
i_assert(ret == 0 ||
smtp_server_command_is_replied(command));
smtp_server_command_unref(&command);
return;
}
}
if (!smtp_server_command_is_replied(command)) {
smtp_server_reply_quit(cmd);
}
smtp_server_command_unref(&command);
}