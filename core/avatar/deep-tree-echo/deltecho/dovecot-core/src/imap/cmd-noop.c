#include "imap-common.h"
#include "imap-notify.h"
#include "imap-commands.h"
bool cmd_noop(struct client_command_context *cmd)
{
if (cmd->client->notify_ctx != NULL) {
imap_notify_flush(cmd->client->notify_ctx);
}
return cmd_sync(cmd, 0, IMAP_SYNC_FLAG_SAFE, "OK NOOP completed.");
}