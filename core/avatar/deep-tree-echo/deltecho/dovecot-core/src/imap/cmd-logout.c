#include "imap-common.h"
#include "ostream.h"
#include "imap-commands.h"
bool cmd_logout(struct client_command_context *cmd)
{
struct client *client = cmd->client;
client->logged_out = TRUE;
client_send_line(client, "* BYE Logging out");
if (client->mailbox != NULL) {
imap_client_close_mailbox(client);
}
client_send_tagline(cmd, "OK Logout completed.");
client_disconnect(client, "Logged out");
return TRUE;
}