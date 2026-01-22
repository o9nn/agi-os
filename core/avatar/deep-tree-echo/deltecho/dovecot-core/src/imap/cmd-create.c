#include "imap-common.h"
#include "imap-resp-code.h"
#include "mail-namespace.h"
#include "imap-commands.h"
bool cmd_create(struct client_command_context *cmd)
{
struct mail_namespace *ns;
const char *mailbox, *orig_mailbox;
struct mailbox *box;
bool directory;
size_t len;
if (!client_read_string_args(cmd, 1, &mailbox))
return FALSE;
orig_mailbox = mailbox;
ns = client_find_namespace(cmd, &mailbox);
if (ns == NULL)
return TRUE;
len = strlen(orig_mailbox);
if (len == 0 || orig_mailbox[len-1] != mail_namespace_get_sep(ns))
directory = FALSE;
else {
directory = TRUE;
if (len == strlen(mailbox))
mailbox = t_strndup(mailbox, len-1);
}
box = mailbox_alloc(ns->list, mailbox, 0);
event_add_str(cmd->global_event, "mailbox", mailbox_get_vname(box));
if (mailbox_create(box, NULL, directory) < 0)
client_send_box_error(cmd, box);
else
client_send_tagline(cmd, "OK Create completed.");
mailbox_free(&box);
return TRUE;
}