#ifndef IMAPC_LIST_H
#define IMAPC_LIST_H
struct imap_arg;
#include "mailbox-list-private.h"
#define MAILBOX_LIST_NAME_IMAPC "imapc"
struct imapc_mailbox_list {
struct mailbox_list list;
const struct imapc_settings *set;
struct imapc_storage_client *client;
struct mailbox_list *index_list;
struct mailbox_tree_context *mailboxes, *tmp_subscriptions;
char root_sep;
time_t last_refreshed_mailboxes;
unsigned int iter_count;
bool refreshed_subscriptions:1;
bool refreshed_mailboxes:1;
bool refreshed_mailboxes_recently:1;
bool index_list_failed:1;
bool root_sep_pending:1;
};
int imapc_list_get_mailbox_flags(struct mailbox_list *list, const char *name,
enum mailbox_info_flags *flags_r);
int imapc_list_try_get_root_sep(struct imapc_mailbox_list *list, char *sep_r);
const char *imapc_list_storage_to_remote_name(struct imapc_mailbox_list *list,
const char *storage_name);
#endif