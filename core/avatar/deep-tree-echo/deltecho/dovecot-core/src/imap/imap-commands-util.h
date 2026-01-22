#ifndef IMAP_COMMANDS_UTIL_H
#define IMAP_COMMANDS_UTIL_H
struct msgset_generator_context {
string_t *str;
uint32_t first_uid, last_uid;
};
struct mail_full_flags;
struct mailbox_keywords;
struct mail_namespace *
client_find_namespace(struct client_command_context *cmd, const char **mailbox);
struct mail_namespace *
client_find_namespace_full(struct client *client,
const char **mailbox, const char **client_error_r);
bool client_verify_open_mailbox(struct client_command_context *cmd);
void imap_client_close_mailbox(struct client *client);
int client_open_save_dest_box(struct client_command_context *cmd,
const char *name, struct mailbox **destbox_r);
const char *imap_client_command_get_reason(struct client_command_context *cmd);
void imap_transaction_set_cmd_reason(struct mailbox_transaction_context *trans,
struct client_command_context *cmd);
const char *
imap_get_error_string(const char *error_string, enum mail_error error);
void client_disconnect_if_inconsistent(struct client *client);
void client_send_error(struct client_command_context *cmd,
const char *error_string, enum mail_error error);
void client_send_list_error(struct client_command_context *cmd,
struct mailbox_list *list);
void client_send_storage_error(struct client_command_context *cmd,
struct mail_storage *storage);
void client_send_box_error(struct client_command_context *cmd,
struct mailbox *box);
void client_send_untagged_storage_error(struct client *client,
struct mail_storage *storage);
bool client_parse_mail_flags(struct client_command_context *cmd,
const struct imap_arg *args,
enum mail_flags *flags_r,
const char *const **keywords_r);
void client_send_mailbox_flags(struct client *client, bool selecting);
void client_update_mailbox_flags(struct client *client,
const ARRAY_TYPE(keywords) *keywords)
ATTR_NULL(2);
const char *const *
client_get_keyword_names(struct client *client, ARRAY_TYPE(keywords) *dest,
const ARRAY_TYPE(keyword_indexes) *src);
void msgset_generator_init(struct msgset_generator_context *ctx, string_t *str);
void msgset_generator_next(struct msgset_generator_context *ctx, uint32_t uid);
void msgset_generator_finish(struct msgset_generator_context *ctx);
#endif