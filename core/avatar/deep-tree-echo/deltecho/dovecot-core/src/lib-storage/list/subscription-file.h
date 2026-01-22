#ifndef SUBSCRIPTION_FILE_H
#define SUBSCRIPTION_FILE_H
struct stat;
struct mailbox_list;
struct subsfile_list_context *
subsfile_list_init(struct mailbox_list *list, const char *path);
int subsfile_list_deinit(struct subsfile_list_context **ctx);
int subsfile_list_fstat(struct subsfile_list_context *ctx, struct stat *st_r);
const char *subsfile_list_next(struct subsfile_list_context *ctx);
int subsfile_set_subscribed(struct mailbox_list *list, const char *path,
const char *temp_prefix, const char *name,
bool set);
#endif