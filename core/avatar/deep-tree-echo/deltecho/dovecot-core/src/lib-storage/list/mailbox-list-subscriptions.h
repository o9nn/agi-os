#ifndef MAILBOX_LIST_SUBSCRIPTIONS_H
#define MAILBOX_LIST_SUBSCRIPTIONS_H
#include "mailbox-list-iter.h"
struct mailbox_tree_context;
struct mailbox_list_iterate_context;
int mailbox_list_subscriptions_refresh(struct mailbox_list *src_list,
struct mailbox_list *dest_list);
void mailbox_list_set_subscription_flags(struct mailbox_list *list,
const char *vname,
enum mailbox_info_flags *flags);
void mailbox_list_subscriptions_fill(struct mailbox_list_iterate_context *ctx,
struct mailbox_tree_context *tree,
bool default_nonexistent);
struct mailbox_list_iterate_context *
mailbox_list_subscriptions_iter_init(struct mailbox_list *list,
const char *const *patterns,
enum mailbox_list_iter_flags flags);
const struct mailbox_info *
mailbox_list_subscriptions_iter_next(struct mailbox_list_iterate_context *ctx);
int mailbox_list_subscriptions_iter_deinit(struct mailbox_list_iterate_context *ctx);
#endif