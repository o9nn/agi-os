#include "lib.h"
#include "ioloop.h"
#include "array.h"
#include "unichar.h"
#include "imap-match.h"
#include "subscription-file.h"
#include "mailbox-tree.h"
#include "mailbox-list-private.h"
#include "mailbox-list-subscriptions.h"
#include <sys/stat.h>
struct subscriptions_mailbox_list_iterate_context {
struct mailbox_list_iterate_context ctx;
struct mailbox_tree_context *tree;
struct mailbox_tree_iterate_context *iter;
struct mailbox_info info;
};
static int
mailbox_list_subscription_fill_one(struct mailbox_list *list,
struct mailbox_list *src_list,
const char *name)
{
struct mail_namespace *ns, *default_ns = list->ns;
struct mail_namespace *namespaces = default_ns->user->namespaces;
struct mailbox_node *node;
const char *vname, *ns_name, *error;
size_t len;
bool created;
if (src_list->ns->prefix_len == 0)
ns_name = name;
else {
ns_name = t_strconcat(src_list->ns->prefix, name, NULL);
}
ns = mail_namespace_find_unsubscribable(namespaces, ns_name);
if (ns != NULL && ns != default_ns) {
if (ns->prefix_len > 0)
return 0;
ns = NULL;
}
if (ns == NULL &&
(default_ns->flags & NAMESPACE_FLAG_SUBSCRIPTIONS) == 0)
return 0;
if (ns != NULL && mail_namespace_is_shared_user_root(ns)) {
(void)mailbox_list_join_refpattern(ns->list, ns_name, "");
}
if (ns == NULL)
ns = default_ns;
else if (strncmp(ns_name, ns->prefix, ns->prefix_len) == 0) {
ns_name += ns->prefix_len;
name = ns_name;
} else {
i_assert(strncmp(ns_name, ns->prefix, ns->prefix_len-1) == 0 &&
ns_name[ns->prefix_len-1] == '\0');
name = "";
}
len = strlen(name);
if (len > 0 && name[len-1] == mail_namespace_get_sep(ns)) {
name = t_strndup(name, len-1);
}
if (!mailbox_list_is_valid_name(list, name, &error)) {
return -1;
} else {
vname = mailbox_list_get_vname(list, name);
if (!uni_utf8_str_is_valid(vname))
return -1;
node = mailbox_tree_get(list->subscriptions, vname, &created);
node->flags = MAILBOX_SUBSCRIBED;
}
return 0;
}
int mailbox_list_subscriptions_refresh(struct mailbox_list *src_list,
struct mailbox_list *dest_list)
{
struct subsfile_list_context *subsfile_ctx;
struct stat st;
enum mailbox_list_path_type type;
const char *path, *name;
char sep;
int ret;
i_assert((src_list->ns->flags & NAMESPACE_FLAG_SUBSCRIPTIONS) != 0);
if (dest_list->subscriptions == NULL) {
sep = mail_namespace_get_sep(src_list->ns);
dest_list->subscriptions = mailbox_tree_init(sep);
}
type = src_list->set.control_dir != NULL ?
MAILBOX_LIST_PATH_TYPE_CONTROL : MAILBOX_LIST_PATH_TYPE_DIR;
if (!mailbox_list_get_root_path(src_list, type, &path) ||
src_list->set.subscription_fname == NULL) {
return 0;
}
path = t_strconcat(path, "/", src_list->set.subscription_fname, NULL);
if (stat(path, &st) < 0) {
if (errno == ENOENT) {
mailbox_tree_clear(dest_list->subscriptions);
dest_list->subscriptions_mtime = 0;
return 0;
}
mailbox_list_set_critical(dest_list, "stat(%s) failed: %m",
path);
return -1;
}
if (st.st_mtime == dest_list->subscriptions_mtime &&
st.st_mtime < dest_list->subscriptions_read_time-1) {
return 0;
}
mailbox_tree_clear(dest_list->subscriptions);
dest_list->subscriptions_read_time = ioloop_time;
subsfile_ctx = subsfile_list_init(dest_list, path);
if (subsfile_list_fstat(subsfile_ctx, &st) == 0)
dest_list->subscriptions_mtime = st.st_mtime;
while ((name = subsfile_list_next(subsfile_ctx)) != NULL) T_BEGIN {
T_BEGIN {
ret = mailbox_list_subscription_fill_one(dest_list,
src_list, name);
} T_END;
if (ret < 0) {
e_warning(dest_list->event,
"Subscriptions file %s: "
"Removing invalid entry: %s",
path, name);
(void)subsfile_set_subscribed(src_list, path,
mailbox_list_get_temp_prefix(src_list),
name, FALSE);
}
} T_END;
if (subsfile_list_deinit(&subsfile_ctx) < 0) {
dest_list->subscriptions_mtime = (time_t)-1;
return -1;
}
return 0;
}
void mailbox_list_set_subscription_flags(struct mailbox_list *list,
const char *vname,
enum mailbox_info_flags *flags)
{
struct mailbox_node *node;
*flags &= ENUM_NEGATE(MAILBOX_SUBSCRIBED | MAILBOX_CHILD_SUBSCRIBED);
node = mailbox_tree_lookup(list->subscriptions, vname);
if (node != NULL) {
*flags |= node->flags & MAILBOX_SUBSCRIBED;
if (node->children != NULL)
*flags |= MAILBOX_CHILD_SUBSCRIBED;
}
}
void mailbox_list_subscriptions_fill(struct mailbox_list_iterate_context *ctx,
struct mailbox_tree_context *tree,
bool default_nonexistent)
{
struct mailbox_list_iter_update_context update_ctx;
struct mailbox_tree_iterate_context *iter;
const char *name;
i_zero(&update_ctx);
update_ctx.iter_ctx = ctx;
update_ctx.tree_ctx = tree;
update_ctx.glob = ctx->glob;
update_ctx.leaf_flags = MAILBOX_SUBSCRIBED;
if (default_nonexistent)
update_ctx.leaf_flags |= MAILBOX_NONEXISTENT;
update_ctx.parent_flags = MAILBOX_CHILD_SUBSCRIBED;
update_ctx.match_parents =
(ctx->flags & MAILBOX_LIST_ITER_SELECT_RECURSIVEMATCH) != 0;
iter = mailbox_tree_iterate_init(ctx->list->subscriptions, NULL,
MAILBOX_SUBSCRIBED);
while (mailbox_tree_iterate_next(iter, &name) != NULL)
mailbox_list_iter_update(&update_ctx, name);
mailbox_tree_iterate_deinit(&iter);
}
struct mailbox_list_iterate_context *
mailbox_list_subscriptions_iter_init(struct mailbox_list *list,
const char *const *patterns,
enum mailbox_list_iter_flags flags)
{
struct subscriptions_mailbox_list_iterate_context *ctx;
pool_t pool;
char sep = mail_namespace_get_sep(list->ns);
pool = pool_alloconly_create("mailbox list subscriptions iter", 1024);
ctx = p_new(pool, struct subscriptions_mailbox_list_iterate_context, 1);
ctx->ctx.pool = pool;
ctx->ctx.list = list;
ctx->ctx.flags = flags;
ctx->ctx.glob = imap_match_init_multiple(pool, patterns, TRUE, sep);
array_create(&ctx->ctx.module_contexts, pool, sizeof(void *), 5);
ctx->tree = mailbox_tree_init(sep);
mailbox_list_subscriptions_fill(&ctx->ctx, ctx->tree, FALSE);
ctx->info.ns = list->ns;
ctx->iter = mailbox_tree_iterate_init(ctx->tree, NULL, MAILBOX_MATCHED);
return &ctx->ctx;
}
const struct mailbox_info *
mailbox_list_subscriptions_iter_next(struct mailbox_list_iterate_context *_ctx)
{
struct subscriptions_mailbox_list_iterate_context *ctx =
(struct subscriptions_mailbox_list_iterate_context *)_ctx;
struct mailbox_list *list = _ctx->list;
struct mailbox_node *node;
enum mailbox_info_flags subs_flags;
const char *vname, *storage_name, *error;
int ret;
node = mailbox_tree_iterate_next(ctx->iter, &vname);
if (node == NULL)
return mailbox_list_iter_default_next(_ctx);
ctx->info.vname = vname;
subs_flags = node->flags & (MAILBOX_SUBSCRIBED |
MAILBOX_CHILD_SUBSCRIBED);
if ((_ctx->flags & MAILBOX_LIST_ITER_RETURN_NO_FLAGS) != 0 &&
(_ctx->flags & MAILBOX_LIST_ITER_RETURN_CHILDREN) == 0) {
ctx->info.flags = subs_flags;
return &ctx->info;
}
storage_name = mailbox_list_get_storage_name(list, vname);
if (!mailbox_list_is_valid_name(list, storage_name, &error)) {
ctx->info.flags = MAILBOX_NONEXISTENT;
} else if (mailbox_list_mailbox(list, storage_name,
&ctx->info.flags) < 0) {
ctx->info.flags = 0;
_ctx->failed = TRUE;
} else if ((_ctx->flags & MAILBOX_LIST_ITER_RETURN_CHILDREN) != 0 &&
(ctx->info.flags & (MAILBOX_CHILDREN |
MAILBOX_NOCHILDREN)) == 0) {
ret = mailbox_has_children(list, storage_name);
if (ret < 0)
_ctx->failed = TRUE;
else if (ret == 0)
ctx->info.flags |= MAILBOX_NOCHILDREN;
else
ctx->info.flags |= MAILBOX_CHILDREN;
}
ctx->info.flags &= ENUM_NEGATE(MAILBOX_SUBSCRIBED | MAILBOX_CHILD_SUBSCRIBED);
ctx->info.flags |=
node->flags & (MAILBOX_SUBSCRIBED | MAILBOX_CHILD_SUBSCRIBED);
return &ctx->info;
}
int mailbox_list_subscriptions_iter_deinit(struct mailbox_list_iterate_context *_ctx)
{
struct subscriptions_mailbox_list_iterate_context *ctx =
(struct subscriptions_mailbox_list_iterate_context *)_ctx;
int ret = _ctx->failed ? -1 : 0;
mailbox_tree_iterate_deinit(&ctx->iter);
mailbox_tree_deinit(&ctx->tree);
pool_unref(&_ctx->pool);
return ret;
}