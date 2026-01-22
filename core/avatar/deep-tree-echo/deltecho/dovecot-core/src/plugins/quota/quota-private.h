#ifndef QUOTA_PRIVATE_H
#define QUOTA_PRIVATE_H
#include "mail-storage-private.h"
#include "mail-namespace.h"
#include "quota.h"
extern unsigned int quota_module_id;
struct quota {
struct mail_user *user;
struct quota_settings *set;
struct event *event;
ARRAY(struct quota_root *) roots;
ARRAY(struct mail_namespace *) namespaces;
struct mail_namespace *unwanted_ns;
};
struct quota_settings {
pool_t pool;
ARRAY(struct quota_root_settings *) root_sets;
struct event *event;
enum quota_alloc_result (*test_alloc)(
struct quota_transaction_context *ctx, uoff_t size,
const char **error_r);
uoff_t max_mail_size;
const char *quota_exceeded_msg;
bool debug:1;
bool initialized:1;
bool vsizes:1;
};
struct quota_rule {
const char *mailbox_mask;
int64_t bytes_limit, count_limit;
int bytes_percent, count_percent;
bool ignore:1;
};
struct quota_warning_rule {
struct quota_rule rule;
const char *command;
bool reverse:1;
};
struct quota_backend_vfuncs {
struct quota_root *(*alloc)(void);
int (*init)(struct quota_root *root, const char *args,
const char **error_r);
void (*deinit)(struct quota_root *root);
bool (*parse_rule)(struct quota_root_settings *root_set,
struct quota_rule *rule,
const char *str, const char **error_r);
int (*init_limits)(struct quota_root *root, const char **error_r);
void (*namespace_added)(struct quota *quota,
struct mail_namespace *ns);
const char *const *(*get_resources)(struct quota_root *root);
enum quota_get_result (*get_resource)(struct quota_root *root,
const char *name,
uint64_t *value_r,
const char **error_r);
int (*update)(struct quota_root *root,
struct quota_transaction_context *ctx,
const char **error_r);
bool (*match_box)(struct quota_root *root, struct mailbox *box);
void (*flush)(struct quota_root *root);
};
struct quota_backend {
const char *name;
struct event *event;
bool use_vsize;
struct quota_backend_vfuncs v;
};
struct quota_root_settings {
const char *name;
const char *set_name;
struct quota_settings *set;
const char *args;
const struct quota_backend *backend;
struct quota_rule default_rule;
ARRAY(struct quota_rule) rules;
ARRAY(struct quota_warning_rule) warning_rules;
const char *limit_set;
uint64_t last_mail_max_extra_bytes;
struct quota_rule grace_rule;
bool force_default_rule:1;
bool have_reverse_warnings:1;
};
struct quota_root {
pool_t pool;
struct quota_root_settings *set;
struct quota *quota;
struct quota_backend backend;
struct dict *limit_set_dict;
struct mail_namespace *ns;
const char *ns_prefix;
int64_t bytes_limit, count_limit;
ARRAY(void) quota_module_contexts;
bool no_enforcing:1;
bool auto_updating:1;
bool disable_unlimited_tracking:1;
bool recounting:1;
bool hidden:1;
bool quota_over_flag_checked:1;
};
struct quota_transaction_context {
union mailbox_transaction_module_context module_ctx;
struct quota *quota;
struct mailbox *box;
int64_t bytes_used, count_used;
uint64_t bytes_ceil, bytes_ceil2, count_ceil;
uint64_t bytes_over, count_over;
struct mail *tmp_mail;
enum quota_recalculate recalculate;
bool limits_set:1;
bool failed:1;
bool sync_transaction:1;
bool auto_updating:1;
bool no_quota_updates:1;
};
void quota_add_user_namespace(struct quota *quota, struct mail_namespace *ns);
void quota_remove_user_namespace(struct mail_namespace *ns);
int quota_root_default_init(struct quota_root *root, const char *args,
const char **error_r);
struct quota *quota_get_mail_user_quota(struct mail_user *user);
bool quota_root_is_namespace_visible(struct quota_root *root,
struct mail_namespace *ns);
struct quota_rule *
quota_root_rule_find(struct quota_root_settings *root_set, const char *name);
void quota_root_recalculate_relative_rules(struct quota_root_settings *root_set,
int64_t bytes_limit,
int64_t count_limit);
int quota_count(struct quota_root *root, uint64_t *bytes_r, uint64_t *count_r,
enum quota_get_result *error_result_r, const char **error_r);
int quota_root_parse_grace(struct quota_root_settings *root_set,
const char *value, const char **error_r);
bool quota_warning_match(const struct quota_warning_rule *w,
uint64_t bytes_before, uint64_t bytes_current,
uint64_t count_before, uint64_t count_current,
const char **reason_r);
bool quota_transaction_is_over(struct quota_transaction_context *ctx, uoff_t size);
int quota_transaction_set_limits(struct quota_transaction_context *ctx,
enum quota_get_result *error_result_r,
const char **error_r);
void quota_backend_register(const struct quota_backend *backend);
void quota_backend_unregister(const struct quota_backend *backend);
#define QUOTA_UNKNOWN_RESOURCE_ERROR_STRING "Unknown quota resource"
#endif