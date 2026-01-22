#ifndef QUOTA_H
#define QUOTA_H
struct mail;
struct mailbox;
struct mail_user;
#define QUOTA_NAME_STORAGE_KILOBYTES "STORAGE"
#define QUOTA_NAME_STORAGE_BYTES "STORAGE_BYTES"
#define QUOTA_NAME_MESSAGES "MESSAGE"
struct quota;
struct quota_settings;
struct quota_root_settings;
struct quota_root;
struct quota_root_iter;
struct quota_transaction_context;
struct quota_param_parser {
char *param_name;
void (* param_handler)(struct quota_root *_root, const char *param_value);
};
extern struct quota_param_parser quota_param_hidden;
extern struct quota_param_parser quota_param_ignoreunlimited;
extern struct quota_param_parser quota_param_noenforcing;
extern struct quota_param_parser quota_param_ns;
enum quota_recalculate {
QUOTA_RECALCULATE_DONT = 0,
QUOTA_RECALCULATE_MISSING_FREES,
QUOTA_RECALCULATE_FORCED
};
enum quota_alloc_result {
QUOTA_ALLOC_RESULT_OK,
QUOTA_ALLOC_RESULT_TEMPFAIL,
QUOTA_ALLOC_RESULT_OVER_MAXSIZE,
QUOTA_ALLOC_RESULT_OVER_QUOTA,
QUOTA_ALLOC_RESULT_OVER_QUOTA_LIMIT,
QUOTA_ALLOC_RESULT_BACKGROUND_CALC,
};
enum quota_get_result {
QUOTA_GET_RESULT_BACKGROUND_CALC,
QUOTA_GET_RESULT_UNKNOWN_RESOURCE,
QUOTA_GET_RESULT_INTERNAL_ERROR,
QUOTA_GET_RESULT_LIMITED,
QUOTA_GET_RESULT_UNLIMITED,
};
const char *quota_alloc_result_errstr(enum quota_alloc_result res,
struct quota_transaction_context *qt);
int quota_user_read_settings(struct mail_user *user,
struct quota_settings **set_r,
const char **error_r);
void quota_settings_deinit(struct quota_settings **quota_set);
int quota_root_add_rule(struct quota_root_settings *root_set,
const char *rule_def, const char **error_r);
int quota_root_add_warning_rule(struct quota_root_settings *root_set,
const char *rule_def, const char **error_r);
int quota_init(struct quota_settings *quota_set, struct mail_user *user,
struct quota **quota_r, const char **error_r);
void quota_deinit(struct quota **quota);
struct quota_root_iter *quota_root_iter_init_user(struct mail_user *user);
struct quota_root_iter *quota_root_iter_init(struct mailbox *box);
struct quota_root *quota_root_iter_next(struct quota_root_iter *iter);
void quota_root_iter_deinit(struct quota_root_iter **iter);
struct quota_root *quota_root_lookup(struct mail_user *user, const char *name);
const char *quota_root_get_name(struct quota_root *root);
const char *const *quota_root_get_resources(struct quota_root *root);
bool quota_root_is_hidden(struct quota_root *root);
enum quota_get_result
quota_get_resource(struct quota_root *root, const char *mailbox_name,
const char *name, uint64_t *value_r, uint64_t *limit_r,
const char **error_r);
int quota_set_resource(struct quota_root *root, const char *name,
uint64_t value, const char **client_error_r);
struct quota_transaction_context *quota_transaction_begin(struct mailbox *box);
int quota_transaction_commit(struct quota_transaction_context **ctx);
void quota_transaction_rollback(struct quota_transaction_context **ctx);
enum quota_alloc_result quota_try_alloc(struct quota_transaction_context *ctx,
struct mail *mail, const char **error_r);
enum quota_alloc_result quota_test_alloc(struct quota_transaction_context *ctx,
uoff_t size, const char **error_r);
void quota_alloc(struct quota_transaction_context *ctx, struct mail *mail);
void quota_free_bytes(struct quota_transaction_context *ctx,
uoff_t physical_size);
void quota_recalculate(struct quota_transaction_context *ctx,
enum quota_recalculate recalculate);
void quota_over_flag_check_startup(struct quota *quota);
int quota_parse_parameters(struct quota_root *root, const char **args, const char **error_r,
const struct quota_param_parser *valid_params, bool fail_on_unknown);
#endif