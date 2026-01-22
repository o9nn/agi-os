#ifndef DICT_H
#define DICT_H
#define DICT_PATH_PRIVATE "priv/"
#define DICT_PATH_SHARED "shared/"
struct timespec;
struct dict;
struct dict_iterate_context;
enum dict_iterate_flags {
DICT_ITERATE_FLAG_RECURSE = 0x01,
DICT_ITERATE_FLAG_SORT_BY_KEY = 0x02,
DICT_ITERATE_FLAG_SORT_BY_VALUE = 0x04,
DICT_ITERATE_FLAG_NO_VALUE = 0x08,
DICT_ITERATE_FLAG_EXACT_KEY = 0x10,
DICT_ITERATE_FLAG_ASYNC = 0x20
};
enum dict_data_type {
DICT_DATA_TYPE_STRING = 0,
DICT_DATA_TYPE_UINT32,
DICT_DATA_TYPE_LAST
};
struct dict_legacy_settings {
const char *base_dir;
struct event *event_parent;
};
struct dict_settings {
pool_t pool;
const char *dict_driver;
};
extern const struct setting_parser_info dict_setting_parser_info;
struct dict_op_settings {
const char *username;
const char *home_dir;
unsigned int expire_secs;
bool no_slowness_warning;
bool hide_log_values;
};
struct dict_lookup_result {
int ret;
const char *value;
const char *const *values;
const char *error;
};
enum dict_commit_ret {
DICT_COMMIT_RET_OK = 1,
DICT_COMMIT_RET_NOTFOUND = 0,
DICT_COMMIT_RET_FAILED = -1,
DICT_COMMIT_RET_WRITE_UNCERTAIN = -2,
};
struct dict_commit_result {
enum dict_commit_ret ret;
const char *error;
};
typedef void dict_lookup_callback_t(const struct dict_lookup_result *result,
void *context);
typedef void dict_iterate_callback_t(void *context);
typedef void
dict_transaction_commit_callback_t(const struct dict_commit_result *result,
void *context);
void dict_driver_register(struct dict *driver);
void dict_driver_unregister(struct dict *driver);
void dict_drivers_register_builtin(void);
void dict_drivers_unregister_builtin(void);
void dict_drivers_register_all(void);
void dict_drivers_unregister_all(void);
int dict_init_legacy(const char *uri, const struct dict_legacy_settings *set,
struct dict **dict_r, const char **error_r);
int dict_init_auto(struct event *event, struct dict **dict_r,
const char **error_r);
void dict_deinit(struct dict **dict);
void dict_wait(struct dict *dict);
bool dict_have_async_operations(struct dict *dict);
bool dict_switch_ioloop(struct dict *dict) ATTR_NOWARN_UNUSED_RESULT;
int dict_expire_scan(struct dict *dict, const char **error_r);
int dict_lookup(struct dict *dict, const struct dict_op_settings *set, pool_t pool,
const char *key, const char **value_r, const char **error_r);
int dict_lookup_values(struct dict *dict, const struct dict_op_settings *set,
pool_t pool, const char *key,
const char *const **values_r, const char **error_r);
void dict_lookup_async(struct dict *dict, const struct dict_op_settings *set,
const char *key, dict_lookup_callback_t *callback,
void *context);
#define dict_lookup_async(dict, set, key, callback, context) \
dict_lookup_async(dict, set, key, (dict_lookup_callback_t *)(callback), \
1 ? (context) : \
CALLBACK_TYPECHECK(callback, \
void (*)(const struct dict_lookup_result *, typeof(context))))
struct dict_iterate_context *
dict_iterate_init(struct dict *dict, const struct dict_op_settings *set,
const char *path, enum dict_iterate_flags flags);
void dict_iterate_set_async_callback(struct dict_iterate_context *ctx,
dict_iterate_callback_t *callback,
void *context);
#define dict_iterate_set_async_callback(ctx, callback, context) \
dict_iterate_set_async_callback(ctx, (dict_iterate_callback_t *)(callback), \
1 ? (context) : \
CALLBACK_TYPECHECK(callback, void (*)(typeof(context))))
void dict_iterate_set_limit(struct dict_iterate_context *ctx,
uint64_t max_rows);
bool dict_iterate_has_more(struct dict_iterate_context *ctx);
bool dict_iterate(struct dict_iterate_context *ctx,
const char **key_r, const char **value_r);
bool dict_iterate_values(struct dict_iterate_context *ctx,
const char **key_r, const char *const **values_r);
int dict_iterate_deinit(struct dict_iterate_context **ctx, const char **error_r);
struct dict_transaction_context *
dict_transaction_begin(struct dict *dict, const struct dict_op_settings *set);
void dict_transaction_set_timestamp(struct dict_transaction_context *ctx,
const struct timespec *ts);
void dict_transaction_set_non_atomic(struct dict_transaction_context *ctx);
void dict_transaction_set_hide_log_values(struct dict_transaction_context *ctx,
bool hide_log_values);
int dict_transaction_commit(struct dict_transaction_context **ctx,
const char **error_r);
void dict_transaction_commit_async(struct dict_transaction_context **ctx,
dict_transaction_commit_callback_t *callback,
void *context) ATTR_NULL(2, 3);
#define dict_transaction_commit_async(ctx, callback, context) \
dict_transaction_commit_async(ctx, (dict_transaction_commit_callback_t *)(callback), \
1 ? (context) : \
CALLBACK_TYPECHECK(callback, \
void (*)(const struct dict_commit_result *, typeof(context))))
void dict_transaction_commit_async_nocallback(
struct dict_transaction_context **ctx);
void dict_transaction_rollback(struct dict_transaction_context **ctx);
void dict_set(struct dict_transaction_context *ctx,
const char *key, const char *value);
void dict_unset(struct dict_transaction_context *ctx,
const char *key);
void dict_atomic_inc(struct dict_transaction_context *ctx,
const char *key, long long diff);
const char *dict_escape_string(const char *str);
const char *dict_unescape_string(const char *str);
#endif