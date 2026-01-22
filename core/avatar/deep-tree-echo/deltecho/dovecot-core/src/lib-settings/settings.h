#ifndef SETTINGS_H
#define SETTINGS_H
struct var_expand_table;
struct var_expand_func_table;
struct setting_parser_info;
struct settings_root;
struct settings_mmap;
struct settings_instance;
enum settings_override_type {
SETTINGS_OVERRIDE_TYPE_USERDB,
SETTINGS_OVERRIDE_TYPE_CLI_PARAM,
SETTINGS_OVERRIDE_TYPE_CODE,
SETTINGS_OVERRIDE_TYPE_COUNT,
};
enum settings_read_flags {
SETTINGS_READ_NO_PROTOCOL_FILTER = BIT(0),
};
enum settings_get_flags {
SETTINGS_GET_FLAG_NO_CHECK = BIT(0),
SETTINGS_GET_FLAG_NO_EXPAND = BIT(1),
SETTINGS_GET_FLAG_FAKE_EXPAND = BIT(2),
SETTINGS_GET_NO_KEY_VALIDATION = BIT(3),
};
#define SETTINGS_EVENT_INSTANCE "settings_instance"
#define SETTINGS_EVENT_ROOT "settings_root"
#define SETTINGS_EVENT_FILTER_NAME "settings_filter_name"
#define SETTINGS_EVENT_MAILBOX_NAME_WITH_PREFIX "mailbox"
#define SETTINGS_EVENT_MAILBOX_NAME_WITHOUT_PREFIX "mailbox_subname"
#define SETTINGS_EVENT_VAR_EXPAND_TABLE \
"settings_var_expand_table"
#define SETTINGS_EVENT_VAR_EXPAND_FUNC_TABLE \
"settings_var_expand_func_table"
#define SETTINGS_EVENT_VAR_EXPAND_FUNC_CONTEXT \
"settings_var_expand_func_context"
#define SETTINGS_EVENT_VAR_EXPAND_CALLBACK \
"settings_var_expand_callback"
typedef void
settings_var_expand_t(struct event *event,
const struct var_expand_table **tab_r,
const struct var_expand_func_table **func_tab_r);
int settings_get(struct event *event,
const struct setting_parser_info *info,
enum settings_get_flags flags,
const char *source_filename,
unsigned int source_linenum,
const void **set_r, const char **error_r);
#ifdef HAVE_TYPE_CHECKS
#  define settings_get(event, info, flags, set_r, error_r) \
settings_get(event, info, flags, \
__FILE__, __LINE__, (void *)set_r, 1 ? (error_r) : \
COMPILE_ERROR_IF_TRUE( \
!__builtin_types_compatible_p(typeof((*set_r)->pool), pool_t)))
#else
#  define settings_get(event, info, flags, set_r, error_r) \
settings_get(event, info, flags, \
__FILE__, __LINE__, (void *)set_r, error_r)
#endif
int settings_get_filter(struct event *event,
const char *filter_key, const char *filter_value,
const struct setting_parser_info *info,
enum settings_get_flags flags,
const char *source_filename,
unsigned int source_linenum,
const void **set_r, const char **error_r);
#ifdef HAVE_TYPE_CHECKS
#  define settings_get_filter(event, filter_key, filter_value, info, flags, \
set_r, error_r) \
settings_get_filter(event, filter_key, filter_value, info, flags, \
__FILE__, __LINE__, (void *)set_r, 1 ? (error_r) : \
COMPILE_ERROR_IF_TRUE( \
!__builtin_types_compatible_p(typeof((*set_r)->pool), pool_t)))
#else
#  define settings_get_filter(event, filter_key, filter_value, info, flags, \
set_r, error_r) \
settings_get_filter(event, filter_key, filter_value, info, flags, \
__FILE__, __LINE__, (void *)set_r, error_r)
#endif
int settings_try_get_filter(struct event *event,
const char *filter_key, const char *filter_value,
const struct setting_parser_info *info,
enum settings_get_flags flags,
const char *source_filename,
unsigned int source_linenum,
const void **set_r, const char **error_r);
#ifdef HAVE_TYPE_CHECKS
#  define settings_try_get_filter(event, filter_key, filter_value, info, \
flags, set_r, error_r) \
settings_try_get_filter(event, filter_key, filter_value, info, flags, \
__FILE__, __LINE__, (void *)set_r, 1 ? (error_r) : \
COMPILE_ERROR_IF_TRUE( \
!__builtin_types_compatible_p(typeof((*set_r)->pool), pool_t)))
#else
#  define settings_try_get_filter(event, filter_key, filter_value, info, flags, \
set_r, error_r) \
settings_try_get_filter(event, filter_key, filter_value, info, flags, \
__FILE__, __LINE__, (void *)set_r, error_r)
#endif
#ifdef HAVE_TYPE_CHECKS
#  define settings_get_filter(event, filter_key, filter_value, info, \
flags, set_r, error_r) \
settings_get_filter(event, filter_key, filter_value, info, flags, \
__FILE__, __LINE__, (void *)set_r, 1 ? (error_r) : \
COMPILE_ERROR_IF_TRUE( \
!__builtin_types_compatible_p(typeof((*set_r)->pool), pool_t)))
#else
#  define settings_get_filter(event, filter_key, filter_value, info, flags, \
set_r, error_r) \
settings_get_filter(event, filter_key, filter_value, info, flags, \
__FILE__, __LINE__, (void *)set_r, error_r)
#endif
const void *
settings_get_or_fatal(struct event *event,
const struct setting_parser_info *info,
const char *source_filename,
unsigned int source_linenum);
#define settings_get_or_fatal(event, info) \
settings_get_or_fatal(event, info, __FILE__, __LINE__)
#define settings_free(set) \
STMT_START { \
if ((set) != NULL) { \
pool_t pool_copy = set->pool; \
pool_unref(&pool_copy); \
(set) = NULL; \
} \
} STMT_END
void settings_override(struct settings_instance *instance,
const char *key, const char *value,
enum settings_override_type type);
void settings_root_override(struct settings_root *root,
const char *key, const char *value,
enum settings_override_type type);
struct settings_instance *
settings_instance_new(struct settings_root *root);
struct settings_instance *
settings_instance_dup(const struct settings_instance *src);
void settings_instance_free(struct settings_instance **instance);
int settings_read(struct settings_root *root, int fd, const char *path,
const char *service_name,
const char *protocol_name,
enum settings_read_flags flags,
const char *const **specific_services_r,
const char **error_r);
bool settings_has_mmap(struct settings_root *root);
struct settings_root *settings_root_init(void);
void settings_root_deinit(struct settings_root **root);
#endif