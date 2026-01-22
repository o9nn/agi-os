#ifndef MAIL_STORAGE_SERVICE_H
#define MAIL_STORAGE_SERVICE_H
#include "net.h"
struct master_service;
struct ssl_iostream_settings;
struct mail_user;
struct setting_parser_context;
struct setting_parser_info;
struct mail_storage_service_user;
enum mail_storage_service_flags {
MAIL_STORAGE_SERVICE_FLAG_ALLOW_ROOT = 0x01,
MAIL_STORAGE_SERVICE_FLAG_USERDB_LOOKUP = 0x02,
MAIL_STORAGE_SERVICE_FLAG_DEBUG = 0x04,
MAIL_STORAGE_SERVICE_FLAG_NO_RESTRICT_ACCESS = 0x08,
MAIL_STORAGE_SERVICE_FLAG_NO_CHDIR = 0x10,
MAIL_STORAGE_SERVICE_FLAG_TEMP_PRIV_DROP = 0x20,
MAIL_STORAGE_SERVICE_FLAG_ENABLE_CORE_DUMPS = 0x40,
MAIL_STORAGE_SERVICE_FLAG_NO_LOG_INIT = 0x80,
MAIL_STORAGE_SERVICE_FLAG_NO_PLUGINS = 0x100,
MAIL_STORAGE_SERVICE_FLAG_NO_IDLE_TIMEOUT = 0x200,
MAIL_STORAGE_SERVICE_FLAG_NO_NAMESPACES = 0x800,
};
struct mail_storage_service_input {
struct event *event_parent;
const char *service;
const char *username;
const char *session_id;
const char *session_id_prefix;
time_t session_create_time;
struct ip_addr local_ip, remote_ip;
in_port_t local_port, remote_port;
const char *local_name;
const char *const *userdb_fields;
const char *const *forward_fields;
struct settings_instance *set_instance;
enum mail_storage_service_flags flags_override_add;
enum mail_storage_service_flags flags_override_remove;
bool no_userdb_lookup:1;
bool debug:1;
bool end_client_tls_secured:1;
bool autocreated:1;
bool no_free_init_failure:1;
};
extern struct module *mail_storage_service_modules;
struct mail_storage_service_ctx *
mail_storage_service_init(struct master_service *service,
enum mail_storage_service_flags flags);
struct auth_master_connection *
mail_storage_service_get_auth_conn(struct mail_storage_service_ctx *ctx);
void mail_storage_service_set_auth_conn(struct mail_storage_service_ctx *ctx,
struct auth_master_connection *conn);
void mail_storage_service_init_settings(struct mail_storage_service_ctx *ctx,
const struct mail_storage_service_input *input)
ATTR_NULL(2);
int mail_storage_service_lookup(struct mail_storage_service_ctx *ctx,
const struct mail_storage_service_input *input,
struct mail_storage_service_user **user_r,
const char **error_r);
int mail_storage_service_next(struct mail_storage_service_ctx *ctx,
struct mail_storage_service_user *user,
struct mail_user **mail_user_r,
const char **error_r);
int mail_storage_service_next_with_session_suffix(struct mail_storage_service_ctx *ctx,
struct mail_storage_service_user *user,
const char *session_id_postfix,
struct mail_user **mail_user_r,
const char **error_r);
void mail_storage_service_restrict_setenv(struct mail_storage_service_user *user);
int mail_storage_service_lookup_next(struct mail_storage_service_ctx *ctx,
const struct mail_storage_service_input *input,
struct mail_user **mail_user_r,
const char **error_r);
void mail_storage_service_user_ref(struct mail_storage_service_user *user);
void mail_storage_service_user_unref(struct mail_storage_service_user **user);
const char *const *
mail_storage_service_user_get_userdb_fields(struct mail_storage_service_user *user);
void mail_storage_service_all_init(struct mail_storage_service_ctx *ctx);
void mail_storage_service_all_init_mask(struct mail_storage_service_ctx *ctx,
const char *user_mask_hint);
int mail_storage_service_all_next(struct mail_storage_service_ctx *ctx,
const char **username_r);
void mail_storage_service_deinit(struct mail_storage_service_ctx **ctx);
void mail_storage_service_io_activate_user(struct mail_storage_service_user *user);
void mail_storage_service_io_deactivate_user(struct mail_storage_service_user *user);
const struct mail_user_settings *
mail_storage_service_user_get_set(struct mail_storage_service_user *user);
const struct mail_storage_service_input *
mail_storage_service_user_get_input(struct mail_storage_service_user *user);
struct settings_instance *
mail_storage_service_user_get_settings_instance(struct mail_storage_service_user *user);
int mail_storage_service_user_init_ssl_client_settings(
struct mail_storage_service_user *user, pool_t pool,
struct ssl_iostream_settings *ssl_set_r, const char **error_r);
struct mail_storage_service_ctx *
mail_storage_service_user_get_service_ctx(struct mail_storage_service_user *user);
pool_t mail_storage_service_user_get_pool(struct mail_storage_service_user *user);
const char *
mail_storage_service_user_get_log_prefix(struct mail_storage_service_user *user);
struct event *
mail_storage_service_user_get_event(const struct mail_storage_service_user *user);
const char *
mail_storage_service_user_get_username(const struct mail_storage_service_user *user);
const char *
mail_storage_service_get_log_prefix(struct mail_storage_service_ctx *ctx);
const struct var_expand_table *
mail_storage_service_get_var_expand_table(struct mail_storage_service_ctx *ctx,
struct mail_storage_service_input *input);
const char *mail_storage_service_fields_var_expand(const char *data,
const char *const *fields);
#endif