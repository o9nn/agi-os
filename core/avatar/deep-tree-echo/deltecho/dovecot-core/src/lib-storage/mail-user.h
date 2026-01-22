#ifndef MAIL_USER_H
#define MAIL_USER_H
#include "net.h"
#include "unichar.h"
#include "mail-storage-settings.h"
#include "process-stat.h"
struct module;
struct fs_settings;
struct ssl_iostream_settings;
struct master_service_anvil_session;
struct mail_user;
struct dict_op_settings;
struct mail_user_vfuncs {
void (*deinit)(struct mail_user *user);
void (*deinit_pre)(struct mail_user *user);
};
struct mail_user_connection_data {
struct ip_addr *local_ip, *remote_ip;
in_port_t local_port, remote_port;
const char *local_name;
bool end_client_tls_secured:1;
};
struct mail_user {
pool_t pool;
struct mail_user_vfuncs v, *vlast;
int refcount;
struct event *event;
struct mail_user *creator;
struct mail_storage_service_user *service_user;
const char *username;
const char *_home;
uid_t uid;
gid_t gid;
const char *service;
const char *session_id;
struct mail_user_connection_data conn;
const char *auth_mech, *auth_token, *auth_user;
const char *const *userdb_fields;
const char *const *_alt_usernames;
time_t session_create_time;
const struct var_expand_table *var_expand_table;
const char *error;
const struct mail_user_settings *set;
struct mail_storage_settings *_mail_set;
struct ssl_iostream_settings *ssl_set;
struct mail_namespace *namespaces;
struct mail_storage *storages;
struct dict_op_settings *dict_op_set;
ARRAY(const struct mail_storage_hooks *) hooks;
normalizer_func_t *default_normalizer;
struct dict *_attr_dict;
ARRAY(union mail_user_module_context *) module_contexts;
struct process_stat proc_stat;
bool nonexistent:1;
bool home_looked_up:1;
bool anonymous:1;
bool autocreated:1;
bool initialized:1;
bool namespaces_created:1;
bool inbox_open_error_logged:1;
bool fuzzy_search:1;
bool dsyncing:1;
bool attr_dict_failed:1;
bool deinitializing:1;
bool admin:1;
bool stats_enabled:1;
bool session_restored:1;
};
struct mail_user_module_register {
unsigned int id;
};
union mail_user_module_context {
struct mail_user_vfuncs super;
struct mail_user_module_register *reg;
};
extern struct mail_user_module_register mail_user_module_register;
extern struct auth_master_connection *mail_user_auth_master_conn;
extern const struct var_expand_func_table *mail_user_var_expand_func_table;
struct mail_user *
mail_user_alloc(struct mail_storage_service_user *service_user);
int mail_user_init(struct mail_user *user, const char **error_r);
void mail_user_ref(struct mail_user *user);
void mail_user_unref(struct mail_user **user);
void mail_user_deinit(struct mail_user **user);
struct mail_user *mail_user_dup(struct mail_user *user);
struct mail_user *mail_user_find(struct mail_user *user, const char *name);
void mail_user_set_vars(struct mail_user *user, const char *service,
const struct mail_user_connection_data *conn);
const struct var_expand_table *
mail_user_var_expand_table(struct mail_user *user);
void mail_user_set_home(struct mail_user *user, const char *home);
int mail_user_get_home(struct mail_user *user, const char **home_r);
void mail_user_set_get_temp_prefix(string_t *dest,
const struct mail_user_settings *set);
const char *mail_user_get_volatile_dir(struct mail_user *user);
int mail_user_lock_file_create(struct mail_user *user, const char *lock_fname,
unsigned int lock_secs,
struct file_lock **lock_r, const char **error_r);
bool mail_user_is_plugin_loaded(struct mail_user *user, struct module *module);
const char *mail_user_plugin_getenv(struct mail_user *user, const char *name);
bool mail_user_plugin_getenv_bool(struct mail_user *user, const char *name);
const char *mail_user_set_plugin_getenv(const struct mail_storage_settings *set,
const char *name);
bool mail_user_set_plugin_getenv_bool(const struct mail_storage_settings *set,
const char *name);
void mail_user_add_namespace(struct mail_user *user,
struct mail_namespace **namespaces);
void mail_user_drop_useless_namespaces(struct mail_user *user);
const char *mail_user_home_expand(struct mail_user *user, const char *path);
int mail_user_try_home_expand(struct mail_user *user, const char **path);
void mail_user_get_anvil_session(struct mail_user *user,
struct master_service_anvil_session *session_r);
const char *const *mail_user_get_alt_usernames(struct mail_user *user);
struct mail_storage *
mail_user_get_storage_class(struct mail_user *user, const char *name);
void mail_user_add_event_fields(struct mail_user *user);
void mail_user_init_fs_settings(struct mail_user *user,
struct fs_settings *fs_set,
struct ssl_iostream_settings *ssl_set_r);
int mail_user_home_mkdir(struct mail_user *user);
const struct dict_op_settings *
mail_user_get_dict_op_settings(struct mail_user *user);
static inline bool
mail_user_get_postmaster_address(struct mail_user *user,
const struct message_address **address_r,
const char **error_r)
{
return mail_user_set_get_postmaster_address(user->set, address_r,
error_r);
}
static inline bool
mail_user_get_postmaster_smtp(struct mail_user *user,
const struct smtp_address **address_r,
const char **error_r)
{
return mail_user_set_get_postmaster_smtp(user->set, address_r,
error_r);
}
#endif