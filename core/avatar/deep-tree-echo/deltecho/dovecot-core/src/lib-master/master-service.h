#ifndef MASTER_SERVICE_H
#define MASTER_SERVICE_H
#include "net.h"
#include "guid.h"
#include <unistd.h>
#include <stdio.h>
#define MASTER_SERVICE_SHUTTING_DOWN_MSG "Server shutting down"
enum master_service_flags {
MASTER_SERVICE_FLAG_STD_CLIENT = 0x01,
MASTER_SERVICE_FLAG_STANDALONE = 0x02,
MASTER_SERVICE_FLAG_DONT_LOG_TO_STDERR = 0x04,
MASTER_SERVICE_FLAG_NO_CONFIG_SETTINGS = 0x10,
MASTER_SERVICE_FLAG_TRACK_LOGIN_STATE = 0x40,
MASTER_SERVICE_FLAG_NO_IDLE_DIE = 0x80,
MASTER_SERVICE_FLAG_UPDATE_PROCTITLE = 0x100,
MASTER_SERVICE_FLAG_NO_SSL_INIT = 0x400,
MASTER_SERVICE_FLAG_NO_INIT_DATASTACK_FRAME = 0x800,
MASTER_SERVICE_FLAG_DONT_SEND_STATS = 0x1000,
MASTER_SERVICE_FLAG_HAVE_STARTTLS = 0x2000,
};
struct master_service_connection_haproxy {
const char *hostname;
const char *cert_common_name;
const unsigned char *alpn;
unsigned int alpn_size;
bool ssl:1;
bool ssl_client_cert:1;
};
struct master_service_connection {
int fd;
int listen_fd;
const char *name;
const char *type;
struct ip_addr remote_ip, local_ip;
in_port_t remote_port, local_port;
struct ip_addr real_remote_ip, real_local_ip;
in_port_t real_remote_port, real_local_port;
struct master_service_connection_haproxy haproxy;
bool haproxied:1;
bool fifo:1;
bool ssl:1;
bool accepted:1;
};
struct master_service_anvil_session {
const char *username;
const char *const *alt_usernames;
const char *service_name;
struct ip_addr ip;
struct ip_addr dest_ip;
};
typedef void
master_service_connection_callback_t(struct master_service_connection *conn);
typedef bool
master_service_avail_overflow_callback_t(bool kill, struct timeval *created_r);
extern struct master_service *master_service;
typedef void master_service_killed_callback_t(void *);
extern const struct option master_service_helpopt;
const char *master_service_getopt_string(void);
struct master_service *
master_service_init(const char *name, enum master_service_flags flags,
int *argc, char **argv[], const char *getopt_str);
void master_service_register_long_options(struct master_service *service,
const struct option *longopts);
int master_getopt(struct master_service *service);
int master_getopt_long(struct master_service *service, const char **longopt_r);
bool master_getopt_str_is_valid(const char *str);
bool master_service_parse_option(struct master_service *service,
int opt, const char *arg);
void master_service_init_finish(struct master_service *service);
void master_service_import_environment(const char *import_environment);
void master_service_env_clean(void);
void master_service_init_log(struct master_service *service);
void master_service_init_log_with_prefix(struct master_service *service,
const char *prefix);
void master_service_init_log_with_pid(struct master_service *service);
void master_service_init_stats_client(struct master_service *service,
bool silent_notfound_errors);
void master_service_set_die_with_master(struct master_service *service,
bool set);
void master_service_set_die_callback(struct master_service *service,
void (*callback)(void));
void master_service_set_idle_die_callback(struct master_service *service,
bool (*callback)(void));
void master_service_set_killed_callback(struct master_service *service,
master_service_killed_callback_t *callback,
void *context);
#define master_service_set_killed_callback(service, callback, context) \
master_service_set_killed_callback(service, \
1 ? (master_service_killed_callback_t *)callback : \
CALLBACK_TYPECHECK(callback, void (*)(typeof(context))), \
context)
void master_service_set_avail_overflow_callback(struct master_service *service,
master_service_avail_overflow_callback_t *callback);
void master_service_set_client_limit(struct master_service *service,
unsigned int client_limit);
unsigned int master_service_get_client_limit(struct master_service *service);
unsigned int master_service_get_process_limit(struct master_service *service);
unsigned int master_service_get_process_min_avail(struct master_service *service);
unsigned int master_service_get_idle_kill_secs(struct master_service *service);
void master_service_set_service_count(struct master_service *service,
unsigned int count);
unsigned int master_service_get_service_count(struct master_service *service);
unsigned int master_service_get_socket_count(struct master_service *service);
const char *master_service_get_socket_name(struct master_service *service,
int listen_fd);
const char *
master_service_get_socket_type(struct master_service *service, int listen_fd);
const char *master_service_get_config_path(struct master_service *service);
const char *master_service_get_version_string(struct master_service *service);
const char *master_service_get_name(struct master_service *service);
struct event *master_service_get_event(struct master_service *service);
const char *master_service_get_configured_name(struct master_service *service);
struct settings_root *
master_service_get_settings_root(struct master_service *service);
void master_service_run(struct master_service *service,
master_service_connection_callback_t *callback)
ATTR_NULL(2);
void master_service_stop(struct master_service *service);
void master_service_stop_new_connections(struct master_service *service);
bool master_service_is_killed(struct master_service *service);
int master_service_get_kill_signal(struct master_service *service);
void master_service_get_kill_time(struct master_service *service,
struct timeval *tv_r);
bool master_service_is_master_stopped(struct master_service *service);
bool master_service_anvil_connect(struct master_service *service,
const struct master_service_anvil_session *session,
bool kick_supported, guid_128_t conn_guid_r);
void master_service_anvil_disconnect(struct master_service *service,
const struct master_service_anvil_session *session,
const guid_128_t conn_guid);
void master_service_client_connection_accept(struct master_service_connection *conn);
void master_service_client_connection_created(struct master_service *service);
void master_service_client_connection_destroyed(struct master_service *service);
const char *
master_service_connection_get_type(
const struct master_service_connection *conn);
void master_service_deinit(struct master_service **service);
void master_service_deinit_forked(struct master_service **_service);
bool version_string_verify(const char *line, const char *service_name,
unsigned int major_version);
bool version_string_verify_full(const char *line, const char *service_name,
unsigned int major_version,
unsigned int *minor_version_r);
void master_service_set_process_shutdown_filter(struct master_service *service,
struct event_filter *filter);
void master_service_unset_process_shutdown_filter(struct master_service *service);
void master_service_set_current_user(struct master_service *service,
const char *user);
void master_service_set_last_kick_signal_user(struct master_service *service,
const char *user);
#endif