#ifndef MASTER_SERVICE_SETTINGS_H
#define MASTER_SERVICE_SETTINGS_H
struct master_service;
struct master_service_settings {
pool_t pool;
const char *base_dir;
const char *state_dir;
const char *instance_name;
const char *log_path;
const char *info_log_path;
const char *debug_log_path;
const char *log_timestamp;
const char *log_debug;
const char *log_core_filter;
const char *process_shutdown_filter;
const char *syslog_facility;
const char *import_environment;
const char *stats_writer_socket_path;
bool version_ignore;
bool shutdown_clients;
bool verbose_proctitle;
const char *haproxy_trusted_networks;
unsigned int haproxy_timeout;
};
struct master_service_settings_input {
const char *config_path;
int config_fd;
bool preserve_environment;
bool preserve_user;
bool preserve_home;
bool no_service_filter;
bool check_full_config;
bool hide_obsolete_warnings;
bool no_protocol_filter;
bool no_key_validation;
bool reload_config;
bool never_exec;
bool always_exec;
bool return_config_fd;
bool use_sysexits;
const char *protocol;
};
struct master_service_settings_output {
const char *const *specific_services;
int config_fd;
bool permission_denied:1;
};
extern const struct setting_parser_info master_service_setting_parser_info;
int master_service_settings_read(struct master_service *service,
const struct master_service_settings_input *input,
struct master_service_settings_output *output_r,
const char **error_r);
int master_service_settings_read_simple(struct master_service *service,
const char **error_r);
const struct master_service_settings *
master_service_get_service_settings(struct master_service *service);
#endif