#ifndef SMTP_CLIENT_H
#define SMTP_CLIENT_H
#include "net.h"
#include "smtp-common.h"
#include "smtp-address.h"
#include "smtp-reply.h"
struct smtp_client;
struct smtp_client_request;
#define SMTP_DEFAULT_CONNECT_TIMEOUT_MSECS (1000*30)
#define SMTP_DEFAULT_COMMAND_TIMEOUT_MSECS (1000*60*5)
#define SMTP_DEFAULT_MAX_REPLY_SIZE (SIZE_MAX)
#define SMTP_DEFAULT_MAX_DATA_CHUNK_SIZE NET_BLOCK_SIZE
#define SMTP_DEFAULT_MAX_DATA_CHUNK_PIPELINE 4
enum smtp_client_command_error {
SMTP_CLIENT_COMMAND_ERROR_CONNECTION_CLOSED      =  421,
SMTP_CLIENT_COMMAND_ERROR_ABORTED                = 9000,
SMTP_CLIENT_COMMAND_ERROR_HOST_LOOKUP_FAILED,
SMTP_CLIENT_COMMAND_ERROR_CONNECT_FAILED,
SMTP_CLIENT_COMMAND_ERROR_AUTH_FAILED,
SMTP_CLIENT_COMMAND_ERROR_CONNECTION_LOST,
SMTP_CLIENT_COMMAND_ERROR_BAD_REPLY,
SMTP_CLIENT_COMMAND_ERROR_BROKEN_PAYLOAD,
SMTP_CLIENT_COMMAND_ERROR_TIMED_OUT
};
struct smtp_client_capability_extra {
const char *name;
const char *const *mail_param_extensions;
const char *const *rcpt_param_extensions;
};
struct smtp_client_settings {
struct ip_addr my_ip;
const char *my_hostname;
const char *temp_path_prefix;
enum smtp_capability forced_capabilities;
const char *const *extra_capabilities;
struct dns_client *dns_client;
const char *dns_client_socket_path;
const struct ssl_iostream_settings *ssl;
const char *master_user;
const char *username;
const char *password;
const struct dsasl_client_mech *sasl_mech;
const char *sasl_mechanisms;
const char *rawlog_dir;
unsigned int command_timeout_msecs;
unsigned int connect_timeout_msecs;
size_t max_reply_size;
uoff_t max_data_chunk_size;
unsigned int max_data_chunk_pipeline;
struct smtp_proxy_data proxy_data;
size_t socket_send_buffer_size;
size_t socket_recv_buffer_size;
struct event *event_parent;
bool debug;
bool peer_trusted;
bool xclient_defer;
bool remember_password;
bool mail_send_broken_path;
bool verbose_user_errors;
};
struct smtp_client *smtp_client_init(const struct smtp_client_settings *set);
void smtp_client_deinit(struct smtp_client **_client);
void smtp_client_switch_ioloop(struct smtp_client *client);
#endif