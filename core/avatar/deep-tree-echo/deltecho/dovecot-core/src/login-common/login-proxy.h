#ifndef LOGIN_PROXY_H
#define LOGIN_PROXY_H
#include "net.h"
#include "guid.h"
#include "auth-proxy.h"
#define LOGIN_PROXY_TTL 7
#define LOGIN_PROXY_DEFAULT_HOST_IMMEDIATE_FAILURE_AFTER_SECS 30
#define LOGIN_PROXY_FAILURE_MSG "Account is temporarily unavailable."
struct client;
struct login_proxy;
enum login_proxy_failure_type {
LOGIN_PROXY_FAILURE_TYPE_CONNECT,
LOGIN_PROXY_FAILURE_TYPE_INTERNAL,
LOGIN_PROXY_FAILURE_TYPE_INTERNAL_CONFIG,
LOGIN_PROXY_FAILURE_TYPE_REMOTE,
LOGIN_PROXY_FAILURE_TYPE_REMOTE_CONFIG,
LOGIN_PROXY_FAILURE_TYPE_PROTOCOL,
LOGIN_PROXY_FAILURE_TYPE_AUTH,
LOGIN_PROXY_FAILURE_TYPE_AUTH_TEMPFAIL,
LOGIN_PROXY_FAILURE_TYPE_AUTH_REDIRECT,
};
struct login_proxy_settings {
const char *host;
struct ip_addr ip, source_ip;
in_port_t port;
unsigned int connect_timeout_msecs;
unsigned int notify_refresh_secs;
unsigned int host_immediate_failure_after_secs;
enum auth_proxy_ssl_flags ssl_flags;
const char *rawlog_dir;
};
typedef void login_proxy_input_callback_t(struct client *client);
typedef void login_proxy_failure_callback_t(struct client *client,
enum login_proxy_failure_type type,
const char *reason,
bool reconnecting);
typedef void login_proxy_redirect_callback_t(struct client *client,
struct event *event,
const char *destination);
int login_proxy_new(struct client *client, struct event *event,
const struct login_proxy_settings *set,
login_proxy_input_callback_t *input_callback,
login_proxy_failure_callback_t *failure_callback,
login_proxy_redirect_callback_t *redirect_callback);
void login_proxy_free(struct login_proxy **proxy);
void login_proxy_get_redirect_path(struct login_proxy *proxy, string_t *str);
void login_proxy_redirect_finish(struct login_proxy *proxy,
const struct ip_addr *ip, in_port_t port);
bool login_proxy_failed(struct login_proxy *proxy, struct event *event,
enum login_proxy_failure_type type, const char *reason);
bool login_proxy_is_ourself(const struct client *client, const char *host,
in_port_t port, const char *destuser);
void login_proxy_detach(struct login_proxy *proxy);
int login_proxy_starttls(struct login_proxy *proxy);
struct istream *login_proxy_get_istream(struct login_proxy *proxy);
struct ostream *login_proxy_get_ostream(struct login_proxy *proxy);
void login_proxy_append_success_log_info(struct login_proxy *proxy,
string_t *str);
struct event *login_proxy_get_event(struct login_proxy *proxy);
const struct ip_addr *
login_proxy_get_source_host(const struct login_proxy *proxy);
const char *login_proxy_get_host(const struct login_proxy *proxy) ATTR_PURE;
const char *login_proxy_get_ip_str(const struct login_proxy *proxy) ATTR_PURE;
in_port_t login_proxy_get_port(const struct login_proxy *proxy) ATTR_PURE;
enum auth_proxy_ssl_flags
login_proxy_get_ssl_flags(const struct login_proxy *proxy) ATTR_PURE;
unsigned int
login_proxy_get_connect_timeout_msecs(const struct login_proxy *proxy) ATTR_PURE;
unsigned int
login_proxy_kick_user_connection(const char *user, const guid_128_t conn_guid);
void login_proxy_kill_idle(void);
unsigned int login_proxies_get_detached_count(void);
struct client *login_proxies_get_first_detached_client(void);
void login_proxy_init(const char *proxy_notify_pipe_path);
void login_proxy_deinit(void);
#endif