#ifndef AUTH_PROXY_H
#define AUTH_PROXY_H
#include "net.h"
enum auth_proxy_ssl_flags {
AUTH_PROXY_SSL_FLAG_YES		= BIT(0),
AUTH_PROXY_SSL_FLAG_STARTTLS	= BIT(1),
AUTH_PROXY_SSL_FLAG_ANY_CERT	= BIT(2),
};
struct auth_proxy_settings {
bool proxy;
const char *host;
struct ip_addr host_ip;
in_port_t port;
enum auth_proxy_ssl_flags ssl_flags;
struct ip_addr source_ip;
const char *username;
const char *master_user;
const char *password;
const char *sasl_mechanism;
unsigned int timeout_msecs;
bool nopipelining:1;
bool noauth:1;
bool remote_not_trusted:1;
bool redirect_reauth:1;
};
int auth_proxy_settings_parse(struct auth_proxy_settings *set, pool_t pool,
const char *key, const char *value,
const char **error_r);
bool auth_proxy_parse_redirect(const char *target, const char **destuser_r,
const char **host_r, in_port_t *port_r);
#endif