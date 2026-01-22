#ifndef LOGIN_PROXY_STATE_H
#define LOGIN_PROXY_STATE_H
#include <sys/time.h>
struct login_proxy_record {
struct ip_addr ip;
in_port_t port;
struct timeval disconnect_timestamp;
unsigned int num_disconnects_since_ts;
unsigned int num_delayed_client_disconnects;
unsigned int num_waiting_connections;
unsigned int num_proxying_connections;
struct timeval last_failure;
struct timeval last_success;
};
struct login_proxy_state *login_proxy_state_init(const char *notify_path);
void login_proxy_state_deinit(struct login_proxy_state **state);
struct login_proxy_record *
login_proxy_state_get(struct login_proxy_state *state,
const struct ip_addr *ip, in_port_t port);
void login_proxy_state_notify(struct login_proxy_state *state,
const char *user);
#endif