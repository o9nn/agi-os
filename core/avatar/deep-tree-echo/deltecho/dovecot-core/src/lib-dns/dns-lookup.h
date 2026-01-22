#ifndef DNS_LOOKUP_H
#define DNS_LOOKUP_H
#define DNS_CLIENT_SOCKET_NAME "dns-client"
struct dns_lookup;
struct dns_lookup_settings {
const char *dns_client_socket_path;
unsigned int timeout_msecs;
unsigned int idle_timeout_msecs;
unsigned int cache_ttl_secs;
struct ioloop *ioloop;
struct event *event_parent;
};
struct dns_lookup_result {
int ret;
const char *error;
unsigned int msecs;
unsigned int ips_count;
const struct ip_addr *ips;
const char *name;
};
typedef void dns_lookup_callback_t(const struct dns_lookup_result *result,
void *context);
int dns_lookup(const char *host, const struct dns_lookup_settings *set,
dns_lookup_callback_t *callback, void *context,
struct dns_lookup **lookup_r) ATTR_NULL(4);
#define dns_lookup(host, set, callback, context, lookup_r) \
dns_lookup(host - \
CALLBACK_TYPECHECK(callback, void (*)( \
const struct dns_lookup_result *, typeof(context))), \
set, (dns_lookup_callback_t *)callback, context, lookup_r)
int dns_lookup_ptr(const struct ip_addr *ip,
const struct dns_lookup_settings *set,
dns_lookup_callback_t *callback, void *context,
struct dns_lookup **lookup_r) ATTR_NULL(4);
#define dns_lookup_ptr(host, set, callback, context, lookup_r) \
dns_lookup_ptr(host - \
CALLBACK_TYPECHECK(callback, void (*)( \
const struct dns_lookup_result *, typeof(context))), \
set, (dns_lookup_callback_t *)callback, context, lookup_r)
void dns_lookup_abort(struct dns_lookup **lookup);
void dns_lookup_switch_ioloop(struct dns_lookup *lookup);
struct dns_client *dns_client_init(const struct dns_lookup_settings *set);
void dns_client_deinit(struct dns_client **client);
int dns_client_connect(struct dns_client *client, const char **error_r);
int dns_client_lookup(struct dns_client *client, const char *host,
struct event *event,
dns_lookup_callback_t *callback, void *context,
struct dns_lookup **lookup_r) ATTR_NULL(4);
#define dns_client_lookup(client, host, event, callback, context, lookup_r) \
dns_client_lookup(client, host - \
CALLBACK_TYPECHECK(callback, void (*)( \
const struct dns_lookup_result *, typeof(context))), \
event, (dns_lookup_callback_t *)callback, context, lookup_r)
int dns_client_lookup_ptr(struct dns_client *client, const struct ip_addr *ip,
struct event *event,
dns_lookup_callback_t *callback, void *context,
struct dns_lookup **lookup_r) ATTR_NULL(4);
#define dns_client_lookup_ptr(client, ip, event, callback, context, lookup_r) \
dns_client_lookup_ptr(client, ip - \
CALLBACK_TYPECHECK(callback, void (*)( \
const struct dns_lookup_result *, typeof(context))), \
event, (dns_lookup_callback_t *)callback, context, lookup_r)
bool dns_client_has_pending_queries(struct dns_client *client);
void dns_client_switch_ioloop(struct dns_client *client);
#endif