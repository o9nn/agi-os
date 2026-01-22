#ifndef HTTP_CLIENT_H
#define HTTP_CLIENT_H
#include "net.h"
#include "http-common.h"
#include "http-response.h"
struct timeval;
struct http_response;
struct http_client_request;
struct http_client;
struct http_client_context;
struct ssl_iostream_settings;
struct http_client_settings {
struct dns_client *dns_client;
const char *dns_client_socket_path;
unsigned int dns_ttl_msecs;
const struct ssl_iostream_settings *ssl;
const char *user_agent;
const char *proxy_socket_path;
const struct http_url *proxy_url;
const char *proxy_username;
const char *proxy_password;
const char *rawlog_dir;
unsigned int max_idle_time_msecs;
unsigned int max_parallel_connections;
unsigned int max_pipelined_requests;
bool no_auto_redirect;
bool no_auto_retry;
bool no_ssl_tunnel;
unsigned int max_redirects;
unsigned int max_attempts;
unsigned int max_connect_attempts;
unsigned int connect_backoff_time_msecs;
unsigned int connect_backoff_max_time_msecs;
struct http_header_limits response_hdr_limits;
unsigned int request_absolute_timeout_msecs;
unsigned int request_timeout_msecs;
unsigned int connect_timeout_msecs;
unsigned int soft_connect_timeout_msecs;
unsigned int max_auto_retry_delay_secs;
size_t socket_send_buffer_size;
size_t socket_recv_buffer_size;
struct event *event_parent;
bool debug;
};
enum http_client_request_error {
HTTP_CLIENT_REQUEST_ERROR_ABORTED = HTTP_RESPONSE_STATUS_INTERNAL,
HTTP_CLIENT_REQUEST_ERROR_INVALID_URL,
HTTP_CLIENT_REQUEST_ERROR_HOST_LOOKUP_FAILED,
HTTP_CLIENT_REQUEST_ERROR_CONNECT_FAILED,
HTTP_CLIENT_REQUEST_ERROR_INVALID_REDIRECT,
HTTP_CLIENT_REQUEST_ERROR_CONNECTION_LOST,
HTTP_CLIENT_REQUEST_ERROR_BROKEN_PAYLOAD,
HTTP_CLIENT_REQUEST_ERROR_BAD_RESPONSE,
HTTP_CLIENT_REQUEST_ERROR_TIMED_OUT,
};
enum http_request_state {
HTTP_REQUEST_STATE_NEW = 0,
HTTP_REQUEST_STATE_QUEUED,
HTTP_REQUEST_STATE_PAYLOAD_OUT,
HTTP_REQUEST_STATE_WAITING,
HTTP_REQUEST_STATE_GOT_RESPONSE,
HTTP_REQUEST_STATE_PAYLOAD_IN,
HTTP_REQUEST_STATE_FINISHED,
HTTP_REQUEST_STATE_ABORTED
};
extern const char *http_request_state_names[];
struct http_client_tunnel {
int fd_in, fd_out;
struct istream *input;
struct ostream *output;
};
struct http_client_request_stats {
unsigned int total_msecs;
unsigned int first_sent_msecs;
unsigned int last_sent_msecs;
unsigned int other_ioloop_msecs;
unsigned int http_ioloop_msecs;
unsigned int lock_msecs;
unsigned int attempts;
unsigned int send_attempts;
};
typedef void
http_client_request_callback_t(const struct http_response *response,
void *context);
struct http_client_request *
http_client_request(struct http_client *client,
const char *method, const char *host, const char *target,
http_client_request_callback_t *callback, void *context);
#define http_client_request(client, method, host, target, callback, context) \
http_client_request(client, method, host, target - \
CALLBACK_TYPECHECK(callback, void (*)( \
const struct http_response *response, \
typeof(context))), \
(http_client_request_callback_t *)callback, context)
struct http_client_request *
http_client_request_url(struct http_client *client, const char *method,
const struct http_url *target_url,
http_client_request_callback_t *callback,
void *context);
#define http_client_request_url(client, method, target_url, callback, context) \
http_client_request_url(client, method, target_url - \
CALLBACK_TYPECHECK(callback, void (*)( \
const struct http_response *response, \
typeof(context))), \
(http_client_request_callback_t *)callback, context)
struct http_client_request *
http_client_request_url_str(struct http_client *client, const char *method,
const char *url_str,
http_client_request_callback_t *callback,
void *context);
#define http_client_request_url_str(client, method, url_str, \
callback, context) \
http_client_request_url_str(client, method, url_str - \
CALLBACK_TYPECHECK(callback, void (*)( \
const struct http_response *response, \
typeof(context))), \
(http_client_request_callback_t *)callback, context)
struct http_client_request *
http_client_request_connect(struct http_client *client,
const char *host, in_port_t port,
http_client_request_callback_t *callback,
void *context);
#define http_client_request_connect(client, host, port, callback, context) \
http_client_request_connect(client, host, port - \
CALLBACK_TYPECHECK(callback, void (*)( \
const struct http_response *response, \
typeof(context))), \
(http_client_request_callback_t *)callback, context)
struct http_client_request *
http_client_request_connect_ip(struct http_client *client,
const struct ip_addr *ip, in_port_t port,
http_client_request_callback_t *callback,
void *context);
#define http_client_request_connect_ip(client, ip, port, callback, context) \
http_client_request_connect_ip(client, ip, port - \
CALLBACK_TYPECHECK(callback, void (*)( \
const struct http_response *response, \
typeof(context))), \
(http_client_request_callback_t *)callback, context)
void http_client_request_set_event(struct http_client_request *req,
struct event *event);
void http_client_request_set_port(struct http_client_request *req,
in_port_t port);
void http_client_request_set_ssl(struct http_client_request *req, bool ssl);
void http_client_request_set_urgent(struct http_client_request *req);
void http_client_request_set_preserve_exact_reason(
struct http_client_request *req);
void http_client_request_add_header(struct http_client_request *req,
const char *key, const char *value);
void http_client_request_add_missing_header(struct http_client_request *req,
const char *key, const char *value);
void http_client_request_remove_header(struct http_client_request *req,
const char *key);
const char *http_client_request_lookup_header(struct http_client_request *req,
const char *key);
void http_client_request_set_date(struct http_client_request *req, time_t date);
void http_client_request_set_payload(struct http_client_request *req,
struct istream *input, bool sync);
void http_client_request_set_payload_data(struct http_client_request *req,
const unsigned char *data,
size_t size);
void http_client_request_set_payload_empty(struct http_client_request *req);
void http_client_request_set_timeout_msecs(struct http_client_request *req,
unsigned int msecs);
void http_client_request_set_timeout(struct http_client_request *req,
const struct timeval *time);
void http_client_request_set_attempt_timeout_msecs(
struct http_client_request *req, unsigned int msecs);
void http_client_request_set_max_attempts(struct http_client_request *req,
unsigned int max_attempts);
void http_client_request_set_event_headers(struct http_client_request *req,
const char *const *headers);
void http_client_request_set_auth_simple(struct http_client_request *req,
const char *username,
const char *password);
void http_client_request_set_proxy_url(struct http_client_request *req,
const struct http_url *proxy_url);
void http_client_request_set_proxy_socket(struct http_client_request *req,
const char *proxy_socket);
void http_client_request_delay_until(struct http_client_request *req,
time_t time);
void http_client_request_delay(struct http_client_request *req, time_t seconds);
void http_client_request_delay_msecs(struct http_client_request *req,
unsigned int msecs);
int http_client_request_delay_from_response(
struct http_client_request *req, const struct http_response *response);
const char *
http_client_request_get_method(const struct http_client_request *req) ATTR_PURE;
const char *
http_client_request_get_target(const struct http_client_request *req) ATTR_PURE;
enum http_request_state
http_client_request_get_state(const struct http_client_request *req) ATTR_PURE;
unsigned int
http_client_request_get_attempts(const struct http_client_request *req)
ATTR_PURE;
const struct http_url *
http_client_request_get_origin_url(const struct http_client_request *req)
ATTR_PURE;
void http_client_request_get_stats(struct http_client_request *req,
struct http_client_request_stats *stats);
void http_client_request_append_stats_text(struct http_client_request *req,
string_t *str);
void http_client_request_submit(struct http_client_request *req);
bool http_client_request_try_retry(struct http_client_request *req);
void http_client_request_abort(struct http_client_request **req);
void http_client_request_set_destroy_callback(struct http_client_request *req,
void (*callback)(void *),
void *context);
#define http_client_request_set_destroy_callback(req, callback, context) \
http_client_request_set_destroy_callback(req, \
(void(*)(void*))callback, \
TRUE ? context : \
CALLBACK_TYPECHECK(callback, void (*)(typeof(context))))
int http_client_request_send_payload(struct http_client_request **req,
const unsigned char *data, size_t size);
int http_client_request_finish_payload(struct http_client_request **req);
void http_client_request_start_tunnel(struct http_client_request *req,
struct http_client_tunnel *tunnel);
struct http_client *http_client_init(const struct http_client_settings *set);
struct http_client *
http_client_init_private(const struct http_client_settings *set);
struct http_client *
http_client_init_shared(struct http_client_context *cctx,
const struct http_client_settings *set) ATTR_NULL(1);
void http_client_deinit(struct http_client **_client);
struct ioloop *http_client_switch_ioloop(struct http_client *client);
void http_client_wait(struct http_client *client);
unsigned int http_client_get_pending_request_count(struct http_client *client);
struct http_client_context *
http_client_context_create(const struct http_client_settings *set);
void http_client_context_ref(struct http_client_context *cctx);
void http_client_context_unref(struct http_client_context **_cctx);
struct http_client_context *http_client_get_global_context(void);
void http_client_global_context_free(void);
#endif