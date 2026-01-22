#ifndef HTTP_SERVER_H
#define HTTP_SERVER_H
#include "http-common.h"
#include "http-auth.h"
#include "http-request.h"
struct istream;
struct ostream;
struct http_request;
struct http_server;
struct http_server_resource;
struct http_server_request;
struct http_server_response;
struct http_server_settings {
const char *default_host;
const char *rawlog_dir;
const struct ssl_iostream_settings *ssl;
unsigned int max_client_idle_time_msecs;
unsigned int max_pipelined_requests;
struct http_request_limits request_limits;
size_t socket_send_buffer_size;
size_t socket_recv_buffer_size;
struct event *event;
bool debug;
};
struct http_server_tunnel {
int fd_in, fd_out;
struct istream *input;
struct ostream *output;
};
typedef void
(*http_server_tunnel_callback_t)(void *context,
const struct http_server_tunnel *tunnel);
struct http_server_response *
http_server_response_create(struct http_server_request *req,
unsigned int status, const char *reason);
void http_server_response_ref(struct http_server_response *resp);
bool http_server_response_unref(struct http_server_response **_resp);
void http_server_response_add_header(struct http_server_response *resp,
const char *key, const char *value);
void http_server_response_add_permanent_header(struct http_server_response *resp,
const char *key, const char *value);
void http_server_response_update_status(struct http_server_response *resp,
unsigned int status, const char *reason);
void http_server_response_set_date(struct http_server_response *resp,
time_t date);
void http_server_response_set_payload(struct http_server_response *resp,
struct istream *input);
void http_server_response_set_payload_data(struct http_server_response *resp,
const unsigned char *data,
size_t size);
struct ostream *
http_server_response_get_payload_output(struct http_server_response *resp,
size_t max_buffer_size, bool blocking);
void http_server_response_get_status(struct http_server_response *resp,
int *status_r, const char **reason_r);
uoff_t http_server_response_get_total_size(struct http_server_response *resp);
void http_server_response_add_auth(struct http_server_response *resp,
const struct http_auth_challenge *chlng);
void http_server_response_add_auth_basic(struct http_server_response *resp,
const char *realm);
void http_server_response_submit(struct http_server_response *resp);
void http_server_response_submit_close(struct http_server_response *resp);
void http_server_response_submit_tunnel(struct http_server_response *resp,
http_server_tunnel_callback_t callback,
void *context);
int http_server_response_send_payload(struct http_server_response **resp,
const unsigned char *data, size_t size);
int http_server_response_finish_payload(struct http_server_response **resp);
void http_server_response_abort_payload(struct http_server_response **resp);
const struct http_request *
http_server_request_get(struct http_server_request *req);
void http_server_request_ref(struct http_server_request *req);
bool http_server_request_unref(struct http_server_request **_req);
void http_server_request_connection_close(struct http_server_request *req,
bool close);
pool_t http_server_request_get_pool(struct http_server_request *req);
struct http_server_response *
http_server_request_get_response(struct http_server_request *req);
bool http_server_request_is_finished(struct http_server_request *req);
void http_server_request_add_response_header(struct http_server_request *req,
const char *key, const char *value);
struct istream *
http_server_request_get_payload_input(struct http_server_request *req,
bool blocking);
void http_server_request_forward_payload(struct http_server_request *req,
struct ostream *output,
uoff_t max_size,
void (*callback)(void *),
void *context);
#define http_server_request_forward_payload(req, output, max_size, \
callback, context) \
http_server_request_forward_payload(req, output, max_size, \
(void(*)(void*))callback, TRUE ? context : \
CALLBACK_TYPECHECK(callback, void (*)(typeof(context))))
void http_server_request_buffer_payload(struct http_server_request *req,
buffer_t *buffer, uoff_t max_size,
void (*callback)(void *),
void *context);
#define http_server_request_buffer_payload(req, buffer, max_size, \
callback, context) \
http_server_request_buffer_payload(req, buffer, max_size, \
(void(*)(void*))callback, TRUE ? context : \
CALLBACK_TYPECHECK(callback, void (*)(typeof(context))))
void http_server_request_handle_payload(struct http_server_request *req,
void (*callback)(void *context),
void *context);
#define http_server_request_handle_payload(req, callback, context) \
http_server_request_handle_payload(req,\
(void(*)(void*))callback, TRUE ? context : \
CALLBACK_TYPECHECK(callback, void (*)(typeof(context))))
int http_server_request_get_auth(struct http_server_request *req,
struct http_auth_credentials *credentials);
void http_server_request_fail(struct http_server_request *req,
unsigned int status, const char *reason);
void http_server_request_fail_close(struct http_server_request *req,
unsigned int status, const char *reason);
void http_server_request_fail_text(struct http_server_request *req,
unsigned int status, const char *reason,
const char *format, ...) ATTR_FORMAT(4, 5);
void http_server_request_fail_auth(struct http_server_request *req,
const char *reason,
const struct http_auth_challenge *chlng)
ATTR_NULL(2);
void http_server_request_fail_auth_basic(struct http_server_request *req,
const char *reason, const char *realm)
ATTR_NULL(2);
void http_server_request_fail_bad_method(struct http_server_request *req,
const char *allow);
void http_server_request_set_destroy_callback(struct http_server_request *req,
void (*callback)(void *),
void *context);
#define http_server_request_set_destroy_callback(req, callback, context) \
http_server_request_set_destroy_callback( \
req, (void(*)(void*))callback, \
(TRUE ? context : \
CALLBACK_TYPECHECK(callback, void (*)(typeof(context)))))
struct http_server_stats {
unsigned int request_count, response_count;
uoff_t input, output;
};
struct http_server_callbacks {
void (*handle_request)(void *context, struct http_server_request *req);
void (*handle_connect_request)(void *context,
struct http_server_request *req,
struct http_url *target);
void (*connection_destroy)(void *context, const char *reason);
};
struct http_server_connection *
http_server_connection_create(struct http_server *server,
int fd_in, int fd_out, bool ssl,
const struct http_server_callbacks *callbacks,
void *context);
void http_server_connection_ref(struct http_server_connection *conn);
bool http_server_connection_unref(struct http_server_connection **_conn);
void http_server_connection_close(struct http_server_connection **_conn,
const char *reason);
const struct http_server_stats *
http_server_connection_get_stats(struct http_server_connection *conn);
struct ioloop *
http_server_connection_switch_ioloop_to(struct http_server_connection *conn,
struct ioloop *ioloop);
struct ioloop *
http_server_connection_switch_ioloop(struct http_server_connection *conn);
typedef void
(http_server_resource_callback_t)(void *context,
struct http_server_request *req,
const char *sub_path);
struct http_server_resource *
http_server_resource_create(struct http_server *server, pool_t pool,
http_server_resource_callback_t *callback,
void *context);
#define http_server_resource_create(server, pool, callback, context) \
http_server_resource_create(server, pool, \
(http_server_resource_callback_t *)callback, \
(TRUE ? context : \
CALLBACK_TYPECHECK(callback, void (*)( \
typeof(context), struct http_server_request *req, \
const char *sub_path))))
void http_server_resource_free(struct http_server_resource **_res);
pool_t http_server_resource_get_pool(struct http_server_resource *res)
ATTR_PURE;
const char *
http_server_resource_get_path(struct http_server_resource *res) ATTR_PURE;
struct event *
http_server_resource_get_event(struct http_server_resource *res) ATTR_PURE;
void http_server_resource_add_location(struct http_server_resource *res,
const char *path);
void http_server_resource_set_destroy_callback(struct http_server_resource *res,
void (*callback)(void *),
void *context);
#define http_server_resource_set_destroy_callback(req, callback, context) \
http_server_resource_set_destroy_callback(req, \
(void(*)(void*))callback, context - \
CALLBACK_TYPECHECK(callback, void (*)(typeof(context))))
struct http_server *http_server_init(const struct http_server_settings *set);
void http_server_deinit(struct http_server **_server);
void http_server_shut_down(struct http_server *server);
void http_server_switch_ioloop(struct http_server *server);
#endif