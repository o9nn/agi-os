#include "auth-common.h"
#include "ioloop.h"
#include "buffer.h"
#include "hash.h"
#include "sha1.h"
#include "hex-binary.h"
#include "str.h"
#include "array.h"
#include "safe-memset.h"
#include "str-sanitize.h"
#include "strescape.h"
#include "var-expand.h"
#include "dns-lookup.h"
#include "hostpid.h"
#include "master-service.h"
#include "auth-cache.h"
#include "auth-request.h"
#include "auth-request-handler.h"
#include "auth-request-handler-private.h"
#include "auth-client-connection.h"
#include "auth-master-connection.h"
#include "auth-policy.h"
#include "passdb.h"
#include "passdb-blocking.h"
#include "passdb-cache.h"
#include "passdb-template.h"
#include "userdb-blocking.h"
#include "userdb-template.h"
#include "password-scheme.h"
#include "wildcard-match.h"
#include <sys/stat.h>
#define AUTH_DNS_WARN_MSECS 500
#define AUTH_REQUEST_MAX_DELAY_SECS (60*5)
#define CACHED_PASSWORD_SCHEME "SHA1"
struct auth_request_proxy_dns_lookup_ctx {
struct auth_request *request;
struct event *event;
auth_request_proxy_cb_t *callback;
struct dns_lookup *dns_lookup;
};
struct auth_policy_check_ctx {
enum {
AUTH_POLICY_CHECK_TYPE_PLAIN,
AUTH_POLICY_CHECK_TYPE_LOOKUP,
AUTH_POLICY_CHECK_TYPE_SUCCESS,
} type;
struct auth_request *request;
buffer_t *success_data;
verify_plain_callback_t *callback_plain;
lookup_credentials_callback_t *callback_lookup;
};
unsigned int auth_request_state_count[AUTH_REQUEST_STATE_MAX];
static void
auth_request_userdb_import(struct auth_request *request, const char *args);
static void auth_request_lookup_credentials_policy_continue(
struct auth_request *request, lookup_credentials_callback_t *callback);
static void auth_request_policy_check_callback(int result, void *context);
#define MAX_LOG_USERNAME_LEN 64
static const char *get_log_prefix(struct auth_request *auth_request)
{
string_t *str = t_str_new(64);
const char *ip;
str_append(str, master_service_get_configured_name(master_service));
str_append_c(str, '(');
if (auth_request->fields.user == NULL)
str_append(str, "?");
else
str_sanitize_append(str, auth_request->fields.user,
MAX_LOG_USERNAME_LEN);
ip = net_ip2addr(&auth_request->fields.remote_ip);
if (ip[0] != '\0') {
str_append_c(str, ',');
str_append(str, ip);
}
if (auth_request->mech != NULL) {
str_append(str, ",sasl:");
str_append(str, t_str_lcase(auth_request->mech->mech_name));
}
if (auth_request->fields.requested_login_user != NULL)
str_append(str, ",master");
str_append_c(str, ')');
if (worker) {
str_append(str, "<");
str_append(str, my_pid);
str_append(str,">");
}
if (auth_request->fields.session_id != NULL)
str_printfa(str, "<%s>", auth_request->fields.session_id);
if (worker)
str_printfa(str, ": request [%u]", auth_request->id);
str_append(str, ": ");
return str_c(str);
}
const char *auth_request_get_log_prefix_db(struct auth_request *auth_request)
{
const char *name;
if (!auth_request->userdb_lookup) {
i_assert(auth_request->passdb != NULL);
name = auth_request->passdb->set->name[0] != '\0' ?
auth_request->passdb->set->name :
auth_request->passdb->passdb->iface.name;
} else {
i_assert(auth_request->userdb != NULL);
name = auth_request->userdb->set->name[0] != '\0' ?
auth_request->userdb->set->name :
auth_request->userdb->userdb->iface->name;
}
return t_strconcat(name, ": ", NULL);
}
static void
auth_request_post_alloc_init(struct auth_request *request,
struct event *parent_event)
{
enum log_type level;
request->state = AUTH_REQUEST_STATE_NEW;
auth_request_state_count[AUTH_REQUEST_STATE_NEW]++;
request->refcount = 1;
request->last_access = ioloop_time;
request->session_pid = (pid_t)-1;
request->set = global_auth_settings;
request->event = event_create(parent_event);
auth_request_fields_init(request);
level = request->set->verbose ? LOG_TYPE_INFO : LOG_TYPE_WARNING;
event_set_min_log_level(request->event, level);
event_set_forced_debug(request->event, request->set->debug);
event_add_category(request->event, &event_category_auth);
event_set_log_prefix_callback(request->event, TRUE,
get_log_prefix, request);
p_array_init(&request->authdb_event, request->pool, 2);
}
struct auth_request *
auth_request_new(const struct mech_module *mech, struct event *parent_event)
{
struct auth_request *request;
request = mech->auth_new();
request->mech = mech;
auth_request_post_alloc_init(request, parent_event);
enum log_type level =
(request->set->verbose ? LOG_TYPE_INFO : LOG_TYPE_WARNING);
const char *prefix = t_strconcat(
t_str_lcase(request->mech->mech_name), ": ", NULL);
request->mech_event = event_create(request->event);
event_set_min_log_level(request->mech_event, level);
event_set_append_log_prefix(request->mech_event, prefix);
return request;
}
struct auth_request *auth_request_new_dummy(struct event *parent_event)
{
struct auth_request *request;
pool_t pool;
pool = pool_alloconly_create(MEMPOOL_GROWING"auth_request", 1024);
request = p_new(pool, struct auth_request, 1);
request->pool = pool;
auth_request_post_alloc_init(request, parent_event);
return request;
}
void auth_request_set_state(struct auth_request *request,
enum auth_request_state state)
{
if (request->state == state)
return;
i_assert(request->to_penalty == NULL);
i_assert(auth_request_state_count[request->state] > 0);
auth_request_state_count[request->state]--;
auth_request_state_count[state]++;
request->state = state;
auth_refresh_proctitle();
}
void auth_request_init(struct auth_request *request)
{
struct auth *auth;
auth = auth_request_get_auth(request);
request->set = auth->set;
request->passdb = auth->passdbs;
request->userdb = auth->userdbs;
}
struct auth *auth_request_get_auth(struct auth_request *request)
{
return auth_find_service(request->fields.service);
}
void auth_request_success(struct auth_request *request,
const void *data, size_t data_size)
{
i_assert(request->state == AUTH_REQUEST_STATE_MECH_CONTINUE);
if (request->fields.userdb_reply != NULL)
auth_fields_snapshot(request->fields.userdb_reply);
struct auth_policy_check_ctx *ctx;
ctx = p_new(request->pool, struct auth_policy_check_ctx, 1);
ctx->request = request;
ctx->success_data = buffer_create_dynamic(request->pool, data_size);
buffer_append(ctx->success_data, data, data_size);
ctx->type = AUTH_POLICY_CHECK_TYPE_SUCCESS;
if (!request->set->policy_check_after_auth) {
auth_request_policy_check_callback(0, ctx);
return;
}
auth_policy_check(request, request->mech_password,
auth_request_policy_check_callback, ctx);
}
struct event_passthrough *
auth_request_finished_event(struct auth_request *request, struct event *event)
{
struct event_passthrough *e = event_create_passthrough(event);
if (request->failed) {
if (request->internal_failure) {
e->add_str("error", "internal failure");
} else {
e->add_str("error", "authentication failed");
}
} else if (request->fields.successful) {
e->add_str("success", "yes");
}
if (request->userdb_lookup) {
return e;
}
if (request->policy_penalty > 0)
e->add_int("policy_penalty", request->policy_penalty);
if (request->policy_refusal) {
e->add_str("policy_result", "refused");
} else if (request->policy_processed && request->policy_penalty > 0) {
e->add_str("policy_result", "delayed");
e->add_int("policy_penalty", request->policy_penalty);
} else if (request->policy_processed) {
e->add_str("policy_result", "ok");
}
return e;
}
void auth_request_log_finished(struct auth_request *request)
{
if (request->event_finished_sent)
return;
request->event_finished_sent = TRUE;
struct event_passthrough *e =
auth_request_finished_event(request, request->event)->
set_name("auth_request_finished");
e_debug(e->event(), "Auth request finished");
}
static void auth_request_success_continue(struct auth_policy_check_ctx *ctx)
{
struct auth_request *request = ctx->request;
i_assert(request->state == AUTH_REQUEST_STATE_MECH_CONTINUE);
timeout_remove(&request->to_penalty);
if (request->failed || !request->passdb_success) {
auth_request_fail(request);
return;
}
auth_request_set_auth_successful(request);
auth_request_log_finished(request);
if (request->delay_until > ioloop_time) {
unsigned int delay_secs = request->delay_until - ioloop_time;
request->to_penalty = timeout_add(delay_secs * 1000,
auth_request_success_continue, ctx);
return;
}
if (ctx->success_data->used > 0 && !request->fields.final_resp_ok) {
i_assert(!request->final_resp_sent);
request->final_resp_sent = TRUE;
auth_request_handler_reply_continue(request,
ctx->success_data->data, ctx->success_data->used);
return;
}
auth_request_set_state(request, AUTH_REQUEST_STATE_FINISHED);
auth_request_refresh_last_access(request);
auth_request_handler_reply(request, AUTH_CLIENT_RESULT_SUCCESS,
ctx->success_data->data, ctx->success_data->used);
}
void auth_request_fail_with_reply(struct auth_request *request,
const void *final_data, size_t final_data_size)
{
i_assert(request->state == AUTH_REQUEST_STATE_MECH_CONTINUE);
if (final_data_size > 0 && !request->fields.final_resp_ok) {
i_assert(!request->final_resp_sent);
request->final_resp_sent = TRUE;
auth_request_handler_reply_continue(request, final_data,
final_data_size);
return;
}
auth_request_set_state(request, AUTH_REQUEST_STATE_FINISHED);
auth_request_refresh_last_access(request);
auth_request_log_finished(request);
auth_request_handler_reply(request, AUTH_CLIENT_RESULT_FAILURE,
final_data, final_data_size);
}
void auth_request_fail(struct auth_request *request)
{
auth_request_fail_with_reply(request, "", 0);
}
void auth_request_internal_failure(struct auth_request *request)
{
request->internal_failure = TRUE;
auth_request_fail(request);
}
void auth_request_ref(struct auth_request *request)
{
request->refcount++;
}
void auth_request_unref(struct auth_request **_request)
{
struct auth_request *request = *_request;
*_request = NULL;
i_assert(request->refcount > 0);
if (--request->refcount > 0)
return;
i_assert(array_count(&request->authdb_event) == 0);
if (request->handler_pending_reply)
auth_request_handler_abort(request);
event_unref(&request->mech_event);
event_unref(&request->event);
auth_request_state_count[request->state]--;
auth_refresh_proctitle();
if (request->mech_password != NULL) {
safe_memset(request->mech_password, 0,
strlen(request->mech_password));
}
if (request->dns_lookup_ctx != NULL)
dns_lookup_abort(&request->dns_lookup_ctx->dns_lookup);
timeout_remove(&request->to_abort);
timeout_remove(&request->to_penalty);
if (request->mech != NULL)
request->mech->auth_free(request);
else
pool_unref(&request->pool);
}
bool auth_request_import_master(struct auth_request *request,
const char *key, const char *value)
{
pid_t pid;
i_assert(value != NULL);
if (strcmp(key, "session_pid") == 0) {
if (str_to_pid(value, &pid) == 0)
request->session_pid = pid;
} else if (strcmp(key, "request_auth_token") == 0)
request->request_auth_token = TRUE;
else
return FALSE;
return TRUE;
}
static bool
auth_request_fail_on_nuls(struct auth_request *request,
const unsigned char *data, size_t data_size)
{
if ((request->mech->flags & MECH_SEC_ALLOW_NULS) != 0)
return FALSE;
if (memchr(data, '\0', data_size) != NULL) {
e_debug(request->mech_event, "Unexpected NUL in auth data");
auth_request_fail(request);
return TRUE;
}
return FALSE;
}
void auth_request_initial(struct auth_request *request)
{
i_assert(request->state == AUTH_REQUEST_STATE_NEW);
auth_request_set_state(request, AUTH_REQUEST_STATE_MECH_CONTINUE);
if (auth_request_fail_on_nuls(request, request->initial_response,
request->initial_response_len))
return;
request->mech->auth_initial(request, request->initial_response,
request->initial_response_len);
}
void auth_request_continue(struct auth_request *request,
const unsigned char *data, size_t data_size)
{
i_assert(request->state == AUTH_REQUEST_STATE_MECH_CONTINUE);
if (request->final_resp_sent) {
if (!request->fields.successful) {
auth_request_fail(request);
return;
}
auth_request_success(request, "", 0);
return;
}
if (auth_request_fail_on_nuls(request, data, data_size))
return;
auth_request_refresh_last_access(request);
request->mech->auth_continue(request, data, data_size);
}
static void
auth_request_save_cache(struct auth_request *request,
enum passdb_result result)
{
struct auth_passdb *passdb = request->passdb;
const char *encoded_password;
string_t *str;
struct password_generate_params gen_params = {
.user = request->fields.user,
.rounds = 0
};
switch (result) {
case PASSDB_RESULT_USER_UNKNOWN:
case PASSDB_RESULT_PASSWORD_MISMATCH:
case PASSDB_RESULT_OK:
case PASSDB_RESULT_SCHEME_NOT_AVAILABLE:
break;
case PASSDB_RESULT_NEXT:
case PASSDB_RESULT_USER_DISABLED:
case PASSDB_RESULT_PASS_EXPIRED:
return;
case PASSDB_RESULT_INTERNAL_FAILURE:
i_unreached();
}
if (passdb_cache == NULL || passdb->cache_key == NULL)
return;
if (result < 0) {
if (result == PASSDB_RESULT_USER_UNKNOWN) {
auth_cache_insert(passdb_cache, request,
passdb->cache_key, "", FALSE);
}
return;
}
if (request->passdb_password == NULL &&
!auth_fields_exists(request->fields.extra_fields, "nopassword")) {
if (result != PASSDB_RESULT_OK ||
request->mech_password == NULL)
return;
if (!password_generate_encoded(request->mech_password,
&gen_params,
CACHED_PASSWORD_SCHEME,
&encoded_password))
i_unreached();
request->passdb_password =
p_strconcat(request->pool, "{"CACHED_PASSWORD_SCHEME"}",
encoded_password, NULL);
}
str = t_str_new(256);
if (request->passdb_password != NULL) {
if (*request->passdb_password != '{') {
str_append_c(str, '{');
str_append(str, passdb->passdb->default_pass_scheme);
str_append_c(str, '}');
}
str_append_tabescaped(str, request->passdb_password);
}
auth_fields_append(request->fields.extra_fields, str,
AUTH_FIELD_FLAG_CHANGED, AUTH_FIELD_FLAG_CHANGED,
TRUE);
auth_cache_insert(passdb_cache, request, passdb->cache_key, str_c(str),
result == PASSDB_RESULT_OK);
}
static bool
auth_request_mechanism_accepted(const char *const *mechs,
const struct mech_module *mech)
{
if (mechs == NULL) return TRUE;
if (mech == NULL)
return str_array_icase_find(mechs, "none");
return str_array_icase_find(mechs, mech->mech_name);
}
bool auth_request_username_accepted(const char *const *filter,
const char *username)
{
bool have_includes = FALSE;
bool matched_inc = FALSE;
for(;*filter != NULL; filter++) {
bool exclude = (**filter == '!');
if (!exclude)
have_includes = TRUE;
if (wildcard_match(username, (*filter)+(exclude?1:0))) {
if (exclude) {
return FALSE;
} else {
matched_inc = TRUE;
}
}
}
return matched_inc || !have_includes;
}
static bool
auth_request_want_skip_passdb(struct auth_request *request,
struct auth_passdb *passdb)
{
const char *const *mechs = passdb->mechanisms;
const char *const *username_filter = passdb->username_filter;
const char *username;
username = request->fields.user;
if (!auth_request_mechanism_accepted(mechs, request->mech)) {
e_debug(request->event, "skipping passdb: mechanism filtered");
return TRUE;
}
if (passdb->username_filter != NULL &&
!auth_request_username_accepted(username_filter, username)) {
e_debug(request->event, "skipping passdb: username filtered");
return TRUE;
}
bool authenticated = request->fields.skip_password_check;
switch (passdb->skip) {
case AUTH_PASSDB_SKIP_NEVER:
return FALSE;
case AUTH_PASSDB_SKIP_AUTHENTICATED:
return authenticated;
case AUTH_PASSDB_SKIP_UNAUTHENTICATED:
return !authenticated;
}
i_unreached();
}
static bool
auth_request_want_skip_userdb(struct auth_request *request,
struct auth_userdb *userdb)
{
switch (userdb->skip) {
case AUTH_USERDB_SKIP_NEVER:
return FALSE;
case AUTH_USERDB_SKIP_FOUND:
return request->userdb_success;
case AUTH_USERDB_SKIP_NOTFOUND:
return !request->userdb_success;
}
i_unreached();
}
static const char *
auth_request_cache_result_to_str(enum auth_request_cache_result result)
{
switch(result) {
case AUTH_REQUEST_CACHE_NONE:
return "none";
case AUTH_REQUEST_CACHE_HIT:
return "hit";
case AUTH_REQUEST_CACHE_MISS:
return "miss";
default:
i_unreached();
}
}
void auth_request_passdb_lookup_begin(struct auth_request *request)
{
struct event *event;
const char *name;
i_assert(request->passdb != NULL);
i_assert(!request->userdb_lookup);
request->passdb_cache_result = AUTH_REQUEST_CACHE_NONE;
name = (request->passdb->set->name[0] != '\0' ?
request->passdb->set->name :
request->passdb->passdb->iface.name);
event = event_create(request->event);
event_add_str(event, "passdb_id", dec2str(request->passdb->passdb->id));
event_add_str(event, "passdb_name", name);
event_add_str(event, "passdb", request->passdb->passdb->iface.name);
event_set_log_prefix_callback(event, FALSE,
auth_request_get_log_prefix_db, request);
if (*request->passdb->set->auth_verbose == 'y')
event_set_min_log_level(event, LOG_TYPE_INFO);
else if (*request->passdb->set->auth_verbose == 'n')
event_set_min_log_level(event, LOG_TYPE_WARNING);
e_debug(event_create_passthrough(event)->
set_name("auth_passdb_request_started")->
event(),
"Performing passdb lookup");
array_push_back(&request->authdb_event, &event);
}
void auth_request_passdb_lookup_end(struct auth_request *request,
enum passdb_result result)
{
i_assert(array_count(&request->authdb_event) > 0);
struct event *event = authdb_event(request);
struct event_passthrough *e =
event_create_passthrough(event)->
set_name("auth_passdb_request_finished")->
add_str("result", passdb_result_to_string(result));
if (request->passdb_cache_result != AUTH_REQUEST_CACHE_NONE &&
request->set->cache_ttl != 0 && request->set->cache_size != 0) {
e->add_str("cache", auth_request_cache_result_to_str(
request->passdb_cache_result));
}
e_debug(e->event(), "Finished passdb lookup");
event_unref(&event);
array_pop_back(&request->authdb_event);
}
void auth_request_userdb_lookup_begin(struct auth_request *request)
{
struct event *event;
const char *name;
i_assert(request->userdb != NULL);
i_assert(request->userdb_lookup);
request->userdb_cache_result = AUTH_REQUEST_CACHE_NONE;
name = (request->userdb->set->name[0] != '\0' ?
request->userdb->set->name :
request->userdb->userdb->iface->name);
event = event_create(request->event);
event_add_str(event, "userdb_id", dec2str(request->userdb->userdb->id));
event_add_str(event, "userdb_name", name);
event_add_str(event, "userdb", request->userdb->userdb->iface->name);
event_set_log_prefix_callback(event, FALSE,
auth_request_get_log_prefix_db, request);
if (*request->userdb->set->auth_verbose == 'y')
event_set_min_log_level(event, LOG_TYPE_INFO);
else if (*request->userdb->set->auth_verbose == 'n')
event_set_min_log_level(event, LOG_TYPE_WARNING);
e_debug(event_create_passthrough(event)->
set_name("auth_userdb_request_started")->
event(),
"Performing userdb lookup");
array_push_back(&request->authdb_event, &event);
}
void auth_request_userdb_lookup_end(struct auth_request *request,
enum userdb_result result)
{
i_assert(array_count(&request->authdb_event) > 0);
struct event *event = authdb_event(request);
struct event_passthrough *e =
event_create_passthrough(event)->
set_name("auth_userdb_request_finished")->
add_str("result", userdb_result_to_string(result));
if (request->userdb_cache_result != AUTH_REQUEST_CACHE_NONE &&
request->set->cache_ttl != 0 && request->set->cache_size != 0) {
e->add_str("cache", auth_request_cache_result_to_str(
request->userdb_cache_result));
}
e_debug(e->event(), "Finished userdb lookup");
event_unref(&event);
array_pop_back(&request->authdb_event);
}
static bool
auth_request_handle_passdb_callback(enum passdb_result *result,
struct auth_request *request)
{
struct auth_passdb *next_passdb;
enum auth_db_rule result_rule;
bool passdb_continue = FALSE;
if (request->passdb_password != NULL) {
safe_memset(request->passdb_password, 0,
strlen(request->passdb_password));
}
auth_request_passdb_lookup_end(request, *result);
if (request->passdb->set->deny &&
*result != PASSDB_RESULT_USER_UNKNOWN) {
if (*result != PASSDB_RESULT_INTERNAL_FAILURE) {
e_info(authdb_event(request),
"User found from deny passdb");
*result = PASSDB_RESULT_USER_DISABLED;
}
return TRUE;
}
if (request->failed) {
if (*result == PASSDB_RESULT_OK)
*result = PASSDB_RESULT_USER_UNKNOWN;
request->failed = FALSE;
}
switch (*result) {
case PASSDB_RESULT_USER_DISABLED:
return TRUE;
case PASSDB_RESULT_PASS_EXPIRED:
auth_request_set_field(request, "reason",
"Password expired", NULL);
return TRUE;
case PASSDB_RESULT_OK:
result_rule = request->passdb->result_success;
break;
case PASSDB_RESULT_INTERNAL_FAILURE:
result_rule = request->passdb->result_internalfail;
break;
case PASSDB_RESULT_NEXT:
e_debug(authdb_event(request),
"Not performing authentication (noauthenticate set)");
result_rule = AUTH_DB_RULE_CONTINUE;
break;
case PASSDB_RESULT_SCHEME_NOT_AVAILABLE:
case PASSDB_RESULT_USER_UNKNOWN:
case PASSDB_RESULT_PASSWORD_MISMATCH:
default:
result_rule = request->passdb->result_failure;
break;
}
switch (result_rule) {
case AUTH_DB_RULE_RETURN:
break;
case AUTH_DB_RULE_RETURN_OK:
request->passdb_success = TRUE;
break;
case AUTH_DB_RULE_RETURN_FAIL:
request->passdb_success = FALSE;
break;
case AUTH_DB_RULE_CONTINUE:
passdb_continue = TRUE;
if (*result == PASSDB_RESULT_OK) {
auth_request_set_password_verified(request);
}
break;
case AUTH_DB_RULE_CONTINUE_OK:
passdb_continue = TRUE;
request->passdb_success = TRUE;
auth_request_set_password_verified(request);
break;
case AUTH_DB_RULE_CONTINUE_FAIL:
passdb_continue = TRUE;
request->passdb_success = FALSE;
break;
}
auth_fields_remove(request->fields.extra_fields, "nopassword");
auth_fields_remove(request->fields.extra_fields, "noauthenticate");
if (request->fields.requested_login_user != NULL &&
*result == PASSDB_RESULT_OK) {
auth_request_master_user_login_finish(request);
next_passdb = auth_request_get_auth(request)->passdbs;
} else {
next_passdb = request->passdb->next;
}
while (next_passdb != NULL &&
auth_request_want_skip_passdb(request, next_passdb))
next_passdb = next_passdb->next;
if (*result == PASSDB_RESULT_OK || *result == PASSDB_RESULT_NEXT) {
auth_fields_snapshot(request->fields.extra_fields);
request->snapshot_have_userdb_prefetch_set =
request->userdb_prefetch_set;
if (request->fields.userdb_reply != NULL)
auth_fields_snapshot(request->fields.userdb_reply);
} else {
auth_fields_rollback(request->fields.extra_fields);
if (request->fields.userdb_reply != NULL) {
auth_fields_rollback(request->fields.userdb_reply);
request->userdb_prefetch_set =
request->snapshot_have_userdb_prefetch_set;
}
}
if (passdb_continue && next_passdb != NULL) {
request->passdb = next_passdb;
request->passdb_password = NULL;
if (*result == PASSDB_RESULT_USER_UNKNOWN) {
request->passdbs_seen_user_unknown = TRUE;
} else if (*result == PASSDB_RESULT_INTERNAL_FAILURE) {
request->passdbs_seen_internal_failure = TRUE;
}
return FALSE;
} else if (*result == PASSDB_RESULT_NEXT) {
e_error(request->event,
"%sLast passdb had noauthenticate field, "
"cannot authenticate user",
auth_request_get_log_prefix_db(request));
*result = PASSDB_RESULT_INTERNAL_FAILURE;
} else if (request->passdb_success) {
*result = PASSDB_RESULT_OK;
} else if (request->passdbs_seen_internal_failure) {
*result = PASSDB_RESULT_INTERNAL_FAILURE;
}
return TRUE;
}
void
auth_request_verify_plain_callback_finish(enum passdb_result result,
struct auth_request *request)
{
const char *error;
if (passdb_template_export(request->passdb->override_fields_tmpl,
request, &error) < 0) {
e_error(authdb_event(request),
"Failed to expand override_fields: %s", error);
result = PASSDB_RESULT_INTERNAL_FAILURE;
}
if (!auth_request_handle_passdb_callback(&result, request)) {
auth_request_verify_plain(request, request->mech_password,
request->private_callback.verify_plain);
} else {
auth_request_ref(request);
request->passdb_result = result;
request->private_callback.verify_plain(request->passdb_result,
request);
auth_request_unref(&request);
}
}
void auth_request_verify_plain_callback(enum passdb_result result,
struct auth_request *request)
{
struct auth_passdb *passdb = request->passdb;
i_assert(request->state == AUTH_REQUEST_STATE_PASSDB);
auth_request_set_state(request, AUTH_REQUEST_STATE_MECH_CONTINUE);
if (result == PASSDB_RESULT_OK &&
auth_fields_exists(request->fields.extra_fields, "noauthenticate"))
result = PASSDB_RESULT_NEXT;
if (result != PASSDB_RESULT_INTERNAL_FAILURE)
auth_request_save_cache(request, result);
else {
const char *cache_key = passdb->cache_key;
if (passdb_cache_verify_plain(request, cache_key,
request->mech_password,
&result, TRUE)) {
e_info(authdb_event(request),
"Falling back to expired data from cache");
return;
}
}
auth_request_verify_plain_callback_finish(result, request);
}
static bool password_has_illegal_chars(const char *password)
{
for (; *password != '\0'; password++) {
switch (*password) {
case '\001':
case '\t':
case '\r':
case '\n':
return TRUE;
}
}
return FALSE;
}
static bool auth_request_is_disabled_master_user(struct auth_request *request)
{
if (request->fields.requested_login_user == NULL ||
request->passdb != NULL)
return FALSE;
e_info(request->event,
"Attempted master login with no master passdbs "
"(trying to log in as user: %s)",
request->fields.requested_login_user);
return TRUE;
}
static void auth_request_policy_penalty_finish(void *context)
{
struct auth_policy_check_ctx *ctx = context;
timeout_remove(&ctx->request->to_penalty);
i_assert(ctx->request->state == AUTH_REQUEST_STATE_MECH_CONTINUE);
switch(ctx->type) {
case AUTH_POLICY_CHECK_TYPE_PLAIN:
ctx->request->handler->verify_plain_continue_callback(
ctx->request, ctx->callback_plain);
return;
case AUTH_POLICY_CHECK_TYPE_LOOKUP:
auth_request_lookup_credentials_policy_continue(
ctx->request, ctx->callback_lookup);
return;
case AUTH_POLICY_CHECK_TYPE_SUCCESS:
auth_request_success_continue(ctx);
return;
default:
i_unreached();
}
}
static void auth_request_policy_check_callback(int result, void *context)
{
struct auth_policy_check_ctx *ctx = context;
ctx->request->policy_processed = TRUE;
ctx->request->policy_penalty += result < 0 ? 0 : result;
if (ctx->request->set->policy_log_only && result != 0) {
auth_request_policy_penalty_finish(context);
return;
}
if (result < 0) {
auth_request_fail(ctx->request);
} else if (ctx->type != AUTH_POLICY_CHECK_TYPE_SUCCESS && result > 0 &&
!ctx->request->fields.no_penalty) {
ctx->request->to_penalty = timeout_add(result * 1000,
auth_request_policy_penalty_finish, context);
} else {
auth_request_policy_penalty_finish(context);
}
}
void auth_request_verify_plain(struct auth_request *request,
const char *password,
verify_plain_callback_t *callback)
{
struct auth_policy_check_ctx *ctx;
i_assert(request->state == AUTH_REQUEST_STATE_MECH_CONTINUE);
if (request->mech_password == NULL)
request->mech_password = p_strdup(request->pool, password);
else
i_assert(request->mech_password == password);
request->user_changed_by_lookup = FALSE;
if (request->policy_processed ||
!request->set->policy_check_before_auth) {
request->handler->verify_plain_continue_callback(request,
callback);
} else {
ctx = p_new(request->pool, struct auth_policy_check_ctx, 1);
ctx->request = request;
ctx->callback_plain = callback;
ctx->type = AUTH_POLICY_CHECK_TYPE_PLAIN;
auth_policy_check(request, request->mech_password,
auth_request_policy_check_callback, ctx);
}
}
void auth_request_default_verify_plain_continue(
struct auth_request *request, verify_plain_callback_t *callback)
{
struct auth_passdb *passdb;
enum passdb_result result;
const char *cache_key, *error;
const char *password = request->mech_password;
i_assert(request->state == AUTH_REQUEST_STATE_MECH_CONTINUE);
if (auth_request_is_disabled_master_user(request)) {
callback(PASSDB_RESULT_USER_UNKNOWN, request);
return;
}
if (password_has_illegal_chars(password)) {
e_info(authdb_event(request),
"Attempted login with password having illegal chars");
callback(PASSDB_RESULT_USER_UNKNOWN, request);
return;
}
passdb = request->passdb;
while (passdb != NULL && auth_request_want_skip_passdb(request, passdb))
passdb = passdb->next;
request->passdb = passdb;
if (passdb == NULL) {
e_error(request->event, "All password databases were skipped");
callback(PASSDB_RESULT_INTERNAL_FAILURE, request);
return;
}
auth_request_passdb_lookup_begin(request);
request->private_callback.verify_plain = callback;
cache_key = passdb_cache == NULL ? NULL : passdb->cache_key;
if (passdb_cache_verify_plain(request, cache_key, password,
&result, FALSE)) {
return;
}
auth_request_set_state(request, AUTH_REQUEST_STATE_PASSDB);
request->wanted_credentials_scheme = NULL;
if (passdb->passdb->iface.verify_plain == NULL) {
auth_request_verify_plain_callback(
PASSDB_RESULT_INTERNAL_FAILURE, request);
} else if (passdb->passdb->blocking) {
passdb_blocking_verify_plain(request);
} else if (passdb_template_export(passdb->default_fields_tmpl,
request, &error) < 0) {
e_error(authdb_event(request),
"Failed to expand default_fields: %s", error);
auth_request_verify_plain_callback(
PASSDB_RESULT_INTERNAL_FAILURE, request);
} else {
passdb->passdb->iface.verify_plain(
request, password, auth_request_verify_plain_callback);
}
}
static void
auth_request_lookup_credentials_finish(enum passdb_result result,
const unsigned char *credentials,
size_t size,
struct auth_request *request)
{
const char *error;
if (passdb_template_export(request->passdb->override_fields_tmpl,
request, &error) < 0) {
e_error(authdb_event(request),
"Failed to expand override_fields: %s", error);
result = PASSDB_RESULT_INTERNAL_FAILURE;
}
if (!auth_request_handle_passdb_callback(&result, request)) {
if (request->fields.skip_password_check &&
request->fields.delayed_credentials == NULL && size > 0) {
auth_request_set_delayed_credentials(request,
credentials, size);
}
auth_request_lookup_credentials(request,
request->wanted_credentials_scheme,
request->private_callback.lookup_credentials);
} else {
if (request->fields.delayed_credentials != NULL && size == 0) {
credentials = request->fields.delayed_credentials;
size = request->fields.delayed_credentials_size;
}
if (request->set->debug_passwords &&
result == PASSDB_RESULT_OK) {
e_debug(authdb_event(request),
"Credentials: %s",
binary_to_hex(credentials, size));
}
if (result == PASSDB_RESULT_SCHEME_NOT_AVAILABLE &&
request->passdbs_seen_user_unknown) {
result = PASSDB_RESULT_USER_UNKNOWN;
}
request->passdb_result = result;
request->private_callback.
lookup_credentials(result, credentials, size, request);
}
}
void auth_request_lookup_credentials_callback(enum passdb_result result,
const unsigned char *credentials,
size_t size,
struct auth_request *request)
{
struct auth_passdb *passdb = request->passdb;
const char *cache_cred, *cache_scheme;
i_assert(request->state == AUTH_REQUEST_STATE_PASSDB);
auth_request_set_state(request, AUTH_REQUEST_STATE_MECH_CONTINUE);
if (result == PASSDB_RESULT_OK &&
auth_fields_exists(request->fields.extra_fields, "noauthenticate"))
result = PASSDB_RESULT_NEXT;
if (result != PASSDB_RESULT_INTERNAL_FAILURE)
auth_request_save_cache(request, result);
else {
const char *cache_key = passdb->cache_key;
if (passdb_cache_lookup_credentials(request, cache_key,
&cache_cred, &cache_scheme,
&result, TRUE)) {
e_info(authdb_event(request),
"Falling back to expired data from cache");
passdb_handle_credentials(
result, cache_cred, cache_scheme,
auth_request_lookup_credentials_finish,
request);
return;
}
}
auth_request_lookup_credentials_finish(result, credentials, size,
request);
}
void auth_request_lookup_credentials(struct auth_request *request,
const char *scheme,
lookup_credentials_callback_t *callback)
{
struct auth_policy_check_ctx *ctx;
i_assert(request->state == AUTH_REQUEST_STATE_MECH_CONTINUE);
if (request->wanted_credentials_scheme == NULL)
request->wanted_credentials_scheme =
p_strdup(request->pool, scheme);
request->user_changed_by_lookup = FALSE;
if (request->policy_processed ||
!request->set->policy_check_before_auth) {
auth_request_lookup_credentials_policy_continue(
request, callback);
} else {
ctx = p_new(request->pool, struct auth_policy_check_ctx, 1);
ctx->request = request;
ctx->callback_lookup = callback;
ctx->type = AUTH_POLICY_CHECK_TYPE_LOOKUP;
auth_policy_check(request, ctx->request->mech_password,
auth_request_policy_check_callback, ctx);
}
}
static void
auth_request_lookup_credentials_policy_continue(
struct auth_request *request, lookup_credentials_callback_t *callback)
{
struct auth_passdb *passdb;
const char *cache_key, *cache_cred, *cache_scheme, *error;
enum passdb_result result;
i_assert(request->state == AUTH_REQUEST_STATE_MECH_CONTINUE);
if (auth_request_is_disabled_master_user(request)) {
callback(PASSDB_RESULT_USER_UNKNOWN, NULL, 0, request);
return;
}
passdb = request->passdb;
while (passdb != NULL && auth_request_want_skip_passdb(request, passdb))
passdb = passdb->next;
request->passdb = passdb;
if (passdb == NULL) {
e_error(request->event, "All password databases were skipped");
callback(PASSDB_RESULT_INTERNAL_FAILURE, NULL, 0, request);
return;
}
auth_request_passdb_lookup_begin(request);
request->private_callback.lookup_credentials = callback;
cache_key = passdb_cache == NULL ? NULL : passdb->cache_key;
if (cache_key != NULL) {
if (passdb_cache_lookup_credentials(request, cache_key,
&cache_cred, &cache_scheme,
&result, FALSE)) {
request->passdb_cache_result = AUTH_REQUEST_CACHE_HIT;
passdb_handle_credentials(
result, cache_cred, cache_scheme,
auth_request_lookup_credentials_finish,
request);
return;
} else {
request->passdb_cache_result = AUTH_REQUEST_CACHE_MISS;
}
}
auth_request_set_state(request, AUTH_REQUEST_STATE_PASSDB);
if (passdb->passdb->iface.lookup_credentials == NULL) {
e_debug(authdb_event(request),
"passdb doesn't support credential lookups");
auth_request_lookup_credentials_callback(
PASSDB_RESULT_SCHEME_NOT_AVAILABLE,
uchar_empty_ptr, 0, request);
} else if (passdb->passdb->blocking) {
passdb_blocking_lookup_credentials(request);
} else if (passdb_template_export(passdb->default_fields_tmpl,
request, &error) < 0) {
e_error(authdb_event(request),
"Failed to expand default_fields: %s", error);
auth_request_lookup_credentials_callback(
PASSDB_RESULT_INTERNAL_FAILURE,
uchar_empty_ptr, 0, request);
} else {
passdb->passdb->iface.lookup_credentials(request,
auth_request_lookup_credentials_callback);
}
}
void auth_request_set_credentials(struct auth_request *request,
const char *scheme, const char *data,
set_credentials_callback_t *callback)
{
struct auth_passdb *passdb = request->passdb;
const char *cache_key, *new_credentials;
cache_key = passdb_cache == NULL ? NULL : passdb->cache_key;
if (cache_key != NULL)
auth_cache_remove(passdb_cache, request, cache_key);
request->private_callback.set_credentials = callback;
new_credentials = t_strdup_printf("{%s}%s", scheme, data);
if (passdb->passdb->blocking)
passdb_blocking_set_credentials(request, new_credentials);
else if (passdb->passdb->iface.set_credentials != NULL) {
passdb->passdb->iface.set_credentials(request, new_credentials,
callback);
} else {
callback(FALSE, request);
}
}
static void
auth_request_userdb_save_cache(struct auth_request *request,
enum userdb_result result)
{
struct auth_userdb *userdb = request->userdb;
string_t *str;
const char *cache_value;
if (passdb_cache == NULL || userdb->cache_key == NULL)
return;
if (result == USERDB_RESULT_USER_UNKNOWN)
cache_value = "";
else {
str = t_str_new(128);
auth_fields_append(request->fields.userdb_reply, str,
AUTH_FIELD_FLAG_CHANGED,
AUTH_FIELD_FLAG_CHANGED, FALSE);
if (request->user_changed_by_lookup) {
if (str_len(str) > 0)
str_append_c(str, '\t');
str_append(str, "user=");
str_append_tabescaped(str, request->fields.user);
}
if (str_len(str) == 0) {
str_append(str, AUTH_REQUEST_USER_KEY_IGNORE);
}
cache_value = str_c(str);
}
auth_cache_insert(passdb_cache, request, userdb->cache_key,
cache_value, FALSE);
}
static bool
auth_request_lookup_user_cache(struct auth_request *request, const char *key,
enum userdb_result *result_r, bool use_expired)
{
const char *value;
struct auth_cache_node *node;
bool expired, neg_expired;
value = auth_cache_lookup(passdb_cache, request, key, &node,
&expired, &neg_expired);
if (value == NULL || (expired && !use_expired)) {
request->userdb_cache_result = AUTH_REQUEST_CACHE_MISS;
e_debug(request->event,
value == NULL ? "%suserdb cache miss" :
"%suserdb cache expired",
auth_request_get_log_prefix_db(request));
return FALSE;
}
request->userdb_cache_result = AUTH_REQUEST_CACHE_HIT;
e_debug(request->event,
"%suserdb cache hit: %s",
auth_request_get_log_prefix_db(request), value);
if (*value == '\0') {
*result_r = USERDB_RESULT_USER_UNKNOWN;
auth_request_init_userdb_reply(request, FALSE);
return TRUE;
}
if (request->fields.userdb_reply == NULL)
auth_request_init_userdb_reply(request, FALSE);
auth_request_userdb_import(request, value);
*result_r = USERDB_RESULT_OK;
return TRUE;
}
void auth_request_userdb_callback(enum userdb_result result,
struct auth_request *request)
{
struct auth_userdb *userdb = request->userdb;
struct auth_userdb *next_userdb;
enum auth_db_rule result_rule;
const char *error;
bool userdb_continue = FALSE;
switch (result) {
case USERDB_RESULT_OK:
result_rule = userdb->result_success;
break;
case USERDB_RESULT_INTERNAL_FAILURE:
result_rule = userdb->result_internalfail;
break;
case USERDB_RESULT_USER_UNKNOWN:
default:
result_rule = userdb->result_failure;
break;
}
switch (result_rule) {
case AUTH_DB_RULE_RETURN:
break;
case AUTH_DB_RULE_RETURN_OK:
request->userdb_success = TRUE;
break;
case AUTH_DB_RULE_RETURN_FAIL:
request->userdb_success = FALSE;
break;
case AUTH_DB_RULE_CONTINUE:
userdb_continue = TRUE;
break;
case AUTH_DB_RULE_CONTINUE_OK:
userdb_continue = TRUE;
request->userdb_success = TRUE;
break;
case AUTH_DB_RULE_CONTINUE_FAIL:
userdb_continue = TRUE;
request->userdb_success = FALSE;
break;
}
auth_request_userdb_lookup_end(request, result);
next_userdb = userdb->next;
while (next_userdb != NULL &&
auth_request_want_skip_userdb(request, next_userdb))
next_userdb = next_userdb->next;
if (userdb_continue && next_userdb != NULL) {
if (result == USERDB_RESULT_INTERNAL_FAILURE)
request->userdbs_seen_internal_failure = TRUE;
if (result == USERDB_RESULT_OK) {
if (userdb_template_export(userdb->override_fields_tmpl,
request, &error) < 0) {
e_error(request->event,
"%sFailed to expand override_fields: %s",
auth_request_get_log_prefix_db(request),
error);
request->private_callback.userdb(
USERDB_RESULT_INTERNAL_FAILURE, request);
return;
}
auth_fields_snapshot(request->fields.userdb_reply);
} else {
auth_fields_rollback(request->fields.userdb_reply);
}
request->user_changed_by_lookup = FALSE;
request->userdb = next_userdb;
auth_request_lookup_user(request,
request->private_callback.userdb);
return;
}
if (request->userdb_success) {
if (userdb_template_export(userdb->override_fields_tmpl,
request, &error) < 0) {
e_error(request->event,
"%sFailed to expand override_fields: %s",
auth_request_get_log_prefix_db(request), error);
result = USERDB_RESULT_INTERNAL_FAILURE;
} else {
result = USERDB_RESULT_OK;
}
} else if (request->userdbs_seen_internal_failure ||
result == USERDB_RESULT_INTERNAL_FAILURE) {
result = USERDB_RESULT_INTERNAL_FAILURE;
} else if (request->client_pid != 0) {
if (auth_request_get_auth(request)->userdbs->next == NULL) {
e_error(request->event,
"%suser not found from userdb",
auth_request_get_log_prefix_db(request));
} else {
e_error(request->event,
"user not found from any userdbs");
}
result = USERDB_RESULT_USER_UNKNOWN;
} else {
result = USERDB_RESULT_USER_UNKNOWN;
}
if (request->userdb_lookup_tempfailed) {
} else if (result != USERDB_RESULT_INTERNAL_FAILURE) {
if (request->userdb_cache_result != AUTH_REQUEST_CACHE_HIT)
auth_request_userdb_save_cache(request, result);
} else if (passdb_cache != NULL && userdb->cache_key != NULL) {
const char *cache_key = userdb->cache_key;
if (auth_request_lookup_user_cache(request, cache_key,
&result, TRUE)) {
e_info(request->event,
"%sFalling back to expired data from cache",
auth_request_get_log_prefix_db(request));
}
}
request->private_callback.userdb(result, request);
}
void auth_request_lookup_user(struct auth_request *request,
userdb_callback_t *callback)
{
struct auth_userdb *userdb = request->userdb;
const char *cache_key, *error;
request->private_callback.userdb = callback;
request->user_changed_by_lookup = FALSE;
request->userdb_lookup = TRUE;
request->userdb_cache_result = AUTH_REQUEST_CACHE_NONE;
if (request->fields.userdb_reply == NULL)
auth_request_init_userdb_reply(request, TRUE);
else {
if (userdb_template_export(userdb->default_fields_tmpl,
request, &error) < 0) {
e_error(authdb_event(request),
"Failed to expand default_fields: %s", error);
auth_request_userdb_callback(
USERDB_RESULT_INTERNAL_FAILURE, request);
return;
}
}
auth_request_userdb_lookup_begin(request);
cache_key = passdb_cache == NULL ? NULL : userdb->cache_key;
if (cache_key != NULL) {
enum userdb_result result;
if (auth_request_lookup_user_cache(request, cache_key,
&result, FALSE)) {
request->userdb_cache_result = AUTH_REQUEST_CACHE_HIT;
auth_request_userdb_callback(result, request);
return;
} else {
request->userdb_cache_result = AUTH_REQUEST_CACHE_MISS;
}
}
if (userdb->userdb->iface->lookup == NULL) {
auth_request_userdb_callback(USERDB_RESULT_INTERNAL_FAILURE,
request);
} else if (userdb->userdb->blocking) {
userdb_blocking_lookup(request);
} else {
userdb->userdb->iface->lookup(
request, auth_request_userdb_callback);
}
}
static void
auth_request_validate_networks(struct auth_request *request,
const char *name, const char *networks,
const struct ip_addr *remote_ip)
{
const char *const *net;
struct ip_addr net_ip;
unsigned int bits;
bool found = FALSE;
for (net = t_strsplit_spaces(networks, ", "); *net != NULL; net++) {
e_debug(authdb_event(request),
"%s: Matching for network %s", name, *net);
if (strcmp(*net, "local") == 0) {
if (remote_ip->family == 0) {
found = TRUE;
break;
}
} else if (net_parse_range(*net, &net_ip, &bits) < 0) {
e_info(authdb_event(request),
"%s: Invalid network '%s'", name, *net);
} else if (remote_ip->family != 0 &&
net_is_in_network(remote_ip, &net_ip, bits)) {
found = TRUE;
break;
}
}
if (found)
;
else if (remote_ip->family == 0) {
e_info(authdb_event(request), "%s check failed: "
"Remote IP not known and 'local' missing", name);
} else {
e_info(authdb_event(request),
"%s check failed: IP %s not in allowed networks",
name, net_ip2addr(remote_ip));
}
if (!found)
request->failed = TRUE;
}
static void
auth_request_set_password(struct auth_request *request, const char *value,
const char *default_scheme, bool noscheme)
{
if (request->passdb_password != NULL) {
e_error(authdb_event(request),
"Multiple password values not supported");
return;
}
if (*value == '{' && !noscheme && strchr(value, '}') != NULL)
request->passdb_password = p_strdup(request->pool, value);
else {
i_assert(default_scheme != NULL);
request->passdb_password =
p_strdup_printf(request->pool, "{%s}%s",
default_scheme, value);
}
}
static const char *
get_updated_username(const char *old_username,
const char *name, const char *value)
{
const char *p;
if (strcmp(name, "user") == 0) {
return value;
}
p = strchr(old_username, '@');
if (strcmp(name, "username") == 0) {
if (strchr(value, '@') != NULL)
return value;
return t_strconcat(value, p, NULL);
}
if (strcmp(name, "domain") == 0) {
if (p == NULL) {
return t_strconcat(old_username, "@", value, NULL);
} else {
p = t_strdup_until(old_username, p + 1);
return t_strconcat(p, value, NULL);
}
}
return NULL;
}
static bool
auth_request_try_update_username(struct auth_request *request,
const char *name, const char *value)
{
const char *new_value;
new_value = get_updated_username(request->fields.user, name, value);
if (new_value == NULL)
return FALSE;
if (new_value[0] == '\0') {
e_error(authdb_event(request),
"username attempted to be changed to empty");
request->failed = TRUE;
return TRUE;
}
if (strcmp(request->fields.user, new_value) != 0) {
e_debug(authdb_event(request),
"username changed %s -> %s",
request->fields.user, new_value);
auth_request_set_username_forced(request, new_value);
request->user_changed_by_lookup = TRUE;
}
return TRUE;
}
static void
auth_request_passdb_import(struct auth_request *request, const char *args,
const char *key_prefix, const char *default_scheme)
{
const char *const *arg, *field;
for (arg = t_strsplit(args, "\t"); *arg != NULL; arg++) {
field = t_strconcat(key_prefix, *arg, NULL);
auth_request_set_field_keyvalue(request, field, default_scheme);
}
}
void auth_request_set_field(struct auth_request *request,
const char *name, const char *value,
const char *default_scheme)
{
const char *suffix;
size_t name_len = strlen(name);
i_assert(*name != '\0');
i_assert(value != NULL);
i_assert(request->passdb != NULL);
if (name_len > 10 && strcmp(name+name_len-10, ":protected") == 0) {
name = t_strndup(name, name_len-10);
if (auth_fields_exists(request->fields.extra_fields, name))
return;
} else if (name_len > 7 && strcmp(name+name_len-7, ":remove") == 0) {
name = t_strndup(name, name_len-7);
auth_fields_remove(request->fields.extra_fields, name);
return;
}
if (strcmp(name, "password") == 0) {
auth_request_set_password(request, value,
default_scheme, FALSE);
return;
}
if (strcmp(name, "password_noscheme") == 0) {
auth_request_set_password(request, value, default_scheme, TRUE);
return;
}
if (auth_request_try_update_username(request, name, value)) {
} else if (strcmp(name, "login_user") == 0) {
auth_request_set_login_username_forced(request, value);
} else if (strcmp(name, "allow_nets") == 0) {
auth_request_validate_networks(request, name, value,
&request->fields.remote_ip);
} else if (strcmp(name, "fail") == 0) {
request->failed = TRUE;
} else if (strcmp(name, "nodelay") == 0) {
request->failure_nodelay = TRUE;
auth_fields_add(request->fields.extra_fields, name, value, 0);
return;
} else if (strcmp(name, "delay_until") == 0) {
time_t timestamp;
unsigned int extra_secs = 0;
const char *p;
p = strchr(value, '+');
if (p != NULL) {
value = t_strdup_until(value, p++);
if (str_to_uint(p, &extra_secs) < 0) {
e_error(authdb_event(request),
"Invalid delay_until randomness number '%s'", p);
request->failed = TRUE;
} else {
extra_secs = i_rand_limit(extra_secs);
}
}
if (str_to_time(value, &timestamp) < 0) {
e_error(authdb_event(request),
"Invalid delay_until timestamp '%s'", value);
request->failed = TRUE;
} else if (timestamp <= ioloop_time) {
} else if (timestamp - ioloop_time > AUTH_REQUEST_MAX_DELAY_SECS) {
e_error(authdb_event(request),
"delay_until timestamp %s is too much in the future, failing", value);
request->failed = TRUE;
} else {
timestamp += extra_secs;
if (timestamp - ioloop_time > AUTH_REQUEST_MAX_DELAY_SECS)
timestamp = ioloop_time + AUTH_REQUEST_MAX_DELAY_SECS;
request->delay_until = timestamp;
}
} else if (strcmp(name, "allow_real_nets") == 0) {
auth_request_validate_networks(request, name, value,
&request->fields.real_remote_ip);
} else if (str_begins(name, "userdb_", &suffix)) {
request->userdb_prefetch_set = TRUE;
if (request->fields.userdb_reply == NULL)
auth_request_init_userdb_reply(request, TRUE);
if (strcmp(name, "userdb_userdb_import") == 0) {
auth_request_passdb_import(request, value,
"userdb_", default_scheme);
return;
}
auth_request_set_userdb_field(request, suffix, value);
} else if (strcmp(name, "noauthenticate") == 0) {
auth_fields_add(request->fields.extra_fields, name, value, 0);
auth_fields_add(request->fields.extra_fields,
"nopassword", NULL, 0);
} else if (strcmp(name, "nopassword") == 0) {
const char *password = request->passdb_password;
if (password != NULL &&
!auth_fields_exists(request->fields.extra_fields,
"noauthenticate")) {
(void)password_get_scheme(&password);
if (*password != '\0') {
e_error(authdb_event(request),
"nopassword set but password is "
"non-empty");
return;
}
}
request->passdb_password = NULL;
auth_fields_add(request->fields.extra_fields, name, value, 0);
return;
} else if (strcmp(name, "passdb_import") == 0) {
auth_request_passdb_import(request, value, "", default_scheme);
return;
} else {
auth_fields_add(request->fields.extra_fields, name, value, 0);
return;
}
auth_fields_add(request->fields.extra_fields, name, value,
AUTH_FIELD_FLAG_HIDDEN);
}
void auth_request_set_null_field(struct auth_request *request, const char *name)
{
if (str_begins_with(name, "userdb_")) {
request->userdb_prefetch_set = TRUE;
}
}
void auth_request_set_field_keyvalue(struct auth_request *request,
const char *field,
const char *default_scheme)
{
const char *key, *value;
value = strchr(field, '=');
if (value == NULL) {
key = field;
value = "";
} else {
key = t_strdup_until(field, value);
value++;
}
auth_request_set_field(request, key, value, default_scheme);
}
void auth_request_set_fields(struct auth_request *request,
const char *const *fields,
const char *default_scheme)
{
for (; *fields != NULL; fields++) {
if (**fields == '\0')
continue;
auth_request_set_field_keyvalue(request, *fields,
default_scheme);
}
}
static void
auth_request_set_uidgid_file(struct auth_request *request,
const char *path_template)
{
string_t *path;
struct stat st;
const char *error;
path = t_str_new(256);
if (auth_request_var_expand(path, path_template, request,
NULL, &error) <= 0) {
e_error(authdb_event(request),
"Failed to expand uidgid_file=%s: %s",
path_template, error);
request->userdb_lookup_tempfailed = TRUE;
} else if (stat(str_c(path), &st) < 0) {
e_error(authdb_event(request),
"stat(%s) failed: %m", str_c(path));
request->userdb_lookup_tempfailed = TRUE;
} else {
auth_fields_add(request->fields.userdb_reply,
"uid", dec2str(st.st_uid), 0);
auth_fields_add(request->fields.userdb_reply,
"gid", dec2str(st.st_gid), 0);
}
}
static void
auth_request_userdb_import(struct auth_request *request, const char *args)
{
const char *key, *value, *const *arg;
for (arg = t_strsplit(args, "\t"); *arg != NULL; arg++) {
value = strchr(*arg, '=');
if (value == NULL) {
key = *arg;
value = "";
} else {
key = t_strdup_until(*arg, value);
value++;
}
auth_request_set_userdb_field(request, key, value);
}
}
void auth_request_set_userdb_field(struct auth_request *request,
const char *name, const char *value)
{
size_t name_len = strlen(name);
uid_t uid;
gid_t gid;
i_assert(value != NULL);
if (name_len > 10 && strcmp(name+name_len-10, ":protected") == 0) {
name = t_strndup(name, name_len-10);
if (auth_fields_exists(request->fields.userdb_reply, name))
return;
} else if (name_len > 7 && strcmp(name+name_len-7, ":remove") == 0) {
name = t_strndup(name, name_len-7);
auth_fields_remove(request->fields.userdb_reply, name);
return;
}
if (strcmp(name, "uid") == 0) {
uid = userdb_parse_uid(request, value);
if (uid == (uid_t)-1) {
request->userdb_lookup_tempfailed = TRUE;
return;
}
value = dec2str(uid);
} else if (strcmp(name, "gid") == 0) {
gid = userdb_parse_gid(request, value);
if (gid == (gid_t)-1) {
request->userdb_lookup_tempfailed = TRUE;
return;
}
value = dec2str(gid);
} else if (strcmp(name, "tempfail") == 0) {
request->userdb_lookup_tempfailed = TRUE;
return;
} else if (auth_request_try_update_username(request, name, value)) {
return;
} else if (strcmp(name, "uidgid_file") == 0) {
auth_request_set_uidgid_file(request, value);
return;
} else if (strcmp(name, "userdb_import") == 0) {
auth_request_userdb_import(request, value);
return;
} else if (strcmp(name, "system_user") == 0) {
static bool warned = FALSE;
if (!warned) {
e_warning(authdb_event(request),
"Replace system_user with system_groups_user");
warned = TRUE;
}
name = "system_groups_user";
} else if (strcmp(name, AUTH_REQUEST_USER_KEY_IGNORE) == 0) {
return;
}
auth_fields_add(request->fields.userdb_reply, name, value, 0);
}
void auth_request_set_userdb_field_values(struct auth_request *request,
const char *name,
const char *const *values)
{
if (*values == NULL)
return;
if (strcmp(name, "gid") == 0) {
string_t *value;
gid_t gid;
value = t_str_new(128);
for (; *values != NULL; values++) {
gid = userdb_parse_gid(request, *values);
if (gid == (gid_t)-1) {
request->userdb_lookup_tempfailed = TRUE;
return;
}
if (str_len(value) > 0)
str_append_c(value, ',');
str_append(value, dec2str(gid));
}
auth_fields_add(request->fields.userdb_reply, name,
str_c(value), 0);
} else {
if (values[1] != NULL) {
e_warning(authdb_event(request),
"Multiple values found for '%s', "
"using value '%s'", name, *values);
}
auth_request_set_userdb_field(request, name, *values);
}
}
static bool auth_request_proxy_is_self(struct auth_request *request)
{
const char *port = NULL;
port = auth_fields_find(request->fields.extra_fields, "port");
if (port != NULL && !str_uint_equals(port, request->fields.local_port))
return FALSE;
return TRUE;
}
static bool
auth_request_proxy_ip_is_self(struct auth_request *request,
const struct ip_addr *ip)
{
unsigned int i;
if (net_ip_compare(ip, &request->fields.real_local_ip))
return TRUE;
for (i = 0; request->set->proxy_self_ips[i].family != 0; i++) {
if (net_ip_compare(ip, &request->set->proxy_self_ips[i]))
return TRUE;
}
return FALSE;
}
static void
auth_request_proxy_finish_ip(struct auth_request *request,
bool proxy_host_is_self)
{
const struct auth_request_fields *fields = &request->fields;
if (!auth_fields_exists(fields->extra_fields, "proxy_maybe")) {
} else if (!proxy_host_is_self ||
!auth_request_proxy_is_self(request)) {
auth_fields_remove(fields->extra_fields, "proxy_maybe");
auth_fields_add(fields->extra_fields, "proxy", NULL, 0);
} else {
bool proxy_always = auth_fields_exists(fields->extra_fields,
"proxy_always");
auth_request_proxy_finish_failure(request);
if (proxy_always) {
auth_fields_add(request->fields.extra_fields,
"proxy", NULL, 0);
}
}
}
static void
auth_request_proxy_dns_callback(const struct dns_lookup_result *result,
struct auth_request_proxy_dns_lookup_ctx *ctx)
{
struct auth_request *request = ctx->request;
const char *host;
unsigned int i;
bool proxy_host_is_self;
request->dns_lookup_ctx = NULL;
ctx->dns_lookup = NULL;
host = auth_fields_find(request->fields.extra_fields, "host");
i_assert(host != NULL);
if (result->ret != 0) {
e_error(ctx->event, "DNS lookup for %s failed: %s",
host, result->error);
request->internal_failure = TRUE;
auth_request_proxy_finish_failure(request);
} else {
if (result->msecs > AUTH_DNS_WARN_MSECS) {
e_warning(ctx->event,
"DNS lookup for %s took %u.%03u s",
host, result->msecs/1000,
result->msecs % 1000);
}
auth_fields_add(request->fields.extra_fields, "hostip",
net_ip2addr(&result->ips[0]), 0);
proxy_host_is_self = FALSE;
for (i = 0; i < result->ips_count; i++) {
if (auth_request_proxy_ip_is_self(request,
&result->ips[i])) {
proxy_host_is_self = TRUE;
break;
}
}
auth_request_proxy_finish_ip(request, proxy_host_is_self);
}
if (ctx->callback != NULL)
ctx->callback(result->ret == 0, request);
event_unref(&ctx->event);
auth_request_unref(&request);
}
static int
auth_request_proxy_host_lookup(struct auth_request *request,
const char *host,
auth_request_proxy_cb_t *callback)
{
struct auth *auth = auth_default_service();
struct event *proxy_event;
struct auth_request_proxy_dns_lookup_ctx *ctx;
const char *value;
unsigned int secs;
proxy_event = event_create(request->event);
event_set_append_log_prefix(proxy_event, "proxy: ");
value = auth_fields_find(request->fields.extra_fields, "proxy_timeout");
if (value != NULL) {
if (str_to_uint(value, &secs) < 0) {
e_error(proxy_event, "Invalid proxy_timeout value: %s",
value);
}
}
ctx = p_new(request->pool, struct auth_request_proxy_dns_lookup_ctx, 1);
ctx->request = request;
ctx->event = proxy_event;
auth_request_ref(request);
request->dns_lookup_ctx = ctx;
if (dns_client_lookup(auth->dns_client, host, proxy_event,
auth_request_proxy_dns_callback, ctx,
&ctx->dns_lookup) < 0) {
return -1;
}
ctx->callback = callback;
return 0;
}
int auth_request_proxy_finish(struct auth_request *request,
auth_request_proxy_cb_t *callback)
{
const char *host, *hostip;
struct ip_addr ip;
bool proxy_host_is_self;
if (request->auth_only)
return 1;
if (!auth_fields_exists(request->fields.extra_fields, "proxy") &&
!auth_fields_exists(request->fields.extra_fields, "proxy_maybe"))
return 1;
host = auth_fields_find(request->fields.extra_fields, "host");
if (host == NULL) {
proxy_host_is_self = FALSE;
if (request->fields.local_ip.family != 0) {
auth_fields_add(request->fields.extra_fields, "lip",
net_ip2addr(&request->fields.local_ip), 0);
}
if (request->fields.local_port != 0) {
auth_fields_add(request->fields.extra_fields, "lport",
dec2str(request->fields.local_port), 0);
}
} else if (net_addr2ip(host, &ip) == 0) {
proxy_host_is_self =
auth_request_proxy_ip_is_self(request, &ip);
} else {
hostip = auth_fields_find(request->fields.extra_fields,
"hostip");
if (hostip != NULL && net_addr2ip(hostip, &ip) < 0) {
e_error(request->event, "proxy: "
"Invalid hostip in passdb: %s", hostip);
return -1;
}
if (hostip == NULL) {
return auth_request_proxy_host_lookup(request, host,
callback);
}
proxy_host_is_self =
auth_request_proxy_ip_is_self(request, &ip);
}
auth_request_proxy_finish_ip(request, proxy_host_is_self);
return 1;
}
void auth_request_proxy_finish_failure(struct auth_request *request)
{
auth_fields_remove(request->fields.extra_fields, "proxy");
auth_fields_remove(request->fields.extra_fields, "proxy_maybe");
auth_fields_remove(request->fields.extra_fields, "proxy_always");
auth_fields_remove(request->fields.extra_fields, "host");
auth_fields_remove(request->fields.extra_fields, "port");
auth_fields_remove(request->fields.extra_fields, "destuser");
}
static void
log_password_failure(struct event *event, const char *plain_password,
const char *crypted_password, const char *scheme,
const struct password_generate_params *params)
{
static bool scheme_ok = FALSE;
string_t *str = t_str_new(256);
const char *working_scheme;
str_printfa(str, "%s(%s) != '%s'", scheme,
plain_password, crypted_password);
if (!scheme_ok) {
working_scheme = password_scheme_detect(
plain_password, crypted_password, params);
if (working_scheme != NULL) {
str_printfa(str, ", try %s scheme instead",
working_scheme);
}
}
e_debug(event, "%s", str_c(str));
}
static void
auth_request_append_password(struct auth_request *request, string_t *str)
{
const char *p, *log_type = request->set->verbose_passwords;
unsigned int max_len = 1024;
if (request->mech_password == NULL)
return;
p = strchr(log_type, ':');
if (p != NULL) {
if (str_to_uint(p+1, &max_len) < 0)
i_unreached();
log_type = t_strdup_until(log_type, p);
}
if (strcmp(log_type, "plain") == 0) {
str_printfa(str, "(given password: %s)",
t_strndup(request->mech_password, max_len));
} else if (strcmp(log_type, "sha1") == 0) {
unsigned char sha1[SHA1_RESULTLEN];
sha1_get_digest(request->mech_password,
strlen(request->mech_password), sha1);
str_printfa(str, "(SHA1 of given password: %s)",
t_strndup(binary_to_hex(sha1, sizeof(sha1)),
max_len));
} else {
i_unreached();
}
}
void auth_request_log_password_mismatch(struct auth_request *request,
struct event *event)
{
auth_request_log_login_failure(request, event,
AUTH_LOG_MSG_PASSWORD_MISMATCH);
}
void auth_request_log_unknown_user(struct auth_request *request,
struct event *event)
{
auth_request_log_login_failure(request, event, "unknown user");
}
void auth_request_log_login_failure(struct auth_request *request,
struct event *event, const char *message)
{
string_t *str;
if (strcmp(request->set->verbose_passwords, "no") == 0) {
e_info(event, "%s", message);
return;
}
enum log_type orig_level = event_get_min_log_level(event);
event_set_min_log_level(event, LOG_TYPE_INFO);
str = t_str_new(128);
str_append(str, message);
str_append(str, " ");
auth_request_append_password(request, str);
if (request->userdb_lookup) {
if (request->userdb->next != NULL)
str_append(str, " - trying the next userdb");
} else {
if (request->passdb->next != NULL)
str_append(str, " - trying the next passdb");
}
e_info(event, "%s", str_c(str));
event_set_min_log_level(event, orig_level);
}
void auth_request_db_log_password_mismatch(struct auth_request *request)
{
auth_request_log_login_failure(request, authdb_event(request),
AUTH_LOG_MSG_PASSWORD_MISMATCH);
}
void auth_request_db_log_unknown_user(struct auth_request *request)
{
auth_request_log_login_failure(request, authdb_event(request),
"unknown user");
}
void auth_request_db_log_login_failure(struct auth_request *request,
const char *message)
{
auth_request_log_login_failure(request, authdb_event(request),
message);
}
enum passdb_result
auth_request_password_verify(struct auth_request *request,
struct event *event,
const char *plain_password,
const char *crypted_password,
const char *scheme)
{
return auth_request_password_verify_log(request, event, plain_password,
crypted_password, scheme, TRUE);
}
enum passdb_result
auth_request_password_verify_log(struct auth_request *request,
struct event *event,
const char *plain_password,
const char *crypted_password,
const char *scheme,
bool log_password_mismatch)
{
enum passdb_result result;
const unsigned char *raw_password;
size_t raw_password_size;
const char *error;
int ret;
struct password_generate_params gen_params = {
.user = request->fields.original_username,
.rounds = 0
};
if (request->fields.skip_password_check) {
return PASSDB_RESULT_OK;
}
if (request->passdb->set->deny) {
return PASSDB_RESULT_PASSWORD_MISMATCH;
}
if (auth_fields_exists(request->fields.extra_fields, "nopassword")) {
e_debug(event, "Allowing any password");
return PASSDB_RESULT_OK;
}
ret = password_decode(crypted_password, scheme,
&raw_password, &raw_password_size, &error);
if (ret <= 0) {
if (ret < 0) {
e_error(event,
"Password data is not valid for scheme %s: %s",
scheme, error);
} else {
e_error(event, "Unknown scheme %s", scheme);
return PASSDB_RESULT_SCHEME_NOT_AVAILABLE;
}
return PASSDB_RESULT_INTERNAL_FAILURE;
}
ret = password_verify(plain_password, &gen_params,
scheme, raw_password, raw_password_size, &error);
if (ret < 0) {
const char *password_str = request->set->debug_passwords ?
t_strdup_printf(" '%s'", crypted_password) : "";
e_error(event, "Invalid password%s in passdb: %s",
password_str, error);
result = PASSDB_RESULT_INTERNAL_FAILURE;
} else if (ret == 0) {
if (log_password_mismatch)
auth_request_log_password_mismatch(request, event);
result = PASSDB_RESULT_PASSWORD_MISMATCH;
} else {
result = PASSDB_RESULT_OK;
}
if (ret <= 0 && request->set->debug_passwords) T_BEGIN {
log_password_failure(event, plain_password, crypted_password,
scheme, &gen_params);
} T_END;
return result;
}
enum passdb_result
auth_request_db_password_verify(struct auth_request *request,
const char *plain_password,
const char *crypted_password,
const char *scheme)
{
return auth_request_password_verify_log(
request, authdb_event(request),
plain_password, crypted_password, scheme, TRUE);
}
enum passdb_result
auth_request_db_password_verify_log(struct auth_request *request,
const char *plain_password,
const char *crypted_password,
const char *scheme,
bool log_password_mismatch)
{
return auth_request_password_verify_log(
request, authdb_event(request),
plain_password, crypted_password, scheme,
log_password_mismatch);
}
enum passdb_result auth_request_password_missing(struct auth_request *request)
{
if (request->fields.skip_password_check) {
return PASSDB_RESULT_OK;
}
e_info(authdb_event(request),
"No password returned (and no nopassword)");
return PASSDB_RESULT_PASSWORD_MISMATCH;
}
void auth_request_refresh_last_access(struct auth_request *request)
{
request->last_access = ioloop_time;
if (request->to_abort != NULL)
timeout_reset(request->to_abort);
}