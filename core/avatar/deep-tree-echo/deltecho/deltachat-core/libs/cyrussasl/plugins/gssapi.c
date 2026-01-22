#include <config.h>
#ifdef HAVE_GSSAPI_H
#include <gssapi.h>
#else
#include <gssapi/gssapi.h>
#endif
#ifdef WIN32
#  include <winsock2.h>
#  ifndef R_OK
#    define R_OK 04
#  endif
#  include <io.h>
#else
#  include <sys/param.h>
#  include <sys/socket.h>
#  include <netinet/in.h>
#  include <arpa/inet.h>
#  include <netdb.h>
#endif
#include <fcntl.h>
#include <stdio.h>
#include <sasl.h>
#include <saslutil.h>
#include <saslplug.h>
#include "plugin_common.h"
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <errno.h>
#include <assert.h>
static const char plugin_id[] = "$Id: gssapi.c,v 1.115 2011/11/21 15:12:35 mel Exp $";
static const char * GSSAPI_BLANK_STRING = "";
static gss_OID_desc gss_spnego_oid = { 6, (void *) "\x2b\x06\x01\x05\x05\x02" };
#if !defined(HAVE_GSS_C_NT_HOSTBASED_SERVICE) && !defined(GSS_C_NT_HOSTBASED_SERVICE)
extern gss_OID gss_nt_service_name;
#define GSS_C_NT_HOSTBASED_SERVICE gss_nt_service_name
#endif
#ifdef WANT_KERBEROS5_3DES
#ifdef CSF_GSS_C_DES3_FLAG
#define K5_MAX_SSF	112
#endif
#ifdef GSS_KRB5_CONF_C_QOP_DES3_KD
#define K5_MAX_SSF	112
#endif
#endif
#ifndef K5_MAX_SSF
#define K5_MAX_SSF	56
#endif
#ifdef GSS_USE_MUTEXES
#define GSS_LOCK_MUTEX(utils)  \
if(((sasl_utils_t *)(utils))->mutex_lock(gss_mutex) != 0) { \
return SASL_FAIL; \
}
#define GSS_UNLOCK_MUTEX(utils) \
if(((sasl_utils_t *)(utils))->mutex_unlock(gss_mutex) != 0) { \
return SASL_FAIL; \
}
static void *gss_mutex = NULL;
#else
#define GSS_LOCK_MUTEX(utils)
#define GSS_UNLOCK_MUTEX(utils)
#endif
typedef struct context {
int state;
gss_OID mech_type;
int http_mode;
gss_ctx_id_t gss_ctx;
gss_name_t   client_name;
gss_name_t   server_name;
gss_cred_id_t server_creds;
gss_cred_id_t client_creds;
sasl_ssf_t limitssf, requiressf;
unsigned char qop;
const sasl_utils_t *utils;
decode_context_t decode_context;
char *encode_buf;
char *decode_buf;
char *decode_once_buf;
unsigned encode_buf_len;
unsigned decode_buf_len;
unsigned decode_once_buf_len;
buffer_info_t *enc_in_buf;
char *out_buf;
unsigned out_buf_len;
char *authid;
const char *user;
} context_t;
enum {
SASL_GSSAPI_STATE_AUTHNEG = 1,
SASL_GSSAPI_STATE_SSFCAP = 2,
SASL_GSSAPI_STATE_SSFREQ = 3,
SASL_GSSAPI_STATE_AUTHENTICATED = 4
};
#define LAYER_CONFIDENTIALITY	4
#define LAYER_INTEGRITY		2
#define LAYER_NONE		1
#define sasl_gss_log(x,y,z) sasl_gss_seterror_(x,y,z,1)
#define sasl_gss_seterror(x,y,z) sasl_gss_seterror_(x,y,z,0)
static int
sasl_gss_seterror_(const sasl_utils_t *utils, OM_uint32 maj, OM_uint32 min,
int logonly)
{
OM_uint32 maj_stat, min_stat;
gss_buffer_desc msg;
OM_uint32 msg_ctx;
int ret;
char *out = NULL;
size_t len, curlen = 0;
const char prefix[] = "GSSAPI Error: ";
if (!utils) return SASL_OK;
len = sizeof(prefix);
ret = _plug_buf_alloc(utils, &out, &curlen, 256);
if (ret != SASL_OK) return SASL_NOMEM;
strcpy(out, prefix);
msg_ctx = 0;
while (1) {
GSS_LOCK_MUTEX(utils);
maj_stat = gss_display_status(&min_stat, maj,
GSS_C_GSS_CODE, GSS_C_NULL_OID,
&msg_ctx, &msg);
GSS_UNLOCK_MUTEX(utils);
if(GSS_ERROR(maj_stat)) {
if (logonly) {
utils->log(utils->conn, SASL_LOG_FAIL,
"GSSAPI Failure: (could not get major error message)");
} else {
utils->seterror(utils->conn, 0,
"GSSAPI Failure "
"(could not get major error message)");
}
utils->free(out);
return SASL_OK;
}
len += len + msg.length;
ret = _plug_buf_alloc(utils, &out, &curlen, len);
if(ret != SASL_OK) {
utils->free(out);
return SASL_NOMEM;
}
strcat(out, msg.value);
GSS_LOCK_MUTEX(utils);
gss_release_buffer(&min_stat, &msg);
GSS_UNLOCK_MUTEX(utils);
if (!msg_ctx)
break;
}
len += 2;
ret = _plug_buf_alloc(utils, &out, &curlen, len);
if(ret != SASL_OK) {
utils->free(out);
return SASL_NOMEM;
}
strcat(out, " (");
msg_ctx = 0;
while (1) {
GSS_LOCK_MUTEX(utils);
maj_stat = gss_display_status(&min_stat, min,
GSS_C_MECH_CODE, GSS_C_NULL_OID,
&msg_ctx, &msg);
GSS_UNLOCK_MUTEX(utils);
if(GSS_ERROR(maj_stat)) {
if (logonly) {
utils->log(utils->conn, SASL_LOG_FAIL,
"GSSAPI Failure: (could not get minor error message)");
} else {
utils->seterror(utils->conn, 0,
"GSSAPI Failure "
"(could not get minor error message)");
}
utils->free(out);
return SASL_OK;
}
len += len + msg.length;
ret = _plug_buf_alloc(utils, &out, &curlen, len);
if(ret != SASL_OK) {
utils->free(out);
return SASL_NOMEM;
}
strcat(out, msg.value);
GSS_LOCK_MUTEX(utils);
gss_release_buffer(&min_stat, &msg);
GSS_UNLOCK_MUTEX(utils);
if (!msg_ctx)
break;
}
len += 1;
ret = _plug_buf_alloc(utils, &out, &curlen, len);
if(ret != SASL_OK) {
utils->free(out);
return SASL_NOMEM;
}
strcat(out, ")");
if (logonly) {
utils->log(utils->conn, SASL_LOG_FAIL, out);
} else {
utils->seterror(utils->conn, 0, out);
}
utils->free(out);
return SASL_OK;
}
static int
sasl_gss_encode(void *context, const struct iovec *invec, unsigned numiov,
const char **output, unsigned *outputlen, int privacy)
{
context_t *text = (context_t *)context;
OM_uint32 maj_stat, min_stat;
gss_buffer_t input_token, output_token;
gss_buffer_desc real_input_token, real_output_token;
int ret;
struct buffer_info *inblob, bufinfo;
if (!output) return SASL_BADPARAM;
if (numiov > 1) {
ret = _plug_iovec_to_buf(text->utils, invec, numiov, &text->enc_in_buf);
if (ret != SASL_OK) return ret;
inblob = text->enc_in_buf;
} else {
bufinfo.data = invec[0].iov_base;
bufinfo.curlen = invec[0].iov_len;
inblob = &bufinfo;
}
if (text->state != SASL_GSSAPI_STATE_AUTHENTICATED) return SASL_NOTDONE;
input_token = &real_input_token;
real_input_token.value  = inblob->data;
real_input_token.length = inblob->curlen;
output_token = &real_output_token;
output_token->value = NULL;
output_token->length = 0;
GSS_LOCK_MUTEX(text->utils);
maj_stat = gss_wrap (&min_stat,
text->gss_ctx,
privacy,
GSS_C_QOP_DEFAULT,
input_token,
NULL,
output_token);
GSS_UNLOCK_MUTEX(text->utils);
if (GSS_ERROR(maj_stat)) {
sasl_gss_seterror(text->utils, maj_stat, min_stat);
if (output_token->value) {
GSS_LOCK_MUTEX(text->utils);
gss_release_buffer(&min_stat, output_token);
GSS_UNLOCK_MUTEX(text->utils);
}
return SASL_FAIL;
}
if (output_token->value && output) {
unsigned char * p;
ret = _plug_buf_alloc(text->utils,
&(text->encode_buf),
&(text->encode_buf_len),
output_token->length + 4);
if (ret != SASL_OK) {
GSS_LOCK_MUTEX(text->utils);
gss_release_buffer(&min_stat, output_token);
GSS_UNLOCK_MUTEX(text->utils);
return ret;
}
p = (unsigned char *) text->encode_buf;
p[0] = (output_token->length>>24) & 0xFF;
p[1] = (output_token->length>>16) & 0xFF;
p[2] = (output_token->length>>8) & 0xFF;
p[3] = output_token->length & 0xFF;
memcpy(text->encode_buf + 4, output_token->value, output_token->length);
}
if (outputlen) {
*outputlen = output_token->length + 4;
}
*output = text->encode_buf;
if (output_token->value) {
GSS_LOCK_MUTEX(text->utils);
gss_release_buffer(&min_stat, output_token);
GSS_UNLOCK_MUTEX(text->utils);
}
return SASL_OK;
}
static int gssapi_privacy_encode(void *context, const struct iovec *invec,
unsigned numiov, const char **output,
unsigned *outputlen)
{
return sasl_gss_encode(context,invec,numiov,output,outputlen,1);
}
static int gssapi_integrity_encode(void *context, const struct iovec *invec,
unsigned numiov, const char **output,
unsigned *outputlen)
{
return sasl_gss_encode(context,invec,numiov,output,outputlen,0);
}
static int
gssapi_decode_packet(void *context,
const char *input,
unsigned inputlen,
char **output,
unsigned *outputlen)
{
context_t *text = (context_t *) context;
OM_uint32 maj_stat, min_stat;
gss_buffer_t input_token, output_token;
gss_buffer_desc real_input_token, real_output_token;
int result;
if (text->state != SASL_GSSAPI_STATE_AUTHENTICATED) {
SETERROR(text->utils, "GSSAPI Failure");
return SASL_NOTDONE;
}
input_token = &real_input_token;
real_input_token.value = (char *) input;
real_input_token.length = inputlen;
output_token = &real_output_token;
output_token->value = NULL;
output_token->length = 0;
GSS_LOCK_MUTEX(text->utils);
maj_stat = gss_unwrap (&min_stat,
text->gss_ctx,
input_token,
output_token,
NULL,
NULL);
GSS_UNLOCK_MUTEX(text->utils);
if (GSS_ERROR(maj_stat)) {
sasl_gss_seterror(text->utils,maj_stat,min_stat);
if (output_token->value) {
GSS_LOCK_MUTEX(text->utils);
gss_release_buffer(&min_stat, output_token);
GSS_UNLOCK_MUTEX(text->utils);
}
return SASL_FAIL;
}
if (outputlen) {
*outputlen = output_token->length;
}
if (output_token->value) {
if (output) {
result = _plug_buf_alloc(text->utils, &text->decode_once_buf,
&text->decode_once_buf_len,
*outputlen);
if (result != SASL_OK) {
GSS_LOCK_MUTEX(text->utils);
gss_release_buffer(&min_stat, output_token);
GSS_UNLOCK_MUTEX(text->utils);
return result;
}
*output = text->decode_once_buf;
memcpy(*output, output_token->value, *outputlen);
}
GSS_LOCK_MUTEX(text->utils);
gss_release_buffer(&min_stat, output_token);
GSS_UNLOCK_MUTEX(text->utils);
}
return SASL_OK;
}
static int gssapi_decode(void *context,
const char *input, unsigned inputlen,
const char **output, unsigned *outputlen)
{
context_t *text = (context_t *) context;
int ret;
ret = _plug_decode(&text->decode_context, input, inputlen,
&text->decode_buf, &text->decode_buf_len, outputlen,
gssapi_decode_packet, text);
*output = text->decode_buf;
return ret;
}
static context_t *sasl_gss_new_context(const sasl_utils_t *utils)
{
context_t *ret;
ret = utils->malloc(sizeof(context_t));
if(!ret) return NULL;
memset(ret,0,sizeof(context_t));
ret->utils = utils;
return ret;
}
static int sasl_gss_free_context_contents(context_t *text)
{
OM_uint32 maj_stat, min_stat;
if (!text) return SASL_OK;
GSS_LOCK_MUTEX(text->utils);
if (text->gss_ctx != GSS_C_NO_CONTEXT) {
maj_stat = gss_delete_sec_context(&min_stat,&text->gss_ctx,
GSS_C_NO_BUFFER);
text->gss_ctx = GSS_C_NO_CONTEXT;
}
if (text->client_name != GSS_C_NO_NAME) {
maj_stat = gss_release_name(&min_stat,&text->client_name);
text->client_name = GSS_C_NO_NAME;
}
if (text->server_name != GSS_C_NO_NAME) {
maj_stat = gss_release_name(&min_stat,&text->server_name);
text->server_name = GSS_C_NO_NAME;
}
if ( text->server_creds != GSS_C_NO_CREDENTIAL) {
maj_stat = gss_release_cred(&min_stat, &text->server_creds);
text->server_creds = GSS_C_NO_CREDENTIAL;
}
if ( text->client_creds != GSS_C_NO_CREDENTIAL) {
maj_stat = gss_release_cred(&min_stat, &text->client_creds);
text->client_creds = GSS_C_NO_CREDENTIAL;
}
GSS_UNLOCK_MUTEX(text->utils);
if (text->out_buf) {
text->utils->free(text->out_buf);
text->out_buf = NULL;
}
if (text->encode_buf) {
text->utils->free(text->encode_buf);
text->encode_buf = NULL;
}
if (text->decode_buf) {
text->utils->free(text->decode_buf);
text->decode_buf = NULL;
}
if (text->decode_once_buf) {
text->utils->free(text->decode_once_buf);
text->decode_once_buf = NULL;
}
if (text->enc_in_buf) {
if(text->enc_in_buf->data) text->utils->free(text->enc_in_buf->data);
text->utils->free(text->enc_in_buf);
text->enc_in_buf = NULL;
}
_plug_decode_free(&text->decode_context);
if (text->authid) {
text->utils->free(text->authid);
text->authid = NULL;
}
return SASL_OK;
}
static void gssapi_common_mech_dispose(void *conn_context,
const sasl_utils_t *utils)
{
sasl_gss_free_context_contents((context_t *)(conn_context));
utils->free(conn_context);
}
static void gssapi_common_mech_free(void *global_context __attribute__((unused)),
const sasl_utils_t *utils)
{
#ifdef GSS_USE_MUTEXES
if (gss_mutex) {
utils->mutex_free(gss_mutex);
gss_mutex=NULL;
}
#endif
}
static int
gssapi_server_mech_new(void *glob_context __attribute__((unused)),
sasl_server_params_t *params,
const char *challenge __attribute__((unused)),
unsigned challen __attribute__((unused)),
void **conn_context)
{
context_t *text;
text = sasl_gss_new_context(params->utils);
if (text == NULL) {
MEMERROR(params->utils);
return SASL_NOMEM;
}
text->gss_ctx = GSS_C_NO_CONTEXT;
text->client_name = GSS_C_NO_NAME;
text->server_name = GSS_C_NO_NAME;
text->server_creds = GSS_C_NO_CREDENTIAL;
text->client_creds = GSS_C_NO_CREDENTIAL;
text->state = SASL_GSSAPI_STATE_AUTHNEG;
text->http_mode = (params->flags & SASL_NEED_HTTP);
*conn_context = text;
return SASL_OK;
}
static int
gssapi_server_mech_authneg(context_t *text,
sasl_server_params_t *params,
const char *clientin,
unsigned clientinlen,
const char **serverout,
unsigned *serveroutlen,
sasl_out_params_t *oparams __attribute__((unused)))
{
gss_buffer_t input_token, output_token;
gss_buffer_desc real_input_token, real_output_token;
OM_uint32 maj_stat = 0, min_stat = 0;
gss_buffer_desc name_token;
int ret, equal = 0;
unsigned out_flags = 0;
gss_cred_id_t server_creds = (gss_cred_id_t) params->gss_creds;
gss_buffer_desc name_without_realm;
gss_name_t client_name_MN = NULL, without = NULL;
gss_OID mech_type;
input_token = &real_input_token;
output_token = &real_output_token;
output_token->value = NULL; output_token->length = 0;
input_token->value = NULL; input_token->length = 0;
if (text->server_name == GSS_C_NO_NAME) {
if (params->serverFQDN == NULL
|| strlen(params->serverFQDN) == 0) {
SETERROR(text->utils, "GSSAPI Failure: no serverFQDN");
sasl_gss_free_context_contents(text);
return SASL_FAIL;
}
name_token.length = strlen(params->service) + 1 + strlen(params->serverFQDN);
name_token.value = (char *)params->utils->malloc((name_token.length + 1) * sizeof(char));
if (name_token.value == NULL) {
MEMERROR(text->utils);
sasl_gss_free_context_contents(text);
return SASL_NOMEM;
}
sprintf(name_token.value,"%s@%s", params->service, params->serverFQDN);
GSS_LOCK_MUTEX(params->utils);
maj_stat = gss_import_name (&min_stat,
&name_token,
GSS_C_NT_HOSTBASED_SERVICE,
&text->server_name);
GSS_UNLOCK_MUTEX(params->utils);
params->utils->free(name_token.value);
name_token.value = NULL;
if (GSS_ERROR(maj_stat)) {
sasl_gss_seterror(text->utils, maj_stat, min_stat);
sasl_gss_free_context_contents(text);
return SASL_FAIL;
}
if ( text->server_creds != GSS_C_NO_CREDENTIAL) {
GSS_LOCK_MUTEX(params->utils);
maj_stat = gss_release_cred(&min_stat, &text->server_creds);
GSS_UNLOCK_MUTEX(params->utils);
text->server_creds = GSS_C_NO_CREDENTIAL;
}
if ( server_creds == GSS_C_NO_CREDENTIAL) {
GSS_LOCK_MUTEX(params->utils);
maj_stat = gss_acquire_cred(&min_stat,
text->server_name,
GSS_C_INDEFINITE,
GSS_C_NO_OID_SET,
GSS_C_ACCEPT,
&text->server_creds,
NULL,
NULL);
GSS_UNLOCK_MUTEX(params->utils);
if (GSS_ERROR(maj_stat)) {
sasl_gss_seterror(text->utils, maj_stat, min_stat);
sasl_gss_free_context_contents(text);
return SASL_FAIL;
}
server_creds = text->server_creds;
}
}
if (clientinlen) {
real_input_token.value = (void *)clientin;
real_input_token.length = clientinlen;
}
GSS_LOCK_MUTEX(params->utils);
maj_stat =
gss_accept_sec_context(&min_stat,
&(text->gss_ctx),
server_creds,
input_token,
GSS_C_NO_CHANNEL_BINDINGS,
&text->client_name,
&mech_type,
output_token,
&out_flags,
NULL,
&(text->client_creds));
GSS_UNLOCK_MUTEX(params->utils);
if (GSS_ERROR(maj_stat)) {
sasl_gss_log(text->utils, maj_stat, min_stat);
text->utils->seterror(text->utils->conn, SASL_NOLOG, "GSSAPI Failure: gss_accept_sec_context");
if (output_token->value) {
GSS_LOCK_MUTEX(params->utils);
gss_release_buffer(&min_stat, output_token);
GSS_UNLOCK_MUTEX(params->utils);
}
sasl_gss_free_context_contents(text);
return SASL_BADAUTH;
}
if (serveroutlen) {
*serveroutlen = output_token->length;
}
if (output_token->value) {
if (serverout) {
ret = _plug_buf_alloc(text->utils, &(text->out_buf),
&(text->out_buf_len), *serveroutlen);
if(ret != SASL_OK) {
GSS_LOCK_MUTEX(params->utils);
gss_release_buffer(&min_stat, output_token);
GSS_UNLOCK_MUTEX(params->utils);
return ret;
}
memcpy(text->out_buf, output_token->value, *serveroutlen);
*serverout = text->out_buf;
}
GSS_LOCK_MUTEX(params->utils);
gss_release_buffer(&min_stat, output_token);
GSS_UNLOCK_MUTEX(params->utils);
} else {
*serverout = GSSAPI_BLANK_STRING;
*serveroutlen = 0;
}
if (maj_stat == GSS_S_CONTINUE_NEEDED) {
return SASL_CONTINUE;
}
assert(maj_stat == GSS_S_COMPLETE);
if ((out_flags & GSS_C_INTEG_FLAG) == 0) {
text->qop = LAYER_NONE;
} else if ((out_flags & GSS_C_CONF_FLAG) == 0) {
text->qop = LAYER_NONE | LAYER_INTEGRITY;
} else {
text->qop = LAYER_NONE | LAYER_INTEGRITY | LAYER_CONFIDENTIALITY;
}
if ((params->props.security_flags & SASL_SEC_PASS_CREDENTIALS) &&
(!(out_flags & GSS_C_DELEG_FLAG) ||
text->client_creds == GSS_C_NO_CREDENTIAL) )
{
text->utils->seterror(text->utils->conn, SASL_LOG_WARN,
"GSSAPI warning: no credentials were passed");
}
GSS_LOCK_MUTEX(params->utils);
maj_stat = gss_canonicalize_name(&min_stat,
text->client_name,
mech_type,
&client_name_MN);
GSS_UNLOCK_MUTEX(params->utils);
if (GSS_ERROR(maj_stat)) {
SETERROR(text->utils, "GSSAPI Failure: gss_canonicalize_name");
sasl_gss_free_context_contents(text);
return SASL_BADAUTH;
}
name_token.value = NULL;
name_without_realm.value = NULL;
GSS_LOCK_MUTEX(params->utils);
maj_stat = gss_display_name (&min_stat,
client_name_MN,
&name_token,
NULL);
GSS_UNLOCK_MUTEX(params->utils);
if (GSS_ERROR(maj_stat)) {
SETERROR(text->utils, "GSSAPI Failure: gss_display_name");
sasl_gss_free_context_contents(text);
ret = SASL_BADAUTH;
goto cleanup;
}
if (strchr((char *) name_token.value, (int) '@') != NULL) {
name_without_realm.value = params->utils->malloc(strlen(name_token.value)+1);
if (name_without_realm.value == NULL) {
MEMERROR(text->utils);
ret = SASL_NOMEM;
goto cleanup;
}
strcpy(name_without_realm.value, name_token.value);
(strchr(name_without_realm.value,'@'))[0] = '\0';
name_without_realm.length = strlen( (char *) name_without_realm.value );
GSS_LOCK_MUTEX(params->utils);
maj_stat = gss_import_name (&min_stat,
&name_without_realm,
#ifdef HAVE_GSS_C_NT_USER_NAME
GSS_C_NT_USER_NAME,
#else
GSS_C_NULL_OID,
#endif
&without);
GSS_UNLOCK_MUTEX(params->utils);
if (GSS_ERROR(maj_stat)) {
SETERROR(text->utils, "GSSAPI Failure: gss_import_name");
sasl_gss_free_context_contents(text);
ret = SASL_BADAUTH;
goto cleanup;
}
GSS_LOCK_MUTEX(params->utils);
maj_stat = gss_compare_name(&min_stat,
client_name_MN,
without,
&equal);
GSS_UNLOCK_MUTEX(params->utils);
if (GSS_ERROR(maj_stat)) {
SETERROR(text->utils, "GSSAPI Failure: gss_compare_name");
sasl_gss_free_context_contents(text);
ret = SASL_BADAUTH;
goto cleanup;
}
} else {
equal = 0;
}
if (equal) {
text->authid = strdup(name_without_realm.value);
} else {
text->authid = strdup(name_token.value);
}
if (text->authid == NULL) {
MEMERROR(params->utils);
ret = SASL_NOMEM;
goto cleanup;
}
if (text->http_mode) {
text->state = SASL_GSSAPI_STATE_AUTHENTICATED;
ret = SASL_OK;
}
else {
text->state = SASL_GSSAPI_STATE_SSFCAP;
ret = SASL_CONTINUE;
}
cleanup:
if (client_name_MN) {
GSS_LOCK_MUTEX(params->utils);
gss_release_name(&min_stat, &client_name_MN);
GSS_UNLOCK_MUTEX(params->utils);
}
if (name_token.value) {
GSS_LOCK_MUTEX(params->utils);
gss_release_buffer(&min_stat, &name_token);
GSS_UNLOCK_MUTEX(params->utils);
}
if (name_without_realm.value) {
params->utils->free(name_without_realm.value);
}
if (without) {
GSS_LOCK_MUTEX(params->utils);
gss_release_name(&min_stat, &without);
GSS_UNLOCK_MUTEX(params->utils);
}
return ret;
}
static int
gssapi_server_mech_ssfcap(context_t *text,
sasl_server_params_t *params,
const char *clientin __attribute__((unused)),
unsigned clientinlen,
const char **serverout,
unsigned *serveroutlen,
sasl_out_params_t *oparams __attribute__((unused)))
{
gss_buffer_t input_token, output_token;
gss_buffer_desc real_input_token, real_output_token;
OM_uint32 maj_stat = 0, min_stat = 0;
unsigned char sasldata[4];
int ret;
input_token = &real_input_token;
output_token = &real_output_token;
output_token->value = NULL; output_token->length = 0;
if (clientinlen != 0) {
SETERROR(text->utils, "GSSAPI server is not expecting data at this stage");
sasl_gss_free_context_contents(text);
return SASL_BADAUTH;
}
if (params->props.max_ssf < params->external_ssf) {
text->limitssf = 0;
} else {
text->limitssf = params->props.max_ssf - params->external_ssf;
}
if (params->props.min_ssf < params->external_ssf) {
text->requiressf = 0;
} else {
text->requiressf = params->props.min_ssf - params->external_ssf;
}
if (text->requiressf != 0 &&
(text->qop & (LAYER_INTEGRITY|LAYER_CONFIDENTIALITY))) {
if (params->props.maxbufsize > 0xFFFFFF) {
sasldata[1] = sasldata[2] = sasldata[3] = 0xFF;
} else {
sasldata[1] = (params->props.maxbufsize >> 16) & 0xFF;
sasldata[2] = (params->props.maxbufsize >> 8) & 0xFF;
sasldata[3] = (params->props.maxbufsize >> 0) & 0xFF;
}
} else {
sasldata[1] = sasldata[2] = sasldata[3] = 0;
}
sasldata[0] = 0;
if(text->requiressf != 0 && !params->props.maxbufsize) {
params->utils->seterror(params->utils->conn, 0,
"GSSAPI needs a security layer but one is forbidden");
return SASL_TOOWEAK;
}
if (text->requiressf == 0) {
sasldata[0] |= LAYER_NONE;
}
if ((text->qop & LAYER_INTEGRITY) &&
text->requiressf <= 1 &&
text->limitssf >= 1 &&
params->props.maxbufsize) {
sasldata[0] |= LAYER_INTEGRITY;
}
if ((text->qop & LAYER_CONFIDENTIALITY) &&
text->requiressf <= K5_MAX_SSF &&
text->limitssf >= K5_MAX_SSF &&
params->props.maxbufsize) {
sasldata[0] |= LAYER_CONFIDENTIALITY;
}
text->qop = sasldata[0];
real_input_token.value = (void *)sasldata;
real_input_token.length = 4;
GSS_LOCK_MUTEX(params->utils);
maj_stat = gss_wrap(&min_stat,
text->gss_ctx,
0,
GSS_C_QOP_DEFAULT,
input_token,
NULL,
output_token);
GSS_UNLOCK_MUTEX(params->utils);
if (GSS_ERROR(maj_stat)) {
sasl_gss_seterror(text->utils, maj_stat, min_stat);
if (output_token->value) {
GSS_LOCK_MUTEX(params->utils);
gss_release_buffer(&min_stat, output_token);
GSS_UNLOCK_MUTEX(params->utils);
}
sasl_gss_free_context_contents(text);
return SASL_FAIL;
}
if (serveroutlen)
*serveroutlen = output_token->length;
if (output_token->value) {
if (serverout) {
ret = _plug_buf_alloc(text->utils, &(text->out_buf),
&(text->out_buf_len), *serveroutlen);
if(ret != SASL_OK) {
GSS_LOCK_MUTEX(params->utils);
gss_release_buffer(&min_stat, output_token);
GSS_UNLOCK_MUTEX(params->utils);
return ret;
}
memcpy(text->out_buf, output_token->value, *serveroutlen);
*serverout = text->out_buf;
}
GSS_LOCK_MUTEX(params->utils);
gss_release_buffer(&min_stat, output_token);
GSS_UNLOCK_MUTEX(params->utils);
}
text->state = SASL_GSSAPI_STATE_SSFREQ;
return SASL_CONTINUE;
}
static int
gssapi_server_mech_ssfreq(context_t *text,
sasl_server_params_t *params,
const char *clientin,
unsigned clientinlen,
const char **serverout __attribute__((unused)),
unsigned *serveroutlen __attribute__((unused)),
sasl_out_params_t *oparams)
{
gss_buffer_t input_token, output_token;
gss_buffer_desc real_input_token, real_output_token;
OM_uint32 maj_stat = 0, min_stat = 0;
OM_uint32 max_input;
int layerchoice;
input_token = &real_input_token;
output_token = &real_output_token;
output_token->value = NULL; output_token->length = 0;
real_input_token.value = (void *)clientin;
real_input_token.length = clientinlen;
GSS_LOCK_MUTEX(params->utils);
maj_stat = gss_unwrap(&min_stat,
text->gss_ctx,
input_token,
output_token,
NULL,
NULL);
GSS_UNLOCK_MUTEX(params->utils);
if (GSS_ERROR(maj_stat)) {
sasl_gss_seterror(text->utils, maj_stat, min_stat);
sasl_gss_free_context_contents(text);
return SASL_FAIL;
}
if (output_token->length < 4) {
SETERROR(text->utils,
"token too short");
GSS_LOCK_MUTEX(params->utils);
gss_release_buffer(&min_stat, output_token);
GSS_UNLOCK_MUTEX(params->utils);
sasl_gss_free_context_contents(text);
return SASL_FAIL;
}
layerchoice = (int)(((char *)(output_token->value))[0]);
if (layerchoice == LAYER_NONE &&
(text->qop & LAYER_NONE)) {
oparams->encode = NULL;
oparams->decode = NULL;
oparams->mech_ssf = 0;
} else if (layerchoice == LAYER_INTEGRITY &&
(text->qop & LAYER_INTEGRITY)) {
oparams->encode = &gssapi_integrity_encode;
oparams->decode = &gssapi_decode;
oparams->mech_ssf = 1;
} else if ((layerchoice == LAYER_CONFIDENTIALITY ||
layerchoice == (LAYER_CONFIDENTIALITY|LAYER_INTEGRITY)) &&
(text->qop & LAYER_CONFIDENTIALITY)) {
oparams->encode = &gssapi_privacy_encode;
oparams->decode = &gssapi_decode;
oparams->mech_ssf = K5_MAX_SSF;
} else {
SETERROR(text->utils,
"protocol violation: client requested invalid layer");
oparams->mech_ssf = 2;
if (output_token->value) {
GSS_LOCK_MUTEX(params->utils);
gss_release_buffer(&min_stat, output_token);
GSS_UNLOCK_MUTEX(params->utils);
}
sasl_gss_free_context_contents(text);
return SASL_FAIL;
}
if (output_token->length > 4) {
int ret;
ret = params->canon_user(params->utils->conn,
((char *) output_token->value) + 4,
(output_token->length - 4) * sizeof(char),
SASL_CU_AUTHZID, oparams);
if (ret != SASL_OK) {
sasl_gss_free_context_contents(text);
return ret;
}
}
oparams->maxoutbuf =
(((unsigned char *) output_token->value)[1] << 16) |
(((unsigned char *) output_token->value)[2] << 8) |
(((unsigned char *) output_token->value)[3] << 0);
if (oparams->mech_ssf) {
maj_stat = gss_wrap_size_limit( &min_stat,
text->gss_ctx,
1,
GSS_C_QOP_DEFAULT,
(OM_uint32) oparams->maxoutbuf,
&max_input);
if(max_input > oparams->maxoutbuf) {
oparams->maxoutbuf -= (max_input - oparams->maxoutbuf);
} else {
oparams->maxoutbuf = max_input;
}
}
GSS_LOCK_MUTEX(params->utils);
gss_release_buffer(&min_stat, output_token);
GSS_UNLOCK_MUTEX(params->utils);
text->state = SASL_GSSAPI_STATE_AUTHENTICATED;
_plug_decode_init(&text->decode_context,
text->utils,
(params->props.maxbufsize > 0xFFFFFF) ? 0xFFFFFF :
params->props.maxbufsize);
return SASL_OK;
}
static int
gssapi_server_mech_step(void *conn_context,
sasl_server_params_t *params,
const char *clientin,
unsigned clientinlen,
const char **serverout,
unsigned *serveroutlen,
sasl_out_params_t *oparams)
{
context_t *text = (context_t *) conn_context;
int ret;
if (!serverout) {
PARAMERROR(text->utils);
return SASL_BADPARAM;
}
*serverout = NULL;
*serveroutlen = 0;
if (text == NULL) return SASL_BADPROT;
params->utils->log(NULL, SASL_LOG_DEBUG,
"GSSAPI server step %d\n", text->state);
switch (text->state) {
case SASL_GSSAPI_STATE_AUTHNEG:
ret = gssapi_server_mech_authneg(text, params, clientin, clientinlen,
serverout, serveroutlen, oparams);
if (ret != SASL_CONTINUE || *serveroutlen) break;
clientinlen = 0;
case SASL_GSSAPI_STATE_SSFCAP:
ret = gssapi_server_mech_ssfcap(text, params, clientin, clientinlen,
serverout, serveroutlen, oparams);
break;
case SASL_GSSAPI_STATE_SSFREQ:
ret = gssapi_server_mech_ssfreq(text, params, clientin, clientinlen,
serverout, serveroutlen, oparams);
break;
default:
params->utils->log(NULL, SASL_LOG_ERR,
"Invalid GSSAPI server step %d\n", text->state);
return SASL_FAIL;
}
if (ret == SASL_OK) {
ret = params->canon_user(params->utils->conn,
text->authid,
0,
(oparams->user ? 0 : SASL_CU_AUTHZID)
| SASL_CU_AUTHID | SASL_CU_EXTERNALLY_VERIFIED,
oparams);
if (ret != SASL_OK) {
sasl_gss_free_context_contents(text);
return ret;
}
if (text->client_creds != GSS_C_NO_CREDENTIAL)	{
oparams->client_creds =  &text->client_creds;
}
else {
oparams->client_creds = NULL;
}
oparams->doneflag = 1;
}
return ret;
}
static sasl_server_plug_t gssapi_server_plugins[] =
{
{
"GSSAPI",
K5_MAX_SSF,
SASL_SEC_NOPLAINTEXT
| SASL_SEC_NOACTIVE
| SASL_SEC_NOANONYMOUS
| SASL_SEC_MUTUAL_AUTH
| SASL_SEC_PASS_CREDENTIALS,
SASL_FEAT_WANT_CLIENT_FIRST
| SASL_FEAT_ALLOWS_PROXY
| SASL_FEAT_DONTUSE_USERPASSWD,
NULL,
&gssapi_server_mech_new,
&gssapi_server_mech_step,
&gssapi_common_mech_dispose,
&gssapi_common_mech_free,
NULL,
NULL,
NULL,
NULL,
NULL
}
#ifdef HAVE_GSS_SPNEGO
,{
"GSS-SPNEGO",
K5_MAX_SSF,
SASL_SEC_NOPLAINTEXT
| SASL_SEC_NOACTIVE
| SASL_SEC_NOANONYMOUS
| SASL_SEC_MUTUAL_AUTH
| SASL_SEC_PASS_CREDENTIALS,
SASL_FEAT_WANT_CLIENT_FIRST
| SASL_FEAT_ALLOWS_PROXY
| SASL_FEAT_DONTUSE_USERPASSWD
| SASL_FEAT_SUPPORTS_HTTP,
NULL,
&gssapi_server_mech_new,
&gssapi_server_mech_step,
&gssapi_common_mech_dispose,
&gssapi_common_mech_free,
NULL,
NULL,
NULL,
NULL,
NULL
}
#endif
};
int gssapiv2_server_plug_init(
#ifndef HAVE_GSSKRB5_REGISTER_ACCEPTOR_IDENTITY
const sasl_utils_t *utils __attribute__((unused)),
#else
const sasl_utils_t *utils,
#endif
int maxversion,
int *out_version,
sasl_server_plug_t **pluglist,
int *plugcount)
{
#ifdef HAVE_GSSKRB5_REGISTER_ACCEPTOR_IDENTITY
const char *keytab = NULL;
char keytab_path[1024];
unsigned int rl;
#endif
if (maxversion < SASL_SERVER_PLUG_VERSION) {
return SASL_BADVERS;
}
#ifdef HAVE_GSSKRB5_REGISTER_ACCEPTOR_IDENTITY
utils->getopt(utils->getopt_context, "GSSAPI", "keytab", &keytab, &rl);
if (keytab != NULL) {
if (access(keytab, R_OK) != 0) {
utils->log(NULL, SASL_LOG_ERR,
"Could not find keytab file: %s: %m",
keytab, errno);
return SASL_FAIL;
}
if(strlen(keytab) > 1024) {
utils->log(NULL, SASL_LOG_ERR,
"path to keytab is > 1024 characters");
return SASL_BUFOVER;
}
strncpy(keytab_path, keytab, 1024);
gsskrb5_register_acceptor_identity(keytab_path);
}
#endif
*out_version = SASL_SERVER_PLUG_VERSION;
*pluglist = gssapi_server_plugins;
#ifdef HAVE_GSS_SPNEGO
*plugcount = 2;
#else
*plugcount = 1;
#endif
#ifdef GSS_USE_MUTEXES
if (!gss_mutex) {
gss_mutex = utils->mutex_alloc();
if (!gss_mutex) {
return SASL_FAIL;
}
}
#endif
return SASL_OK;
}
static int gssapi_client_mech_new(void *glob_context,
sasl_client_params_t *params,
void **conn_context)
{
context_t *text;
text = sasl_gss_new_context(params->utils);
if (text == NULL) {
MEMERROR(params->utils);
return SASL_NOMEM;
}
text->state = SASL_GSSAPI_STATE_AUTHNEG;
text->mech_type = (gss_OID) glob_context;
text->gss_ctx = GSS_C_NO_CONTEXT;
text->client_name = GSS_C_NO_NAME;
text->server_creds = GSS_C_NO_CREDENTIAL;
text->client_creds  = GSS_C_NO_CREDENTIAL;
text->http_mode = (params->flags & SASL_NEED_HTTP);
*conn_context = text;
return SASL_OK;
}
static int gssapi_client_mech_step(void *conn_context,
sasl_client_params_t *params,
const char *serverin,
unsigned serverinlen,
sasl_interact_t **prompt_need,
const char **clientout,
unsigned *clientoutlen,
sasl_out_params_t *oparams)
{
context_t *text = (context_t *)conn_context;
gss_buffer_t input_token, output_token;
gss_buffer_desc real_input_token, real_output_token;
OM_uint32 maj_stat = 0, min_stat = 0;
OM_uint32 max_input;
gss_buffer_desc name_token;
int ret;
OM_uint32 req_flags = 0, out_req_flags = 0;
input_token = &real_input_token;
output_token = &real_output_token;
output_token->value = NULL;
input_token->value = NULL;
input_token->length = 0;
gss_cred_id_t client_creds = (gss_cred_id_t)params->gss_creds;
*clientout = NULL;
*clientoutlen = 0;
params->utils->log(NULL, SASL_LOG_DEBUG,
"GSSAPI client step %d", text->state);
switch (text->state) {
case SASL_GSSAPI_STATE_AUTHNEG:
if (text->user == NULL) {
int user_result = SASL_OK;
user_result = _plug_get_userid(params->utils, &text->user,
prompt_need);
if ((user_result != SASL_OK) && (user_result != SASL_INTERACT)) {
sasl_gss_free_context_contents(text);
return user_result;
}
if (prompt_need && *prompt_need) {
params->utils->free(*prompt_need);
*prompt_need = NULL;
}
if (user_result == SASL_INTERACT) {
int result =
_plug_make_prompts(params->utils, prompt_need,
user_result == SASL_INTERACT ?
"Please enter your authorization name" : NULL, NULL,
NULL, NULL,
NULL, NULL,
NULL, NULL, NULL,
NULL, NULL, NULL);
if (result != SASL_OK) return result;
return SASL_INTERACT;
}
}
if (text->server_name == GSS_C_NO_NAME) {
if (params->serverFQDN == NULL
|| strlen(params->serverFQDN) == 0) {
SETERROR(text->utils, "GSSAPI Failure: no serverFQDN");
sasl_gss_free_context_contents(text);
return SASL_FAIL;
}
name_token.length = strlen(params->service) + 1 + strlen(params->serverFQDN);
name_token.value = (char *)params->utils->malloc((name_token.length + 1) * sizeof(char));
if (name_token.value == NULL) {
sasl_gss_free_context_contents(text);
return SASL_NOMEM;
}
sprintf(name_token.value,"%s@%s", params->service, params->serverFQDN);
GSS_LOCK_MUTEX(params->utils);
maj_stat = gss_import_name (&min_stat,
&name_token,
GSS_C_NT_HOSTBASED_SERVICE,
&text->server_name);
GSS_UNLOCK_MUTEX(params->utils);
params->utils->free(name_token.value);
name_token.value = NULL;
if (GSS_ERROR(maj_stat)) {
sasl_gss_seterror(text->utils, maj_stat, min_stat);
sasl_gss_free_context_contents(text);
return SASL_FAIL;
}
}
if (serverinlen == 0)
input_token = GSS_C_NO_BUFFER;
if (serverinlen) {
real_input_token.value = (void *)serverin;
real_input_token.length = serverinlen;
}
else if (text->gss_ctx != GSS_C_NO_CONTEXT ) {
GSS_LOCK_MUTEX(params->utils);
maj_stat = gss_delete_sec_context (&min_stat,&text->gss_ctx,GSS_C_NO_BUFFER);
GSS_UNLOCK_MUTEX(params->utils);
text->gss_ctx = GSS_C_NO_CONTEXT;
}
req_flags = GSS_C_INTEG_FLAG;
if (params->props.max_ssf > params->external_ssf) {
req_flags |= GSS_C_MUTUAL_FLAG | GSS_C_SEQUENCE_FLAG;
if (params->props.max_ssf - params->external_ssf > 1) {
req_flags |= GSS_C_CONF_FLAG;
}
}
if (params->props.security_flags & SASL_SEC_PASS_CREDENTIALS) {
req_flags = req_flags |  GSS_C_DELEG_FLAG;
}
GSS_LOCK_MUTEX(params->utils);
maj_stat = gss_init_sec_context(&min_stat,
client_creds,
&text->gss_ctx,
text->server_name,
text->mech_type,
req_flags,
0,
GSS_C_NO_CHANNEL_BINDINGS,
input_token,
NULL,
output_token,
&out_req_flags,
NULL);
GSS_UNLOCK_MUTEX(params->utils);
if (GSS_ERROR(maj_stat)) {
sasl_gss_seterror(text->utils, maj_stat, min_stat);
if (output_token->value) {
GSS_LOCK_MUTEX(params->utils);
gss_release_buffer(&min_stat, output_token);
GSS_UNLOCK_MUTEX(params->utils);
}
sasl_gss_free_context_contents(text);
return SASL_FAIL;
}
if ((out_req_flags & GSS_C_INTEG_FLAG) == 0) {
text->qop = LAYER_NONE;
} else if ((out_req_flags & GSS_C_CONF_FLAG) == 0) {
text->qop = LAYER_NONE | LAYER_INTEGRITY;
} else {
text->qop = LAYER_NONE | LAYER_INTEGRITY | LAYER_CONFIDENTIALITY;
}
if ((out_req_flags & GSS_C_DELEG_FLAG) != (req_flags & GSS_C_DELEG_FLAG)) {
text->utils->seterror(text->utils->conn, SASL_LOG_WARN, "GSSAPI warning: no credentials were passed");
}
*clientoutlen = output_token->length;
if (output_token->value) {
if (clientout) {
ret = _plug_buf_alloc(text->utils, &(text->out_buf),
&(text->out_buf_len), *clientoutlen);
if(ret != SASL_OK) {
GSS_LOCK_MUTEX(params->utils);
gss_release_buffer(&min_stat, output_token);
GSS_UNLOCK_MUTEX(params->utils);
return ret;
}
memcpy(text->out_buf, output_token->value, *clientoutlen);
*clientout = text->out_buf;
}
GSS_LOCK_MUTEX(params->utils);
gss_release_buffer(&min_stat, output_token);
GSS_UNLOCK_MUTEX(params->utils);
}
if (maj_stat == GSS_S_COMPLETE) {
GSS_LOCK_MUTEX(params->utils);
maj_stat = gss_inquire_context(&min_stat,
text->gss_ctx,
&text->client_name,
NULL,
NULL,
NULL,
NULL,
NULL,
NULL);
GSS_UNLOCK_MUTEX(params->utils);
if (GSS_ERROR(maj_stat)) {
sasl_gss_seterror(text->utils, maj_stat, min_stat);
sasl_gss_free_context_contents(text);
return SASL_FAIL;
}
name_token.length = 0;
GSS_LOCK_MUTEX(params->utils);
maj_stat = gss_display_name(&min_stat,
text->client_name,
&name_token,
NULL);
GSS_UNLOCK_MUTEX(params->utils);
if (GSS_ERROR(maj_stat)) {
if (name_token.value) {
GSS_LOCK_MUTEX(params->utils);
gss_release_buffer(&min_stat, &name_token);
GSS_UNLOCK_MUTEX(params->utils);
}
SETERROR(text->utils, "GSSAPI Failure");
sasl_gss_free_context_contents(text);
return SASL_FAIL;
}
if (text->user && text->user[0]) {
ret = params->canon_user(params->utils->conn,
text->user, 0,
SASL_CU_AUTHZID, oparams);
if (ret == SASL_OK)
ret = params->canon_user(params->utils->conn,
name_token.value, 0,
SASL_CU_AUTHID, oparams);
} else {
ret = params->canon_user(params->utils->conn,
name_token.value, 0,
SASL_CU_AUTHID | SASL_CU_AUTHZID,
oparams);
}
GSS_LOCK_MUTEX(params->utils);
gss_release_buffer(&min_stat, &name_token);
GSS_UNLOCK_MUTEX(params->utils);
if (ret != SASL_OK) return ret;
if (text->http_mode) {
text->state = SASL_GSSAPI_STATE_AUTHENTICATED;
oparams->doneflag = 1;
return SASL_OK;
}
text->state = SASL_GSSAPI_STATE_SSFCAP;
}
return SASL_CONTINUE;
case SASL_GSSAPI_STATE_SSFCAP: {
sasl_security_properties_t *secprops = &(params->props);
unsigned int alen, external = params->external_ssf;
sasl_ssf_t need, allowed;
char serverhas, mychoice;
real_input_token.value = (void *) serverin;
real_input_token.length = serverinlen;
GSS_LOCK_MUTEX(params->utils);
maj_stat = gss_unwrap(&min_stat,
text->gss_ctx,
input_token,
output_token,
NULL,
NULL);
GSS_UNLOCK_MUTEX(params->utils);
if (GSS_ERROR(maj_stat)) {
sasl_gss_seterror(text->utils, maj_stat, min_stat);
sasl_gss_free_context_contents(text);
if (output_token->value) {
GSS_LOCK_MUTEX(params->utils);
gss_release_buffer(&min_stat, output_token);
GSS_UNLOCK_MUTEX(params->utils);
}
return SASL_FAIL;
}
if (output_token->length != 4) {
SETERROR(text->utils,
(output_token->length < 4) ? "token too short" : "token too long");
GSS_LOCK_MUTEX(params->utils);
gss_release_buffer(&min_stat, output_token);
GSS_UNLOCK_MUTEX(params->utils);
sasl_gss_free_context_contents(text);
return SASL_FAIL;
}
if (secprops->min_ssf > (K5_MAX_SSF + external)) {
return SASL_TOOWEAK;
} else if (secprops->min_ssf > secprops->max_ssf) {
return SASL_BADPARAM;
}
if (secprops->max_ssf >= external) {
allowed = secprops->max_ssf - external;
} else {
allowed = 0;
}
if (secprops->min_ssf >= external) {
need = secprops->min_ssf - external;
} else {
need = 0;
}
serverhas = ((char *)output_token->value)[0];
if ((text->qop & LAYER_CONFIDENTIALITY) &&
allowed >= K5_MAX_SSF &&
need <= K5_MAX_SSF &&
(serverhas & LAYER_CONFIDENTIALITY)) {
const char *ad_compat;
oparams->encode = &gssapi_privacy_encode;
oparams->decode = &gssapi_decode;
oparams->mech_ssf = K5_MAX_SSF;
mychoice = LAYER_CONFIDENTIALITY;
if (serverhas & LAYER_INTEGRITY) {
params->utils->getopt(params->utils->getopt_context,
"GSSAPI",
"ad_compat",
&ad_compat,
NULL);
if (ad_compat &&
(ad_compat[0] == '1' || ad_compat[0] == 'y' ||
(ad_compat[0] == 'o' && ad_compat[1] == 'n') ||
ad_compat[0] == 't')) {
mychoice = LAYER_INTEGRITY|LAYER_CONFIDENTIALITY;
}
}
} else if ((text->qop & LAYER_INTEGRITY) &&
allowed >= 1 &&
need <= 1 &&
(serverhas & LAYER_INTEGRITY)) {
oparams->encode = &gssapi_integrity_encode;
oparams->decode = &gssapi_decode;
oparams->mech_ssf = 1;
mychoice = LAYER_INTEGRITY;
} else if ((text->qop & LAYER_NONE) &&
need <= 0 && (serverhas & LAYER_NONE)) {
oparams->encode = NULL;
oparams->decode = NULL;
oparams->mech_ssf = 0;
mychoice = LAYER_NONE;
} else {
sasl_gss_free_context_contents(text);
return SASL_TOOWEAK;
}
oparams->maxoutbuf =
(((unsigned char *) output_token->value)[1] << 16) |
(((unsigned char *) output_token->value)[2] << 8) |
(((unsigned char *) output_token->value)[3] << 0);
if (oparams->mech_ssf) {
maj_stat = gss_wrap_size_limit( &min_stat,
text->gss_ctx,
1,
GSS_C_QOP_DEFAULT,
(OM_uint32) oparams->maxoutbuf,
&max_input);
if (max_input > oparams->maxoutbuf) {
oparams->maxoutbuf -= (max_input - oparams->maxoutbuf);
} else {
oparams->maxoutbuf = max_input;
}
}
GSS_LOCK_MUTEX(params->utils);
gss_release_buffer(&min_stat, output_token);
GSS_UNLOCK_MUTEX(params->utils);
if (text->user && text->user[0]) {
alen = strlen(oparams->user);
} else {
alen = 0;
}
input_token->length = 4 + alen;
input_token->value =
(char *)params->utils->malloc((input_token->length + 1)*sizeof(char));
if (input_token->value == NULL) {
sasl_gss_free_context_contents(text);
return SASL_NOMEM;
}
if (alen)
memcpy((char *)input_token->value+4,oparams->user,alen);
if (mychoice > 1) {
if (params->props.maxbufsize > 0xFFFFFF) {
((unsigned char *)input_token->value)[1] = 0xFF;
((unsigned char *)input_token->value)[2] = 0xFF;
((unsigned char *)input_token->value)[3] = 0xFF;
} else {
((unsigned char *)input_token->value)[1] =
(params->props.maxbufsize >> 16) & 0xFF;
((unsigned char *)input_token->value)[2] =
(params->props.maxbufsize >> 8) & 0xFF;
((unsigned char *)input_token->value)[3] =
(params->props.maxbufsize >> 0) & 0xFF;
}
} else {
((unsigned char *)input_token->value)[1] = 0;
((unsigned char *)input_token->value)[2] = 0;
((unsigned char *)input_token->value)[3] = 0;
}
((unsigned char *)input_token->value)[0] = mychoice;
GSS_LOCK_MUTEX(params->utils);
maj_stat = gss_wrap (&min_stat,
text->gss_ctx,
0,
GSS_C_QOP_DEFAULT,
input_token,
NULL,
output_token);
GSS_UNLOCK_MUTEX(params->utils);
params->utils->free(input_token->value);
input_token->value = NULL;
if (GSS_ERROR(maj_stat)) {
sasl_gss_seterror(text->utils, maj_stat, min_stat);
if (output_token->value) {
GSS_LOCK_MUTEX(params->utils);
gss_release_buffer(&min_stat, output_token);
GSS_UNLOCK_MUTEX(params->utils);
}
sasl_gss_free_context_contents(text);
return SASL_FAIL;
}
if (clientoutlen) {
*clientoutlen = output_token->length;
}
if (output_token->value) {
if (clientout) {
ret = _plug_buf_alloc(text->utils,
&(text->out_buf),
&(text->out_buf_len),
*clientoutlen);
if (ret != SASL_OK) {
GSS_LOCK_MUTEX(params->utils);
gss_release_buffer(&min_stat, output_token);
GSS_UNLOCK_MUTEX(params->utils);
return ret;
}
memcpy(text->out_buf, output_token->value, *clientoutlen);
*clientout = text->out_buf;
}
GSS_LOCK_MUTEX(params->utils);
gss_release_buffer(&min_stat, output_token);
GSS_UNLOCK_MUTEX(params->utils);
}
text->state = SASL_GSSAPI_STATE_AUTHENTICATED;
oparams->doneflag = 1;
_plug_decode_init(&text->decode_context, text->utils,
(params->props.maxbufsize > 0xFFFFFF) ? 0xFFFFFF :
params->props.maxbufsize);
return SASL_OK;
}
default:
params->utils->log(NULL, SASL_LOG_ERR,
"Invalid GSSAPI client step %d\n", text->state);
return SASL_FAIL;
}
return SASL_FAIL;
}
static const unsigned long gssapi_required_prompts[] = {
SASL_CB_LIST_END
};
static sasl_client_plug_t gssapi_client_plugins[] =
{
{
"GSSAPI",
K5_MAX_SSF,
SASL_SEC_NOPLAINTEXT
| SASL_SEC_NOACTIVE
| SASL_SEC_NOANONYMOUS
| SASL_SEC_MUTUAL_AUTH
| SASL_SEC_PASS_CREDENTIALS,
SASL_FEAT_NEEDSERVERFQDN
| SASL_FEAT_WANT_CLIENT_FIRST
| SASL_FEAT_ALLOWS_PROXY,
gssapi_required_prompts,
GSS_C_NO_OID,
&gssapi_client_mech_new,
&gssapi_client_mech_step,
&gssapi_common_mech_dispose,
&gssapi_common_mech_free,
NULL,
NULL,
NULL
}
#ifdef HAVE_GSS_SPNEGO
,{
"GSS-SPNEGO",
K5_MAX_SSF,
SASL_SEC_NOPLAINTEXT
| SASL_SEC_NOACTIVE
| SASL_SEC_NOANONYMOUS
| SASL_SEC_MUTUAL_AUTH
| SASL_SEC_PASS_CREDENTIALS,
SASL_FEAT_NEEDSERVERFQDN
| SASL_FEAT_WANT_CLIENT_FIRST
| SASL_FEAT_ALLOWS_PROXY
| SASL_FEAT_SUPPORTS_HTTP,
gssapi_required_prompts,
&gss_spnego_oid,
&gssapi_client_mech_new,
&gssapi_client_mech_step,
&gssapi_common_mech_dispose,
&gssapi_common_mech_free,
NULL,
NULL,
NULL
}
#endif
};
int gssapiv2_client_plug_init(const sasl_utils_t *utils __attribute__((unused)),
int maxversion,
int *out_version,
sasl_client_plug_t **pluglist,
int *plugcount)
{
if (maxversion < SASL_CLIENT_PLUG_VERSION) {
SETERROR(utils, "Version mismatch in GSSAPI");
return SASL_BADVERS;
}
*out_version = SASL_CLIENT_PLUG_VERSION;
*pluglist = gssapi_client_plugins;
#ifdef HAVE_GSS_SPNEGO
*plugcount = 2;
#else
*plugcount = 1;
#endif
#ifdef GSS_USE_MUTEXES
if(!gss_mutex) {
gss_mutex = utils->mutex_alloc();
if(!gss_mutex) {
return SASL_FAIL;
}
}
#endif
return SASL_OK;
}