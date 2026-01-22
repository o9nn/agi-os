#ifndef SASLPLUG_H
#define SASLPLUG_H 1
#ifndef MD5GLOBAL_H
#include "md5global.h"
#endif
#ifndef MD5_H
#include "md5.h"
#endif
#ifndef HMAC_MD5_H
#include "hmac-md5.h"
#endif
#ifndef PROP_H
#include "prop.h"
#endif
#ifdef __cplusplus
extern "C" {
#endif
typedef int (*sasl_callback_ft)(void);
typedef int sasl_getcallback_t(sasl_conn_t *conn,
unsigned long callbackid,
sasl_callback_ft * pproc,
void **pcontext);
#define SASL_UTILS_VERSION 4
typedef struct sasl_utils {
int version;
sasl_conn_t *conn;
sasl_rand_t *rpool;
void *getopt_context;
sasl_getopt_t *getopt;
sasl_malloc_t *malloc;
sasl_calloc_t *calloc;
sasl_realloc_t *realloc;
sasl_free_t *free;
sasl_mutex_alloc_t *mutex_alloc;
sasl_mutex_lock_t *mutex_lock;
sasl_mutex_unlock_t *mutex_unlock;
sasl_mutex_free_t *mutex_free;
void (*MD5Init)(MD5_CTX *);
void (*MD5Update)(MD5_CTX *, const unsigned char *text, unsigned int len);
void (*MD5Final)(unsigned char [16], MD5_CTX *);
void (*hmac_md5)(const unsigned char *text, int text_len,
const unsigned char *key, int key_len,
unsigned char [16]);
void (*hmac_md5_init)(HMAC_MD5_CTX *, const unsigned char *key, int len);
void (*hmac_md5_final)(unsigned char [16], HMAC_MD5_CTX *);
void (*hmac_md5_precalc)(HMAC_MD5_STATE *,
const unsigned char *key, int len);
void (*hmac_md5_import)(HMAC_MD5_CTX *, HMAC_MD5_STATE *);
int (*mkchal)(sasl_conn_t *conn, char *buf, unsigned maxlen,
unsigned hostflag);
int (*utf8verify)(const char *str, unsigned len);
void (*rand)(sasl_rand_t *rpool, char *buf, unsigned len);
void (*churn)(sasl_rand_t *rpool, const char *data, unsigned len);
int (*checkpass)(sasl_conn_t *conn,
const char *user, unsigned userlen,
const char *pass, unsigned passlen);
int (*decode64)(const char *in, unsigned inlen,
char *out, unsigned outmax, unsigned *outlen);
int (*encode64)(const char *in, unsigned inlen,
char *out, unsigned outmax, unsigned *outlen);
void (*erasebuffer)(char *buf, unsigned len);
int (*getprop)(sasl_conn_t *conn, int propnum, const void **pvalue);
int (*setprop)(sasl_conn_t *conn, int propnum, const void *value);
sasl_getcallback_t *getcallback;
void (*log)(sasl_conn_t *conn, int level, const char *fmt, ...);
void (*seterror)(sasl_conn_t *conn, unsigned flags, const char *fmt, ...);
int *(*spare_fptr)(void);
struct propctx *(*prop_new)(unsigned estimate);
int (*prop_dup)(struct propctx *src_ctx, struct propctx **dst_ctx);
int (*prop_request)(struct propctx *ctx, const char **names);
const struct propval *(*prop_get)(struct propctx *ctx);
int (*prop_getnames)(struct propctx *ctx, const char **names,
struct propval *vals);
void (*prop_clear)(struct propctx *ctx, int requests);
void (*prop_dispose)(struct propctx **ctx);
int (*prop_format)(struct propctx *ctx, const char *sep, int seplen,
char *outbuf, unsigned outmax, unsigned *outlen);
int (*prop_set)(struct propctx *ctx, const char *name,
const char *value, int vallen);
int (*prop_setvals)(struct propctx *ctx, const char *name,
const char **values);
void (*prop_erase)(struct propctx *ctx, const char *name);
int (*auxprop_store)(sasl_conn_t *conn,
struct propctx *ctx, const char *user);
int (*spare_fptr1)(void);
int (*spare_fptr2)(void);
} sasl_utils_t;
typedef struct sasl_out_params {
unsigned doneflag;
const char *user;
const char *authid;
unsigned ulen;
unsigned alen;
unsigned maxoutbuf;
sasl_ssf_t mech_ssf;
void *encode_context;
int (*encode)(void *context, const struct iovec *invec, unsigned numiov,
const char **output, unsigned *outputlen);
void *decode_context;
int (*decode)(void *context, const char *input, unsigned inputlen,
const char **output, unsigned *outputlen);
void *client_creds;
const void *gss_peer_name;
const void *gss_local_name;
const char *cbindingname;
int (*spare_fptr1)(void);
int (*spare_fptr2)(void);
unsigned int cbindingdisp;
int spare_int2;
int spare_int3;
int spare_int4;
int param_version;
} sasl_out_params_t;
typedef enum {
SASL_INFO_LIST_START = 0,
SASL_INFO_LIST_MECH,
SASL_INFO_LIST_END
} sasl_info_callback_stage_t;
typedef enum {
SASL_CB_DISP_NONE = 0,
SASL_CB_DISP_WANT,
SASL_CB_DISP_USED
} sasl_cbinding_disp_t;
#define SASL_CB_PRESENT(params) ((params)->cbinding != NULL)
#define SASL_CB_CRITICAL(params) (SASL_CB_PRESENT(params) && \
(params)->cbinding->critical)
typedef struct sasl_client_params {
const char *service;
const char *serverFQDN;
const char *clientFQDN;
const sasl_utils_t *utils;
const sasl_callback_t *prompt_supp;
const char *iplocalport;
const char *ipremoteport;
unsigned servicelen;
unsigned slen;
unsigned clen;
unsigned iploclen;
unsigned ipremlen;
sasl_security_properties_t props;
sasl_ssf_t external_ssf;
const void *gss_creds;
const sasl_channel_binding_t *cbinding;
const sasl_http_request_t *http_request;
void *spare_ptr4;
int (*canon_user)(sasl_conn_t *conn,
const char *in, unsigned len,
unsigned flags,
sasl_out_params_t *oparams);
int (*spare_fptr1)(void);
unsigned int cbindingdisp;
int spare_int2;
int spare_int3;
unsigned flags;
int param_version;
} sasl_client_params_t;
#define SASL_FEAT_WANT_CLIENT_FIRST 0x0002
#define SASL_FEAT_SERVER_FIRST 0x0010
#define SASL_FEAT_ALLOWS_PROXY 0x0020
#define SASL_FEAT_DONTUSE_USERPASSWD 0x0080
#define SASL_FEAT_GSS_FRAMING 0x0100
#define SASL_FEAT_CHANNEL_BINDING 0x0800
#define SASL_FEAT_SUPPORTS_HTTP 0x1000
#define SASL_FEAT_NEEDSERVERFQDN 0x0001
typedef struct sasl_client_plug {
const char *mech_name;
sasl_ssf_t max_ssf;
unsigned security_flags;
unsigned features;
const unsigned long *required_prompts;
void *glob_context;
int (*mech_new)(void *glob_context,
sasl_client_params_t *cparams,
void **conn_context);
int (*mech_step)(void *conn_context,
sasl_client_params_t *cparams,
const char *serverin,
unsigned serverinlen,
sasl_interact_t **prompt_need,
const char **clientout,
unsigned *clientoutlen,
sasl_out_params_t *oparams);
void (*mech_dispose)(void *conn_context, const sasl_utils_t *utils);
void (*mech_free)(void *glob_context, const sasl_utils_t *utils);
int (*idle)(void *glob_context,
void *conn_context,
sasl_client_params_t *cparams);
int (*spare_fptr1)(void);
int (*spare_fptr2)(void);
} sasl_client_plug_t;
#define SASL_CLIENT_PLUG_VERSION 4
typedef int sasl_client_plug_init_t(const sasl_utils_t *utils,
int max_version,
int *out_version,
sasl_client_plug_t **pluglist,
int *plugcount);
LIBSASL_API int sasl_client_add_plugin(const char *plugname,
sasl_client_plug_init_t *cplugfunc);
typedef struct client_sasl_mechanism
{
int version;
char *plugname;
const sasl_client_plug_t *plug;
} client_sasl_mechanism_t;
typedef void sasl_client_info_callback_t (client_sasl_mechanism_t *m,
sasl_info_callback_stage_t stage,
void *rock);
LIBSASL_API int sasl_client_plugin_info (const char *mech_list,
sasl_client_info_callback_t *info_cb,
void *info_cb_rock);
typedef void sasl_logmsg_p(sasl_conn_t *conn, const char *fmt, ...);
typedef struct sasl_server_params {
const char *service;
const char *appname;
const char *serverFQDN;
const char *user_realm;
const char *iplocalport;
const char *ipremoteport;
unsigned servicelen;
unsigned applen;
unsigned slen;
unsigned urlen;
unsigned iploclen;
unsigned ipremlen;
int log_level;
const sasl_utils_t *utils;
const sasl_callback_t *callbacks;
sasl_security_properties_t props;
sasl_ssf_t external_ssf;
int (*transition)(sasl_conn_t *conn, const char *pass, unsigned passlen);
int (*canon_user)(sasl_conn_t *conn,
const char *user, unsigned ulen,
unsigned flags,
sasl_out_params_t *oparams);
struct propctx *propctx;
const void *gss_creds;
const sasl_channel_binding_t *cbinding;
const sasl_http_request_t *http_request;
void *spare_ptr4;
int (*spare_fptr1)(void);
int (*spare_fptr2)(void);
int spare_int1;
int spare_int2;
int spare_int3;
unsigned flags;
int param_version;
} sasl_server_params_t;
#define SASL_LOG_NONE 0
#define SASL_LOG_ERR 1
#define SASL_LOG_FAIL 2
#define SASL_LOG_WARN 3
#define SASL_LOG_NOTE 4
#define SASL_LOG_DEBUG 5
#define SASL_LOG_TRACE 6
#define SASL_LOG_PASS 7
#define SASL_SET_REMOVE SASL_SET_CREATE
#define SASL_FEAT_SERVICE 0x0200
#define SASL_FEAT_GETSECRET 0x0400
typedef struct sasl_server_plug {
const char *mech_name;
sasl_ssf_t max_ssf;
unsigned security_flags;
unsigned features;
void *glob_context;
int (*mech_new)(void *glob_context,
sasl_server_params_t *sparams,
const char *challenge,
unsigned challen,
void **conn_context);
int (*mech_step)(void *conn_context,
sasl_server_params_t *sparams,
const char *clientin,
unsigned clientinlen,
const char **serverout,
unsigned *serveroutlen,
sasl_out_params_t *oparams);
void (*mech_dispose)(void *conn_context, const sasl_utils_t *utils);
void (*mech_free)(void *glob_context, const sasl_utils_t *utils);
int (*setpass)(void *glob_context,
sasl_server_params_t *sparams,
const char *user,
const char *pass, unsigned passlen,
const char *oldpass, unsigned oldpasslen,
unsigned flags);
int (*user_query)(void *glob_context,
sasl_server_params_t *sparams,
const char *user,
int maxmech,
const char **mechlist);
int (*idle)(void *glob_context,
void *conn_context,
sasl_server_params_t *sparams);
int (*mech_avail)(void *glob_context,
sasl_server_params_t *sparams,
void **conn_context);
int (*spare_fptr2)(void);
} sasl_server_plug_t;
#define SASL_SERVER_PLUG_VERSION 4
typedef int sasl_server_plug_init_t(const sasl_utils_t *utils,
int max_version,
int *out_version,
sasl_server_plug_t **pluglist,
int *plugcount);
LIBSASL_API int sasl_server_add_plugin(const char *plugname,
sasl_server_plug_init_t *splugfunc);
typedef struct server_sasl_mechanism
{
int version;
int condition;
char *plugname;
const sasl_server_plug_t *plug;
char *f;
} server_sasl_mechanism_t;
typedef void sasl_server_info_callback_t (server_sasl_mechanism_t *m,
sasl_info_callback_stage_t stage,
void *rock);
LIBSASL_API int sasl_server_plugin_info (const char *mech_list,
sasl_server_info_callback_t *info_cb,
void *info_cb_rock);
typedef struct sasl_canonuser {
int features;
int spare_int1;
void *glob_context;
char *name;
void (*canon_user_free)(void *glob_context, const sasl_utils_t *utils);
int (*canon_user_server)(void *glob_context,
sasl_server_params_t *sparams,
const char *user, unsigned len,
unsigned flags,
char *out,
unsigned out_umax, unsigned *out_ulen);
int (*canon_user_client)(void *glob_context,
sasl_client_params_t *cparams,
const char *user, unsigned len,
unsigned flags,
char *out,
unsigned out_max, unsigned *out_len);
int (*spare_fptr1)(void);
int (*spare_fptr2)(void);
int (*spare_fptr3)(void);
} sasl_canonuser_plug_t;
#define SASL_CANONUSER_PLUG_VERSION 5
typedef int sasl_canonuser_init_t(const sasl_utils_t *utils,
int max_version,
int *out_version,
sasl_canonuser_plug_t **plug,
const char *plugname);
LIBSASL_API int sasl_canonuser_add_plugin(const char *plugname,
sasl_canonuser_init_t *canonuserfunc);
typedef struct sasl_auxprop_plug {
int features;
int spare_int1;
void *glob_context;
void (*auxprop_free)(void *glob_context, const sasl_utils_t *utils);
int (*auxprop_lookup)(void *glob_context,
sasl_server_params_t *sparams,
unsigned flags,
const char *user, unsigned ulen);
char *name;
int (*auxprop_store)(void *glob_context,
sasl_server_params_t *sparams,
struct propctx *ctx,
const char *user, unsigned ulen);
} sasl_auxprop_plug_t;
#define SASL_AUXPROP_OVERRIDE 0x01
#define SASL_AUXPROP_AUTHZID 0x02
#define SASL_AUXPROP_VERIFY_AGAINST_HASH 0x10
#define SASL_AUXPROP_PLUG_VERSION 8
typedef int sasl_auxprop_init_t(const sasl_utils_t *utils,
int max_version,
int *out_version,
sasl_auxprop_plug_t **plug,
const char *plugname);
LIBSASL_API int sasl_auxprop_add_plugin(const char *plugname,
sasl_auxprop_init_t *auxpropfunc);
typedef void auxprop_info_callback_t (sasl_auxprop_plug_t *m,
sasl_info_callback_stage_t stage,
void *rock);
LIBSASL_API int auxprop_plugin_info (const char *mech_list,
auxprop_info_callback_t *info_cb,
void *info_cb_rock);
#ifdef __cplusplus
}
#endif
#endif