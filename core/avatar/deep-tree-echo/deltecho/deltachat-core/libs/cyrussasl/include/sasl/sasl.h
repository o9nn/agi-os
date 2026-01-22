#ifndef SASL_H
#define SASL_H 1
#define SASL_VERSION_MAJOR 2
#define SASL_VERSION_MINOR 1
#define SASL_VERSION_STEP 26
#define SASL_VERSION_FULL ((SASL_VERSION_MAJOR << 16) |\
(SASL_VERSION_MINOR << 8) | SASL_VERSION_STEP)
#include "prop.h"
#define SASL_CONTINUE 1
#define SASL_OK 0
#define SASL_FAIL -1
#define SASL_NOMEM -2
#define SASL_BUFOVER -3
#define SASL_NOMECH -4
#define SASL_BADPROT -5
#define SASL_NOTDONE -6
#define SASL_BADPARAM -7
#define SASL_TRYAGAIN -8
#define SASL_BADMAC -9
#define SASL_NOTINIT -12
#define SASL_INTERACT 2
#define SASL_BADSERV -10
#define SASL_WRONGMECH -11
#define SASL_BADAUTH -13
#define SASL_NOAUTHZ -14
#define SASL_TOOWEAK -15
#define SASL_ENCRYPT -16
#define SASL_TRANS -17
#define SASL_EXPIRED -18
#define SASL_DISABLED -19
#define SASL_NOUSER -20
#define SASL_BADVERS -23
#define SASL_UNAVAIL -24
#define SASL_NOVERIFY -26
#define SASL_PWLOCK -21
#define SASL_NOCHANGE -22
#define SASL_WEAKPASS -27
#define SASL_NOUSERPASS -28
#define SASL_NEED_OLD_PASSWD -29
#define SASL_CONSTRAINT_VIOLAT -30
#define SASL_BADBINDING -32
#define SASL_MECHNAMEMAX 20
#ifdef _WIN32
#ifndef STRUCT_IOVEC_DEFINED
#define STRUCT_IOVEC_DEFINED 1
struct iovec {
long iov_len;
char *iov_base;
};
#endif
#else
struct iovec;
#endif
typedef struct sasl_conn sasl_conn_t;
typedef struct sasl_secret {
unsigned long len;
unsigned char data[1];
} sasl_secret_t;
typedef struct sasl_rand_s sasl_rand_t;
#ifdef __cplusplus
extern "C" {
#endif
typedef void *sasl_malloc_t(size_t);
typedef void *sasl_calloc_t(size_t, size_t);
typedef void *sasl_realloc_t(void *, size_t);
typedef void sasl_free_t(void *);
LIBSASL_API void sasl_set_alloc(sasl_malloc_t *,
sasl_calloc_t *,
sasl_realloc_t *,
sasl_free_t *);
typedef void *sasl_mutex_alloc_t(void);
typedef int sasl_mutex_lock_t(void *mutex);
typedef int sasl_mutex_unlock_t(void *mutex);
typedef void sasl_mutex_free_t(void *mutex);
LIBSASL_API void sasl_set_mutex(sasl_mutex_alloc_t *, sasl_mutex_lock_t *,
sasl_mutex_unlock_t *, sasl_mutex_free_t *);
typedef unsigned sasl_ssf_t;
#define SASL_SUCCESS_DATA 0x0004
#define SASL_NEED_PROXY 0x0008
#define SASL_NEED_HTTP 0x0010
#define SASL_SEC_NOPLAINTEXT 0x0001
#define SASL_SEC_NOACTIVE 0x0002
#define SASL_SEC_NODICTIONARY 0x0004
#define SASL_SEC_FORWARD_SECRECY 0x0008
#define SASL_SEC_NOANONYMOUS 0x0010
#define SASL_SEC_PASS_CREDENTIALS 0x0020
#define SASL_SEC_MUTUAL_AUTH 0x0040
#define SASL_SEC_MAXIMUM 0x00FF
typedef struct sasl_security_properties
{
sasl_ssf_t min_ssf;
sasl_ssf_t max_ssf;
unsigned maxbufsize;
unsigned security_flags;
const char **property_names;
const char **property_values;
} sasl_security_properties_t;
typedef struct sasl_callback {
unsigned long id;
int (*proc)(void);
void *context;
} sasl_callback_t;
#define SASL_CB_LIST_END 0
typedef int sasl_getopt_t(void *context, const char *plugin_name,
const char *option,
const char **result, unsigned *len);
#define SASL_CB_GETOPT 1
#define SASL_LOG_NONE 0
#define SASL_LOG_ERR 1
#define SASL_LOG_FAIL 2
#define SASL_LOG_WARN 3
#define SASL_LOG_NOTE 4
#define SASL_LOG_DEBUG 5
#define SASL_LOG_TRACE 6
#define SASL_LOG_PASS 7
typedef int sasl_log_t(void *context,
int level,
const char *message);
#define SASL_CB_LOG 2
typedef int sasl_getpath_t(void *context,
const char **path);
#define SASL_CB_GETPATH 3
typedef enum {
SASL_VRFY_PLUGIN=0,
SASL_VRFY_CONF=1,
SASL_VRFY_PASSWD=2,
SASL_VRFY_OTHER=3
} sasl_verify_type_t;
typedef int sasl_verifyfile_t(void *context,
const char *file, sasl_verify_type_t type);
#define SASL_CB_VERIFYFILE 4
typedef int sasl_getconfpath_t(void *context,
char **path);
#define SASL_CB_GETCONFPATH 5
typedef int sasl_getsimple_t(void *context, int id,
const char **result, unsigned *len);
#define SASL_CB_USER 0x4001
#define SASL_CB_AUTHNAME 0x4002
#define SASL_CB_LANGUAGE 0x4003
#define SASL_CB_CNONCE 0x4007
typedef int sasl_getsecret_t(sasl_conn_t *conn, void *context, int id,
sasl_secret_t **psecret);
#define SASL_CB_PASS 0x4004
typedef int sasl_chalprompt_t(void *context, int id,
const char *challenge,
const char *prompt, const char *defresult,
const char **result, unsigned *len);
#define SASL_CB_ECHOPROMPT 0x4005
#define SASL_CB_NOECHOPROMPT 0x4006
typedef int sasl_getrealm_t(void *context, int id,
const char **availrealms,
const char **result);
#define SASL_CB_GETREALM (0x4008)
typedef int sasl_authorize_t(sasl_conn_t *conn,
void *context,
const char *requested_user, unsigned rlen,
const char *auth_identity, unsigned alen,
const char *def_realm, unsigned urlen,
struct propctx *propctx);
#define SASL_CB_PROXY_POLICY 0x8001
typedef int sasl_server_userdb_checkpass_t(sasl_conn_t *conn,
void *context,
const char *user,
const char *pass,
unsigned passlen,
struct propctx *propctx);
#define SASL_CB_SERVER_USERDB_CHECKPASS (0x8005)
typedef int sasl_server_userdb_setpass_t(sasl_conn_t *conn,
void *context,
const char *user,
const char *pass,
unsigned passlen,
struct propctx *propctx,
unsigned flags);
#define SASL_CB_SERVER_USERDB_SETPASS (0x8006)
#define SASL_CU_NONE 0x00
#define SASL_CU_AUTHID 0x01
#define SASL_CU_AUTHZID 0x02
#define SASL_CU_EXTERNALLY_VERIFIED 0x04
#define SASL_CU_OVERRIDE 0x08
#define SASL_CU_ASIS_MASK 0xFFF0
#define SASL_CU_VERIFY_AGAINST_HASH 0x10
typedef int sasl_canon_user_t(sasl_conn_t *conn,
void *context,
const char *in, unsigned inlen,
unsigned flags,
const char *user_realm,
char *out,
unsigned out_max, unsigned *out_len);
#define SASL_CB_CANON_USER (0x8007)
#define SASL_PATH_TYPE_PLUGIN 0
#define SASL_PATH_TYPE_CONFIG 1
LIBSASL_API int sasl_set_path (int path_type, char * path);
LIBSASL_API void sasl_version(const char **implementation,
int *version);
LIBSASL_API void sasl_version_info (const char **implementation,
const char **version_string,
int *version_major,
int *version_minor,
int *version_step,
int *version_patch);
LIBSASL_API void sasl_done(void);
LIBSASL_API int sasl_server_done(void);
LIBSASL_API int sasl_client_done(void);
LIBSASL_API void sasl_dispose(sasl_conn_t **pconn);
LIBSASL_API const char *sasl_errstring(int saslerr,
const char *langlist,
const char **outlang);
LIBSASL_API const char *sasl_errdetail(sasl_conn_t *conn);
LIBSASL_API void sasl_seterror(sasl_conn_t *conn, unsigned flags,
const char *fmt, ...);
#define SASL_NOLOG 0x01
LIBSASL_API int sasl_getprop(sasl_conn_t *conn, int propnum,
const void **pvalue);
#define SASL_USERNAME 0
#define SASL_SSF 1
#define SASL_MAXOUTBUF 2
#define SASL_DEFUSERREALM 3
#define SASL_GETOPTCTX 4
#define SASL_CALLBACK 7
#define SASL_IPLOCALPORT 8
#define SASL_IPREMOTEPORT 9
#define SASL_PLUGERR 10
#define SASL_DELEGATEDCREDS 11
#define SASL_SERVICE 12
#define SASL_SERVERFQDN 13
#define SASL_AUTHSOURCE 14
#define SASL_MECHNAME 15
#define SASL_AUTHUSER 16
#define SASL_APPNAME 17
#define SASL_GSS_CREDS 18
#define SASL_GSS_PEER_NAME 19
#define SASL_GSS_LOCAL_NAME 20
typedef struct sasl_channel_binding {
const char *name;
int critical;
unsigned long len;
const unsigned char *data;
} sasl_channel_binding_t;
#define SASL_CHANNEL_BINDING 21
typedef struct sasl_http_request {
const char *method;
const char *uri;
const unsigned char *entity;
unsigned long elen;
unsigned non_persist;
} sasl_http_request_t;
#define SASL_HTTP_REQUEST 22
LIBSASL_API int sasl_setprop(sasl_conn_t *conn,
int propnum,
const void *value);
#define SASL_SSF_EXTERNAL 100
#define SASL_SEC_PROPS 101
#define SASL_AUTH_EXTERNAL 102
LIBSASL_API int sasl_idle(sasl_conn_t *conn);
typedef struct sasl_interact {
unsigned long id;
const char *challenge;
const char *prompt;
const char *defresult;
const void *result;
unsigned len;
} sasl_interact_t;
LIBSASL_API int sasl_client_init(const sasl_callback_t *callbacks);
LIBSASL_API int sasl_client_new(const char *service,
const char *serverFQDN,
const char *iplocalport,
const char *ipremoteport,
const sasl_callback_t *prompt_supp,
unsigned flags,
sasl_conn_t **pconn);
LIBSASL_API int sasl_client_start(sasl_conn_t *conn,
const char *mechlist,
sasl_interact_t **prompt_need,
const char **clientout,
unsigned *clientoutlen,
const char **mech);
LIBSASL_API int sasl_client_step(sasl_conn_t *conn,
const char *serverin,
unsigned serverinlen,
sasl_interact_t **prompt_need,
const char **clientout,
unsigned *clientoutlen);
LIBSASL_API int sasl_server_init(const sasl_callback_t *callbacks,
const char *appname);
LIBSASL_API int sasl_server_new(const char *service,
const char *serverFQDN,
const char *user_realm,
const char *iplocalport,
const char *ipremoteport,
const sasl_callback_t *callbacks,
unsigned flags,
sasl_conn_t **pconn);
LIBSASL_API const char ** sasl_global_listmech(void);
LIBSASL_API int sasl_listmech(sasl_conn_t *conn,
const char *user,
const char *prefix,
const char *sep,
const char *suffix,
const char **result,
unsigned *plen,
int *pcount);
LIBSASL_API int sasl_server_start(sasl_conn_t *conn,
const char *mech,
const char *clientin,
unsigned clientinlen,
const char **serverout,
unsigned *serveroutlen);
LIBSASL_API int sasl_server_step(sasl_conn_t *conn,
const char *clientin,
unsigned clientinlen,
const char **serverout,
unsigned *serveroutlen);
LIBSASL_API int sasl_checkapop(sasl_conn_t *conn,
const char *challenge, unsigned challen,
const char *response, unsigned resplen);
LIBSASL_API int sasl_checkpass(sasl_conn_t *conn,
const char *user, unsigned userlen,
const char *pass, unsigned passlen);
LIBSASL_API int sasl_user_exists(sasl_conn_t *conn,
const char *service,
const char *user_realm,
const char *user);
LIBSASL_API int sasl_setpass(sasl_conn_t *conn,
const char *user,
const char *pass, unsigned passlen,
const char *oldpass, unsigned oldpasslen,
unsigned flags);
#define SASL_SET_CREATE 0x01
#define SASL_SET_DISABLE 0x02
#define SASL_SET_NOPLAIN 0x04
#define SASL_SET_CURMECH_ONLY 0x08
#define SASL_AUX_END NULL
#define SASL_AUX_ALL "*"
#define SASL_AUX_PASSWORD_PROP "userPassword"
#define SASL_AUX_PASSWORD "*" SASL_AUX_PASSWORD_PROP
#define SASL_AUX_UIDNUM "uidNumber"
#define SASL_AUX_GIDNUM "gidNumber"
#define SASL_AUX_FULLNAME "gecos"
#define SASL_AUX_HOMEDIR "homeDirectory"
#define SASL_AUX_SHELL "loginShell"
#define SASL_AUX_MAILADDR "mail"
#define SASL_AUX_UNIXMBX "mailMessageStore"
#define SASL_AUX_MAILCHAN "mailSMTPSubmitChannel"
LIBSASL_API int sasl_auxprop_request(sasl_conn_t *conn,
const char **propnames);
LIBSASL_API struct propctx *sasl_auxprop_getctx(sasl_conn_t *conn);
LIBSASL_API int sasl_auxprop_store(sasl_conn_t *conn,
struct propctx *ctx, const char *user);
LIBSASL_API int sasl_encode(sasl_conn_t *conn,
const char *input, unsigned inputlen,
const char **output, unsigned *outputlen);
LIBSASL_API int sasl_encodev(sasl_conn_t *conn,
const struct iovec *invec, unsigned numiov,
const char **output, unsigned *outputlen);
LIBSASL_API int sasl_decode(sasl_conn_t *conn,
const char *input, unsigned inputlen,
const char **output, unsigned *outputlen);
#ifdef __cplusplus
}
#endif
#endif