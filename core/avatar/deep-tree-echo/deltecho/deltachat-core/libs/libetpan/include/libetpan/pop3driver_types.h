#ifndef POP3DRIVER_TYPES_H
#define POP3DRIVER_TYPES_H
#include <libetpan/libetpan-config.h>
#include <libetpan/maildriver_types.h>
#include <libetpan/mailpop3.h>
#include <libetpan/maildriver_types.h>
#include <libetpan/chash.h>
#include <libetpan/mailstorage_types.h>
#ifdef __cplusplus
extern "C" {
#endif
enum {
POP3DRIVER_SET_AUTH_TYPE = 1
};
enum {
POP3DRIVER_AUTH_TYPE_PLAIN = 0,
POP3DRIVER_AUTH_TYPE_APOP,
POP3DRIVER_AUTH_TYPE_TRY_APOP
};
struct pop3_session_state_data {
int pop3_auth_type;
mailpop3 * pop3_session;
void (* pop3_ssl_callback)(struct mailstream_ssl_context * ssl_context, void * data);
void * pop3_ssl_cb_data;
};
enum {
POP3DRIVER_CACHED_SET_AUTH_TYPE = 1,
POP3DRIVER_CACHED_SET_SSL_CALLBACK = 2,
POP3DRIVER_CACHED_SET_SSL_CALLBACK_DATA = 3,
POP3DRIVER_CACHED_SET_CACHE_DIRECTORY = 1001,
POP3DRIVER_CACHED_SET_FLAGS_DIRECTORY = 1002
};
struct pop3_cached_session_state_data {
mailsession * pop3_ancestor;
char pop3_cache_directory[PATH_MAX];
char pop3_flags_directory[PATH_MAX];
chash * pop3_flags_hash;
carray * pop3_flags_array;
struct mail_flags_store * pop3_flags_store;
};
struct pop3_mailstorage {
char * pop3_servername;
uint16_t pop3_port;
char * pop3_command;
int pop3_connection_type;
int pop3_auth_type;
char * pop3_login;
char * pop3_password;
int pop3_cached;
char * pop3_cache_directory;
char * pop3_flags_directory;
struct {
int sasl_enabled;
char * sasl_auth_type;
char * sasl_server_fqdn;
char * sasl_local_ip_port;
char * sasl_remote_ip_port;
char * sasl_login;
char * sasl_auth_name;
char * sasl_password;
char * sasl_realm;
} pop3_sasl;
char * pop3_local_address;
uint16_t pop3_local_port;
};
enum {
POP3_AUTH_TYPE_PLAIN,
POP3_AUTH_TYPE_APOP,
POP3_AUTH_TYPE_TRY_APOP,
POP3_AUTH_TYPE_SASL_ANONYMOUS,
POP3_AUTH_TYPE_SASL_CRAM_MD5,
POP3_AUTH_TYPE_SASL_KERBEROS_V4,
POP3_AUTH_TYPE_SASL_PLAIN,
POP3_AUTH_TYPE_SASL_SCRAM_MD5,
POP3_AUTH_TYPE_SASL_GSSAPI,
POP3_AUTH_TYPE_SASL_DIGEST_MD5
};
#define POP3_SASL_AUTH_TYPE_APOP "X-LIBETPAN-APOP"
#define POP3_SASL_AUTH_TYPE_TRY_APOP "X-LIBETPAN-TRY-APOP"
#ifdef __cplusplus
}
#endif
#endif