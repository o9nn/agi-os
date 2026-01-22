#ifndef IMAPDRIVER_TYPES_H
#define IMAPDRIVER_TYPES_H
#include <libetpan/libetpan-config.h>
#include <libetpan/mailimap.h>
#include <libetpan/maildriver_types.h>
#include <libetpan/generic_cache_types.h>
#include <libetpan/mailstorage_types.h>
#ifdef __cplusplus
extern "C" {
#endif
struct imap_session_state_data {
mailimap * imap_session;
char * imap_mailbox;
struct mail_flags_store * imap_flags_store;
void (* imap_ssl_callback)(struct mailstream_ssl_context * ssl_context, void * data);
void * imap_ssl_cb_data;
};
enum {
IMAP_SECTION_MESSAGE,
IMAP_SECTION_HEADER,
IMAP_SECTION_MIME,
IMAP_SECTION_BODY
};
enum {
IMAPDRIVER_CACHED_SET_SSL_CALLBACK = 1,
IMAPDRIVER_CACHED_SET_SSL_CALLBACK_DATA = 2,
IMAPDRIVER_CACHED_SET_CACHE_DIRECTORY = 1001
};
struct imap_cached_session_state_data {
mailsession * imap_ancestor;
char * imap_quoted_mb;
char imap_cache_directory[PATH_MAX];
carray * imap_uid_list;
uint32_t imap_uidvalidity;
};
struct imap_mailstorage {
char * imap_servername;
uint16_t imap_port;
char * imap_command;
int imap_connection_type;
int imap_auth_type;
char * imap_login;
char * imap_password;
int imap_cached;
char * imap_cache_directory;
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
} imap_sasl;
char * imap_local_address;
uint16_t imap_local_port;
};
enum {
IMAP_AUTH_TYPE_PLAIN,
IMAP_AUTH_TYPE_SASL_ANONYMOUS,
IMAP_AUTH_TYPE_SASL_CRAM_MD5,
IMAP_AUTH_TYPE_SASL_KERBEROS_V4,
IMAP_AUTH_TYPE_SASL_PLAIN,
IMAP_AUTH_TYPE_SASL_SCRAM_MD5,
IMAP_AUTH_TYPE_SASL_GSSAPI,
IMAP_AUTH_TYPE_SASL_DIGEST_MD5
};
#ifdef __cplusplus
}
#endif
#endif