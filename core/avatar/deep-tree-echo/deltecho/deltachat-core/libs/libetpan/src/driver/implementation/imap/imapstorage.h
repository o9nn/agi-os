#ifndef IMAPSTORAGE_H
#define IMAPSTORAGE_H
#ifdef __cplusplus
extern "C" {
#endif
#include <libetpan/imapdriver_types.h>
LIBETPAN_EXPORT
int imap_mailstorage_init(struct mailstorage * storage,
const char * imap_servername, uint16_t imap_port,
const char * imap_command,
int imap_connection_type, int imap_auth_type,
const char * imap_login, const char * imap_password,
int imap_cached, const char * imap_cache_directory);
LIBETPAN_EXPORT
int imap_mailstorage_init_sasl(struct mailstorage * storage,
const char * imap_servername, uint16_t imap_port,
const char * imap_command,
int imap_connection_type,
const char * auth_type,
const char * server_fqdn,
const char * local_ip_port,
const char * remote_ip_port,
const char * login, const char * auth_name,
const char * password, const char * realm,
int imap_cached, const char * imap_cache_directory);
LIBETPAN_EXPORT
int imap_mailstorage_init_sasl_with_local_address(struct mailstorage * storage,
const char * imap_servername, uint16_t imap_port,
const char * imap_local_address, uint16_t imap_local_port,
const char * imap_command,
int imap_connection_type,
const char * auth_type,
const char * server_fqdn,
const char * local_ip_port,
const char * remote_ip_port,
const char * login, const char * auth_name,
const char * password, const char * realm,
int imap_cached, const char * imap_cache_directory);
#ifdef __cplusplus
}
#endif
#endif