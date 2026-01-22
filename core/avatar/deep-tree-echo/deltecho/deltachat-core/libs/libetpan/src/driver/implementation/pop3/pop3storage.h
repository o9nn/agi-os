#ifndef POP3STORAGE_H
#define POP3STORAGE_H
#ifdef __cplusplus
extern "C" {
#endif
#include <libetpan/pop3driver_types.h>
#include <libetpan/pop3driver.h>
#include <libetpan/pop3driver_cached.h>
LIBETPAN_EXPORT
int pop3_mailstorage_init(struct mailstorage * storage,
const char * pop3_servername, uint16_t pop3_port,
const char * pop3_command,
int pop3_connection_type, int pop3_auth_type,
const char * pop3_login, const char * pop3_password,
int pop3_cached, const char * pop3_cache_directory,
const char * pop3_flags_directory);
LIBETPAN_EXPORT
int pop3_mailstorage_init_sasl(struct mailstorage * storage,
const char * pop3_servername, uint16_t pop3_port,
const char * pop3_command,
int pop3_connection_type,
const char * auth_type,
const char * server_fqdn,
const char * local_ip_port,
const char * remote_ip_port,
const char * login, const char * auth_name,
const char * password, const char * realm,
int pop3_cached, const char * pop3_cache_directory,
const char * pop3_flags_directory);
LIBETPAN_EXPORT
int pop3_mailstorage_init_sasl_with_local_address(struct mailstorage * storage,
const char * pop3_servername, uint16_t pop3_port,
const char * imap_local_address, uint16_t imap_local_port,
const char * pop3_command,
int pop3_connection_type,
const char * auth_type,
const char * server_fqdn,
const char * local_ip_port,
const char * remote_ip_port,
const char * login, const char * auth_name,
const char * password, const char * realm,
int pop3_cached, const char * pop3_cache_directory,
const char * pop3_flags_directory);
#ifdef __cplusplus
}
#endif
#endif