#include "mailstorage.h"
#ifndef MAILSTORAGE_TOOLS_H
#define MAILSTORAGE_TOOLS_H
#ifdef __cplusplus
extern "C" {
#endif
int mailstorage_generic_connect(mailsession_driver * driver,
char * servername,
uint16_t port,
char * command,
int connection_type,
int cache_function_id,
char * cache_directory,
int flags_function_id,
char * flags_directory,
mailsession ** result);
int mailstorage_generic_connect_with_local_address(mailsession_driver * driver,
char * servername,
uint16_t port,
char * local_address,
uint16_t local_port,
char * command,
int connection_type,
int cache_function_id,
char * cache_directory,
int flags_function_id,
char * flags_directory,
mailsession ** result);
int mailstorage_generic_auth(mailsession * session,
int connect_result,
int auth_type,
char * login,
char * password);
int mailstorage_generic_auth_sasl(mailsession * session,
int connect_result,
const char * auth_type,
const char * server_fqdn,
const char * local_ip_port,
const char * remote_ip_port,
const char * login, const char * auth_name,
const char * password, const char * realm);
#ifdef __cplusplus
}
#endif
#endif