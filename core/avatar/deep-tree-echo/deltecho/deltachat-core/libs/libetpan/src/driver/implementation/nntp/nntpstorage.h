#ifndef NNTPSTORAGE_H
#define NNTPSTORAGE_H
#include <libetpan/nntpdriver_types.h>
#ifdef __cplusplus
extern "C" {
#endif
LIBETPAN_EXPORT
int nntp_mailstorage_init(struct mailstorage * storage,
const char * nntp_servername, uint16_t nntp_port,
const char * nntp_command,
int nntp_connection_type, int nntp_auth_type,
const char * nntp_login, const char * nntp_password,
int nntp_cached, const char * nntp_cache_directory,
const char * nntp_flags_directory);
LIBETPAN_EXPORT
int nntp_mailstorage_init_with_local_address(struct mailstorage * storage,
const char * nntp_servername, uint16_t nntp_port,
const char * nntp_local_servername, uint16_t nntp_local_port,
const char * nntp_command,
int nntp_connection_type, int nntp_auth_type,
const char * nntp_login, const char * nntp_password,
int nntp_cached, const char * nntp_cache_directory,
const char * nntp_flags_directory);
#ifdef __cplusplus
}
#endif
#endif