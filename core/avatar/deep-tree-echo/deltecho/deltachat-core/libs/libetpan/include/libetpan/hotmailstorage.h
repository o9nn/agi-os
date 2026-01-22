#ifndef HOTMAILSTORAGE_H
#define HOTMAILSTORAGE_H
#ifdef __cplusplus
extern "C" {
#endif
#include "mailstorage_types.h"
LIBETPAN_EXPORT
int hotmail_mailstorage_init(struct mailstorage * storage,
char * hotmail_login, char * hotmail_password,
int hotmail_cached, char * hotmail_cache_directory,
char * hotmail_flags_directory);
#ifdef __cplusplus
}
#endif
#endif