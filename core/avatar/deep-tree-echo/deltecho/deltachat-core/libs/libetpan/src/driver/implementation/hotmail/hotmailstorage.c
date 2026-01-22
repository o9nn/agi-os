#ifdef HAVE_CONFIG_H
#	include <config.h>
#endif
#include "hotmailstorage.h"
#include "pop3storage.h"
#include "pop3driver_types.h"
static char hotway_command[512] = "/usr/bin/hotwayd";
LIBETPAN_EXPORT
int hotmail_mailstorage_init(struct mailstorage * storage,
char * hotmail_login, char * hotmail_password,
int hotmail_cached, char * hotmail_cache_directory,
char * hotmail_flags_directory)
{
return pop3_mailstorage_init(storage,
"hotmail.dummy", 0,
hotway_command,
CONNECTION_TYPE_COMMAND, POP3_AUTH_TYPE_PLAIN,
hotmail_login, hotmail_password,
hotmail_cached, hotmail_cache_directory,
hotmail_flags_directory);
}