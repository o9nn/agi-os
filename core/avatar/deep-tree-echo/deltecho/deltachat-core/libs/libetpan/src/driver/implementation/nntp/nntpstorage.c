#ifdef HAVE_CONFIG_H
# include <config.h>
#endif
#include "nntpstorage.h"
#include <stdlib.h>
#include <string.h>
#include "maildriver.h"
#include "nntpdriver.h"
#include "nntpdriver_cached.h"
#include "mailstorage_tools.h"
#include "mail.h"
#define NNTP_DEFAULT_PORT 119
#define NNTPS_DEFAULT_PORT 563
static int nntp_mailstorage_connect(struct mailstorage * storage);
static int nntp_mailstorage_get_folder_session(struct mailstorage * storage,
char * pathname, mailsession ** result);
static void nntp_mailstorage_uninitialize(struct mailstorage * storage);
static mailstorage_driver nntp_mailstorage_driver = {
"nntp",
nntp_mailstorage_connect,
nntp_mailstorage_get_folder_session,
nntp_mailstorage_uninitialize
};
LIBETPAN_EXPORT
int nntp_mailstorage_init(struct mailstorage * storage,
const char * nntp_servername, uint16_t nntp_port,
const char * nntp_command,
int nntp_connection_type, int nntp_auth_type,
const char * nntp_login, const char * nntp_password,
int nntp_cached,
const char * nntp_cache_directory, const char * nntp_flags_directory)
{
return nntp_mailstorage_init_with_local_address(storage,
nntp_servername, nntp_port,
NULL, 0,
nntp_command,
nntp_connection_type, nntp_auth_type,
nntp_login, nntp_password,
nntp_cached, nntp_cache_directory,
nntp_flags_directory);
}
LIBETPAN_EXPORT
int nntp_mailstorage_init_with_local_address(struct mailstorage * storage,
const char * nntp_servername, uint16_t nntp_port,
const char * nntp_local_address, uint16_t nntp_local_port,
const char * nntp_command,
int nntp_connection_type, int nntp_auth_type,
const char * nntp_login, const char * nntp_password,
int nntp_cached, const char * nntp_cache_directory,
const char * nntp_flags_directory)
{
struct nntp_mailstorage * nntp_storage;
int res;
nntp_storage = malloc(sizeof(* nntp_storage));
if (nntp_storage == NULL) {
res = MAIL_ERROR_MEMORY;
goto err;
}
if (nntp_servername != NULL) {
nntp_storage->nntp_servername = strdup(nntp_servername);
if (nntp_storage->nntp_servername == NULL) {
res = MAIL_ERROR_MEMORY;
goto free;
}
}
else {
nntp_storage->nntp_servername = NULL;
}
if (nntp_local_address != NULL) {
nntp_storage->nntp_local_address = strdup(nntp_local_address);
if (nntp_storage->nntp_local_address == NULL) {
res = MAIL_ERROR_MEMORY;
goto free_servername;
}
}
else {
nntp_storage->nntp_local_address = NULL;
}
nntp_storage->nntp_local_port = nntp_local_port;
nntp_storage->nntp_connection_type = nntp_connection_type;
if (nntp_port == 0) {
switch (nntp_connection_type) {
case CONNECTION_TYPE_PLAIN:
case CONNECTION_TYPE_COMMAND:
nntp_port = NNTP_DEFAULT_PORT;
break;
case CONNECTION_TYPE_TLS:
case CONNECTION_TYPE_COMMAND_TLS:
nntp_port = NNTPS_DEFAULT_PORT;
break;
default:
nntp_port = NNTP_DEFAULT_PORT;
break;
}
}
nntp_storage->nntp_port = nntp_port;
if (nntp_command != NULL) {
nntp_storage->nntp_command = strdup(nntp_command);
if (nntp_storage->nntp_command == NULL) {
res = MAIL_ERROR_MEMORY;
goto free_local_address;
}
}
else
nntp_storage->nntp_command = NULL;
nntp_storage->nntp_auth_type = nntp_auth_type;
if (nntp_login != NULL) {
nntp_storage->nntp_login = strdup(nntp_login);
if (nntp_storage->nntp_login == NULL) {
res = MAIL_ERROR_MEMORY;
goto free_command;
}
}
else
nntp_storage->nntp_login = NULL;
if (nntp_password != NULL) {
nntp_storage->nntp_password = strdup(nntp_password);
if (nntp_storage->nntp_password == NULL) {
res = MAIL_ERROR_MEMORY;
goto free_login;
}
}
else
nntp_storage->nntp_password = NULL;
nntp_storage->nntp_cached = nntp_cached;
if (nntp_cached && (nntp_cache_directory != NULL) &&
(nntp_flags_directory != NULL)) {
nntp_storage->nntp_cache_directory = strdup(nntp_cache_directory);
if (nntp_storage->nntp_cache_directory == NULL) {
res = MAIL_ERROR_MEMORY;
goto free_password;
}
nntp_storage->nntp_flags_directory = strdup(nntp_flags_directory);
if (nntp_storage->nntp_flags_directory == NULL) {
res = MAIL_ERROR_MEMORY;
goto free_cache_directory;
}
}
else {
nntp_storage->nntp_cached = FALSE;
nntp_storage->nntp_cache_directory = NULL;
nntp_storage->nntp_flags_directory = NULL;
}
storage->sto_data = nntp_storage;
storage->sto_driver = &nntp_mailstorage_driver;
return MAIL_NO_ERROR;
free_cache_directory:
free(nntp_storage->nntp_cache_directory);
free_password:
free(nntp_storage->nntp_password);
free_login:
free(nntp_storage->nntp_login);
free_command:
free(nntp_storage->nntp_command);
free_local_address:
free(nntp_storage->nntp_local_address);
free_servername:
free(nntp_storage->nntp_servername);
free:
free(nntp_storage);
err:
return res;
}
static void nntp_mailstorage_uninitialize(struct mailstorage * storage)
{
struct nntp_mailstorage * nntp_storage;
nntp_storage = storage->sto_data;
free(nntp_storage->nntp_flags_directory);
free(nntp_storage->nntp_cache_directory);
free(nntp_storage->nntp_password);
free(nntp_storage->nntp_login);
free(nntp_storage->nntp_command);
free(nntp_storage->nntp_local_address);
free(nntp_storage->nntp_servername);
free(nntp_storage);
storage->sto_data = NULL;
}
static int nntp_mailstorage_connect(struct mailstorage * storage)
{
struct nntp_mailstorage * nntp_storage;
mailsession_driver * driver;
int r;
int res;
mailsession * session;
nntp_storage = storage->sto_data;
if (nntp_storage->nntp_cached)
driver = nntp_cached_session_driver;
else
driver = nntp_session_driver;
r = mailstorage_generic_connect_with_local_address(driver,
nntp_storage->nntp_servername, nntp_storage->nntp_port,
nntp_storage->nntp_local_address, nntp_storage->nntp_local_port,
nntp_storage->nntp_command,
nntp_storage->nntp_connection_type,
NNTPDRIVER_CACHED_SET_CACHE_DIRECTORY,
nntp_storage->nntp_cache_directory,
NNTPDRIVER_CACHED_SET_FLAGS_DIRECTORY,
nntp_storage->nntp_flags_directory,
&session);
switch (r) {
case MAIL_NO_ERROR_NON_AUTHENTICATED:
case MAIL_NO_ERROR_AUTHENTICATED:
case MAIL_NO_ERROR:
break;
default:
res = r;
goto err;
}
r = mailstorage_generic_auth(session, r,
nntp_storage->nntp_connection_type,
nntp_storage->nntp_login,
nntp_storage->nntp_password);
if (r != MAIL_NO_ERROR) {
res = r;
goto free;
}
storage->sto_session = session;
return MAIL_NO_ERROR;
free:
mailsession_free(session);
err:
return res;
}
static int nntp_mailstorage_get_folder_session(struct mailstorage * storage,
char * pathname, mailsession ** result)
{
mailsession_select_folder(storage->sto_session, pathname);
* result = storage->sto_session;
return MAIL_NO_ERROR;
}