#ifdef HAVE_CONFIG_H
#	include <config.h>
#endif
#include "maildirstorage.h"
#include "mailstorage.h"
#include "mail.h"
#include "mailmessage.h"
#include "maildirdriver.h"
#include "maildirdriver_cached.h"
#include "maildriver.h"
#include <stdlib.h>
#include <string.h>
static int maildir_mailstorage_connect(struct mailstorage * storage);
static int
maildir_mailstorage_get_folder_session(struct mailstorage * storage,
char * pathname, mailsession ** result);
static void maildir_mailstorage_uninitialize(struct mailstorage * storage);
static mailstorage_driver maildir_mailstorage_driver = {
"maildir",
maildir_mailstorage_connect,
maildir_mailstorage_get_folder_session,
maildir_mailstorage_uninitialize
};
LIBETPAN_EXPORT
int maildir_mailstorage_init(struct mailstorage * storage,
const char * md_pathname, int md_cached,
const char * md_cache_directory, const char * md_flags_directory)
{
struct maildir_mailstorage * maildir_storage;
maildir_storage = malloc(sizeof(* maildir_storage));
if (maildir_storage == NULL)
goto err;
maildir_storage->md_pathname = strdup(md_pathname);
if (maildir_storage->md_pathname == NULL)
goto free;
maildir_storage->md_cached = md_cached;
if (md_cached && (md_cache_directory != NULL) &&
(md_flags_directory != NULL)) {
maildir_storage->md_cache_directory = strdup(md_cache_directory);
if (maildir_storage->md_cache_directory == NULL)
goto free_pathname;
maildir_storage->md_flags_directory = strdup(md_flags_directory);
if (maildir_storage->md_flags_directory == NULL)
goto free_cache_directory;
}
else {
maildir_storage->md_cached = FALSE;
maildir_storage->md_cache_directory = NULL;
maildir_storage->md_flags_directory = NULL;
}
storage->sto_data = maildir_storage;
storage->sto_driver = &maildir_mailstorage_driver;
return MAIL_NO_ERROR;
free_cache_directory:
free(maildir_storage->md_cache_directory);
free_pathname:
free(maildir_storage->md_pathname);
free:
free(maildir_storage);
err:
return MAIL_ERROR_MEMORY;
}
static void maildir_mailstorage_uninitialize(struct mailstorage * storage)
{
struct maildir_mailstorage * maildir_storage;
maildir_storage = storage->sto_data;
if (maildir_storage->md_flags_directory != NULL)
free(maildir_storage->md_flags_directory);
if (maildir_storage->md_cache_directory != NULL)
free(maildir_storage->md_cache_directory);
free(maildir_storage->md_pathname);
free(maildir_storage);
storage->sto_data = NULL;
}
static int maildir_mailstorage_connect(struct mailstorage * storage)
{
struct maildir_mailstorage * maildir_storage;
mailsession_driver * driver;
int r;
int res;
mailsession * session;
maildir_storage = storage->sto_data;
if (maildir_storage->md_cached)
driver = maildir_cached_session_driver;
else
driver = maildir_session_driver;
session = mailsession_new(driver);
if (session == NULL) {
res = MAIL_ERROR_MEMORY;
goto err;
}
if (maildir_storage->md_cached) {
r = mailsession_parameters(session,
MAILDIRDRIVER_CACHED_SET_CACHE_DIRECTORY,
maildir_storage->md_cache_directory);
if (r != MAIL_NO_ERROR) {
res = r;
goto free;
}
r = mailsession_parameters(session,
MAILDIRDRIVER_CACHED_SET_FLAGS_DIRECTORY,
maildir_storage->md_flags_directory);
if (r != MAIL_NO_ERROR) {
res = r;
goto free;
}
}
r = mailsession_connect_path(session, maildir_storage->md_pathname);
switch (r) {
case MAIL_NO_ERROR_NON_AUTHENTICATED:
case MAIL_NO_ERROR_AUTHENTICATED:
case MAIL_NO_ERROR:
break;
default:
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
static int
maildir_mailstorage_get_folder_session(struct mailstorage * storage,
char * pathname, mailsession ** result)
{
* result = storage->sto_session;
return MAIL_NO_ERROR;
}