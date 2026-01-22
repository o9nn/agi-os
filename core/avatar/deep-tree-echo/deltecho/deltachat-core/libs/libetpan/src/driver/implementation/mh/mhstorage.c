#ifdef HAVE_CONFIG_H
#	include <config.h>
#endif
#include "mhstorage.h"
#include "mhdriver.h"
#include "mhdriver_cached.h"
#include "mail.h"
#include <stdlib.h>
#include <string.h>
static int mh_mailstorage_connect(struct mailstorage * storage);
static int mh_mailstorage_get_folder_session(struct mailstorage * storage,
char * pathname, mailsession ** result);
static void mh_mailstorage_uninitialize(struct mailstorage * storage);
static mailstorage_driver mh_mailstorage_driver = {
"mh",
mh_mailstorage_connect,
mh_mailstorage_get_folder_session,
mh_mailstorage_uninitialize
};
LIBETPAN_EXPORT
int mh_mailstorage_init(struct mailstorage * storage,
const char * mh_pathname, int mh_cached,
const char * mh_cache_directory, const char * mh_flags_directory)
{
struct mh_mailstorage * mh_storage;
mh_storage = malloc(sizeof(* mh_storage));
if (mh_storage == NULL)
goto err;
mh_storage->mh_pathname = strdup(mh_pathname);
if (mh_storage->mh_pathname == NULL)
goto free;
mh_storage->mh_cached = mh_cached;
if (mh_cached && (mh_cache_directory != NULL) &&
(mh_flags_directory != NULL)) {
mh_storage->mh_cache_directory = strdup(mh_cache_directory);
if (mh_storage->mh_cache_directory == NULL)
goto free_pathname;
mh_storage->mh_flags_directory = strdup(mh_flags_directory);
if (mh_storage->mh_flags_directory == NULL)
goto free_cache_directory;
}
else {
mh_storage->mh_cached = FALSE;
mh_storage->mh_cache_directory = NULL;
mh_storage->mh_flags_directory = NULL;
}
storage->sto_data = mh_storage;
storage->sto_driver = &mh_mailstorage_driver;
return MAIL_NO_ERROR;
free_cache_directory:
free(mh_storage->mh_cache_directory);
free_pathname:
free(mh_storage->mh_pathname);
free:
free(mh_storage);
err:
return MAIL_ERROR_MEMORY;
}
static void mh_mailstorage_uninitialize(struct mailstorage * storage)
{
struct mh_mailstorage * mh_storage;
mh_storage = storage->sto_data;
if (mh_storage->mh_flags_directory != NULL)
free(mh_storage->mh_flags_directory);
if (mh_storage->mh_cache_directory != NULL)
free(mh_storage->mh_cache_directory);
free(mh_storage->mh_pathname);
free(mh_storage);
storage->sto_data = NULL;
}
static int mh_mailstorage_connect(struct mailstorage * storage)
{
struct mh_mailstorage * mh_storage;
mailsession_driver * driver;
int r;
int res;
mailsession * session;
mh_storage = storage->sto_data;
if (mh_storage->mh_cached)
driver = mh_cached_session_driver;
else
driver = mh_session_driver;
session = mailsession_new(driver);
if (session == NULL) {
res = MAIL_ERROR_MEMORY;
goto err;
}
if (mh_storage->mh_cached) {
r = mailsession_parameters(session,
MHDRIVER_CACHED_SET_CACHE_DIRECTORY,
mh_storage->mh_cache_directory);
if (r != MAIL_NO_ERROR) {
res = r;
goto free;
}
r = mailsession_parameters(session,
MHDRIVER_CACHED_SET_FLAGS_DIRECTORY,
mh_storage->mh_flags_directory);
if (r != MAIL_NO_ERROR) {
res = r;
goto free;
}
}
r = mailsession_connect_path(session, mh_storage->mh_pathname);
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
static int mh_mailstorage_get_folder_session(struct mailstorage * storage,
char * pathname, mailsession ** result)
{
int r;
r = mailsession_select_folder(storage->sto_session, pathname);
if (r != MAIL_NO_ERROR)
return r;
* result = storage->sto_session;
return MAIL_NO_ERROR;
}