#ifdef HAVE_CONFIG_H
#	include <config.h>
#endif
#include "mboxstorage.h"
#include "mail.h"
#include "mailmessage.h"
#include "mboxdriver.h"
#include "mboxdriver_cached.h"
#include "maildriver.h"
#include <stdlib.h>
#include <string.h>
static int mbox_mailstorage_connect(struct mailstorage * storage);
static int
mbox_mailstorage_get_folder_session(struct mailstorage * storage,
char * pathname, mailsession ** result);
static void mbox_mailstorage_uninitialize(struct mailstorage * storage);
static mailstorage_driver mbox_mailstorage_driver = {
"mbox",
mbox_mailstorage_connect,
mbox_mailstorage_get_folder_session,
mbox_mailstorage_uninitialize
};
LIBETPAN_EXPORT
int mbox_mailstorage_init(struct mailstorage * storage,
const char * mbox_pathname, int mbox_cached,
const char * mbox_cache_directory, const char * mbox_flags_directory)
{
struct mbox_mailstorage * mbox_storage;
mbox_storage = malloc(sizeof(* mbox_storage));
if (mbox_storage == NULL)
goto err;
mbox_storage->mbox_pathname = strdup(mbox_pathname);
if (mbox_storage->mbox_pathname == NULL)
goto free;
mbox_storage->mbox_cached = mbox_cached;
if (mbox_cached && (mbox_cache_directory != NULL) &&
(mbox_flags_directory != NULL)) {
mbox_storage->mbox_cache_directory = strdup(mbox_cache_directory);
if (mbox_storage->mbox_cache_directory == NULL)
goto free_pathname;
mbox_storage->mbox_flags_directory = strdup(mbox_flags_directory);
if (mbox_storage->mbox_flags_directory == NULL)
goto free_cache_directory;
}
else {
mbox_storage->mbox_cached = FALSE;
mbox_storage->mbox_cache_directory = NULL;
mbox_storage->mbox_flags_directory = NULL;
}
storage->sto_data = mbox_storage;
storage->sto_driver = &mbox_mailstorage_driver;
return MAIL_NO_ERROR;
free_cache_directory:
free(mbox_storage->mbox_cache_directory);
free_pathname:
free(mbox_storage->mbox_pathname);
free:
free(mbox_storage);
err:
return MAIL_ERROR_MEMORY;
}
static void mbox_mailstorage_uninitialize(struct mailstorage * storage)
{
struct mbox_mailstorage * mbox_storage;
mbox_storage = storage->sto_data;
if (mbox_storage->mbox_flags_directory != NULL)
free(mbox_storage->mbox_flags_directory);
if (mbox_storage->mbox_cache_directory != NULL)
free(mbox_storage->mbox_cache_directory);
free(mbox_storage->mbox_pathname);
free(mbox_storage);
storage->sto_data = NULL;
}
static int mbox_mailstorage_connect(struct mailstorage * storage)
{
struct mbox_mailstorage * mbox_storage;
mailsession_driver * driver;
int r;
int res;
mailsession * session;
mbox_storage = storage->sto_data;
if (mbox_storage->mbox_cached)
driver = mbox_cached_session_driver;
else
driver = mbox_session_driver;
session = mailsession_new(driver);
if (session == NULL) {
res = MAIL_ERROR_MEMORY;
goto err;
}
if (mbox_storage->mbox_cached) {
r = mailsession_parameters(session,
MBOXDRIVER_CACHED_SET_CACHE_DIRECTORY,
mbox_storage->mbox_cache_directory);
if (r != MAIL_NO_ERROR) {
res = r;
goto free;
}
r = mailsession_parameters(session,
MBOXDRIVER_CACHED_SET_FLAGS_DIRECTORY,
mbox_storage->mbox_flags_directory);
if (r != MAIL_NO_ERROR) {
res = r;
goto free;
}
}
r = mailsession_connect_path(session, mbox_storage->mbox_pathname);
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
mbox_mailstorage_get_folder_session(struct mailstorage * storage,
char * pathname, mailsession ** result)
{
* result = storage->sto_session;
return MAIL_NO_ERROR;
}