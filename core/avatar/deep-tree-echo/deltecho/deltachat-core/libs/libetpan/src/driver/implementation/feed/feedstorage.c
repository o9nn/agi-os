#ifdef HAVE_CONFIG_H
#	include <config.h>
#endif
#include "feedstorage.h"
#include <stdlib.h>
#include <string.h>
#include "maildriver.h"
#include "feeddriver.h"
#include "mailstorage_tools.h"
#include "mail.h"
#define FEED_DEFAULT_PORT  119
#define FEEDS_DEFAULT_PORT 563
static int feed_mailstorage_connect(struct mailstorage * storage);
static int feed_mailstorage_get_folder_session(struct mailstorage * storage,
char * pathname, mailsession ** result);
static void feed_mailstorage_uninitialize(struct mailstorage * storage);
static mailstorage_driver feed_mailstorage_driver = {
"feed",
feed_mailstorage_connect,
feed_mailstorage_get_folder_session,
feed_mailstorage_uninitialize,
};
int feed_mailstorage_init(struct mailstorage * storage,
const char * feed_url,
int feed_cached, const char * feed_cache_directory,
const char * feed_flags_directory)
{
struct feed_mailstorage * feed_storage;
int res;
feed_storage = malloc(sizeof(* feed_storage));
if (feed_storage == NULL) {
res = MAIL_ERROR_MEMORY;
goto err;
}
feed_storage->feed_url = strdup(feed_url);
if (feed_storage->feed_url == NULL) {
res = MAIL_ERROR_MEMORY;
goto free;
}
feed_storage->feed_cached = feed_cached;
if (feed_cached && (feed_cache_directory != NULL) &&
(feed_flags_directory != NULL)) {
feed_storage->feed_cache_directory = strdup(feed_cache_directory);
if (feed_storage->feed_cache_directory == NULL) {
res = MAIL_ERROR_MEMORY;
goto free_url;
}
feed_storage->feed_flags_directory = strdup(feed_flags_directory);
if (feed_storage->feed_flags_directory == NULL) {
res = MAIL_ERROR_MEMORY;
goto free_cache_directory;
}
}
else {
feed_storage->feed_cached = FALSE;
feed_storage->feed_cache_directory = NULL;
feed_storage->feed_flags_directory = NULL;
}
storage->sto_data = feed_storage;
storage->sto_driver = &feed_mailstorage_driver;
return MAIL_NO_ERROR;
free_cache_directory:
free(feed_storage->feed_cache_directory);
free_url:
free(feed_storage->feed_url);
free:
free(feed_storage);
err:
return res;
}
static void feed_mailstorage_uninitialize(struct mailstorage * storage)
{
struct feed_mailstorage * feed_storage;
feed_storage = storage->sto_data;
if (feed_storage->feed_flags_directory != NULL)
free(feed_storage->feed_flags_directory);
if (feed_storage->feed_cache_directory != NULL)
free(feed_storage->feed_cache_directory);
free(feed_storage->feed_url);
free(feed_storage);
storage->sto_data = NULL;
}
static int feed_mailstorage_connect(struct mailstorage * storage)
{
struct feed_mailstorage * feed_storage;
mailsession_driver * driver;
int r;
int res;
mailsession * session;
feed_storage = storage->sto_data;
driver = feed_session_driver;
session = mailsession_new(driver);
if (session == NULL) {
res = MAIL_ERROR_MEMORY;
goto err;
}
r = mailsession_connect_path(session, feed_storage->feed_url);
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
static int feed_mailstorage_get_folder_session(struct mailstorage * storage,
char * pathname, mailsession ** result)
{
* result = storage->sto_session;
return MAIL_NO_ERROR;
}