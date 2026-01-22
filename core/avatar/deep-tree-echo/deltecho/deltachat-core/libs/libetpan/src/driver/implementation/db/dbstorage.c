#ifdef HAVE_CONFIG_H
#	include <config.h>
#endif
#include "dbstorage.h"
#include "mailstorage.h"
#include "mail.h"
#include "mailmessage.h"
#include "dbdriver.h"
#include "maildriver.h"
#include <stdlib.h>
#include <string.h>
static int db_mailstorage_connect(struct mailstorage * storage);
static int
db_mailstorage_get_folder_session(struct mailstorage * storage,
char * pathname, mailsession ** result);
static void db_mailstorage_uninitialize(struct mailstorage * storage);
static mailstorage_driver db_mailstorage_driver = {
"db",
db_mailstorage_connect,
db_mailstorage_get_folder_session,
db_mailstorage_uninitialize
};
LIBETPAN_EXPORT
int db_mailstorage_init(struct mailstorage * storage,
char * db_pathname)
{
struct db_mailstorage * db_storage;
db_storage = malloc(sizeof(* db_storage));
if (db_storage == NULL)
goto err;
db_storage->db_pathname = strdup(db_pathname);
if (db_storage->db_pathname == NULL)
goto free;
storage->sto_data = db_storage;
storage->sto_driver = &db_mailstorage_driver;
return MAIL_NO_ERROR;
free:
free(db_storage);
err:
return MAIL_ERROR_MEMORY;
}
static void db_mailstorage_uninitialize(struct mailstorage * storage)
{
struct db_mailstorage * db_storage;
db_storage = storage->sto_data;
free(db_storage->db_pathname);
free(db_storage);
storage->sto_data = NULL;
}
static int db_mailstorage_connect(struct mailstorage * storage)
{
struct db_mailstorage * db_storage;
mailsession_driver * driver;
int r;
int res;
mailsession * session;
db_storage = storage->sto_data;
driver = db_session_driver;
session = mailsession_new(driver);
if (session == NULL) {
res = MAIL_ERROR_MEMORY;
goto err;
}
r = mailsession_connect_path(session, db_storage->db_pathname);
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
db_mailstorage_get_folder_session(struct mailstorage * storage,
char * pathname, mailsession ** result)
{
* result = storage->sto_session;
return MAIL_NO_ERROR;
}