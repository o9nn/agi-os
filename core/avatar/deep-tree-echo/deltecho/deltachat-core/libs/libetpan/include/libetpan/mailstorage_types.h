#ifndef MAILSTORAGE_TYPES_H
#define MAILSTORAGE_TYPES_H
#include <libetpan/maildriver_types.h>
#ifdef __cplusplus
extern "C" {
#endif
struct mailstorage;
typedef struct mailstorage_driver mailstorage_driver;
struct mailstorage_driver {
char * sto_name;
int (* sto_connect)(struct mailstorage * storage);
int (* sto_get_folder_session)(struct mailstorage * storage,
char * pathname, mailsession ** result);
void (* sto_uninitialize)(struct mailstorage * storage);
};
struct mailstorage {
char * sto_id;
void * sto_data;
mailsession * sto_session;
mailstorage_driver * sto_driver;
clist * sto_shared_folders;
void * sto_user_data;
};
struct mailfolder {
char * fld_pathname;
char * fld_virtual_name;
struct mailstorage * fld_storage;
mailsession * fld_session;
int fld_shared_session;
clistiter * fld_pos;
struct mailfolder * fld_parent;
unsigned int fld_sibling_index;
carray * fld_children;
void * fld_user_data;
};
enum {
CONNECTION_TYPE_PLAIN,
CONNECTION_TYPE_STARTTLS,
CONNECTION_TYPE_TRY_STARTTLS,
CONNECTION_TYPE_TLS,
CONNECTION_TYPE_COMMAND,
CONNECTION_TYPE_COMMAND_STARTTLS,
CONNECTION_TYPE_COMMAND_TRY_STARTTLS,
CONNECTION_TYPE_COMMAND_TLS
};
#ifdef __cplusplus
}
#endif
#endif