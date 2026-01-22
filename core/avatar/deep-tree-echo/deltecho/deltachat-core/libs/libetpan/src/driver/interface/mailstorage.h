#ifndef MAIL_STORAGE_H
#define MAIL_STORAGE_H
#include <libetpan/maildriver_types.h>
#include <libetpan/mailstorage_types.h>
#ifdef __cplusplus
extern "C" {
#endif
LIBETPAN_EXPORT
struct mailstorage * mailstorage_new(const char * sto_id);
LIBETPAN_EXPORT
void mailstorage_free(struct mailstorage * storage);
LIBETPAN_EXPORT
int mailstorage_connect(struct mailstorage * storage);
LIBETPAN_EXPORT
void mailstorage_disconnect(struct mailstorage * storage);
LIBETPAN_EXPORT
int mailstorage_noop(struct mailstorage * storage);
LIBETPAN_EXPORT
struct mailfolder * mailfolder_new(struct mailstorage * fld_storage,
const char * fld_pathname, const char * fld_virtual_name);
LIBETPAN_EXPORT
void mailfolder_free(struct mailfolder * folder);
LIBETPAN_EXPORT
int mailfolder_add_child(struct mailfolder * parent,
struct mailfolder * child);
LIBETPAN_EXPORT
int mailfolder_detach_parent(struct mailfolder * folder);
LIBETPAN_EXPORT
int mailfolder_connect(struct mailfolder * folder);
LIBETPAN_EXPORT
void mailfolder_disconnect(struct mailfolder * folder);
#ifdef __cplusplus
}
#endif
#endif