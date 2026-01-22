#ifndef DBSTORAGE_H
#define DBSTORAGE_H
#include <libetpan/dbdriver_types.h>
#ifdef __cplusplus
extern "C" {
#endif
LIBETPAN_EXPORT
int db_mailstorage_init(struct mailstorage * storage,
char * db_pathname);
#ifdef __cplusplus
}
#endif
#endif