#ifndef DBDRIVER_TYPES_H
#define DBDRIVER_TYPES_H
#include <libetpan/libetpan-config.h>
#include <libetpan/maildriver_types.h>
#include <libetpan/generic_cache_types.h>
#include <libetpan/mailstorage_types.h>
#ifdef __cplusplus
extern "C" {
#endif
struct db_session_state_data {
char db_filename[PATH_MAX];
struct mail_flags_store * db_flags_store;
};
struct db_mailstorage {
char * db_pathname;
};
#ifdef __cplusplus
}
#endif
#endif