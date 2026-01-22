#ifndef libetpan_mailimap_sort_h
#define libetpan_mailimap_sort_h
#ifdef __cplusplus
extern "C" {
#endif
#include <libetpan/libetpan-config.h>
#include <libetpan/mailimap_extension.h>
#include <libetpan/mailimap_sort_types.h>
LIBETPAN_EXPORT
extern struct mailimap_extension_api mailimap_extension_sort;
LIBETPAN_EXPORT
int
mailimap_sort(mailimap * session, const char * charset,
struct mailimap_sort_key * key, struct mailimap_search_key * searchkey,
clist ** result);
LIBETPAN_EXPORT
int
mailimap_uid_sort(mailimap * session, const char * charset,
struct mailimap_sort_key * key, struct mailimap_search_key * searchkey,
clist ** result);
LIBETPAN_EXPORT
void mailimap_sort_result_free(clist * search_result);
#ifdef __cplusplus
}
#endif
#endif