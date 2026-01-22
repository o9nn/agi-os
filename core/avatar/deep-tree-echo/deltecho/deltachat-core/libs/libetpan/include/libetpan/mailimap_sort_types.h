#ifndef MAILIMAP_SORT_TYPES_H
#define MAILIMAP_SORT_TYPES_H
#ifdef __cplusplus
extern "C" {
#endif
#ifndef WIN32
#include <inttypes.h>
#endif
#include "mailimap_types.h"
enum {
MAILIMAP_SORT_KEY_ARRIVAL,
MAILIMAP_SORT_KEY_CC,
MAILIMAP_SORT_KEY_DATE,
MAILIMAP_SORT_KEY_FROM,
MAILIMAP_SORT_KEY_SIZE,
MAILIMAP_SORT_KEY_SUBJECT,
MAILIMAP_SORT_KEY_TO,
MAILIMAP_SORT_KEY_MULTIPLE
};
struct mailimap_sort_key {
int sortk_type;
int sortk_is_reverse;
clist * sortk_multiple;
};
LIBETPAN_EXPORT
struct mailimap_sort_key *
mailimap_sort_key_new(int sortk_type,
int is_reverse,
clist * sortk_multiple);
LIBETPAN_EXPORT
void mailimap_sort_key_free(struct mailimap_sort_key * key);
LIBETPAN_EXPORT
struct mailimap_sort_key *
mailimap_sort_key_new_arrival(int is_reverse);
LIBETPAN_EXPORT
struct mailimap_sort_key *
mailimap_sort_key_new_cc(int is_reverse);
LIBETPAN_EXPORT
struct mailimap_sort_key *
mailimap_sort_key_new_date(int is_reverse);
LIBETPAN_EXPORT
struct mailimap_sort_key *
mailimap_sort_key_new_from(int is_reverse);
LIBETPAN_EXPORT
struct mailimap_sort_key *
mailimap_sort_key_new_size(int is_reverse);
LIBETPAN_EXPORT
struct mailimap_sort_key *
mailimap_sort_key_new_subject(int is_reverse);
LIBETPAN_EXPORT
struct mailimap_sort_key *
mailimap_sort_key_new_to(int is_reverse);
LIBETPAN_EXPORT
struct mailimap_sort_key *
mailimap_sort_key_new_multiple(clist * keys);
LIBETPAN_EXPORT
struct mailimap_sort_key *
mailimap_sort_key_new_multiple_empty(void);
LIBETPAN_EXPORT
int
mailimap_sort_key_multiple_add(struct mailimap_sort_key * keys,
struct mailimap_sort_key * key_item);
#ifdef __cplusplus
}
#endif
#endif