#ifndef ANNOTATEMORE_H
#define ANNOTATEMORE_H
#include <libetpan/libetpan-config.h>
#include <libetpan/mailimap_extension.h>
#include <libetpan/annotatemore_types.h>
#ifdef __cplusplus
extern "C" {
#endif
LIBETPAN_EXPORT
extern struct mailimap_extension_api mailimap_extension_annotatemore;
LIBETPAN_EXPORT
int mailimap_annotatemore_getannotation(mailimap * session,
const char * list_mb,
struct mailimap_annotatemore_entry_match_list * entries,
struct mailimap_annotatemore_attrib_match_list * attribs,
clist ** result);
LIBETPAN_EXPORT
int mailimap_annotatemore_setannotation(mailimap * session,
const char * list_mb,
struct mailimap_annotatemore_entry_att_list * en_att,
int * result);
LIBETPAN_EXPORT
int mailimap_has_annotatemore(mailimap * session);
#ifdef __cplusplus
}
#endif
#endif