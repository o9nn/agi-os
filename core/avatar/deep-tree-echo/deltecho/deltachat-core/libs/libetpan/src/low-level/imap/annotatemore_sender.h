#ifndef ANNOTATEMORE_SENDER_H
#define ANNOTATEMORE_SENDER_H
#include "mailimap_sender.h"
#include "annotatemore_types.h"
#ifdef __cplusplus
extern "C" {
#endif
int mailimap_annotatemore_getannotation_send(mailstream * fd,
const char * list_mb,
struct mailimap_annotatemore_entry_match_list * entries,
struct mailimap_annotatemore_attrib_match_list * attribs);
int mailimap_annotatemore_setannotation_send(mailstream * fd,
const char * list_mb,
struct mailimap_annotatemore_entry_att_list * en_att);
#ifdef __cplusplus
}
#endif
#endif