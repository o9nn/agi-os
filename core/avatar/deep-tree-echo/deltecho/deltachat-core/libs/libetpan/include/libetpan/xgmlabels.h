#ifndef XGMLABELS_H
#define XGMLABELS_H
#ifdef __cplusplus
extern "C" {
#endif
#include <libetpan/libetpan-config.h>
#include <libetpan/mailimap_extension.h>
struct mailimap_msg_att_xgmlabels {
clist * att_labels;
};
LIBETPAN_EXPORT
extern struct mailimap_extension_api mailimap_extension_xgmlabels;
LIBETPAN_EXPORT
struct mailimap_fetch_att * mailimap_fetch_att_new_xgmlabels(void);
LIBETPAN_EXPORT
int mailimap_has_xgmlabels(mailimap * session);
LIBETPAN_EXPORT
struct mailimap_msg_att_xgmlabels * mailimap_msg_att_xgmlabels_new(clist * att_labels);
LIBETPAN_EXPORT
struct mailimap_msg_att_xgmlabels * mailimap_msg_att_xgmlabels_new_empty(void);
LIBETPAN_EXPORT
int mailimap_msg_att_xgmlabels_add(struct mailimap_msg_att_xgmlabels * att, char * label);
LIBETPAN_EXPORT void mailimap_msg_att_xgmlabels_free(struct mailimap_msg_att_xgmlabels * att);
LIBETPAN_EXPORT
int
mailimap_store_xgmlabels(mailimap * session,
struct mailimap_set * set,
int fl_sign, int fl_silent,
struct mailimap_msg_att_xgmlabels * labels);
LIBETPAN_EXPORT
int
mailimap_uid_store_xgmlabels(mailimap * session,
struct mailimap_set * set,
int fl_sign, int fl_silent,
struct mailimap_msg_att_xgmlabels * labels);
#ifdef __cplusplus
}
#endif
#endif