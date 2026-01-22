#ifndef QRESYNC_TYPES_H
#define QRESYNC_TYPES_H
#include <libetpan/libetpan-config.h>
enum {
MAILIMAP_QRESYNC_TYPE_VANISHED,
MAILIMAP_QRESYNC_TYPE_RESP_TEXT_CODE
};
struct mailimap_qresync_vanished {
int qr_earlier;
struct mailimap_set * qr_known_uids;
};
enum {
MAILIMAP_QRESYNC_RESPTEXTCODE_CLOSED
};
struct mailimap_qresync_resptextcode {
int qr_type;
};
LIBETPAN_EXPORT
struct mailimap_qresync_vanished * mailimap_qresync_vanished_new(int qr_earlier, struct mailimap_set * qr_known_uids);
LIBETPAN_EXPORT
void mailimap_qresync_vanished_free(struct mailimap_qresync_vanished * vanished);
LIBETPAN_EXPORT
struct mailimap_qresync_resptextcode * mailimap_qresync_resptextcode_new(int qr_type);
LIBETPAN_EXPORT
void mailimap_qresync_resptextcode_free(struct mailimap_qresync_resptextcode * resptextcode);
#endif