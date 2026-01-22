#include "qresync_types.h"
#include <stdlib.h>
#include "mailimap_types.h"
LIBETPAN_EXPORT
struct mailimap_qresync_vanished * mailimap_qresync_vanished_new(int qr_earlier, struct mailimap_set * qr_known_uids)
{
struct mailimap_qresync_vanished * vanished;
vanished = malloc(sizeof(* vanished));
if (vanished == NULL)
return vanished;
vanished->qr_earlier = qr_earlier;
vanished->qr_known_uids = qr_known_uids;
return vanished;
}
LIBETPAN_EXPORT
void mailimap_qresync_vanished_free(struct mailimap_qresync_vanished * vanished)
{
mailimap_set_free(vanished->qr_known_uids);
free(vanished);
}
LIBETPAN_EXPORT
struct mailimap_qresync_resptextcode * mailimap_qresync_resptextcode_new(int qr_type)
{
struct mailimap_qresync_resptextcode * resptextcode;
resptextcode = malloc(sizeof(* resptextcode));
if (resptextcode == NULL)
return resptextcode;
resptextcode->qr_type = qr_type;
return resptextcode;
}
LIBETPAN_EXPORT
void mailimap_qresync_resptextcode_free(struct mailimap_qresync_resptextcode * resptextcode)
{
free(resptextcode);
}