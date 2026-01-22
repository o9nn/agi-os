#ifndef QRESYNC_PRIVATE_H
#define QRESYNC_PRIVATE_H
int mailimap_fetch_qresync_vanished(mailimap * session,
struct mailimap_set * set,
struct mailimap_fetch_type * fetch_type, uint64_t mod_sequence_value, int vanished,
clist ** fetch_result, struct mailimap_qresync_vanished ** p_vanished);
LIBETPAN_EXPORT
int mailimap_uid_fetch_qresync_vanished(mailimap * session,
struct mailimap_set * set,
struct mailimap_fetch_type * fetch_type, uint64_t mod_sequence_value, int vanished,
clist ** fetch_result, struct mailimap_qresync_vanished ** p_vanished);
#endif