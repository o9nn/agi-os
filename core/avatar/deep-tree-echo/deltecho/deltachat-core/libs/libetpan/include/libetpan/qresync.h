#ifndef QRESYNC_H
#define QRESYNC_H
#include <libetpan/mailimap_extension_types.h>
#include <libetpan/mailimap_types.h>
#include <libetpan/clist.h>
#include <libetpan/qresync_types.h>
LIBETPAN_EXPORT
extern struct mailimap_extension_api mailimap_extension_qresync;
LIBETPAN_EXPORT
int mailimap_select_qresync(mailimap * session, const char * mb,
uint32_t uidvalidity, uint64_t modseq_value,
struct mailimap_set * known_uids,
struct mailimap_set * seq_match_data_sequences,
struct mailimap_set * seq_match_data_uids,
clist ** fetch_result, struct mailimap_qresync_vanished ** p_vanished,
uint64_t * p_mod_sequence_value);
LIBETPAN_EXPORT
int mailimap_fetch_qresync(mailimap * session,
struct mailimap_set * set,
struct mailimap_fetch_type * fetch_type, uint64_t mod_sequence_value,
clist ** fetch_result, struct mailimap_qresync_vanished ** p_vanished);
LIBETPAN_EXPORT
int mailimap_uid_fetch_qresync(mailimap * session,
struct mailimap_set * set,
struct mailimap_fetch_type * fetch_type, uint64_t mod_sequence_value,
clist ** fetch_result, struct mailimap_qresync_vanished ** p_vanished);
LIBETPAN_EXPORT
int mailimap_has_qresync(mailimap * session);
#endif