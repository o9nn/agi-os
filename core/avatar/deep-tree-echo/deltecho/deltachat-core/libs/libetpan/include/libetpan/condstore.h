#ifndef CONDSTORE_H
#define CONDSTORE_H
#ifdef __cplusplus
extern "C" {
#endif
#include <libetpan/libetpan-config.h>
#include <libetpan/mailimap_extension.h>
#include <libetpan/condstore_types.h>
LIBETPAN_EXPORT
extern struct mailimap_extension_api mailimap_extension_condstore;
LIBETPAN_EXPORT
int mailimap_store_unchangedsince(mailimap * session,
struct mailimap_set * set, uint64_t mod_sequence_valzer,
struct mailimap_store_att_flags * store_att_flags);
LIBETPAN_EXPORT
int mailimap_uid_store_unchangedsince(mailimap * session,
struct mailimap_set * set, uint64_t mod_sequence_valzer,
struct mailimap_store_att_flags * store_att_flags);
LIBETPAN_EXPORT
int mailimap_fetch_changedsince(mailimap * session,
struct mailimap_set * set,
struct mailimap_fetch_type * fetch_type, uint64_t mod_sequence_value,
clist ** result);
LIBETPAN_EXPORT
int mailimap_uid_fetch_changedsince(mailimap * session,
struct mailimap_set * set,
struct mailimap_fetch_type * fetch_type, uint64_t mod_sequence_value,
clist ** result);
LIBETPAN_EXPORT
struct mailimap_fetch_att * mailimap_fetch_att_new_modseq(void);
LIBETPAN_EXPORT
int mailimap_search_modseq(mailimap * session, const char * charset,
struct mailimap_search_key * key, clist ** result, uint64_t * p_mod_sequence_value);
LIBETPAN_EXPORT
int mailimap_uid_search_modseq(mailimap * session, const char * charset,
struct mailimap_search_key * key, clist ** result, uint64_t * p_mod_sequence_value);
LIBETPAN_EXPORT
int mailimap_search_literalplus_modseq(mailimap * session, const char * charset,
struct mailimap_search_key * key, clist ** result, uint64_t * p_mod_sequence_value);
LIBETPAN_EXPORT
int mailimap_uid_search_literalplus_modseq(mailimap * session, const char * charset,
struct mailimap_search_key * key, clist ** result, uint64_t * p_mod_sequence_value);
LIBETPAN_EXPORT
int mailimap_select_condstore(mailimap * session, const char * mb, uint64_t * p_mod_sequence_value);
LIBETPAN_EXPORT
int mailimap_examine_condstore(mailimap * session, const char * mb, uint64_t * p_mod_sequence_value);
LIBETPAN_EXPORT
int mailimap_has_condstore(mailimap * session);
#ifdef __cplusplus
}
#endif
#endif