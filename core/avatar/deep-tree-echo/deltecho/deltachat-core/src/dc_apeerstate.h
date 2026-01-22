#ifndef __DC_APEERSTATE_H__
#define __DC_APEERSTATE_H__
#ifdef __cplusplus
extern "C" {
#endif
#include "dc_key.h"
#include "dc_aheader.h"
#include "dc_hash.h"
typedef struct _dc_apeerstate dc_apeerstate_t;
#define DC_PE_NOPREFERENCE   0
#define DC_PE_MUTUAL         1
#define DC_PE_RESET         20
struct _dc_apeerstate
{
dc_context_t*  context;
char*          addr;
time_t         last_seen;
time_t         last_seen_autocrypt;
int            prefer_encrypt;
#define        DC_NOT_VERIFIED      0
#define        DC_BIDIRECT_VERIFIED 2
dc_key_t*      public_key;
char*          public_key_fingerprint;
dc_key_t*      gossip_key;
time_t         gossip_timestamp;
char*          gossip_key_fingerprint;
dc_key_t*      verified_key;
char*          verified_key_fingerprint;
#define        DC_SAVE_TIMESTAMPS 0x01
#define        DC_SAVE_ALL        0x02
int            to_save;
#define        DC_DE_ENCRYPTION_PAUSED   0x01
#define        DC_DE_FINGERPRINT_CHANGED 0x02
int            degrade_event;
};
dc_apeerstate_t* dc_apeerstate_new                  (dc_context_t*);
void             dc_apeerstate_unref                (dc_apeerstate_t*);
int              dc_apeerstate_init_from_header     (dc_apeerstate_t*, const dc_aheader_t*, time_t message_time);
int              dc_apeerstate_init_from_gossip     (dc_apeerstate_t*, const dc_aheader_t*, time_t message_time);
int              dc_apeerstate_degrade_encryption   (dc_apeerstate_t*, time_t message_time);
void             dc_apeerstate_apply_header         (dc_apeerstate_t*, const dc_aheader_t*, time_t message_time);
void             dc_apeerstate_apply_gossip         (dc_apeerstate_t*, const dc_aheader_t*, time_t message_time);
char*            dc_apeerstate_render_gossip_header (const dc_apeerstate_t*, int min_verified);
dc_key_t*        dc_apeerstate_peek_key             (const dc_apeerstate_t*, int min_verified);
int              dc_apeerstate_recalc_fingerprint   (dc_apeerstate_t*);
#define          DC_PS_GOSSIP_KEY 0
#define          DC_PS_PUBLIC_KEY 1
int              dc_apeerstate_set_verified         (dc_apeerstate_t*, int which_key, const char* fingerprint, int verified);
int              dc_apeerstate_load_by_addr         (dc_apeerstate_t*, dc_sqlite3_t*, const char* addr);
int              dc_apeerstate_load_by_fingerprint  (dc_apeerstate_t*, dc_sqlite3_t*, const char* fingerprint);
int              dc_apeerstate_save_to_db           (const dc_apeerstate_t*, dc_sqlite3_t*, int create);
int              dc_apeerstate_has_verified_key     (const dc_apeerstate_t*, const dc_hash_t* fingerprints);
#ifdef __cplusplus
}
#endif
#endif