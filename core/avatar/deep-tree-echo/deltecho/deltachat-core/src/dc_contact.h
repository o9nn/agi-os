#ifndef __DC_CONTACT_H__
#define __DC_CONTACT_H__
#ifdef __cplusplus
extern "C" {
#endif
#include "dc_contact.h"
#include "dc_apeerstate.h"
struct _dc_contact
{
uint32_t        magic;
dc_context_t*   context;
uint32_t        id;
char*           name;
char*           authname;
char*           addr;
int             blocked;
int             origin;
};
#define DC_ORIGIN_INCOMING_UNKNOWN_FROM      0x10
#define DC_ORIGIN_INCOMING_UNKNOWN_CC        0x20
#define DC_ORIGIN_INCOMING_UNKNOWN_TO        0x40
#define DC_ORIGIN_UNHANDLED_QR_SCAN          0x80
#define DC_ORIGIN_INCOMING_REPLY_TO         0x100
#define DC_ORIGIN_INCOMING_CC               0x200
#define DC_ORIGIN_INCOMING_TO               0x400
#define DC_ORIGIN_CREATE_CHAT               0x800
#define DC_ORIGIN_OUTGOING_BCC             0x1000
#define DC_ORIGIN_OUTGOING_CC              0x2000
#define DC_ORIGIN_OUTGOING_TO              0x4000
#define DC_ORIGIN_INTERNAL                0x40000
#define DC_ORIGIN_ADRESS_BOOK             0x80000
#define DC_ORIGIN_SECUREJOIN_INVITED    0x1000000
#define DC_ORIGIN_SECUREJOIN_JOINED     0x2000000
#define DC_ORIGIN_MANUALLY_CREATED      0x4000000
#define DC_ORIGIN_MIN_CONTACT_LIST    (DC_ORIGIN_INCOMING_REPLY_TO)
#define DC_ORIGIN_MIN_VERIFIED        (DC_ORIGIN_INCOMING_REPLY_TO)
#define DC_ORIGIN_MIN_START_NEW_NCHAT (0x7FFFFFFF)
int          dc_contact_load_from_db             (dc_contact_t*, dc_sqlite3_t*, uint32_t contact_id);
int          dc_contact_is_verified_ex           (dc_contact_t*, const dc_apeerstate_t*);
void         dc_normalize_name                   (char* full_name);
char*        dc_get_first_name                   (const char* full_name);
int          dc_addr_cmp                         (const char* addr1, const char* addr2);
char*        dc_addr_normalize                   (const char* addr);
int          dc_addr_equals_self                 (dc_context_t*, const char* addr);
int          dc_addr_equals_contact              (dc_context_t*, const char* addr, uint32_t contact_id);
size_t       dc_get_real_contact_cnt             (dc_context_t*);
uint32_t     dc_add_or_lookup_contact            (dc_context_t*, const char* display_name , const char* addr_spec, int origin, int* sth_modified);
int          dc_get_contact_origin               (dc_context_t*, uint32_t contact_id, int* ret_blocked);
int          dc_is_contact_blocked               (dc_context_t*, uint32_t contact_id);
int          dc_real_contact_exists              (dc_context_t*, uint32_t contact_id);
void         dc_scaleup_contact_origin           (dc_context_t*, uint32_t contact_id, int origin);
#ifdef __cplusplus
}
#endif
#endif