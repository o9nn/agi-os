#ifndef __DC_MIMEPARSER_H__
#define __DC_MIMEPARSER_H__
#ifdef __cplusplus
extern "C" {
#endif
#include "dc_hash.h"
#include "dc_param.h"
typedef struct _dc_mimepart    dc_mimepart_t;
typedef struct _dc_mimeparser  dc_mimeparser_t;
struct _dc_mimepart
{
int                 type;
int                 is_meta;
int                 int_mimetype;
char*               msg;
char*               msg_raw;
int                 bytes;
dc_param_t*          param;
};
struct _dc_mimeparser
{
carray*                parts;
struct mailmime*       mimeroot;
dc_hash_t              header;
struct mailimf_fields* header_root;
struct mailimf_fields* header_protected;
char*                  subject;
int                    is_send_by_messenger;
int                    decrypting_failed;
struct _dc_e2ee_helper* e2ee_helper;
const char*            blobdir;
int                    is_forwarded;
dc_context_t*          context;
carray*                reports;
int                    is_system_message;
struct _dc_kml*        location_kml;
struct _dc_kml*        message_kml;
};
dc_mimeparser_t*  dc_mimeparser_new                    (const char* blobdir, dc_context_t*);
void             dc_mimeparser_unref                  (dc_mimeparser_t*);
void             dc_mimeparser_empty                  (dc_mimeparser_t*);
void             dc_mimeparser_parse                  (dc_mimeparser_t*, const char* body_not_terminated, size_t body_bytes);
struct mailimf_field*          dc_mimeparser_lookup_field           (dc_mimeparser_t*, const char* field_name);
struct mailimf_optional_field* dc_mimeparser_lookup_optional_field  (dc_mimeparser_t*, const char* field_name);
dc_mimepart_t*                 dc_mimeparser_get_last_nonmeta       (dc_mimeparser_t*);
#define                        dc_mimeparser_has_nonmeta(a)         (dc_mimeparser_get_last_nonmeta((a))!=NULL)
int                            dc_mimeparser_is_mailinglist_message (dc_mimeparser_t*);
int                            dc_mimeparser_sender_equals_recipient(dc_mimeparser_t*);
void                           dc_mimeparser_repl_msg_by_error      (dc_mimeparser_t*, const char* error_msg);
#ifdef DC_USE_MIME_DEBUG
void                           mailmime_print                (struct mailmime*);
#endif
struct mailmime_parameter*     mailmime_find_ct_parameter    (struct mailmime*, const char* name);
int                            mailmime_transfer_decode      (struct mailmime*, const char** ret_decoded_data, size_t* ret_decoded_data_bytes, char** ret_to_mmap_string_unref);
struct mailimf_fields*         mailmime_find_mailimf_fields  (struct mailmime*);
char*                          mailimf_find_first_addr       (const struct mailimf_mailbox_list*);
struct mailimf_field*          mailimf_find_field            (struct mailimf_fields*, int wanted_fld_type);
struct mailimf_optional_field* mailimf_find_optional_field   (struct mailimf_fields*, const char* wanted_fld_name);
dc_hash_t*                      mailimf_get_recipients        (struct mailimf_fields*);
#ifdef __cplusplus
}
#endif
#endif