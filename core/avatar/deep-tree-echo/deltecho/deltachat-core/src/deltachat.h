#ifndef __DELTACHAT_H__
#define __DELTACHAT_H__
#ifdef __cplusplus
extern "C" {
#endif
#ifndef PY_CFFI
#include <stdint.h>
#include <time.h>
#endif
#define DC_VERSION_STR "0.45.0"
typedef struct _dc_context  dc_context_t;
typedef struct _dc_array    dc_array_t;
typedef struct _dc_chatlist dc_chatlist_t;
typedef struct _dc_chat     dc_chat_t;
typedef struct _dc_msg      dc_msg_t;
typedef struct _dc_contact  dc_contact_t;
typedef struct _dc_lot      dc_lot_t;
typedef uintptr_t (*dc_callback_t) (dc_context_t*, int event, uintptr_t data1, uintptr_t data2);
dc_context_t*   dc_context_new               (dc_callback_t, void* userdata, const char* os_name);
void            dc_context_unref             (dc_context_t*);
void*           dc_get_userdata              (dc_context_t*);
int             dc_open                      (dc_context_t*, const char* dbfile, const char* blobdir);
void            dc_close                     (dc_context_t*);
int             dc_is_open                   (const dc_context_t*);
char*           dc_get_blobdir               (const dc_context_t*);
int             dc_set_config                (dc_context_t*, const char* key, const char* value);
char*           dc_get_config                (dc_context_t*, const char* key);
char*           dc_get_info                  (dc_context_t*);
char*           dc_get_oauth2_url            (dc_context_t*, const char* addr, const char* redirect);
char*           dc_get_version_str           (void);
void            dc_openssl_init_not_required (void);
void            dc_no_compound_msgs          (void);
void            dc_configure                 (dc_context_t*);
int             dc_is_configured             (const dc_context_t*);
void            dc_perform_imap_jobs         (dc_context_t*);
void            dc_perform_imap_fetch        (dc_context_t*);
void            dc_perform_imap_idle         (dc_context_t*);
void            dc_interrupt_imap_idle       (dc_context_t*);
void            dc_perform_mvbox_fetch       (dc_context_t*);
void            dc_perform_mvbox_idle        (dc_context_t*);
void            dc_interrupt_mvbox_idle      (dc_context_t*);
void            dc_perform_sentbox_fetch     (dc_context_t*);
void            dc_perform_sentbox_idle      (dc_context_t*);
void            dc_interrupt_sentbox_idle    (dc_context_t*);
void            dc_perform_smtp_jobs         (dc_context_t*);
void            dc_perform_smtp_idle         (dc_context_t*);
void            dc_interrupt_smtp_idle       (dc_context_t*);
void            dc_maybe_network             (dc_context_t*);
#define         DC_GCL_ARCHIVED_ONLY         0x01
#define         DC_GCL_NO_SPECIALS           0x02
#define         DC_GCL_ADD_ALLDONE_HINT      0x04
dc_chatlist_t*  dc_get_chatlist              (dc_context_t*, int flags, const char* query_str, uint32_t query_id);
uint32_t        dc_create_chat_by_msg_id     (dc_context_t*, uint32_t msg_id);
uint32_t        dc_create_chat_by_contact_id (dc_context_t*, uint32_t contact_id);
uint32_t        dc_get_chat_id_by_contact_id (dc_context_t*, uint32_t contact_id);
uint32_t        dc_prepare_msg               (dc_context_t*, uint32_t chat_id, dc_msg_t*);
uint32_t        dc_send_msg                  (dc_context_t*, uint32_t chat_id, dc_msg_t*);
uint32_t        dc_send_text_msg             (dc_context_t*, uint32_t chat_id, const char* text_to_send);
void            dc_set_draft                 (dc_context_t*, uint32_t chat_id, dc_msg_t*);
dc_msg_t*       dc_get_draft                 (dc_context_t*, uint32_t chat_id);
#define         DC_GCM_ADDDAYMARKER          0x01
dc_array_t*     dc_get_chat_msgs             (dc_context_t*, uint32_t chat_id, uint32_t flags, uint32_t marker1before);
int             dc_get_msg_cnt               (dc_context_t*, uint32_t chat_id);
int             dc_get_fresh_msg_cnt         (dc_context_t*, uint32_t chat_id);
dc_array_t*     dc_get_fresh_msgs            (dc_context_t*);
void            dc_marknoticed_chat          (dc_context_t*, uint32_t chat_id);
void            dc_marknoticed_all_chats     (dc_context_t*);
dc_array_t*     dc_get_chat_media            (dc_context_t*, uint32_t chat_id, int msg_type, int or_msg_type2, int or_msg_type3);
uint32_t        dc_get_next_media            (dc_context_t*, uint32_t msg_id, int dir, int msg_type, int or_msg_type2, int or_msg_type3);
void            dc_archive_chat              (dc_context_t*, uint32_t chat_id, int archive);
void            dc_delete_chat               (dc_context_t*, uint32_t chat_id);
dc_array_t*     dc_get_chat_contacts         (dc_context_t*, uint32_t chat_id);
dc_array_t*     dc_search_msgs               (dc_context_t*, uint32_t chat_id, const char* query);
dc_chat_t*      dc_get_chat                  (dc_context_t*, uint32_t chat_id);
uint32_t        dc_create_group_chat         (dc_context_t*, int verified, const char* name);
int             dc_is_contact_in_chat        (dc_context_t*, uint32_t chat_id, uint32_t contact_id);
int             dc_add_contact_to_chat       (dc_context_t*, uint32_t chat_id, uint32_t contact_id);
int             dc_remove_contact_from_chat  (dc_context_t*, uint32_t chat_id, uint32_t contact_id);
int             dc_set_chat_name             (dc_context_t*, uint32_t chat_id, const char* name);
int             dc_set_chat_profile_image    (dc_context_t*, uint32_t chat_id, const char* image);
char*           dc_get_msg_info              (dc_context_t*, uint32_t msg_id);
char*           dc_get_mime_headers          (dc_context_t*, uint32_t msg_id);
void            dc_delete_msgs               (dc_context_t*, const uint32_t* msg_ids, int msg_cnt);
void            dc_forward_msgs              (dc_context_t*, const uint32_t* msg_ids, int msg_cnt, uint32_t chat_id);
void            dc_marknoticed_contact       (dc_context_t*, uint32_t contact_id);
void            dc_markseen_msgs             (dc_context_t*, const uint32_t* msg_ids, int msg_cnt);
void            dc_star_msgs                 (dc_context_t*, const uint32_t* msg_ids, int msg_cnt, int star);
dc_msg_t*       dc_get_msg                   (dc_context_t*, uint32_t msg_id);
int             dc_may_be_valid_addr         (const char* addr);
uint32_t        dc_lookup_contact_id_by_addr (dc_context_t*, const char* addr);
uint32_t        dc_create_contact            (dc_context_t*, const char* name, const char* addr);
int             dc_add_address_book          (dc_context_t*, const char*);
#define         DC_GCL_VERIFIED_ONLY         0x01
#define         DC_GCL_ADD_SELF              0x02
dc_array_t*     dc_get_contacts              (dc_context_t*, uint32_t flags, const char* query);
int             dc_get_blocked_cnt           (dc_context_t*);
dc_array_t*     dc_get_blocked_contacts      (dc_context_t*);
void            dc_block_contact             (dc_context_t*, uint32_t contact_id, int block);
char*           dc_get_contact_encrinfo      (dc_context_t*, uint32_t contact_id);
int             dc_delete_contact            (dc_context_t*, uint32_t contact_id);
dc_contact_t*   dc_get_contact               (dc_context_t*, uint32_t contact_id);
#define         DC_IMEX_EXPORT_SELF_KEYS      1
#define         DC_IMEX_IMPORT_SELF_KEYS      2
#define         DC_IMEX_EXPORT_BACKUP        11
#define         DC_IMEX_IMPORT_BACKUP        12
void            dc_imex                      (dc_context_t*, int what, const char* param1, const char* param2);
char*           dc_imex_has_backup           (dc_context_t*, const char* dir);
int             dc_check_password            (dc_context_t*, const char* pw);
char*           dc_initiate_key_transfer     (dc_context_t*);
int             dc_continue_key_transfer     (dc_context_t*, uint32_t msg_id, const char* setup_code);
void            dc_stop_ongoing_process      (dc_context_t*);
#define         DC_EMPTY_MVBOX               0x01
#define         DC_EMPTY_INBOX               0x02
void            dc_empty_server              (dc_context_t*, int flags);
#define         DC_QR_ASK_VERIFYCONTACT      200
#define         DC_QR_ASK_VERIFYGROUP        202
#define         DC_QR_FPR_OK                 210
#define         DC_QR_FPR_MISMATCH           220
#define         DC_QR_FPR_WITHOUT_ADDR       230
#define         DC_QR_ADDR                   320
#define         DC_QR_TEXT                   330
#define         DC_QR_URL                    332
#define         DC_QR_ERROR                  400
dc_lot_t*       dc_check_qr                  (dc_context_t*, const char* qr);
char*           dc_get_securejoin_qr         (dc_context_t*, uint32_t chat_id);
uint32_t        dc_join_securejoin           (dc_context_t*, const char* qr);
void        dc_send_locations_to_chat       (dc_context_t*, uint32_t chat_id, int seconds);
int         dc_is_sending_locations_to_chat (dc_context_t*, uint32_t chat_id);
int         dc_set_location                 (dc_context_t*, double latitude, double longitude, double accuracy);
dc_array_t* dc_get_locations                (dc_context_t*, uint32_t chat_id, uint32_t contact_id, time_t timestamp_begin, time_t timestamp_end);
void        dc_delete_all_locations         (dc_context_t*);
void             dc_array_unref              (dc_array_t*);
void             dc_array_add_uint           (dc_array_t*, uintptr_t);
void             dc_array_add_id             (dc_array_t*, uint32_t);
void             dc_array_add_ptr            (dc_array_t*, void*);
size_t           dc_array_get_cnt            (const dc_array_t*);
uintptr_t        dc_array_get_uint           (const dc_array_t*, size_t index);
uint32_t         dc_array_get_id             (const dc_array_t*, size_t index);
void*            dc_array_get_ptr            (const dc_array_t*, size_t index);
double           dc_array_get_latitude       (const dc_array_t*, size_t index);
double           dc_array_get_longitude      (const dc_array_t*, size_t index);
double           dc_array_get_accuracy       (const dc_array_t*, size_t index);
time_t           dc_array_get_timestamp      (const dc_array_t*, size_t index);
uint32_t         dc_array_get_chat_id        (const dc_array_t*, size_t index);
uint32_t         dc_array_get_contact_id     (const dc_array_t*, size_t index);
uint32_t         dc_array_get_msg_id         (const dc_array_t*, size_t index);
char*            dc_array_get_marker         (const dc_array_t*, size_t index);
int              dc_array_is_independent     (const dc_array_t*, size_t index);
int              dc_array_search_id          (const dc_array_t*, uint32_t needle, size_t* indx);
const uintptr_t* dc_array_get_raw            (const dc_array_t*);
dc_chatlist_t*   dc_chatlist_new             (dc_context_t*);
void             dc_chatlist_empty           (dc_chatlist_t*);
void             dc_chatlist_unref           (dc_chatlist_t*);
size_t           dc_chatlist_get_cnt         (const dc_chatlist_t*);
uint32_t         dc_chatlist_get_chat_id     (const dc_chatlist_t*, size_t index);
uint32_t         dc_chatlist_get_msg_id      (const dc_chatlist_t*, size_t index);
dc_lot_t*        dc_chatlist_get_summary     (const dc_chatlist_t*, size_t index, dc_chat_t*);
dc_context_t*    dc_chatlist_get_context     (dc_chatlist_t*);
#define         DC_CHAT_ID_DEADDROP          1
#define         DC_CHAT_ID_TRASH             3
#define         DC_CHAT_ID_MSGS_IN_CREATION  4
#define         DC_CHAT_ID_STARRED           5
#define         DC_CHAT_ID_ARCHIVED_LINK     6
#define         DC_CHAT_ID_ALLDONE_HINT      7
#define         DC_CHAT_ID_LAST_SPECIAL      9
#define         DC_CHAT_TYPE_UNDEFINED       0
#define         DC_CHAT_TYPE_SINGLE          100
#define         DC_CHAT_TYPE_GROUP           120
#define         DC_CHAT_TYPE_VERIFIED_GROUP  130
dc_chat_t*      dc_chat_new                  (dc_context_t*);
void            dc_chat_empty                (dc_chat_t*);
void            dc_chat_unref                (dc_chat_t*);
uint32_t        dc_chat_get_id               (const dc_chat_t*);
int             dc_chat_get_type             (const dc_chat_t*);
char*           dc_chat_get_name             (const dc_chat_t*);
char*           dc_chat_get_subtitle         (const dc_chat_t*);
char*           dc_chat_get_profile_image    (const dc_chat_t*);
uint32_t        dc_chat_get_color            (const dc_chat_t*);
int             dc_chat_get_archived         (const dc_chat_t*);
int             dc_chat_is_unpromoted        (const dc_chat_t*);
int             dc_chat_is_self_talk         (const dc_chat_t*);
int             dc_chat_is_verified          (const dc_chat_t*);
int             dc_chat_is_sending_locations (const dc_chat_t*);
#define         DC_MSG_ID_MARKER1            1
#define         DC_MSG_ID_DAYMARKER          9
#define         DC_MSG_ID_LAST_SPECIAL       9
#define         DC_STATE_UNDEFINED           0
#define         DC_STATE_IN_FRESH            10
#define         DC_STATE_IN_NOTICED          13
#define         DC_STATE_IN_SEEN             16
#define         DC_STATE_OUT_PREPARING       18
#define         DC_STATE_OUT_DRAFT           19
#define         DC_STATE_OUT_PENDING         20
#define         DC_STATE_OUT_FAILED          24
#define         DC_STATE_OUT_DELIVERED       26
#define         DC_STATE_OUT_MDN_RCVD        28
#define         DC_MAX_GET_TEXT_LEN          30000
#define         DC_MAX_GET_INFO_LEN          100000
dc_msg_t*       dc_msg_new                    (dc_context_t*, int viewtype);
void            dc_msg_unref                  (dc_msg_t*);
void            dc_msg_empty                  (dc_msg_t*);
uint32_t        dc_msg_get_id                 (const dc_msg_t*);
uint32_t        dc_msg_get_from_id            (const dc_msg_t*);
uint32_t        dc_msg_get_chat_id            (const dc_msg_t*);
int             dc_msg_get_viewtype           (const dc_msg_t*);
int             dc_msg_get_state              (const dc_msg_t*);
time_t          dc_msg_get_timestamp          (const dc_msg_t*);
time_t          dc_msg_get_received_timestamp (const dc_msg_t*);
time_t          dc_msg_get_sort_timestamp     (const dc_msg_t*);
char*           dc_msg_get_text               (const dc_msg_t*);
char*           dc_msg_get_file               (const dc_msg_t*);
char*           dc_msg_get_filename           (const dc_msg_t*);
char*           dc_msg_get_filemime           (const dc_msg_t*);
uint64_t        dc_msg_get_filebytes          (const dc_msg_t*);
int             dc_msg_get_width              (const dc_msg_t*);
int             dc_msg_get_height             (const dc_msg_t*);
int             dc_msg_get_duration           (const dc_msg_t*);
int             dc_msg_get_showpadlock        (const dc_msg_t*);
dc_lot_t*       dc_msg_get_summary            (const dc_msg_t*, const dc_chat_t*);
char*           dc_msg_get_summarytext        (const dc_msg_t*, int approx_characters);
int             dc_msg_has_deviating_timestamp(const dc_msg_t*);
int             dc_msg_has_location           (const dc_msg_t*);
int             dc_msg_is_sent                (const dc_msg_t*);
int             dc_msg_is_starred             (const dc_msg_t*);
int             dc_msg_is_forwarded           (const dc_msg_t*);
int             dc_msg_is_info                (const dc_msg_t*);
int             dc_msg_is_increation          (const dc_msg_t*);
int             dc_msg_is_setupmessage        (const dc_msg_t*);
char*           dc_msg_get_setupcodebegin     (const dc_msg_t*);
void            dc_msg_set_text               (dc_msg_t*, const char* text);
void            dc_msg_set_file               (dc_msg_t*, const char* file, const char* filemime);
void            dc_msg_set_dimension          (dc_msg_t*, int width, int height);
void            dc_msg_set_duration           (dc_msg_t*, int duration);
void            dc_msg_set_location           (dc_msg_t*, double latitude, double longitude);
void            dc_msg_latefiling_mediasize   (dc_msg_t*, int width, int height, int duration);
#define         DC_CONTACT_ID_SELF           1
#define         DC_CONTACT_ID_DEVICE         2
#define         DC_CONTACT_ID_LAST_SPECIAL   9
dc_contact_t*   dc_contact_new               (dc_context_t*);
void            dc_contact_empty             (dc_contact_t*);
void            dc_contact_unref             (dc_contact_t*);
uint32_t        dc_contact_get_id            (const dc_contact_t*);
char*           dc_contact_get_addr          (const dc_contact_t*);
char*           dc_contact_get_name          (const dc_contact_t*);
char*           dc_contact_get_display_name  (const dc_contact_t*);
char*           dc_contact_get_name_n_addr   (const dc_contact_t*);
char*           dc_contact_get_first_name    (const dc_contact_t*);
char*           dc_contact_get_profile_image (const dc_contact_t*);
uint32_t        dc_contact_get_color         (const dc_contact_t*);
int             dc_contact_is_blocked        (const dc_contact_t*);
int             dc_contact_is_verified       (dc_contact_t*);
#define         DC_TEXT1_DRAFT     1
#define         DC_TEXT1_USERNAME  2
#define         DC_TEXT1_SELF      3
dc_lot_t*       dc_lot_new               ();
void            dc_lot_empty             (dc_lot_t*);
void            dc_lot_unref             (dc_lot_t*);
char*           dc_lot_get_text1         (const dc_lot_t*);
char*           dc_lot_get_text2         (const dc_lot_t*);
int             dc_lot_get_text1_meaning (const dc_lot_t*);
int             dc_lot_get_state         (const dc_lot_t*);
uint32_t        dc_lot_get_id            (const dc_lot_t*);
time_t          dc_lot_get_timestamp     (const dc_lot_t*);
#define DC_MSG_TEXT      10
#define DC_MSG_IMAGE     20
#define DC_MSG_GIF       21
#define DC_MSG_AUDIO     40
#define DC_MSG_VOICE     41
#define DC_MSG_VIDEO     50
#define DC_MSG_FILE      60
#define DC_LP_AUTH_OAUTH2                0x2
#define DC_LP_AUTH_NORMAL                0x4
#define DC_LP_IMAP_SOCKET_STARTTLS     0x100
#define DC_LP_IMAP_SOCKET_SSL          0x200
#define DC_LP_IMAP_SOCKET_PLAIN        0x400
#define DC_LP_SMTP_SOCKET_STARTTLS   0x10000
#define DC_LP_SMTP_SOCKET_SSL        0x20000
#define DC_LP_SMTP_SOCKET_PLAIN      0x40000
#define DC_LP_AUTH_FLAGS        (DC_LP_AUTH_OAUTH2|DC_LP_AUTH_NORMAL)
#define DC_LP_IMAP_SOCKET_FLAGS (DC_LP_IMAP_SOCKET_STARTTLS|DC_LP_IMAP_SOCKET_SSL|DC_LP_IMAP_SOCKET_PLAIN)
#define DC_LP_SMTP_SOCKET_FLAGS (DC_LP_SMTP_SOCKET_STARTTLS|DC_LP_SMTP_SOCKET_SSL|DC_LP_SMTP_SOCKET_PLAIN)
#define DC_EVENT_INFO                     100
#define DC_EVENT_SMTP_CONNECTED           101
#define DC_EVENT_IMAP_CONNECTED           102
#define DC_EVENT_SMTP_MESSAGE_SENT        103
#define DC_EVENT_WARNING                  300
#define DC_EVENT_ERROR                    400
#define DC_EVENT_ERROR_NETWORK            401
#define DC_EVENT_ERROR_SELF_NOT_IN_GROUP  410
#define DC_EVENT_MSGS_CHANGED             2000
#define DC_EVENT_INCOMING_MSG             2005
#define DC_EVENT_MSG_DELIVERED            2010
#define DC_EVENT_MSG_FAILED               2012
#define DC_EVENT_MSG_READ                 2015
#define DC_EVENT_CHAT_MODIFIED            2020
#define DC_EVENT_CONTACTS_CHANGED         2030
#define DC_EVENT_LOCATION_CHANGED         2035
#define DC_EVENT_CONFIGURE_PROGRESS       2041
#define DC_EVENT_IMEX_PROGRESS            2051
#define DC_EVENT_IMEX_FILE_WRITTEN        2052
#define DC_EVENT_SECUREJOIN_INVITER_PROGRESS      2060
#define DC_EVENT_SECUREJOIN_JOINER_PROGRESS       2061
#define DC_EVENT_GET_STRING               2091
#define DC_EVENT_HTTP_GET                 2100
#define DC_EVENT_HTTP_POST                2110
#define DC_EVENT_FILE_COPIED         2055
#define DC_EVENT_IS_OFFLINE          2081
#define DC_ERROR_SEE_STRING          0
#define DC_ERROR_SELF_NOT_IN_GROUP   1
#define DC_STR_SELFNOTINGRP          21
#define DC_EVENT_DATA1_IS_STRING(e)  ((e)==DC_EVENT_HTTP_GET || (e)==DC_EVENT_IMEX_FILE_WRITTEN || (e)==DC_EVENT_FILE_COPIED)
#define DC_EVENT_DATA2_IS_STRING(e)  ((e)>=100 && (e)<=499)
#define DC_EVENT_RETURNS_INT(e)      ((e)==DC_EVENT_IS_OFFLINE)
#define DC_EVENT_RETURNS_STRING(e)   ((e)==DC_EVENT_GET_STRING || (e)==DC_EVENT_HTTP_GET)
#define DC_SHOW_EMAILS_OFF               0
#define DC_SHOW_EMAILS_ACCEPTED_CONTACTS 1
#define DC_SHOW_EMAILS_ALL               2
#define DC_STR_NOMESSAGES                 1
#define DC_STR_SELF                       2
#define DC_STR_DRAFT                      3
#define DC_STR_MEMBER                     4
#define DC_STR_CONTACT                    6
#define DC_STR_VOICEMESSAGE               7
#define DC_STR_DEADDROP                   8
#define DC_STR_IMAGE                      9
#define DC_STR_VIDEO                      10
#define DC_STR_AUDIO                      11
#define DC_STR_FILE                       12
#define DC_STR_STATUSLINE                 13
#define DC_STR_NEWGROUPDRAFT              14
#define DC_STR_MSGGRPNAME                 15
#define DC_STR_MSGGRPIMGCHANGED           16
#define DC_STR_MSGADDMEMBER               17
#define DC_STR_MSGDELMEMBER               18
#define DC_STR_MSGGROUPLEFT               19
#define DC_STR_GIF                        23
#define DC_STR_ENCRYPTEDMSG               24
#define DC_STR_E2E_AVAILABLE              25
#define DC_STR_ENCR_TRANSP                27
#define DC_STR_ENCR_NONE                  28
#define DC_STR_CANTDECRYPT_MSG_BODY       29
#define DC_STR_FINGERPRINTS               30
#define DC_STR_READRCPT                   31
#define DC_STR_READRCPT_MAILBODY          32
#define DC_STR_MSGGRPIMGDELETED           33
#define DC_STR_E2E_PREFERRED              34
#define DC_STR_CONTACT_VERIFIED           35
#define DC_STR_CONTACT_NOT_VERIFIED       36
#define DC_STR_CONTACT_SETUP_CHANGED      37
#define DC_STR_ARCHIVEDCHATS              40
#define DC_STR_STARREDMSGS                41
#define DC_STR_AC_SETUP_MSG_SUBJECT       42
#define DC_STR_AC_SETUP_MSG_BODY          43
#define DC_STR_SELFTALK_SUBTITLE          50
#define DC_STR_CANNOT_LOGIN               60
#define DC_STR_SERVER_RESPONSE            61
#define DC_STR_MSGACTIONBYUSER            62
#define DC_STR_MSGACTIONBYME              63
#define DC_STR_MSGLOCATIONENABLED         64
#define DC_STR_MSGLOCATIONDISABLED        65
#define DC_STR_LOCATION                   66
#define DC_STR_COUNT                      66
#ifdef __cplusplus
}
#endif
#endif