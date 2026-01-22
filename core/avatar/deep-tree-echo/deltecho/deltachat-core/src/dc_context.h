#ifndef __DC_CONTEXT_H__
#define __DC_CONTEXT_H__
#ifdef __cplusplus
extern "C" {
#endif
#include <stdlib.h>
#include <string.h>
#include <pthread.h>
#include <libetpan/libetpan.h>
#include "deltachat.h"
#include "dc_sqlite3.h"
#include "dc_tools.h"
#include "dc_strbuilder.h"
#include "dc_strencode.h"
#include "dc_param.h"
#include "dc_stock.h"
#include "dc_array.h"
#include "dc_chat.h"
#include "dc_chatlist.h"
#include "dc_lot.h"
#include "dc_msg.h"
#include "dc_contact.h"
#include "dc_jobthread.h"
#include "dc_imap.h"
#include "dc_smtp.h"
#include "dc_job.h"
#include "dc_mimeparser.h"
#include "dc_hash.h"
struct _dc_context
{
#define          DC_CONTEXT_MAGIC         0x11a11807
uint32_t         magic;
void*            userdata;
char*            dbfile;
char*            blobdir;
dc_sqlite3_t*    sql;
dc_imap_t*       inbox;
pthread_mutex_t  inboxidle_condmutex;
int              perform_inbox_jobs_needed;
int              probe_imap_network;
dc_jobthread_t   sentbox_thread;
dc_jobthread_t   mvbox_thread;
dc_smtp_t*       smtp;
pthread_cond_t   smtpidle_cond;
pthread_mutex_t  smtpidle_condmutex;
int              smtpidle_condflag;
int              smtp_suspended;
int              smtp_doing_jobs;
#define          DC_JOBS_NEEDED_AT_ONCE   1
#define          DC_JOBS_NEEDED_AVOID_DOS 2
int              perform_smtp_jobs_needed;
int              probe_smtp_network;
pthread_mutex_t  oauth2_critical;
dc_callback_t    cb;
char*            os_name;
uint32_t         cmdline_sel_chat_id;
#define          DC_VC_AUTH_REQUIRED     2
#define          DC_VC_CONTACT_CONFIRM   6
int              bob_expects;
#define          DC_BOB_ERROR       0
#define          DC_BOB_SUCCESS     1
int              bobs_status;
dc_lot_t*        bobs_qr_scan;
pthread_mutex_t  bobs_qr_critical;
time_t           last_smeared_timestamp;
pthread_mutex_t  smear_critical;
int              ongoing_running;
int              shall_stop_ongoing;
};
void            dc_log_event         (dc_context_t*, int event_code, int data1, const char* msg, ...);
void            dc_log_event_seq     (dc_context_t*, int event_code, int* sequence_start, const char* msg, ...);
void            dc_log_error         (dc_context_t*, int data1, const char* msg, ...);
void            dc_log_warning       (dc_context_t*, int data1, const char* msg, ...);
void            dc_log_info          (dc_context_t*, int data1, const char* msg, ...);
void            dc_receive_imf       (dc_context_t*, const char* imf_raw_not_terminated, size_t imf_raw_bytes, const char* server_folder, uint32_t server_uid, uint32_t flags);
#define         DC_NOT_CONNECTED     0
#define         DC_ALREADY_CONNECTED 1
#define         DC_JUST_CONNECTED    2
int             dc_connect_to_configured_imap (dc_context_t*, dc_imap_t*);
#define         DC_CREATE_MVBOX      0x01
#define         DC_FOLDERS_CONFIGURED_VERSION 3
void            dc_configure_folders (dc_context_t*, dc_imap_t*, int flags);
void            dc_do_heuristics_moves(dc_context_t*, const char* folder, uint32_t msg_id);
int             dc_is_inbox          (dc_context_t*, const char* folder);
int             dc_is_sentbox        (dc_context_t*, const char* folder);
int             dc_is_mvbox          (dc_context_t*, const char* folder);
typedef struct _dc_location
{
#define DC_ARRAY_LOCATIONS  1
uint32_t location_id;
double   latitude;
double   longitude;
double   accuracy;
time_t   timestamp;
uint32_t contact_id;
uint32_t msg_id;
uint32_t chat_id;
char*    marker;
int      independent;
} dc_location_t;
typedef struct _dc_kml
{
char*         addr;
dc_array_t*   locations;
int           tag;
dc_location_t curr;
} dc_kml_t;
char*           dc_get_location_kml       (dc_context_t*, uint32_t chat_id, uint32_t* last_added_location_id);
char*           dc_get_message_kml        (dc_context_t*, time_t timestamp, double latitude, double longitude);
void            dc_set_kml_sent_timestamp (dc_context_t*, uint32_t chat_id, time_t);
void            dc_set_msg_location_id    (dc_context_t*, uint32_t msg_id, uint32_t location_id);
uint32_t        dc_save_locations         (dc_context_t*, uint32_t chat_id, uint32_t contact_id, const dc_array_t*, int independent);
dc_kml_t*       dc_kml_parse              (dc_context_t*, const char* content, size_t content_bytes);
void            dc_kml_unref              (dc_kml_t*);
void            dc_job_do_DC_JOB_MAYBE_SEND_LOCATIONS (dc_context_t*, dc_job_t*);
void            dc_job_do_DC_JOB_MAYBE_SEND_LOC_ENDED (dc_context_t*, dc_job_t*);
#define         DC_BAK_PREFIX                "delta-chat"
#define         DC_BAK_SUFFIX                "bak"
#define DC_MSGSIZE_MAX_RECOMMENDED  ((24*1024*1024)/4*3)
#define DC_MSGSIZE_UPPER_LIMIT      ((49*1024*1024)/4*3)
#define DC_E2EE_DEFAULT_ENABLED   1
#define DC_MDNS_DEFAULT_ENABLED   1
#define DC_INBOX_WATCH_DEFAULT    1
#define DC_SENTBOX_WATCH_DEFAULT  1
#define DC_MVBOX_WATCH_DEFAULT    1
#define DC_MVBOX_MOVE_DEFAULT     1
#define DC_SHOW_EMAILS_DEFAULT    DC_SHOW_EMAILS_OFF
typedef struct _dc_e2ee_helper dc_e2ee_helper_t;
struct _dc_e2ee_helper {
int        encryption_successfull;
void*      cdata_to_free;
int        encrypted;
dc_hash_t* signatures;
dc_hash_t* gossipped_addr;
};
void            dc_e2ee_encrypt      (dc_context_t*, const clist* recipients_addr,
int force_plaintext, int e2ee_guaranteed, int min_verified,
int do_gossip, struct mailmime* in_out_message, dc_e2ee_helper_t*);
void            dc_e2ee_decrypt      (dc_context_t*, struct mailmime* in_out_message, dc_e2ee_helper_t*);
void            dc_e2ee_thanks       (dc_e2ee_helper_t*);
int             dc_ensure_secret_key_exists (dc_context_t*);
char*           dc_create_setup_code (dc_context_t*);
char*           dc_normalize_setup_code(dc_context_t*, const char* passphrase);
char*           dc_render_setup_file (dc_context_t*, const char* passphrase);
char*           dc_decrypt_setup_file(dc_context_t*, const char* passphrase, const char* filecontent);
extern int      dc_shall_stop_ongoing;
int             dc_has_ongoing       (dc_context_t*);
int             dc_alloc_ongoing     (dc_context_t*);
void            dc_free_ongoing      (dc_context_t*);
#define         DC_HANDSHAKE_CONTINUE_NORMAL_PROCESSING 0x01
#define         DC_HANDSHAKE_STOP_NORMAL_PROCESSING     0x02
#define         DC_HANDSHAKE_ADD_DELETE_JOB             0x04
int             dc_handle_securejoin_handshake(dc_context_t*, dc_mimeparser_t*, uint32_t contact_id);
void            dc_handle_degrade_event       (dc_context_t*, dc_apeerstate_t*);
#define DC_OPENPGP4FPR_SCHEME "OPENPGP4FPR:"
void            dc_add_to_keyhistory(dc_context_t*, const char* rfc724_mid, time_t, const char* addr, const char* fingerprint);
#ifdef __cplusplus
}
#endif
#endif