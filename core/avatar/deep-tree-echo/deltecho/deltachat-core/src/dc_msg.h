#ifndef __DC_MSG_H__
#define __DC_MSG_H__
#ifdef __cplusplus
extern "C" {
#endif
#include "dc_param.h"
typedef enum {
DC_MOVE_STATE_UNDEFINED = 0
,DC_MOVE_STATE_PENDING = 1
,DC_MOVE_STATE_STAY = 2
,DC_MOVE_STATE_MOVING = 3
} dc_move_state_t;
struct _dc_msg
{
uint32_t magic;
uint32_t id;
uint32_t from_id;
uint32_t to_id;
uint32_t chat_id;
dc_move_state_t move_state;
int type;
int state;
int hidden;
time_t timestamp_sort;
time_t timestamp_sent;
time_t timestamp_rcvd;
char* text;
dc_context_t* context;
char* rfc724_mid;
char* in_reply_to;
char* server_folder;
uint32_t server_uid;
int is_dc_message;
int starred;
int chat_blocked;
uint32_t location_id;
dc_param_t* param;
};
dc_msg_t* dc_msg_new_untyped (dc_context_t*);
dc_msg_t* dc_msg_new_load (dc_context_t*, uint32_t id);
int dc_msg_load_from_db (dc_msg_t*, dc_context_t*, uint32_t id);
int dc_msg_is_increation (const dc_msg_t*);
char* dc_msg_get_summarytext_by_raw (int type, const char* text, dc_param_t*, int approx_bytes, dc_context_t*);
void dc_msg_save_param_to_disk (dc_msg_t*);
void dc_msg_guess_msgtype_from_suffix (const char* pathNfilename, int* ret_msgtype, char** ret_mime);
void dc_delete_msg_from_db (dc_context_t*, uint32_t);
#define DC_MSG_NEEDS_ATTACHMENT(a) ((a)==DC_MSG_IMAGE || (a)==DC_MSG_GIF || (a)==DC_MSG_AUDIO || (a)==DC_MSG_VOICE || (a)==DC_MSG_VIDEO || (a)==DC_MSG_FILE)
#define DC_APPROX_SUBJECT_CHARS 32
int dc_msg_exists (dc_context_t*, uint32_t msg_id);
void dc_update_msg_chat_id (dc_context_t*, uint32_t msg_id, uint32_t chat_id);
void dc_update_msg_state (dc_context_t*, uint32_t msg_id, int state);
void dc_update_msg_move_state (dc_context_t*, const char* rfc724_mid, dc_move_state_t);
void dc_set_msg_failed (dc_context_t*, uint32_t msg_id, const char* error);
int dc_mdn_from_ext (dc_context_t*, uint32_t from_id, const char* rfc724_mid, time_t, uint32_t* ret_chat_id, uint32_t* ret_msg_id);
size_t dc_get_real_msg_cnt (dc_context_t*);
size_t dc_get_deaddrop_msg_cnt (dc_context_t*);
int dc_rfc724_mid_cnt (dc_context_t*, const char* rfc724_mid);
uint32_t dc_rfc724_mid_exists (dc_context_t*, const char* rfc724_mid, char** ret_server_folder, uint32_t* ret_server_uid);
void dc_update_server_uid (dc_context_t*, const char* rfc724_mid, const char* server_folder, uint32_t server_uid);
#ifdef __cplusplus
}
#endif
#endif