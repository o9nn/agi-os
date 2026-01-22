#ifndef MAILMBOX_TYPES_H
#define MAILMBOX_TYPES_H
#ifdef __cplusplus
extern "C" {
#endif
#include <sys/types.h>
#include <libetpan/libetpan-config.h>
#include <libetpan/mailimf.h>
#include <libetpan/carray.h>
#include <libetpan/chash.h>
enum {
MAILMBOX_NO_ERROR = 0,
MAILMBOX_ERROR_PARSE,
MAILMBOX_ERROR_INVAL,
MAILMBOX_ERROR_FILE_NOT_FOUND,
MAILMBOX_ERROR_MEMORY,
MAILMBOX_ERROR_TEMPORARY_FILE,
MAILMBOX_ERROR_FILE,
MAILMBOX_ERROR_MSG_NOT_FOUND,
MAILMBOX_ERROR_READONLY
};
struct mailmbox_folder {
char mb_filename[PATH_MAX];
time_t mb_mtime;
int mb_fd;
int mb_read_only;
int mb_no_uid;
int mb_changed;
unsigned int mb_deleted_count;
char * mb_mapping;
size_t mb_mapping_size;
uint32_t mb_written_uid;
uint32_t mb_max_uid;
chash * mb_hash;
carray * mb_tab;
};
struct mailmbox_folder * mailmbox_folder_new(const char * mb_filename);
void mailmbox_folder_free(struct mailmbox_folder * folder);
struct mailmbox_msg_info {
unsigned int msg_index;
uint32_t msg_uid;
int msg_written_uid;
int msg_deleted;
size_t msg_start;
size_t msg_start_len;
size_t msg_headers;
size_t msg_headers_len;
size_t msg_body;
size_t msg_body_len;
size_t msg_size;
size_t msg_padding;
};
int mailmbox_msg_info_update(struct mailmbox_folder * folder,
size_t msg_start, size_t msg_start_len,
size_t msg_headers, size_t msg_headers_len,
size_t msg_body, size_t msg_body_len,
size_t msg_size, size_t msg_padding,
uint32_t msg_uid);
struct mailmbox_msg_info *
mailmbox_msg_info_new(size_t msg_start, size_t msg_start_len,
size_t msg_headers, size_t msg_headers_len,
size_t msg_body, size_t msg_body_len,
size_t msg_size, size_t msg_padding,
uint32_t msg_uid);
void mailmbox_msg_info_free(struct mailmbox_msg_info * info);
struct mailmbox_append_info {
const char * ai_message;
size_t ai_size;
unsigned int ai_uid;
};
struct mailmbox_append_info *
mailmbox_append_info_new(const char * ai_message, size_t ai_size);
void mailmbox_append_info_free(struct mailmbox_append_info * info);
#ifdef __cplusplus
}
#endif
#endif