#ifndef MAIL_TRANSACTION_LOG_H
#define MAIL_TRANSACTION_LOG_H
#include "mail-index.h"
#define MAIL_TRANSACTION_LOG_SUFFIX ".log"
#define MAIL_TRANSACTION_LOG_MAJOR_VERSION 1
#define MAIL_TRANSACTION_LOG_MINOR_VERSION 3
#define MAIL_TRANSACTION_LOG_HEADER_MIN_SIZE 24
#define MAIL_TRANSACTION_LOG_VERSION_FULL(major, minor) \
((major) << 8 | (minor))
#define MAIL_TRANSACTION_LOG_VERSION_HAVE(version, wanted_feature) \
((version) >= MAIL_TRANSACTION_LOG_VERSION_FEATURE_##wanted_feature)
#define MAIL_TRANSACTION_LOG_HDR_VERSION(hdr) \
MAIL_TRANSACTION_LOG_VERSION_FULL((hdr)->major_version, (hdr)->minor_version)
#define MAIL_TRANSACTION_LOG_VERSION_FEATURE_COMPAT_FLAGS \
MAIL_TRANSACTION_LOG_VERSION_FULL(1, 2)
#define MAIL_TRANSACTION_LOG_VERSION_FEATURE_HIDE_INTERNAL_MODSEQS \
MAIL_TRANSACTION_LOG_VERSION_FULL(1, 3)
struct mail_transaction_log_header {
uint8_t major_version;
uint8_t minor_version;
uint16_t hdr_size;
uint32_t indexid;
uint32_t file_seq;
uint32_t prev_file_seq;
uint32_t prev_file_offset;
uint32_t create_stamp;
uint64_t initial_modseq;
uint8_t compat_flags;
uint8_t unused[3];
uint32_t unused2;
};
enum mail_transaction_type {
MAIL_TRANSACTION_EXPUNGE = 0x00000001,
MAIL_TRANSACTION_APPEND = 0x00000002,
MAIL_TRANSACTION_FLAG_UPDATE = 0x00000004,
MAIL_TRANSACTION_HEADER_UPDATE = 0x00000020,
MAIL_TRANSACTION_EXT_INTRO = 0x00000040,
MAIL_TRANSACTION_EXT_RESET = 0x00000080,
MAIL_TRANSACTION_EXT_HDR_UPDATE = 0x00000100,
MAIL_TRANSACTION_EXT_REC_UPDATE = 0x00000200,
MAIL_TRANSACTION_KEYWORD_UPDATE = 0x00000400,
MAIL_TRANSACTION_KEYWORD_RESET = 0x00000800,
MAIL_TRANSACTION_EXT_ATOMIC_INC = 0x00001000,
MAIL_TRANSACTION_EXPUNGE_GUID = 0x00002000,
MAIL_TRANSACTION_MODSEQ_UPDATE = 0x00008000,
MAIL_TRANSACTION_EXT_HDR_UPDATE32 = 0x00010000,
MAIL_TRANSACTION_INDEX_DELETED = 0x00020000,
MAIL_TRANSACTION_INDEX_UNDELETED = 0x00040000,
MAIL_TRANSACTION_BOUNDARY = 0x00080000,
MAIL_TRANSACTION_ATTRIBUTE_UPDATE = 0x00100000,
MAIL_TRANSACTION_TYPE_MASK = 0x0fffffff,
#define MAIL_TRANSACTION_EXT_MASK \
(MAIL_TRANSACTION_EXT_INTRO | MAIL_TRANSACTION_EXT_RESET | \
MAIL_TRANSACTION_EXT_HDR_UPDATE | MAIL_TRANSACTION_EXT_HDR_UPDATE32 | \
MAIL_TRANSACTION_EXT_REC_UPDATE | MAIL_TRANSACTION_EXT_ATOMIC_INC)
MAIL_TRANSACTION_EXPUNGE_PROT = 0x0000cd90,
MAIL_TRANSACTION_EXTERNAL = 0x10000000,
MAIL_TRANSACTION_SYNC = 0x20000000
};
struct mail_transaction_header {
uint32_t size;
uint32_t type;
};
struct mail_transaction_modseq_update {
uint32_t uid;
uint32_t modseq_low32;
uint32_t modseq_high32;
};
struct mail_transaction_expunge {
uint32_t uid1, uid2;
};
struct mail_transaction_expunge_guid {
uint32_t uid;
guid_128_t guid_128;
};
struct mail_transaction_flag_update {
uint32_t uid1, uid2;
uint8_t add_flags;
uint8_t remove_flags;
uint8_t modseq_inc_flag;
uint8_t padding;
};
struct mail_transaction_keyword_update {
uint8_t modify_type;
uint8_t padding;
uint16_t name_size;
};
struct mail_transaction_keyword_reset {
uint32_t uid1, uid2;
};
struct mail_transaction_header_update {
uint16_t offset;
uint16_t size;
};
enum {
MAIL_TRANSACTION_EXT_INTRO_FLAG_NO_SHRINK = 0x01
};
struct mail_transaction_ext_intro {
uint32_t ext_id;
uint32_t reset_id;
uint32_t hdr_size;
uint16_t record_size;
uint16_t record_align;
uint16_t flags;
uint16_t name_size;
};
struct mail_transaction_ext_reset {
uint32_t new_reset_id;
uint8_t preserve_data;
uint8_t unused_padding[3];
};
struct mail_transaction_ext_hdr_update {
uint16_t offset;
uint16_t size;
};
struct mail_transaction_ext_hdr_update32 {
uint32_t offset;
uint32_t size;
};
struct mail_transaction_ext_rec_update {
uint32_t uid;
};
struct mail_transaction_ext_atomic_inc {
uint32_t uid;
int32_t diff;
};
struct mail_transaction_boundary {
uint32_t size;
};
struct mail_transaction_log_append_ctx {
struct mail_transaction_log *log;
buffer_t *output;
enum mail_transaction_type trans_flags;
uint64_t new_highest_modseq;
unsigned int transaction_count;
bool index_sync_transaction:1;
bool tail_offset_changed:1;
bool sync_includes_this:1;
bool want_fsync:1;
};
#define LOG_IS_BEFORE(seq1, offset1, seq2, offset2) \
(((offset1) < (offset2) && (seq1) == (seq2)) || (seq1) < (seq2))
struct mail_transaction_log *
mail_transaction_log_alloc(struct mail_index *index);
void mail_transaction_log_free(struct mail_transaction_log **log);
int mail_transaction_log_open(struct mail_transaction_log *log);
int mail_transaction_log_create(struct mail_transaction_log *log, bool reset);
void mail_transaction_log_close(struct mail_transaction_log *log);
void mail_transaction_log_indexid_changed(struct mail_transaction_log *log);
void mail_transaction_log_get_mailbox_sync_pos(struct mail_transaction_log *log,
uint32_t *file_seq_r,
uoff_t *file_offset_r);
void mail_transaction_log_set_mailbox_sync_pos(struct mail_transaction_log *log,
uint32_t file_seq,
uoff_t file_offset);
struct mail_transaction_log_view *
mail_transaction_log_view_open(struct mail_transaction_log *log);
void mail_transaction_log_view_close(struct mail_transaction_log_view **view);
int mail_transaction_log_view_set(struct mail_transaction_log_view *view,
uint32_t min_file_seq, uoff_t min_file_offset,
uint32_t max_file_seq, uoff_t max_file_offset,
bool *reset_r, const char **reason_r);
int mail_transaction_log_view_set_all(struct mail_transaction_log_view *view);
void mail_transaction_log_view_clear(struct mail_transaction_log_view *view,
uint32_t oldest_file_seq);
int mail_transaction_log_view_next(struct mail_transaction_log_view *view,
const struct mail_transaction_header **hdr_r,
const void **data_r);
void mail_transaction_log_view_mark(struct mail_transaction_log_view *view);
void mail_transaction_log_view_rewind(struct mail_transaction_log_view *view);
void
mail_transaction_log_view_get_prev_pos(struct mail_transaction_log_view *view,
uint32_t *file_seq_r,
uoff_t *file_offset_r);
uint64_t
mail_transaction_log_view_get_prev_modseq(struct mail_transaction_log_view *view);
bool mail_transaction_log_view_is_last(struct mail_transaction_log_view *view);
void
mail_transaction_log_view_set_corrupted(struct mail_transaction_log_view *view,
const char *fmt, ...)
ATTR_FORMAT(2, 3) ATTR_COLD;
bool
mail_transaction_log_view_is_corrupted(struct mail_transaction_log_view *view);
int mail_transaction_log_append_begin(struct mail_index *index,
enum mail_transaction_type flags,
struct mail_transaction_log_append_ctx **ctx_r);
void mail_transaction_log_append_add(struct mail_transaction_log_append_ctx *ctx,
enum mail_transaction_type type,
const void *data, size_t size);
int mail_transaction_log_append_commit(struct mail_transaction_log_append_ctx **ctx);
int mail_transaction_log_sync_lock(struct mail_transaction_log *log,
const char *lock_reason,
uint32_t *file_seq_r, uoff_t *file_offset_r);
void mail_transaction_log_sync_unlock(struct mail_transaction_log *log,
const char *lock_reason);
void mail_transaction_log_get_head(struct mail_transaction_log *log,
uint32_t *file_seq_r, uoff_t *file_offset_r);
void mail_transaction_log_get_tail(struct mail_transaction_log *log,
uint32_t *file_seq_r);
bool mail_transaction_log_is_head_prev(struct mail_transaction_log *log,
uint32_t file_seq, uoff_t file_offset);
int mail_transaction_log_move_to_memory(struct mail_transaction_log *log);
int mail_transaction_log_unlink(struct mail_transaction_log *log);
#endif