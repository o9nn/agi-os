#ifndef MAIL_INDEX_H
#define MAIL_INDEX_H
#include "file-lock.h"
#include "fsync-mode.h"
#include "guid.h"
#include "mail-types.h"
#include "seq-range-array.h"
#define MAIL_INDEX_MAJOR_VERSION 7
#define MAIL_INDEX_MINOR_VERSION 3
#define MAIL_INDEX_HEADER_MIN_SIZE 120
#define MAIL_TRANSACTION_LOG_LOCK_WARN_SECS 30
enum mail_index_open_flags {
MAIL_INDEX_OPEN_FLAG_CREATE = 0x01,
MAIL_INDEX_OPEN_FLAG_MMAP_DISABLE = 0x04,
MAIL_INDEX_OPEN_FLAG_DOTLOCK_USE_EXCL = 0x10,
MAIL_INDEX_OPEN_FLAG_NFS_FLUSH = 0x40,
MAIL_INDEX_OPEN_FLAG_READONLY = 0x80,
MAIL_INDEX_OPEN_FLAG_KEEP_BACKUPS = 0x100,
MAIL_INDEX_OPEN_FLAG_NEVER_IN_MEMORY = 0x200,
MAIL_INDEX_OPEN_FLAG_SAVEONLY = 0x400,
MAIL_INDEX_OPEN_FLAG_DEBUG = 0x800,
MAIL_INDEX_OPEN_FLAG_NO_DIRTY = 0x1000,
};
enum mail_index_header_compat_flags {
MAIL_INDEX_COMPAT_LITTLE_ENDIAN = 0x01
};
enum mail_index_header_flag {
MAIL_INDEX_HDR_FLAG_CORRUPTED = 0x0001,
MAIL_INDEX_HDR_FLAG_HAVE_DIRTY = 0x0002,
MAIL_INDEX_HDR_FLAG_FSCKD = 0x0004,
};
enum mail_index_mail_flags {
MAIL_INDEX_MAIL_FLAG_UNUSED = 0x20,
MAIL_INDEX_MAIL_FLAG_BACKEND = 0x40,
MAIL_INDEX_MAIL_FLAG_DIRTY = 0x80,
MAIL_INDEX_MAIL_FLAG_UPDATE_MODSEQ = 0x100
};
enum mail_index_error_code {
MAIL_INDEX_ERROR_CODE_NONE,
MAIL_INDEX_ERROR_CODE_NO_SPACE,
MAIL_INDEX_ERROR_CODE_NO_ACCESS,
};
#define MAIL_INDEX_FLAGS_MASK \
(MAIL_ANSWERED | MAIL_FLAGGED | MAIL_DELETED | MAIL_SEEN | MAIL_DRAFT)
struct mail_index_header {
uint8_t major_version;
uint8_t minor_version;
uint16_t base_header_size;
uint32_t header_size;
uint32_t record_size;
uint8_t compat_flags;
uint8_t unused[3];
uint32_t indexid;
uint32_t flags;
uint32_t uid_validity;
uint32_t next_uid;
uint32_t messages_count;
uint32_t unused_old_recent_messages_count;
uint32_t seen_messages_count;
uint32_t deleted_messages_count;
uint32_t first_recent_uid;
uint32_t first_unseen_uid_lowwater;
uint32_t first_deleted_uid_lowwater;
uint32_t log_file_seq;
uint32_t log_file_tail_offset;
uint32_t log_file_head_offset;
uint32_t unused_old_sync_size_part1;
uint32_t log2_rotate_time;
uint32_t last_temp_file_scan;
uint32_t day_stamp;
uint32_t day_first_uid[8];
};
#define MAIL_INDEX_RECORD_MIN_SIZE (sizeof(uint32_t) + sizeof(uint8_t))
struct mail_index_record {
uint32_t uid;
uint8_t flags;
};
struct mail_keywords {
struct mail_index *index;
unsigned int count;
int refcount;
unsigned int idx[FLEXIBLE_ARRAY_MEMBER];
};
enum mail_index_transaction_flags {
MAIL_INDEX_TRANSACTION_FLAG_HIDE = 0x01,
MAIL_INDEX_TRANSACTION_FLAG_EXTERNAL = 0x02,
MAIL_INDEX_TRANSACTION_FLAG_AVOID_FLAG_UPDATES = 0x04,
MAIL_INDEX_TRANSACTION_FLAG_FSYNC = 0x08,
MAIL_INDEX_TRANSACTION_FLAG_SYNC = 0x10
};
enum mail_index_sync_type {
MAIL_INDEX_SYNC_TYPE_EXPUNGE = 0x02,
MAIL_INDEX_SYNC_TYPE_FLAGS = 0x04,
MAIL_INDEX_SYNC_TYPE_KEYWORD_ADD = 0x08,
MAIL_INDEX_SYNC_TYPE_KEYWORD_REMOVE = 0x10
};
enum mail_index_fsync_mask {
MAIL_INDEX_FSYNC_MASK_APPENDS = 0x01,
MAIL_INDEX_FSYNC_MASK_EXPUNGES = 0x02,
MAIL_INDEX_FSYNC_MASK_FLAGS = 0x04,
MAIL_INDEX_FSYNC_MASK_KEYWORDS = 0x08
};
enum mail_index_sync_flags {
MAIL_INDEX_SYNC_FLAG_FLUSH_DIRTY = 0x01,
MAIL_INDEX_SYNC_FLAG_DROP_RECENT = 0x02,
MAIL_INDEX_SYNC_FLAG_AVOID_FLAG_UPDATES = 0x04,
MAIL_INDEX_SYNC_FLAG_REQUIRE_CHANGES = 0x08,
MAIL_INDEX_SYNC_FLAG_FSYNC = 0x10,
MAIL_INDEX_SYNC_FLAG_DELETING_INDEX = 0x20,
MAIL_INDEX_SYNC_FLAG_TRY_DELETING_INDEX = 0x40,
MAIL_INDEX_SYNC_FLAG_UPDATE_TAIL_OFFSET = 0x80
};
enum mail_index_view_sync_flags {
MAIL_INDEX_VIEW_SYNC_FLAG_NOEXPUNGES = 0x01,
MAIL_INDEX_VIEW_SYNC_FLAG_FIX_INCONSISTENT = 0x02,
MAIL_INDEX_VIEW_SYNC_FLAG_2ND_INDEX = 0x04,
};
struct mail_index_sync_rec {
uint32_t uid1, uid2;
enum mail_index_sync_type type;
uint8_t add_flags;
uint8_t remove_flags;
unsigned int keyword_idx;
guid_128_t guid_128;
};
enum mail_index_view_sync_type {
MAIL_INDEX_VIEW_SYNC_TYPE_FLAGS = 0x01,
MAIL_INDEX_VIEW_SYNC_TYPE_MODSEQ = 0x02
};
struct mail_index_view_sync_rec {
uint32_t uid1, uid2;
enum mail_index_view_sync_type type;
bool hidden:1;
};
enum mail_index_transaction_change {
MAIL_INDEX_TRANSACTION_CHANGE_APPEND = BIT(0),
MAIL_INDEX_TRANSACTION_CHANGE_EXPUNGE = BIT(1),
MAIL_INDEX_TRANSACTION_CHANGE_FLAGS = BIT(2),
MAIL_INDEX_TRANSACTION_CHANGE_KEYWORDS = BIT(3),
MAIL_INDEX_TRANSACTION_CHANGE_MODSEQ = BIT(4),
MAIL_INDEX_TRANSACTION_CHANGE_ATTRIBUTE = BIT(5),
MAIL_INDEX_TRANSACTION_CHANGE_OTHERS = BIT(30),
};
struct mail_index_transaction_commit_result {
uint32_t log_file_seq;
uoff_t log_file_offset;
uoff_t commit_size;
enum mail_index_transaction_change changes_mask;
unsigned int ignored_modseq_changes;
};
struct mail_index_base_optimization_settings {
uoff_t rewrite_min_log_bytes;
uoff_t rewrite_max_log_bytes;
};
struct mail_index_log_optimization_settings {
uoff_t min_size;
uoff_t max_size;
unsigned int min_age_secs;
unsigned int log2_max_age_secs;
};
struct mail_index_cache_optimization_settings {
unsigned int unaccessed_field_drop_secs;
unsigned int record_max_size;
unsigned int max_header_name_length;
unsigned int max_headers_count;
uoff_t max_size;
uoff_t purge_min_size;
unsigned int purge_delete_percentage;
unsigned int purge_continued_percentage;
unsigned int purge_header_continue_count;
};
struct mail_index_optimization_settings {
struct mail_index_base_optimization_settings index;
struct mail_index_log_optimization_settings log;
struct mail_index_cache_optimization_settings cache;
};
struct mail_index;
struct mail_index_map;
struct mail_index_view;
struct mail_index_transaction;
struct mail_index_sync_ctx;
struct mail_index_view_sync_ctx;
struct mail_index *mail_index_alloc(struct event *parent_event,
const char *dir, const char *prefix);
void mail_index_free(struct mail_index **index);
void mail_index_set_cache_dir(struct mail_index *index, const char *dir);
void mail_index_set_fsync_mode(struct mail_index *index, enum fsync_mode mode,
enum mail_index_fsync_mask mask);
bool mail_index_use_existing_permissions(struct mail_index *index);
void mail_index_set_permissions(struct mail_index *index,
mode_t mode, gid_t gid, const char *gid_origin);
void mail_index_set_lock_method(struct mail_index *index,
enum file_lock_method lock_method,
unsigned int max_timeout_secs);
void mail_index_set_optimization_settings(struct mail_index *index,
const struct mail_index_optimization_settings *set);
void mail_index_set_ext_init_data(struct mail_index *index, uint32_t ext_id,
const void *data, size_t size);
int mail_index_open(struct mail_index *index, enum mail_index_open_flags flags);
int mail_index_open_or_create(struct mail_index *index,
enum mail_index_open_flags flags);
void mail_index_close(struct mail_index *index);
int mail_index_unlink(struct mail_index *index);
bool mail_index_is_in_memory(struct mail_index *index);
int mail_index_move_to_memory(struct mail_index *index);
struct mail_cache *mail_index_get_cache(struct mail_index *index);
int ATTR_NOWARN_UNUSED_RESULT
mail_index_refresh(struct mail_index *index);
struct mail_index_view *
mail_index_view_open(struct mail_index *index,
const char *source_filename, unsigned int source_linenum);
#define mail_index_view_open(index) \
mail_index_view_open(index, __FILE__, __LINE__)
void mail_index_view_close(struct mail_index_view **view);
struct mail_index *mail_index_view_get_index(struct mail_index_view *view);
uint32_t mail_index_view_get_messages_count(struct mail_index_view *view);
bool mail_index_view_is_inconsistent(struct mail_index_view *view);
bool mail_index_view_have_transactions(struct mail_index_view *view);
struct mail_index_transaction *
mail_index_transaction_begin(struct mail_index_view *view,
enum mail_index_transaction_flags flags);
int mail_index_transaction_commit(struct mail_index_transaction **t);
int mail_index_transaction_commit_full(struct mail_index_transaction **t,
struct mail_index_transaction_commit_result *result_r);
void mail_index_transaction_rollback(struct mail_index_transaction **t);
void mail_index_transaction_reset(struct mail_index_transaction *t);
void mail_index_transaction_set_max_modseq(struct mail_index_transaction *t,
uint64_t max_modseq,
ARRAY_TYPE(seq_range) *seqs);
struct mail_index_view *
mail_index_transaction_get_view(struct mail_index_transaction *t);
bool mail_index_transaction_is_expunged(struct mail_index_transaction *t,
uint32_t seq);
struct mail_index_view *
mail_index_transaction_open_updated_view(struct mail_index_transaction *t);
int mail_index_sync_begin(struct mail_index *index,
struct mail_index_sync_ctx **ctx_r,
struct mail_index_view **view_r,
struct mail_index_transaction **trans_r,
enum mail_index_sync_flags flags);
int mail_index_sync_begin_to(struct mail_index *index,
struct mail_index_sync_ctx **ctx_r,
struct mail_index_view **view_r,
struct mail_index_transaction **trans_r,
uint32_t log_file_seq, uoff_t log_file_offset,
enum mail_index_sync_flags flags);
bool mail_index_sync_have_any(struct mail_index *index,
enum mail_index_sync_flags flags);
bool mail_index_sync_have_any_expunges(struct mail_index *index);
void mail_index_sync_get_offsets(struct mail_index_sync_ctx *ctx,
uint32_t *seq1_r, uoff_t *offset1_r,
uint32_t *seq2_r, uoff_t *offset2_r);
bool mail_index_sync_next(struct mail_index_sync_ctx *ctx,
struct mail_index_sync_rec *sync_rec);
bool mail_index_sync_have_more(struct mail_index_sync_ctx *ctx);
bool mail_index_sync_has_expunges(struct mail_index_sync_ctx *ctx);
void mail_index_sync_reset(struct mail_index_sync_ctx *ctx);
void mail_index_sync_set_commit_result(struct mail_index_sync_ctx *ctx,
struct mail_index_transaction_commit_result *result);
void mail_index_sync_no_warning(struct mail_index_sync_ctx *ctx);
void mail_index_sync_set_reason(struct mail_index_sync_ctx *ctx,
const char *reason);
int mail_index_sync_commit(struct mail_index_sync_ctx **ctx);
void mail_index_sync_rollback(struct mail_index_sync_ctx **ctx);
int mail_index_lock_sync(struct mail_index *index, const char *lock_reason);
void mail_index_unlock(struct mail_index *index, const char *long_lock_reason);
bool mail_index_is_locked(struct mail_index *index);
void mail_index_mark_corrupted(struct mail_index *index) ATTR_COLD;
int mail_index_fsck(struct mail_index *index) ATTR_COLD;
bool mail_index_reset_fscked(struct mail_index *index);
struct mail_index_view_sync_ctx *
mail_index_view_sync_begin(struct mail_index_view *view,
enum mail_index_view_sync_flags flags);
bool mail_index_view_sync_next(struct mail_index_view_sync_ctx *ctx,
struct mail_index_view_sync_rec *sync_rec);
void
mail_index_view_sync_get_expunges(struct mail_index_view_sync_ctx *ctx,
const ARRAY_TYPE(seq_range) **expunges_r);
int mail_index_view_sync_commit(struct mail_index_view_sync_ctx **ctx,
bool *delayed_expunges_r);
const struct mail_index_header *
mail_index_get_header(struct mail_index_view *view);
const struct mail_index_record *
mail_index_lookup(struct mail_index_view *view, uint32_t seq);
const struct mail_index_record *
mail_index_lookup_full(struct mail_index_view *view, uint32_t seq,
struct mail_index_map **map_r, bool *expunged_r);
bool mail_index_is_expunged(struct mail_index_view *view, uint32_t seq);
void mail_index_lookup_keywords(struct mail_index_view *view, uint32_t seq,
ARRAY_TYPE(keyword_indexes) *keyword_idx);
void mail_index_map_lookup_keywords(struct mail_index_map *map, uint32_t seq,
ARRAY_TYPE(keyword_indexes) *keyword_idx);
void mail_index_lookup_view_flags(struct mail_index_view *view, uint32_t seq,
enum mail_flags *flags_r,
ARRAY_TYPE(keyword_indexes) *keyword_idx);
void mail_index_lookup_uid(struct mail_index_view *view, uint32_t seq,
uint32_t *uid_r);
bool mail_index_lookup_seq_range(struct mail_index_view *view,
uint32_t first_uid, uint32_t last_uid,
uint32_t *first_seq_r, uint32_t *last_seq_r);
bool mail_index_lookup_seq(struct mail_index_view *view,
uint32_t uid, uint32_t *seq_r);
void mail_index_lookup_first(struct mail_index_view *view,
enum mail_flags flags, uint8_t flags_mask,
uint32_t *seq_r);
void mail_index_append(struct mail_index_transaction *t, uint32_t uid,
uint32_t *seq_r);
void mail_index_append_finish_uids_full(struct mail_index_transaction *t,
uint32_t min_allowed_uid,
uint32_t first_new_uid,
ARRAY_TYPE(seq_range) *uids_r);
void mail_index_append_finish_uids(struct mail_index_transaction *t,
uint32_t first_uid,
ARRAY_TYPE(seq_range) *uids_r);
void mail_index_expunge(struct mail_index_transaction *t, uint32_t seq);
void mail_index_expunge_guid(struct mail_index_transaction *t, uint32_t seq,
const guid_128_t guid_128);
void mail_index_revert_changes(struct mail_index_transaction *t, uint32_t seq);
void mail_index_update_flags(struct mail_index_transaction *t, uint32_t seq,
enum modify_type modify_type,
enum mail_flags flags);
void mail_index_update_flags_range(struct mail_index_transaction *t,
uint32_t seq1, uint32_t seq2,
enum modify_type modify_type,
enum mail_flags flags);
void mail_index_attribute_set(struct mail_index_transaction *t,
bool pvt, const char *key,
time_t timestamp, uint32_t value_len);
void mail_index_attribute_unset(struct mail_index_transaction *t,
bool pvt, const char *key, time_t timestamp);
void mail_index_update_modseq(struct mail_index_transaction *t, uint32_t seq,
uint64_t min_modseq);
void mail_index_update_highest_modseq(struct mail_index_transaction *t,
uint64_t min_modseq);
void mail_index_reset(struct mail_index_transaction *t);
void mail_index_unset_fscked(struct mail_index_transaction *t);
void mail_index_set_deleted(struct mail_index_transaction *t);
void mail_index_set_undeleted(struct mail_index_transaction *t);
bool mail_index_is_deleted(struct mail_index *index);
int mail_index_get_modification_time(struct mail_index *index, time_t *mtime_r);
bool mail_index_keyword_lookup(struct mail_index *index,
const char *keyword, unsigned int *idx_r);
void mail_index_keyword_lookup_or_create(struct mail_index *index,
const char *keyword,
unsigned int *idx_r);
const ARRAY_TYPE(keywords) *mail_index_get_keywords(struct mail_index *index);
struct mail_keywords *
mail_index_keywords_create(struct mail_index *index,
const char *const keywords[]) ATTR_NULL(2);
struct mail_keywords *
mail_index_keywords_create_from_indexes(struct mail_index *index,
const ARRAY_TYPE(keyword_indexes)
*keyword_indexes);
void mail_index_keywords_ref(struct mail_keywords *keywords);
void mail_index_keywords_unref(struct mail_keywords **keywords);
void mail_index_update_keywords(struct mail_index_transaction *t, uint32_t seq,
enum modify_type modify_type,
struct mail_keywords *keywords);
void mail_index_update_header(struct mail_index_transaction *t,
size_t offset, const void *data, size_t size,
bool prepend);
const char *mail_index_get_last_error(struct mail_index *index,
enum mail_index_error_code *error_r);
void mail_index_reset_error(struct mail_index *index);
void mail_index_sync_flags_apply(const struct mail_index_sync_rec *sync_rec,
uint8_t *flags);
bool mail_index_sync_keywords_apply(const struct mail_index_sync_rec *sync_rec,
ARRAY_TYPE(keyword_indexes) *keywords);
uint32_t mail_index_ext_register(struct mail_index *index, const char *name,
uint32_t default_hdr_size,
uint16_t default_record_size,
uint16_t default_record_align);
void mail_index_ext_register_resize_defaults(struct mail_index *index,
uint32_t ext_id,
uint32_t default_hdr_size,
uint16_t default_record_size,
uint16_t default_record_align);
bool mail_index_ext_lookup(struct mail_index *index, const char *name,
uint32_t *ext_id_r);
void mail_index_ext_resize(struct mail_index_transaction *t, uint32_t ext_id,
uint32_t hdr_size, uint16_t record_size,
uint16_t record_align);
void mail_index_ext_resize_hdr(struct mail_index_transaction *t,
uint32_t ext_id, uint32_t hdr_size);
void mail_index_ext_reset(struct mail_index_transaction *t, uint32_t ext_id,
uint32_t reset_id, bool clear_data);
void mail_index_ext_reset_inc(struct mail_index_transaction *t, uint32_t ext_id,
uint32_t prev_reset_id, bool clear_data);
void mail_index_ext_set_reset_id(struct mail_index_transaction *t,
uint32_t ext_id, uint32_t reset_id);
bool mail_index_ext_get_reset_id(struct mail_index_view *view,
struct mail_index_map *map,
uint32_t ext_id, uint32_t *reset_id_r);
void mail_index_get_header_ext(struct mail_index_view *view, uint32_t ext_id,
const void **data_r, size_t *data_size_r);
void mail_index_map_get_header_ext(struct mail_index_view *view,
struct mail_index_map *map, uint32_t ext_id,
const void **data_r, size_t *data_size_r);
void mail_index_lookup_ext(struct mail_index_view *view, uint32_t seq,
uint32_t ext_id, const void **data_r,
bool *expunged_r);
void mail_index_lookup_ext_full(struct mail_index_view *view, uint32_t seq,
uint32_t ext_id, struct mail_index_map **map_r,
const void **data_r, bool *expunged_r);
void mail_index_ext_get_size(struct mail_index_map *map, uint32_t ext_id,
uint32_t *hdr_size_r, uint16_t *record_size_r,
uint16_t *record_align_r);
void mail_index_update_header_ext(struct mail_index_transaction *t,
uint32_t ext_id, size_t offset,
const void *data, size_t size);
void mail_index_update_ext(struct mail_index_transaction *t, uint32_t seq,
uint32_t ext_id, const void *data, void *old_data)
ATTR_NULL(5);
int mail_index_atomic_inc_ext(struct mail_index_transaction *t,
uint32_t seq, uint32_t ext_id, int diff);
#endif