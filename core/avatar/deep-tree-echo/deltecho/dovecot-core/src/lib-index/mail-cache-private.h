#ifndef MAIL_CACHE_PRIVATE_H
#define MAIL_CACHE_PRIVATE_H
#include "file-dotlock.h"
#include "mail-index-private.h"
#include "mail-cache.h"
#define MAIL_CACHE_MAJOR_VERSION 1
#define MAIL_CACHE_MINOR_VERSION 1
#define MAIL_CACHE_LOCK_TIMEOUT 10
#define MAIL_CACHE_LOCK_CHANGE_TIMEOUT 300
#define MAIL_CACHE_MAX_WRITE_BUFFER (1024*256)
#define MAIL_CACHE_IS_UNUSABLE(cache) \
((cache)->hdr == NULL)
struct mail_cache_header {
uint8_t major_version;
uint8_t compat_sizeof_uoff_t;
uint8_t minor_version;
uint8_t unused;
uint32_t indexid;
uint32_t file_seq;
uint32_t continued_record_count;
uint32_t record_count;
uint32_t backwards_compat_used_file_size;
uint32_t deleted_record_count;
uint32_t field_header_offset;
};
struct mail_cache_header_fields {
uint32_t next_offset;
uint32_t size;
uint32_t fields_count;
#if 0
uint32_t last_used[fields_count];
uint32_t size[fields_count];
uint8_t type[fields_count];
uint8_t decision[fields_count];
char name[fields_count][];
#endif
};
#define MAIL_CACHE_FIELD_LAST_USED() \
(sizeof(uint32_t) * 3)
#define MAIL_CACHE_FIELD_SIZE(count) \
(MAIL_CACHE_FIELD_LAST_USED() + sizeof(uint32_t) * (count))
#define MAIL_CACHE_FIELD_TYPE(count) \
(MAIL_CACHE_FIELD_SIZE(count) + sizeof(uint32_t) * (count))
#define MAIL_CACHE_FIELD_DECISION(count) \
(MAIL_CACHE_FIELD_TYPE(count) + sizeof(uint8_t) * (count))
#define MAIL_CACHE_FIELD_NAMES(count) \
(MAIL_CACHE_FIELD_DECISION(count) + sizeof(uint8_t) * (count))
struct mail_cache_record {
uint32_t prev_offset;
uint32_t size;
};
struct mail_cache_field_private {
struct mail_cache_field field;
uint32_t uid_highwater;
bool used:1;
bool decision_dirty:1;
};
struct mail_cache {
struct mail_index *index;
struct event *event;
uint32_t ext_id;
char *filepath;
int fd;
struct dotlock_settings dotlock_settings;
struct file_lock *file_lock;
ino_t st_ino;
dev_t st_dev;
uoff_t last_stat_size;
time_t last_mmap_error_time;
void *mmap_base;
struct file_cache *file_cache;
uoff_t read_offset;
buffer_t *read_buf;
size_t mmap_length;
unsigned int remap_counter;
struct mail_cache_view *views;
const struct mail_cache_header *hdr;
struct mail_cache_header hdr_ro_copy;
struct mail_cache_header hdr_copy;
uint32_t last_field_header_offset;
pool_t field_pool;
unsigned int fields_count;
struct mail_cache_field_private *fields;
uint32_t *field_file_map;
HASH_TABLE(char *, void *) field_name_hash;
unsigned int *file_field_map;
unsigned int file_fields_count;
uint32_t need_purge_file_seq;
char *need_purge_reason;
bool opened:1;
bool locked:1;
bool last_lock_failed:1;
bool hdr_modified:1;
bool field_header_write_pending:1;
bool purging:1;
bool map_with_read:1;
bool headers_capped:1;
};
struct mail_cache_loop_track {
uoff_t min_offset, max_offset;
uoff_t size_sum;
};
struct mail_cache_missing_reason_cache {
uint32_t highest_checked_seq;
uint32_t highest_seq_with_cache;
uint32_t reset_id;
uint32_t log_file_head_seq;
uoff_t log_file_head_offset;
};
struct mail_cache_view {
struct mail_cache *cache;
struct mail_cache_view *prev, *next;
struct mail_index_view *view, *trans_view;
struct mail_cache_transaction_ctx *transaction;
uint32_t trans_seq1, trans_seq2;
struct mail_cache_loop_track loop_track;
struct mail_cache_missing_reason_cache reason_cache;
buffer_t *cached_exists_buf;
uint8_t cached_exists_value;
uint32_t cached_exists_seq;
bool no_decision_updates:1;
};
struct mail_cache_iterate_field {
unsigned int field_idx;
unsigned int size;
const void *data;
uoff_t offset;
};
struct mail_cache_lookup_iterate_ctx {
struct mail_cache_view *view;
unsigned int remap_counter;
uint32_t seq;
const struct mail_cache_record *rec;
unsigned int pos;
unsigned int rec_size;
uint32_t offset;
unsigned int trans_next_idx;
bool stop:1;
bool failed:1;
bool memory_appends_checked:1;
bool disk_appends_checked:1;
bool inmemory_field_idx:1;
};
int mail_cache_lock(struct mail_cache *cache);
int mail_cache_flush_and_unlock(struct mail_cache *cache);
void mail_cache_unlock(struct mail_cache *cache);
int mail_cache_write(struct mail_cache *cache, const void *data, size_t size,
uoff_t offset);
int mail_cache_append(struct mail_cache *cache, const void *data, size_t size,
uint32_t *offset);
int mail_cache_header_fields_read(struct mail_cache *cache);
int mail_cache_header_fields_update(struct mail_cache *cache);
void mail_cache_header_fields_get(struct mail_cache *cache, buffer_t *dest);
int mail_cache_header_fields_get_next_offset(struct mail_cache *cache,
uint32_t *offset_r);
void mail_cache_expunge_count(struct mail_cache *cache, unsigned int count);
uint32_t mail_cache_lookup_cur_offset(struct mail_index_view *view,
uint32_t seq, uint32_t *reset_id_r);
int mail_cache_get_record(struct mail_cache *cache, uint32_t offset,
const struct mail_cache_record **rec_r);
uint32_t mail_cache_get_first_new_seq(struct mail_index_view *view);
bool mail_cache_track_loops(struct mail_cache_loop_track *loop_track,
uoff_t offset, uoff_t size);
void mail_cache_lookup_iter_init(struct mail_cache_view *view, uint32_t seq,
struct mail_cache_lookup_iterate_ctx *ctx_r);
int mail_cache_lookup_iter_next(struct mail_cache_lookup_iterate_ctx *ctx,
struct mail_cache_iterate_field *field_r);
const struct mail_cache_record *
mail_cache_transaction_lookup_rec(struct mail_cache_transaction_ctx *ctx,
unsigned int seq,
unsigned int *trans_next_idx);
bool mail_cache_transactions_have_changes(struct mail_cache *cache);
int mail_cache_map(struct mail_cache *cache, size_t offset, size_t size,
const void **data_r);
int mail_cache_map_all(struct mail_cache *cache);
void mail_cache_file_close(struct mail_cache *cache);
int mail_cache_reopen(struct mail_cache *cache);
int mail_cache_sync_reset_id(struct mail_cache *cache);
void mail_cache_decision_state_update(struct mail_cache_view *view,
uint32_t seq, unsigned int field);
const char *mail_cache_decision_to_string(enum mail_cache_decision_type dec);
struct event_passthrough *
mail_cache_decision_changed_event(struct mail_cache *cache, struct event *event,
unsigned int field);
bool mail_cache_headers_check_capped(struct mail_cache *cache);
struct mail_cache_purge_drop_ctx {
struct mail_cache *cache;
time_t max_yes_downgrade_time;
time_t max_temp_drop_time;
};
enum mail_cache_purge_drop_decision {
MAIL_CACHE_PURGE_DROP_DECISION_NONE,
MAIL_CACHE_PURGE_DROP_DECISION_DROP,
MAIL_CACHE_PURGE_DROP_DECISION_TO_TEMP,
};
void mail_cache_purge_drop_init(struct mail_cache *cache,
const struct mail_index_header *hdr,
struct mail_cache_purge_drop_ctx *ctx_r);
enum mail_cache_purge_drop_decision
mail_cache_purge_drop_test(struct mail_cache_purge_drop_ctx *ctx,
unsigned int field);
int mail_cache_expunge_handler(struct mail_index_sync_map_ctx *sync_ctx,
const void *data, void **sync_context);
void mail_cache_set_syscall_error(struct mail_cache *cache,
const char *function) ATTR_COLD;
#endif