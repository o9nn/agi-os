#ifndef MDBOX_MAP_H
#define MDBOX_MAP_H
#include "seq-range-array.h"
struct dbox_file_append_context;
struct mdbox_map_append_context;
struct mdbox_storage;
enum mdbox_map_append_flags {
DBOX_MAP_APPEND_FLAG_ALT	= 0x01
};
struct mdbox_map_mail_index_header {
uint32_t highest_file_id;
uint32_t rebuild_count;
};
struct mdbox_map_mail_index_record {
uint32_t file_id;
uint32_t offset;
uint32_t size;
};
struct mdbox_map_file_msg {
uint32_t map_uid;
uint32_t offset;
uint32_t refcount;
};
ARRAY_DEFINE_TYPE(mdbox_map_file_msg, struct mdbox_map_file_msg);
struct mdbox_map *
mdbox_map_init(struct mdbox_storage *storage, struct mailbox_list *root_list);
void mdbox_map_deinit(struct mdbox_map **map);
int mdbox_map_open(struct mdbox_map *map);
int mdbox_map_open_or_create(struct mdbox_map *map);
int mdbox_map_refresh(struct mdbox_map *map);
bool mdbox_map_is_fscked(struct mdbox_map *map);
uint32_t mdbox_map_get_rebuild_count(struct mdbox_map *map);
int mdbox_map_lookup(struct mdbox_map *map, uint32_t map_uid,
uint32_t *file_id_r, uoff_t *offset_r);
int mdbox_map_lookup_full(struct mdbox_map *map, uint32_t map_uid,
struct mdbox_map_mail_index_record *rec_r,
uint16_t *refcount_r);
int mdbox_map_lookup_seq_full(struct mdbox_map *map, uint32_t seq,
struct mdbox_map_mail_index_record *rec_r,
uint16_t *refcount_r);
uint32_t mdbox_map_lookup_uid(struct mdbox_map *map, uint32_t seq);
unsigned int mdbox_map_get_messages_count(struct mdbox_map *map);
int mdbox_map_get_file_msgs(struct mdbox_map *map, uint32_t file_id,
ARRAY_TYPE(mdbox_map_file_msg) *recs);
struct mdbox_map_atomic_context *mdbox_map_atomic_begin(struct mdbox_map *map);
int mdbox_map_atomic_lock(struct mdbox_map_atomic_context *atomic,
const char *reason);
bool mdbox_map_atomic_is_locked(struct mdbox_map_atomic_context *atomic);
void mdbox_map_atomic_set_failed(struct mdbox_map_atomic_context *atomic);
void mdbox_map_atomic_set_success(struct mdbox_map_atomic_context *atomic);
void mdbox_map_atomic_unset_fscked(struct mdbox_map_atomic_context *atomic);
int mdbox_map_atomic_finish(struct mdbox_map_atomic_context **atomic);
struct mdbox_map_transaction_context *
mdbox_map_transaction_begin(struct mdbox_map_atomic_context *atomic,
bool external);
int mdbox_map_transaction_commit(struct mdbox_map_transaction_context *ctx,
const char *reason);
void mdbox_map_transaction_free(struct mdbox_map_transaction_context **ctx);
int mdbox_map_update_refcount(struct mdbox_map_transaction_context *ctx,
uint32_t map_uid, int diff);
int mdbox_map_update_refcounts(struct mdbox_map_transaction_context *ctx,
const ARRAY_TYPE(uint32_t) *map_uids, int diff);
int mdbox_map_remove_file_id(struct mdbox_map *map, uint32_t file_id);
int mdbox_map_get_zero_ref_files(struct mdbox_map *map,
ARRAY_TYPE(seq_range) *file_ids_r);
struct mdbox_map_append_context *
mdbox_map_append_begin(struct mdbox_map_atomic_context *atomic);
int mdbox_map_append_next(struct mdbox_map_append_context *ctx, uoff_t mail_size,
enum mdbox_map_append_flags flags,
struct dbox_file_append_context **file_append_ctx_r,
struct ostream **output_r);
void mdbox_map_append_finish(struct mdbox_map_append_context *ctx);
void mdbox_map_append_abort(struct mdbox_map_append_context *ctx);
int mdbox_map_append_assign_map_uids(struct mdbox_map_append_context *ctx,
uint32_t *first_map_uid_r,
uint32_t *last_map_uid_r);
int mdbox_map_append_move(struct mdbox_map_append_context *ctx,
const ARRAY_TYPE(uint32_t) *map_uids,
const ARRAY_TYPE(seq_range) *expunge_map_uids);
int mdbox_map_append_flush(struct mdbox_map_append_context *ctx);
int mdbox_map_append_commit(struct mdbox_map_append_context *ctx);
void mdbox_map_append_free(struct mdbox_map_append_context **ctx);
uint32_t mdbox_map_get_uid_validity(struct mdbox_map *map);
void mdbox_map_set_corrupted(struct mdbox_map *map, const char *format, ...)
ATTR_FORMAT(2, 3);
#endif