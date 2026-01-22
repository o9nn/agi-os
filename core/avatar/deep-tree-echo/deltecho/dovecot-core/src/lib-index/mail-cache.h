#ifndef MAIL_CACHE_H
#define MAIL_CACHE_H
#include "mail-index.h"
#define MAIL_CACHE_FILE_SUFFIX ".cache"
struct mail_cache;
struct mail_cache_view;
struct mail_cache_transaction_ctx;
enum mail_cache_decision_type {
MAIL_CACHE_DECISION_NO		= 0x00,
MAIL_CACHE_DECISION_TEMP	= 0x01,
MAIL_CACHE_DECISION_YES		= 0x02,
MAIL_CACHE_DECISION_FORCED	= 0x80
};
enum mail_cache_field_type {
MAIL_CACHE_FIELD_FIXED_SIZE,
MAIL_CACHE_FIELD_VARIABLE_SIZE,
MAIL_CACHE_FIELD_STRING,
MAIL_CACHE_FIELD_BITMASK,
MAIL_CACHE_FIELD_HEADER,
MAIL_CACHE_FIELD_COUNT
};
struct mail_cache_field {
const char *name;
unsigned int idx;
enum mail_cache_field_type type;
unsigned int field_size;
enum mail_cache_decision_type decision;
time_t last_used;
};
struct mail_cache *mail_cache_open_or_create(struct mail_index *index);
struct mail_cache *
mail_cache_open_or_create_path(struct mail_index *index, const char *path);
void mail_cache_free(struct mail_cache **cache);
#define MAIL_CACHE_TRUNCATE_NAME_FAIL NULL
void mail_cache_register_fields(struct mail_cache *cache,
struct mail_cache_field *fields,
unsigned int fields_count,
pool_t fields_modify_pool);
unsigned int
mail_cache_register_lookup(struct mail_cache *cache, const char *name);
const struct mail_cache_field *
mail_cache_register_get_field(struct mail_cache *cache, unsigned int field_idx);
struct mail_cache_field *
mail_cache_register_get_list(struct mail_cache *cache, pool_t *pool_r,
unsigned int *count_r);
bool mail_cache_need_purge(struct mail_cache *cache, const char **reason_r);
void mail_cache_purge_later(struct mail_cache *cache,
const char *reason_format, ...) ATTR_FORMAT(2, 3);
void mail_cache_purge_later_reset(struct mail_cache *cache);
int mail_cache_purge_with_trans(struct mail_cache *cache,
struct mail_index_transaction *trans,
uint32_t purge_file_seq, const char *reason);
int mail_cache_purge(struct mail_cache *cache, uint32_t purge_file_seq,
const char *reason);
bool mail_cache_exists(struct mail_cache *cache);
int mail_cache_open_and_verify(struct mail_cache *cache);
struct mail_cache_view *
mail_cache_view_open(struct mail_cache *cache, struct mail_index_view *iview);
void mail_cache_view_close(struct mail_cache_view **view);
void mail_cache_view_update_cache_decisions(struct mail_cache_view *view,
bool update);
int mail_cache_decisions_copy(struct mail_cache *src, struct mail_cache *dst);
struct mail_cache_transaction_ctx *
mail_cache_get_transaction(struct mail_cache_view *view,
struct mail_index_transaction *t);
void mail_cache_transaction_reset(struct mail_cache_transaction_ctx *ctx);
int mail_cache_transaction_commit(struct mail_cache_transaction_ctx **ctx);
void mail_cache_transaction_rollback(struct mail_cache_transaction_ctx **ctx);
void mail_cache_add(struct mail_cache_transaction_ctx *ctx, uint32_t seq,
unsigned int field_idx, const void *data, size_t data_size);
bool mail_cache_field_want_add(struct mail_cache_transaction_ctx *ctx,
uint32_t seq, unsigned int field_idx);
bool mail_cache_field_can_add(struct mail_cache_transaction_ctx *ctx,
uint32_t seq, unsigned int field_idx);
void mail_cache_close_mail(struct mail_cache_transaction_ctx *ctx,
uint32_t seq);
int mail_cache_field_exists(struct mail_cache_view *view, uint32_t seq,
unsigned int field_idx);
bool mail_cache_field_exists_any(struct mail_cache_view *view, uint32_t seq);
enum mail_cache_decision_type
mail_cache_field_get_decision(struct mail_cache *cache, unsigned int field_idx);
void mail_cache_decision_add(struct mail_cache_view *view, uint32_t seq,
unsigned int field, bool *rejected_r);
int mail_cache_lookup_field(struct mail_cache_view *view, buffer_t *dest_buf,
uint32_t seq, unsigned int field_idx);
int mail_cache_lookup_headers(struct mail_cache_view *view, string_t *dest,
uint32_t seq, const unsigned int field_idxs[],
unsigned int fields_count);
void mail_cache_set_corrupted(struct mail_cache *cache, const char *fmt, ...)
ATTR_FORMAT(2, 3) ATTR_COLD;
void mail_cache_set_seq_corrupted_reason(struct mail_cache_view *cache_view,
uint32_t seq, const char *reason)
ATTR_COLD;
const char *
mail_cache_get_missing_reason(struct mail_cache_view *view, uint32_t seq);
#endif