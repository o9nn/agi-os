#ifndef MAIL_INDEX_STRMAP_H
#define MAIL_INDEX_STRMAP_H
#include "hash2.h"
struct mail_index;
struct mail_index_view;
struct mail_index_strmap_header {
#define MAIL_INDEX_STRMAP_VERSION 1
uint8_t version;
uint8_t unused[3];
uint32_t uid_validity;
};
struct mail_index_strmap_rec {
uint32_t uid;
uint32_t ref_index;
uint32_t str_idx;
};
ARRAY_DEFINE_TYPE(mail_index_strmap_rec, struct mail_index_strmap_rec);
typedef bool
mail_index_strmap_key_cmp_t(const char *key,
const struct mail_index_strmap_rec *rec,
void *context);
typedef int
mail_index_strmap_rec_cmp_t(const struct mail_index_strmap_rec *rec1,
const struct mail_index_strmap_rec *rec2,
void *context);
typedef void mail_index_strmap_remap_t(const uint32_t *idx_map,
unsigned int old_count,
unsigned int new_count, void *context);
struct mail_index_strmap *
mail_index_strmap_init(struct mail_index *index, const char *suffix);
void mail_index_strmap_deinit(struct mail_index_strmap **strmap);
struct mail_index_strmap_view *
mail_index_strmap_view_open(struct mail_index_strmap *strmap,
struct mail_index_view *idx_view,
mail_index_strmap_key_cmp_t *key_compare_cb,
mail_index_strmap_rec_cmp_t *rec_compare_cb,
mail_index_strmap_remap_t *remap_cb,
void *context,
const ARRAY_TYPE(mail_index_strmap_rec) **recs_r,
const struct hash2_table **hash_r);
void mail_index_strmap_view_close(struct mail_index_strmap_view **view);
void mail_index_strmap_view_set_corrupted(struct mail_index_strmap_view *view)
ATTR_COLD;
uint32_t mail_index_strmap_view_get_highest_idx(struct mail_index_strmap_view *view);
struct mail_index_strmap_view_sync *
mail_index_strmap_view_sync_init(struct mail_index_strmap_view *view,
uint32_t *last_uid_r);
void mail_index_strmap_view_sync_add(struct mail_index_strmap_view_sync *sync,
uint32_t uid, uint32_t ref_index,
const char *key);
void mail_index_strmap_view_sync_add_unique(struct mail_index_strmap_view_sync *sync,
uint32_t uid, uint32_t ref_index);
void mail_index_strmap_view_sync_commit(struct mail_index_strmap_view_sync **sync);
void mail_index_strmap_view_sync_rollback(struct mail_index_strmap_view_sync **sync);
#endif