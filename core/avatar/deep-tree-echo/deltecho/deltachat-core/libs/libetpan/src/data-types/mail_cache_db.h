#ifndef MAIL_CACHE_DB_H
#define MAIL_CACHE_DB_H
#include <sys/types.h>
#include "mail_cache_db_types.h"
#include "chash.h"
#ifdef __cplusplus
extern "C" {
#endif
int mail_cache_db_open(const char * filename,
struct mail_cache_db ** pcache_db);
void mail_cache_db_close(struct mail_cache_db * cache_db);
int mail_cache_db_open_lock(const char * filename,
struct mail_cache_db ** pcache_db);
void mail_cache_db_close_unlock(const char * filename,
struct mail_cache_db * cache_db);
int mail_cache_db_put(struct mail_cache_db * cache_db,
const void * key, size_t key_len, const void * value, size_t value_len);
int mail_cache_db_get(struct mail_cache_db * cache_db,
const void * key, size_t key_len, void ** pvalue, size_t * pvalue_len);
int mail_cache_db_get_size(struct mail_cache_db * cache_db,
const void * key, size_t key_len, size_t * pvalue_len);
int mail_cache_db_del(struct mail_cache_db * cache_db,
const void * key, size_t key_len);
int mail_cache_db_clean_up(struct mail_cache_db * cache_db,
chash * exist);
int mail_cache_db_get_keys(struct mail_cache_db * cache_db,
chash * keys);
#ifdef __cplusplus
}
#endif
#endif