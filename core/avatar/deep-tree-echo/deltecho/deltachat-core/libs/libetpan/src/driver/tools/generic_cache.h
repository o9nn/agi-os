#ifndef GENERIC_CACHE_H
#define GENERIC_CACHE_H
#ifdef __cplusplus
extern "C" {
#endif
#include "generic_cache_types.h"
#include "mailmessage_types.h"
#include "chash.h"
#include "carray.h"
#include "mail_cache_db_types.h"
int generic_cache_create_dir(char * dirname);
int generic_cache_store(char * filename, char * content, size_t length);
int generic_cache_read(char * filename, char ** result, size_t * result_len);
int generic_cache_fields_read(struct mail_cache_db * cache_db,
MMAPString * mmapstr,
char * keyname, struct mailimf_fields ** result);
int generic_cache_fields_write(struct mail_cache_db * cache_db,
MMAPString * mmapstr,
char * keyname, struct mailimf_fields * fields);
int generic_cache_flags_read(struct mail_cache_db * cache_db,
MMAPString * mmapstr,
char * keyname, struct mail_flags ** result);
int generic_cache_flags_write(struct mail_cache_db * cache_db,
MMAPString * mmapstr,
char * keyname, struct mail_flags * flags);
int generic_cache_delete(struct mail_cache_db * cache_db, char * keyname);
#if 0
int generic_cache_fields_read(DB * dbp, MMAPString * mmapstr,
char * keyname, struct mailimf_fields ** result);
int generic_cache_fields_write(DB * dbp, MMAPString * mmapstr,
char * keyname, struct mailimf_fields * fields);
int generic_cache_flags_read(DB * dbp, MMAPString * mmapstr,
char * keyname, struct mail_flags ** result);
int generic_cache_flags_write(DB * dbp, MMAPString * mmapstr,
char * keyname, struct mail_flags * flags);
int generic_cache_delete(DB * dbp, char * keyname);
#endif
struct mail_flags_store * mail_flags_store_new(void);
void mail_flags_store_clear(struct mail_flags_store * flags_store);
void mail_flags_store_free(struct mail_flags_store * flags_store);
int mail_flags_store_set(struct mail_flags_store * flags_store,
mailmessage * msg);
void mail_flags_store_sort(struct mail_flags_store * flags_store);
struct mail_flags *
mail_flags_store_get(struct mail_flags_store * flags_store, uint32_t indx);
int mail_flags_compare(struct mail_flags * flags1, struct mail_flags * flags2);
#ifdef __cplusplus
}
#endif
#endif