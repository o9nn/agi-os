#ifndef MAIL_INDEX_ALLOC_CACHE_H
#define MAIL_INDEX_ALLOC_CACHE_H
struct mail_index * ATTR_NULL(1, 2)
mail_index_alloc_cache_get(struct event *parent_event, const char *mailbox_path,
const char *index_dir, const char *prefix);
void mail_index_alloc_cache_unref(struct mail_index **index);
struct mail_index *
mail_index_alloc_cache_find(const char *index_dir);
void mail_index_alloc_cache_destroy_unrefed(void);
void mail_index_alloc_cache_index_opened(struct mail_index *index);
void mail_index_alloc_cache_index_closing(struct mail_index *index);
#endif