#ifndef __CACHEQ_H__
#define __CACHEQ_H__
#include <stddef.h>
#include <errno.h>
struct cacheq_hdr
{
void *next, *prev;
};
struct cacheq
{
size_t entry_size;
void (*init_entry) (void *entry);
void (*move_entry) (void *from, void *to);
void (*finalize_entry) (void *entry);
int length;
void *entries;
void *lru, *mru;
};
void cacheq_make_mru (struct cacheq *cq, void *entry);
void cacheq_make_lru (struct cacheq *cq, void *entry);
error_t cacheq_set_length (struct cacheq *cq, int length);
#endif