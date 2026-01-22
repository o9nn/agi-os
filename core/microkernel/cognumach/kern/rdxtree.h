#ifndef _RDXTREE_H
#define _RDXTREE_H
#include <stddef.h>
#include <stdint.h>
void rdxtree_cache_init(void);
#if 0
#define RDXTREE_KEY_32
#endif
#ifdef RDXTREE_KEY_32
typedef uint32_t rdxtree_key_t;
#else
typedef uint64_t rdxtree_key_t;
#endif
struct rdxtree;
struct rdxtree_iter;
#define RDXTREE_INITIALIZER { 0, NULL }
#include "rdxtree_i.h"
static inline void
rdxtree_init(struct rdxtree *tree)
{
tree->height = 0;
tree->root = NULL;
}
static inline int
rdxtree_insert(struct rdxtree *tree, rdxtree_key_t key, void *ptr)
{
return rdxtree_insert_common(tree, key, ptr, NULL);
}
static inline int
rdxtree_insert_slot(struct rdxtree *tree, rdxtree_key_t key,
void *ptr, void ***slotp)
{
return rdxtree_insert_common(tree, key, ptr, slotp);
}
static inline int
rdxtree_insert_alloc(struct rdxtree *tree, void *ptr, rdxtree_key_t *keyp)
{
return rdxtree_insert_alloc_common(tree, ptr, keyp, NULL);
}
static inline int
rdxtree_insert_alloc_slot(struct rdxtree *tree, void *ptr,
rdxtree_key_t *keyp, void ***slotp)
{
return rdxtree_insert_alloc_common(tree, ptr, keyp, slotp);
}
void * rdxtree_remove(struct rdxtree *tree, rdxtree_key_t key);
static inline void *
rdxtree_lookup(const struct rdxtree *tree, rdxtree_key_t key)
{
return rdxtree_lookup_common(tree, key, 0);
}
static inline void **
rdxtree_lookup_slot(const struct rdxtree *tree, rdxtree_key_t key)
{
return rdxtree_lookup_common(tree, key, 1);
}
void * rdxtree_replace_slot(void **slot, void *ptr);
#define rdxtree_for_each(tree, iter, ptr)                       \
for (rdxtree_iter_init(iter), ptr = rdxtree_walk(tree, iter);   \
ptr != NULL;                                               \
ptr = rdxtree_walk(tree, iter))
static inline rdxtree_key_t
rdxtree_iter_key(const struct rdxtree_iter *iter)
{
return iter->key;
}
void rdxtree_remove_all(struct rdxtree *tree);
#endif