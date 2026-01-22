#ifndef _RDXTREE_I_H
#define _RDXTREE_I_H
struct rdxtree {
unsigned int height;
void *root;
};
struct rdxtree_iter {
void *node;
rdxtree_key_t key;
};
static inline void
rdxtree_iter_init(struct rdxtree_iter *iter)
{
iter->node = NULL;
iter->key = (rdxtree_key_t)-1;
}
int rdxtree_insert_common(struct rdxtree *tree, rdxtree_key_t key,
void *ptr, void ***slotp);
int rdxtree_insert_alloc_common(struct rdxtree *tree, void *ptr,
rdxtree_key_t *keyp, void ***slotp);
void * rdxtree_lookup_common(const struct rdxtree *tree, rdxtree_key_t key,
int get_slot);
void * rdxtree_walk(struct rdxtree *tree, struct rdxtree_iter *iter);
#endif