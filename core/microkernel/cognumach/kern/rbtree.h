#ifndef _KERN_RBTREE_H
#define _KERN_RBTREE_H
#include <stddef.h>
#include <kern/assert.h>
#include <kern/macros.h>
#include <sys/types.h>
#define RBTREE_LEFT 0
#define RBTREE_RIGHT 1
struct rbtree_node;
struct rbtree;
#define RBTREE_INITIALIZER { NULL }
#include "rbtree_i.h"
static inline void rbtree_init(struct rbtree *tree)
{
tree->root = NULL;
}
static inline void rbtree_node_init(struct rbtree_node *node)
{
assert(rbtree_check_alignment(node));
node->parent = (unsigned long)node | RBTREE_COLOR_RED;
node->children[RBTREE_LEFT] = NULL;
node->children[RBTREE_RIGHT] = NULL;
}
static inline int rbtree_node_unlinked(const struct rbtree_node *node)
{
return rbtree_parent(node) == node;
}
#define rbtree_entry(node, type, member) structof(node, type, member)
static inline int rbtree_empty(const struct rbtree *tree)
{
return tree->root == NULL;
}
#define rbtree_lookup(tree, key, cmp_fn) \
MACRO_BEGIN \
struct rbtree_node *___cur; \
int ___diff; \
\
___cur = (tree)->root; \
\
while (___cur != NULL) { \
___diff = cmp_fn(key, ___cur); \
\
if (___diff == 0) \
break; \
\
___cur = ___cur->children[rbtree_d2i(___diff)]; \
} \
\
___cur; \
MACRO_END
#define rbtree_lookup_nearest(tree, key, cmp_fn, dir) \
MACRO_BEGIN \
struct rbtree_node *___cur, *___prev; \
int ___diff, ___index; \
\
___prev = NULL; \
___index = -1; \
___cur = (tree)->root; \
\
while (___cur != NULL) { \
___diff = cmp_fn(key, ___cur); \
\
if (___diff == 0) \
break; \
\
___prev = ___cur; \
___index = rbtree_d2i(___diff); \
___cur = ___cur->children[___index]; \
} \
\
if (___cur == NULL) \
___cur = rbtree_nearest(___prev, ___index, dir); \
\
___cur; \
MACRO_END
#define rbtree_insert(tree, node, cmp_fn) \
MACRO_BEGIN \
struct rbtree_node *___cur, *___prev; \
int ___diff, ___index; \
\
___prev = NULL; \
___index = -1; \
___cur = (tree)->root; \
\
while (___cur != NULL) { \
___diff = cmp_fn(node, ___cur); \
assert(___diff != 0); \
___prev = ___cur; \
___index = rbtree_d2i(___diff); \
___cur = ___cur->children[___index]; \
} \
\
rbtree_insert_rebalance(tree, ___prev, ___index, node); \
MACRO_END
#define rbtree_lookup_slot(tree, key, cmp_fn, slot) \
MACRO_BEGIN \
struct rbtree_node *___cur, *___prev; \
int ___diff, ___index; \
\
___prev = NULL; \
___index = 0; \
___cur = (tree)->root; \
\
while (___cur != NULL) { \
___diff = cmp_fn(key, ___cur); \
\
if (___diff == 0) \
break; \
\
___prev = ___cur; \
___index = rbtree_d2i(___diff); \
___cur = ___cur->children[___index]; \
} \
\
(slot) = rbtree_slot(___prev, ___index); \
___cur; \
MACRO_END
static inline void
rbtree_insert_slot(struct rbtree *tree, unsigned long slot,
struct rbtree_node *node)
{
struct rbtree_node *parent;
int index;
parent = rbtree_slot_parent(slot);
index = rbtree_slot_index(slot);
rbtree_insert_rebalance(tree, parent, index, node);
}
void rbtree_remove(struct rbtree *tree, struct rbtree_node *node);
#define rbtree_first(tree) rbtree_firstlast(tree, RBTREE_LEFT)
#define rbtree_last(tree) rbtree_firstlast(tree, RBTREE_RIGHT)
#define rbtree_prev(node) rbtree_walk(node, RBTREE_LEFT)
#define rbtree_next(node) rbtree_walk(node, RBTREE_RIGHT)
#define rbtree_for_each_remove(tree, node, tmp) \
for (node = rbtree_postwalk_deepest(tree), \
tmp = rbtree_postwalk_unlink(node); \
node != NULL; \
node = tmp, tmp = rbtree_postwalk_unlink(node))
#endif