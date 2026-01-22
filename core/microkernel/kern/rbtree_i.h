#ifndef _KERN_RBTREE_I_H
#define _KERN_RBTREE_I_H
#include <kern/assert.h>
struct rbtree_node {
unsigned long parent;
struct rbtree_node *children[2];
};
struct rbtree {
struct rbtree_node *root;
};
#define RBTREE_COLOR_MASK   0x1UL
#define RBTREE_PARENT_MASK  (~0x3UL)
#define RBTREE_COLOR_RED    0
#define RBTREE_COLOR_BLACK  1
#define RBTREE_SLOT_INDEX_MASK  0x1UL
#define RBTREE_SLOT_PARENT_MASK (~RBTREE_SLOT_INDEX_MASK)
static inline int rbtree_check_alignment(const struct rbtree_node *node)
{
return ((unsigned long)node & (~RBTREE_PARENT_MASK)) == 0;
}
static inline int rbtree_check_index(int index)
{
return index == (index & 1);
}
static inline int rbtree_d2i(int diff)
{
return !(diff <= 0);
}
static inline struct rbtree_node * rbtree_parent(const struct rbtree_node *node)
{
return (struct rbtree_node *)(node->parent & RBTREE_PARENT_MASK);
}
static inline unsigned long rbtree_slot(struct rbtree_node *parent, int index)
{
assert(rbtree_check_alignment(parent));
assert(rbtree_check_index(index));
return (unsigned long)parent | index;
}
static inline struct rbtree_node * rbtree_slot_parent(unsigned long slot)
{
return (struct rbtree_node *)(slot & RBTREE_SLOT_PARENT_MASK);
}
static inline int rbtree_slot_index(unsigned long slot)
{
return slot & RBTREE_SLOT_INDEX_MASK;
}
void rbtree_insert_rebalance(struct rbtree *tree, struct rbtree_node *parent,
int index, struct rbtree_node *node);
struct rbtree_node * rbtree_nearest(struct rbtree_node *parent, int index,
int direction);
struct rbtree_node * rbtree_firstlast(const struct rbtree *tree, int direction);
struct rbtree_node * rbtree_walk(struct rbtree_node *node, int direction);
struct rbtree_node * rbtree_postwalk_deepest(const struct rbtree *tree);
struct rbtree_node * rbtree_postwalk_unlink(struct rbtree_node *node);
#endif