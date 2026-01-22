#ifndef VIRT_INODE_H
#define VIRT_INODE_H
#include <errno.h>
#include <dirent.h>
struct vi_key
{
ino_t dir_inode;
int dir_offset;
};
typedef struct vi_key vi_key_t;
extern vi_key_t vi_zero_key;
typedef struct v_inode *inode_t;
error_t vi_new(vi_key_t key, ino_t *inode, inode_t *v_inode);
vi_key_t vi_key(inode_t v_inode);
inode_t vi_lookup(ino_t inode);
error_t vi_rlookup(vi_key_t key, ino_t *inode, inode_t *v_inode, int create);
vi_key_t vi_change(inode_t v_inode, vi_key_t key);
vi_key_t vi_free(inode_t v_inode);
#endif