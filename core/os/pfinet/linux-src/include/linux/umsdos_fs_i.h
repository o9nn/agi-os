#ifndef UMSDOS_FS_I_H
#define UMSDOS_FS_I_H
#ifndef _LINUX_TYPES_H
#include <linux/types.h>
#endif
#include <linux/msdos_fs_i.h>
#include <linux/pipe_fs_i.h>
struct dir_locking_info {
struct wait_queue *p;
short int looking;
short int creating;
long pid;
};
struct umsdos_inode_info {
union {
struct msdos_inode_info msdos_info;
struct pipe_inode_info pipe_info;
struct dir_locking_info dir_info;
} u;
int i_patched;
int i_is_hlink;
unsigned long i_emd_owner;
off_t pos;
struct dentry *i_emd_dentry;
unsigned long i_emd_dir;
};
#endif