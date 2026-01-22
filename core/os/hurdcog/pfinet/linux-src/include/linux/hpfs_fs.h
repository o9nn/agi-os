#ifndef _LINUX_HPFS_FS_H
#define _LINUX_HPFS_FS_H
#define HPFS_SUPER_MAGIC 0xf995e849
extern struct super_block *hpfs_read_super (struct super_block *, void *, int);
extern int init_hpfs_fs(void);
#endif