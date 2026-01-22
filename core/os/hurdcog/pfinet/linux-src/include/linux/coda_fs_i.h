#ifndef _LINUX_CODA_FS_I
#define _LINUX_CODA_FS_I
#ifdef __KERNEL__
#include <linux/types.h>
#include <linux/list.h>
#include <linux/coda.h>
#define CODA_CNODE_MAGIC 0x47114711
struct coda_inode_info {
struct pipe_inode_info pipeinfo;
struct ViceFid c_fid;
u_short c_flags;
u_short c_ocount;
u_short c_owrite;
u_short c_mmcount;
struct inode *c_ovp;
struct list_head c_cnhead;
struct list_head c_volrootlist;
struct inode *c_vnode;
int c_magic;
};
#define C_VATTR 0x1
#define C_PURGE 0x8
#define C_ZAPDIR 0x10
#define C_DYING 0x4
#define C_INITED 0x20
#define C_FLUSH 0x2
int coda_cnode_make(struct inode **, struct ViceFid *, struct super_block *);
int coda_cnode_makectl(struct inode **inode, struct super_block *sb);
struct inode *coda_fid_to_inode(ViceFid *fid, struct super_block *sb);
void coda_replace_fid(struct inode *, ViceFid *, ViceFid *);
#endif
#endif