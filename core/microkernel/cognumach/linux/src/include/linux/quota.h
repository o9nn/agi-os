#ifndef _LINUX_QUOTA_
#define _LINUX_QUOTA_
#include <linux/errno.h>
#define dbtob(num) (num << 10)
#define btodb(num) (num >> 10)
#define fs_to_dq_blocks(num, blksize) (((num) * (blksize)) / BLOCK_SIZE)
#define MAX_IQ_TIME 604800
#define MAX_DQ_TIME 604800
#define MAXQUOTAS 2
#define USRQUOTA 0
#define GRPQUOTA 1
#define INITQFNAMES { \
"user", \
"group", \
"undefined", \
};
#define QUOTAFILENAME "quota"
#define QUOTAGROUP "staff"
#define NR_DQHASH 43
#define NR_DQUOTS 256
#define SUBCMDMASK 0x00ff
#define SUBCMDSHIFT 8
#define QCMD(cmd, type) (((cmd) << SUBCMDSHIFT) | ((type) & SUBCMDMASK))
#define Q_QUOTAON 0x0100
#define Q_QUOTAOFF 0x0200
#define Q_GETQUOTA 0x0300
#define Q_SETQUOTA 0x0400
#define Q_SETUSE 0x0500
#define Q_SYNC 0x0600
#define Q_SETQLIM 0x0700
#define Q_GETSTATS 0x0800
struct dqblk {
__u32 dqb_bhardlimit;
__u32 dqb_bsoftlimit;
__u32 dqb_curblocks;
__u32 dqb_ihardlimit;
__u32 dqb_isoftlimit;
__u32 dqb_curinodes;
time_t dqb_btime;
time_t dqb_itime;
};
#define dq_bhardlimit dq_dqb.dqb_bhardlimit
#define dq_bsoftlimit dq_dqb.dqb_bsoftlimit
#define dq_curblocks dq_dqb.dqb_curblocks
#define dq_ihardlimit dq_dqb.dqb_ihardlimit
#define dq_isoftlimit dq_dqb.dqb_isoftlimit
#define dq_curinodes dq_dqb.dqb_curinodes
#define dq_btime dq_dqb.dqb_btime
#define dq_itime dq_dqb.dqb_itime
#define dqoff(UID) ((off_t)((UID) * sizeof (struct dqblk)))
struct dqstats {
__u32 lookups;
__u32 drops;
__u32 reads;
__u32 writes;
__u32 cache_hits;
__u32 pages_allocated;
__u32 allocated_dquots;
__u32 free_dquots;
__u32 syncs;
};
#ifdef __KERNEL__
#include <linux/mount.h>
#define MAX_QUOTA_MESSAGE 75
#define DQ_LOCKED 0x01
#define DQ_WANT 0x02
#define DQ_MOD 0x04
#define DQ_BLKS 0x10
#define DQ_INODES 0x20
#define DQ_FAKE 0x40
struct dquot {
unsigned int dq_id;
short dq_type;
kdev_t dq_dev;
short dq_flags;
short dq_count;
short dq_locknest;
struct task_struct *dq_lockproc;
struct vfsmount *dq_mnt;
struct dqblk dq_dqb;
struct wait_queue *dq_wait;
struct dquot *dq_prev;
struct dquot *dq_next;
struct dquot *dq_hash_prev;
struct dquot *dq_hash_next;
};
#define NODQUOT (struct dquot *)NULL
#define QUOTA_SYSCALL 0x01
#define SET_QUOTA 0x02
#define SET_USE 0x04
#define SET_QLIMIT 0x08
#define QUOTA_OK 0
#define NO_QUOTA 1
extern void dquot_initialize(struct inode *inode, short type);
extern void dquot_drop(struct inode *inode);
extern int dquot_alloc_block(const struct inode *inode, unsigned long number);
extern int dquot_alloc_inode(const struct inode *inode, unsigned long number);
extern void dquot_free_block(const struct inode *inode, unsigned long number);
extern void dquot_free_inode(const struct inode *inode, unsigned long number);
extern int dquot_transfer(struct inode *inode, struct iattr *iattr, char direction);
extern void invalidate_dquots(kdev_t dev, short type);
extern int quota_off(kdev_t dev, short type);
extern int sync_dquots(kdev_t dev, short type);
#else
#include <sys/cdefs.h>
__BEGIN_DECLS
int quotactl __P ((int, const char *, int, caddr_t));
__END_DECLS
#endif
#endif