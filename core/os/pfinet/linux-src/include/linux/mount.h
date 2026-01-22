#ifndef _LINUX_MOUNT_H
#define _LINUX_MOUNT_H
#define DQUOT_USR_ENABLED	0x01
#define DQUOT_GRP_ENABLED	0x02
struct quota_mount_options
{
unsigned int flags;
struct semaphore dqio_sem;
struct semaphore dqoff_sem;
struct file *files[MAXQUOTAS];
time_t inode_expire[MAXQUOTAS];
time_t block_expire[MAXQUOTAS];
char rsquash[MAXQUOTAS];
};
struct vfsmount
{
kdev_t mnt_dev;
char *mnt_devname;
char *mnt_dirname;
unsigned int mnt_flags;
struct super_block *mnt_sb;
struct quota_mount_options mnt_dquot;
struct vfsmount *mnt_next;
};
struct vfsmount *lookup_vfsmnt(kdev_t dev);
#define MNT_FORCE	0x00000001
#endif