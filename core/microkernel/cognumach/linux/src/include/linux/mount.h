#ifndef _LINUX_MOUNT_H
#define _LINUX_MOUNT_H
struct vfsmount
{
kdev_t mnt_dev;
char *mnt_devname;
char *mnt_dirname;
unsigned int mnt_flags;
struct semaphore mnt_sem;
struct super_block *mnt_sb;
struct file *mnt_quotas[MAXQUOTAS];
time_t mnt_iexp[MAXQUOTAS];
time_t mnt_bexp[MAXQUOTAS];
struct vfsmount *mnt_next;
};
struct vfsmount *lookup_vfsmnt(kdev_t dev);
#endif