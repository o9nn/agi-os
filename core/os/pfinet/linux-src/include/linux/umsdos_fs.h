#ifndef LINUX_UMSDOS_FS_H
#define LINUX_UMSDOS_FS_H
#define UMS_DEBUG 1
#define UMSDOS_PARANOIA 1
#define UMSDOS_VERSION	0
#define UMSDOS_RELEASE	4
#define UMSDOS_ROOT_INO 1
#define UMSDOS_EMD_FILE		"--linux-.---"
#define UMSDOS_EMD_NAMELEN	12
#define UMSDOS_PSDROOT_NAME	"linux"
#define UMSDOS_PSDROOT_LEN	5
#ifndef _LINUX_TYPES_H
#include <linux/types.h>
#endif
#ifndef _LINUX_LIMITS_H
#include <linux/limits.h>
#endif
#ifndef _LINUX_DIRENT_H
#include <linux/dirent.h>
#endif
#ifndef _LINUX_IOCTL_H
#include <linux/ioctl.h>
#endif
#ifdef __KERNEL__
#	define PRINTK(x)
#	ifdef UMSDOS_DEBUG
#		define Printk(x) printk x
#	else
#		define Printk(x)
#	endif
#endif
struct umsdos_fake_info {
char fname[13];
int len;
};
#define UMSDOS_MAXNAME	220
struct umsdos_dirent {
unsigned char name_len;
unsigned char flags;
unsigned short nlink;
uid_t uid;
gid_t gid;
time_t atime;
time_t mtime;
time_t ctime;
dev_t rdev;
umode_t mode;
char spare[12];
char name[UMSDOS_MAXNAME];
};
#define UMSDOS_HIDDEN	1
#define UMSDOS_HLINK	2
#define UMSDOS_REC_SIZE		64
struct umsdos_info {
int msdos_reject;
struct umsdos_fake_info fake;
struct umsdos_dirent entry;
off_t f_pos;
int recsize;
};
#define UMSDOS_READDIR_DOS _IO(0x04,210)
#define UMSDOS_UNLINK_DOS  _IO(0x04,211)
#define UMSDOS_RMDIR_DOS   _IO(0x04,212)
#define UMSDOS_STAT_DOS    _IO(0x04,213)
#define UMSDOS_CREAT_EMD   _IO(0x04,214)
#define UMSDOS_UNLINK_EMD  _IO(0x04,215)
#define UMSDOS_READDIR_EMD _IO(0x04,216)
#define UMSDOS_GETVERSION  _IO(0x04,217)
#define UMSDOS_INIT_EMD    _IO(0x04,218)
#define UMSDOS_DOS_SETUP   _IO(0x04,219)
#define UMSDOS_RENAME_DOS  _IO(0x04,220)
struct umsdos_ioctl {
struct dirent dos_dirent;
struct umsdos_dirent umsdos_dirent;
struct {
dev_t st_dev;
unsigned short __pad1;
ino_t st_ino;
umode_t st_mode;
nlink_t st_nlink;
uid_t st_uid;
gid_t st_gid;
dev_t st_rdev;
unsigned short __pad2;
off_t st_size;
unsigned long st_blksize;
unsigned long st_blocks;
time_t st_atime;
unsigned long __unused1;
time_t st_mtime;
unsigned long __unused2;
time_t st_ctime;
unsigned long __unused3;
unsigned long __unused4;
unsigned long __unused5;
} stat;
char version, release;
};
#define EDM_ENTRY_ISUSED(e) ((e)->name_len!=0)
#ifdef __KERNEL__
#ifndef LINUX_FS_H
#include <linux/fs.h>
#endif
extern struct inode_operations umsdos_dir_inode_operations;
extern struct file_operations umsdos_file_operations;
extern struct inode_operations umsdos_file_inode_operations;
extern struct inode_operations umsdos_file_inode_operations_no_bmap;
extern struct inode_operations umsdos_file_inode_operations_readpage;
extern struct inode_operations umsdos_symlink_inode_operations;
extern int init_umsdos_fs (void);
#include <linux/umsdos_fs.p>
#endif
#endif