#ifndef _MSDOS_FS_SB
#define _MSDOS_FS_SB
#include<linux/fat_cvf.h>
struct fat_mount_options {
uid_t fs_uid;
gid_t fs_gid;
unsigned short fs_umask;
unsigned short codepage;
char *iocharset;
unsigned char name_check;
unsigned char conversion;
unsigned quiet:1,
showexec:1,
sys_immutable:1,
dotsOK:1,
isvfat:1,
utf8:1,
unicode_xlate:1,
posixfs:1,
numtail:1,
atari:1,
fat32:1;
};
struct vfat_unicode {
unsigned char uni1;
unsigned char uni2;
};
struct msdos_sb_info {
unsigned short cluster_size;
unsigned char fats,fat_bits;
unsigned short fat_start;
unsigned long fat_length;
unsigned long dir_start;
unsigned short dir_entries;
unsigned long data_start;
unsigned long clusters;
unsigned long root_cluster;
unsigned long fsinfo_offset;
struct wait_queue *fat_wait;
int fat_lock;
int prev_free;
int free_clusters;
struct fat_mount_options options;
struct nls_table *nls_disk;
struct nls_table *nls_io;
struct cvf_format* cvf_format;
void *dir_ops;
void (*put_super_callback)(struct super_block *);
void *private_data;
};
#endif