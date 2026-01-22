#ifndef __EFS_FS_I_H__
#define __EFS_FS_I_H__
typedef int32_t efs_block_t;
typedef uint32_t efs_ino_t;
#define EFS_DIRECTEXTENTS 12
typedef union extent_u {
unsigned char raw[8];
struct extent_s {
unsigned int ex_magic:8;
unsigned int ex_bn:24;
unsigned int ex_length:8;
unsigned int ex_offset:24;
} cooked;
} efs_extent;
typedef struct edevs {
short odev;
short dev_filler;
unsigned int ndev;
} efs_devs;
struct efs_dinode {
u_short di_mode;
short di_nlink;
u_short di_uid;
u_short di_gid;
int32_t di_size;
int32_t di_atime;
int32_t di_mtime;
int32_t di_ctime;
uint32_t di_gen;
short di_numextents;
u_char di_version;
u_char di_spare;
union di_addr {
efs_extent di_extents[EFS_DIRECTEXTENTS];
efs_devs di_dev;
} di_u;
};
struct efs_inode_info {
int numextents;
int lastextent;
efs_extent extents[EFS_DIRECTEXTENTS];
};
#endif