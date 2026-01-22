#ifndef _LINUX_NTFS_FS_I_H
#define _LINUX_NTFS_FS_I_H
struct ntfs_attribute;
struct ntfs_sb_info;
#ifndef NTFS_INTEGRAL_TYPES
#define NTFS_INTEGRAL_TYPES
typedef u8 ntfs_u8;
typedef u16 ntfs_u16;
typedef u32 ntfs_u32;
typedef u64 ntfs_u64;
typedef s8 ntfs_s8;
typedef s16 ntfs_s16;
typedef s32 ntfs_s32;
typedef s64 ntfs_s64;
#endif
#ifndef NTMODE_T
#define NTMODE_T
typedef __kernel_mode_t ntmode_t;
#endif
#ifndef NTFS_UID_T
#define NTFS_UID_T
typedef __kernel_uid_t ntfs_uid_t;
#endif
#ifndef NTFS_GID_T
#define NTFS_GID_T
typedef __kernel_gid_t ntfs_gid_t;
#endif
#ifndef NTFS_SIZE_T
#define NTFS_SIZE_T
typedef __kernel_size_t ntfs_size_t;
#endif
#ifndef NTFS_TIME_T
#define NTFS_TIME_T
typedef __kernel_time_t ntfs_time_t;
#endif
#ifndef NTFS_WCHAR_T
#define NTFS_WCHAR_T
typedef unsigned short ntfs_wchar_t;
#endif
#ifndef NTFS_OFFSET_T
#define NTFS_OFFSET_T
typedef unsigned long long ntfs_offset_t;
#endif
#ifndef NTFS_TIME64_T
#define NTFS_TIME64_T
typedef unsigned long long ntfs_time64_t;
#endif
#ifndef NTFS_CLUSTER_T
#define NTFS_CLUSTER_T
typedef unsigned int ntfs_cluster_t;
#endif
struct ntfs_inode_info{
struct ntfs_sb_info *vol;
int i_number;
unsigned sequence_number;
unsigned char* attr;
int attr_count;
struct ntfs_attribute *attrs;
int record_count;
int *records;
union{
struct{
int recordsize;
int clusters_per_record;
}index;
} u;
};
#endif