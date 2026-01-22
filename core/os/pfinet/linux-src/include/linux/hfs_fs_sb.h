#ifndef _LINUX_HFS_FS_SB_H
#define _LINUX_HFS_FS_SB_H
struct hfs_name;
typedef int (*hfs_namein_fn) (char *, const struct hfs_name *);
typedef void (*hfs_nameout_fn) (struct hfs_name *, const char *, int);
typedef void (*hfs_ifill_fn) (struct inode *, ino_t, const int);
struct hfs_sb_info {
int			magic;
struct hfs_mdb		*s_mdb;
int			s_quiet;
int			s_lowercase;
int			s_afpd;
int                     s_version;
hfs_namein_fn		s_namein;
hfs_nameout_fn		s_nameout;
hfs_ifill_fn		s_ifill;
const struct hfs_name	*s_reserved1;
const struct hfs_name	*s_reserved2;
__u32			s_type;
__u32			s_creator;
umode_t			s_umask;
uid_t			s_uid;
gid_t			s_gid;
char			s_conv;
};
#endif