#ifndef _LINUX_HFS_FS_I_H
#define _LINUX_HFS_FS_I_H
struct hfs_inode_info {
int				magic;
struct hfs_cat_entry		*entry;
struct hfs_fork 		*fork;
int				convert;
ino_t				file_type;
char				dir_size;
const struct hfs_hdr_layout	*default_layout;
struct hfs_hdr_layout		*layout;
int                             tz_secondswest;
void (*d_drop_op)(struct dentry *, const ino_t);
};
#endif