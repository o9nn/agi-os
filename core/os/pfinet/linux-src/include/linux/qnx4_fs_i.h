#ifndef _QNX4_FS_I
#define _QNX4_FS_I
#include <linux/qnxtypes.h>
struct qnx4_inode_info {
char i_reserved[16];
off_t i_size;
_xtnt_t i_first_xtnt;
long i_xblk;
time_t i_ftime;
time_t i_mtime;
time_t i_atime;
time_t i_ctime;
_nxtnt_t i_num_xtnts;
mode_t i_mode;
muid_t i_uid;
mgid_t i_gid;
nlink_t i_nlink;
char i_zero[4];
_ftype_t i_type;
unsigned char i_status;
};
#endif