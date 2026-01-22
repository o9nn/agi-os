#ifndef _LINUX_NCP_MOUNT_H
#define _LINUX_NCP_MOUNT_H
#include <linux/types.h>
#include <linux/ipx.h>
#include <linux/ncp.h>
#include <linux/ncp_fs_i.h>
#define NCP_MOUNT_VERSION 3
#define NCP_MOUNT_SOFT		0x0001
#define NCP_MOUNT_INTR		0x0002
#define NCP_MOUNT_STRONG	0x0004
#define NCP_MOUNT_NO_OS2	0x0008
#define NCP_MOUNT_NO_NFS	0x0010
#define NCP_MOUNT_EXTRAS	0x0020
#define NCP_MOUNT_SYMLINKS	0x0040
struct ncp_mount_data {
int version;
unsigned int ncp_fd;
__kernel_uid_t mounted_uid;
__kernel_pid_t wdog_pid;
unsigned char mounted_vol[NCP_VOLNAME_LEN + 1];
unsigned int time_out;
unsigned int retry_count;
unsigned int flags;
__kernel_uid_t uid;
__kernel_gid_t gid;
__kernel_mode_t file_mode;
__kernel_mode_t dir_mode;
};
#endif