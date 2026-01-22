#ifndef _LINUX_SMB_MOUNT_H
#define _LINUX_SMB_MOUNT_H
#include <linux/types.h>
#define SMB_MOUNT_VERSION 6
struct smb_mount_data {
int version;
__kernel_uid_t mounted_uid;
__kernel_uid_t uid;
__kernel_gid_t gid;
__kernel_mode_t file_mode;
__kernel_mode_t dir_mode;
};
#endif