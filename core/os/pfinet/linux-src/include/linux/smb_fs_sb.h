#ifndef _SMB_FS_SB
#define _SMB_FS_SB
#ifdef __KERNEL__
#include <linux/types.h>
#include <linux/smb.h>
#define server_from_dentry(dentry) &dentry->d_sb->u.smbfs_sb
#define SB_of(server) ((struct super_block *) ((char *)(server) - \
(unsigned long)(&((struct super_block *)0)->u.smbfs_sb)))
struct smb_sb_info {
enum smb_conn_state state;
struct file * sock_file;
struct smb_mount_data *mnt;
unsigned char *temp_buf;
unsigned int generation;
pid_t conn_pid;
struct smb_conn_opt opt;
struct semaphore sem;
struct wait_queue * wait;
__u32              packet_size;
unsigned char *    packet;
unsigned short     rcls;
unsigned short     err;
void *data_ready;
};
#endif
#endif