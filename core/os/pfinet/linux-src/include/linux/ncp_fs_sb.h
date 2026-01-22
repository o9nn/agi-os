#ifndef _NCP_FS_SB
#define _NCP_FS_SB
#include <asm/semaphore.h>
#include <linux/ncp_mount.h>
#include <linux/types.h>
#ifdef __KERNEL__
#define NCP_DEFAULT_BUFSIZE 1024
#define NCP_DEFAULT_OPTIONS 0
struct ncp_server {
struct ncp_mount_data m;
__u8 name_space[NCP_NUMBER_OF_VOLUMES + 2];
struct file *ncp_filp;
u8 sequence;
u8 task;
u16 connection;
u8 completion;
u8 conn_status;
int buffer_size;
int reply_size;
int packet_size;
unsigned char *packet;
int lock;
struct semaphore sem;
int current_size;
int has_subfunction;
int ncp_reply_size;
struct ncp_inode_info root;
struct dentry* root_dentry;
int root_setuped;
int sign_wanted;
int sign_active;
char sign_root[8];
char sign_last[16];
struct {
int auth_type;
size_t object_name_len;
void* object_name;
int object_type;
} auth;
struct {
size_t len;
void* data;
} priv;
struct ncp_nls_ioctl nls_charsets;
struct nls_table *nls_vol;
struct nls_table *nls_io;
};
static inline int ncp_conn_valid(struct ncp_server *server)
{
return ((server->conn_status & 0x11) == 0);
}
static inline void ncp_invalidate_conn(struct ncp_server *server)
{
server->conn_status |= 0x01;
}
#endif
#endif