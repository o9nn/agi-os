#ifndef NFS_NFS_H
#define NFS_NFS_H
#include <sys/stat.h>
#include <sys/types.h>
#include <stdint.h>
#include <pthread.h>
#include <sys/mman.h>
#include "nfs-spec.h"
#include <hurd/ihash.h>
#include <hurd/netfs.h>
struct fhandle
{
size_t size;
char data[NFS3_FHSIZE];
};
struct netnode
{
hurd_ihash_locp_t slot;
struct fhandle handle;
time_t stat_updated;
enum
{
NOT_POSSIBLE,
POSSIBLE,
SYMLINK,
CHRDEV,
BLKDEV,
FIFO,
SOCK,
} dtrans;
union
{
char *name;
dev_t indexes;
} transarg;
#ifdef notyet
off_t extend_len;
#endif
struct user_pager_info *fileinfo;
struct node *dead_dir;
char *dead_name;
};
extern int main_udp_socket;
extern char *hostname;
extern volatile struct mapped_time_value *mapped_time;
extern int stat_timeout;
extern int cache_timeout;
extern int name_cache_timeout;
extern int name_cache_neg_timeout;
extern int initial_transmit_timeout;
extern int max_transmit_timeout;
extern int soft_retries;
extern int mounted_soft;
extern int read_size;
extern int write_size;
extern char *pmap_service_name;
extern short pmap_service_number;
extern int mount_program;
extern int mount_version;
extern short mount_port;
extern int mount_port_override;
extern int nfs_program;
extern int nfs_version;
extern short nfs_port;
extern int nfs_port_override;
extern int protocol_version;
#define INTSIZE(len) (((len)+3)>>2)
int hurd_mode_to_nfs_type (mode_t);
int *xdr_encode_fhandle (int *, const struct fhandle *);
int *xdr_encode_data (int *, const char *, size_t);
int *xdr_encode_string (int *, const char *);
int *xdr_encode_sattr_mode (int *, mode_t);
int *xdr_encode_sattr_ids (int *, u_int, u_int);
int *xdr_encode_sattr_size (int *, off_t);
int *xdr_encode_sattr_times (int *, const struct timespec *, const struct timespec *);
int *xdr_encode_sattr_stat (int *, const struct stat *);
int *xdr_encode_create_state (int *, mode_t, uid_t);
int *xdr_encode_64bit (int *p, uint64_t n);
int *xdr_decode_fattr (int *, struct stat *);
int *xdr_decode_string (int *, char *);
int *xdr_decode_fhandle (int *, struct node **);
int *xdr_decode_64bit (int *p, uint64_t *n);
int *nfs_initialize_rpc (int, struct iouser *, size_t, void **,
struct node *, uid_t);
error_t nfs_error_trans (int);
struct node *mount_root (char *, char *);
extern const char *mounted_hostname;
extern uint16_t mounted_nfs_port;
int *register_fresh_stat (struct node *, int *);
int *initialize_rpc (int, int, int, size_t, void **, uid_t, gid_t, gid_t);
error_t conduct_rpc (void **, int **);
void *timeout_service_thread (void *);
void *rpc_receive_thread (void *);
void lookup_fhandle (struct fhandle *, struct node **);
int *recache_handle (int *, struct node *);
void enter_lookup_cache (char *, size_t, struct node *, const char *);
void purge_lookup_cache (struct node *, const char *, size_t);
struct node *check_lookup_cache (struct node *, const char *);
void purge_lookup_cache_node (struct node *);
#endif