#ifndef __FTPFS_H__
#define __FTPFS_H__
#include <stdlib.h>
#include <pthread.h>
#include <ftpconn.h>
#include <maptime.h>
#include <hurd/ihash.h>
struct ccache;
struct ftpfs_conn;
struct ftpfs_dir_entry
{
char *name;
struct node *node;
struct stat stat;
char *symlink_target;
time_t stat_timestamp;
struct ftpfs_dir *dir;
struct ftpfs_dir_entry *ordered_next, **ordered_self_p;
time_t name_timestamp;
hurd_ihash_locp_t inode_locp;
hurd_ihash_locp_t dir_locp;
int noent : 1;
int valid : 1;
int deleted : 1;
};
struct ftpfs_dir
{
struct hurd_ihash htable;
size_t num_live_entries;
struct ftpfs_dir_entry *ordered;
struct node *node;
struct ftpfs *fs;
const char *rmt_path;
time_t stat_timestamp;
time_t name_timestamp;
time_t bulk_stat_base_stamp;
unsigned bulk_stat_count_first_half;
unsigned bulk_stat_count_second_half;
};
struct netnode
{
struct ftpfs *fs;
struct ftpfs_dir_entry *dir_entry;
const char *rmt_path;
struct ccache *contents;
struct ftpfs_dir *dir;
struct node *ncache_next, *ncache_prev;
};
struct ftpfs_params
{
time_t name_timeout;
time_t stat_timeout;
time_t bulk_stat_period;
unsigned bulk_stat_threshold;
size_t node_cache_max;
};
struct ftpfs
{
struct node *root;
struct ftpfs_conn *free_conns;
struct ftpfs_conn *conns;
pthread_spinlock_t conn_lock;
struct ftp_conn_params *ftp_params;
struct ftp_conn_hooks *ftp_hooks;
ino_t next_inode;
int fsid;
struct hurd_ihash inode_mappings;
pthread_spinlock_t inode_mappings_lock;
struct ftpfs_params params;
struct node *node_cache_mru, *node_cache_lru;
size_t node_cache_len;
pthread_mutex_t node_cache_lock;
};
extern volatile struct mapped_time_value *ftpfs_maptime;
#define NOW \
({ struct timeval tv; maptime_read (ftpfs_maptime, &tv); tv.tv_sec; })
error_t ftpfs_create (char *rmt_root, int fsid,
struct ftp_conn_params *ftp_params,
struct ftp_conn_hooks *ftp_hooks,
struct ftpfs_params *params,
struct ftpfs **fs);
error_t ftpfs_refresh_node (struct node *node);
error_t ftpfs_detach_node (struct node *node);
error_t ftpfs_create_node (struct ftpfs_dir_entry *e, const char *rmt_path,
struct node **node);
void ftpfs_cache_node (struct node *node);
error_t ftpfs_get_ftp_conn (struct ftpfs *fs, struct ftp_conn **conn);
void ftpfs_release_ftp_conn (struct ftpfs *fs, struct ftp_conn *conn);
error_t ftpfs_dir_create (struct ftpfs *fs, struct node *node,
const char *rmt_path, struct ftpfs_dir **dir);
void ftpfs_dir_free (struct ftpfs_dir *dir);
error_t ftpfs_dir_refresh (struct ftpfs_dir *dir);
error_t ftpfs_dir_lookup (struct ftpfs_dir *dir, const char *name,
struct node **node);
error_t ftpfs_dir_null_lookup (struct ftpfs_dir *dir, struct node **node);
#endif