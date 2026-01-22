#ifndef _9PFS_H
#define _9PFS_H
#include <hurd/netfs.h>
#include <pthread.h>
#include "9p.h"
struct netnode
{
struct p9_connection *conn;
struct p9_fid *fid;
struct p9_qid qid;
struct p9_stat stat;
pthread_mutex_t lock;
int refs;
time_t stat_cache_time;
int stat_cache_valid;
};
extern struct p9_connection *p9_conn;
extern struct p9_namespace *p9_ns;
extern char *p9_server_addr;
extern int p9_server_port;
extern char *p9_username;
extern char *p9_attach_name;
error_t p9fs_init(void);
void p9fs_shutdown(void);
struct node *p9fs_make_node(struct p9_fid *fid);
void p9fs_free_node(struct node *node);
error_t p9fs_refresh_node(struct node *node);
error_t p9fs_dir_lookup(struct node *dir, const char *name, struct node **node);
error_t p9fs_dir_readdir(struct node *dir, char **entries, size_t *count);
error_t p9fs_file_read(struct node *node, off_t offset, size_t count,
void *buf, size_t *bytes_read);
error_t p9fs_file_write(struct node *node, off_t offset, size_t count,
const void *buf, size_t *bytes_written);
error_t p9_to_hurd_error(int p9_error);
mode_t p9_to_hurd_mode(uint32_t p9_mode);
uint32_t hurd_to_p9_mode(mode_t mode);
#endif