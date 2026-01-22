#include "9pfs.h"
#include <stdlib.h>
#include <string.h>
struct node *
p9fs_make_node(struct p9_fid *fid)
{
struct node *node;
struct netnode *nn;
node = netfs_make_node(sizeof(struct netnode));
if (!node)
return NULL;
nn = node->nn;
memset(nn, 0, sizeof(struct netnode));
nn->conn = p9_conn;
nn->fid = fid;
nn->refs = 1;
nn->stat_cache_valid = 0;
pthread_mutex_init(&nn->lock, NULL);
if (fid)
nn->qid = fid->qid;
node->nn_stat.st_ino = nn->qid.path;
node->nn_stat.st_mode = p9_to_hurd_mode(nn->qid.type == P9_QTDIR ? P9_DMDIR : 0);
node->nn_stat.st_nlink = 1;
return node;
}
void
p9fs_free_node(struct node *node)
{
struct netnode *nn;
if (!node)
return;
nn = node->nn;
pthread_mutex_lock(&nn->lock);
if (nn->fid) {
p9_clunk(nn->fid);
nn->fid = NULL;
}
free(nn->stat.name);
free(nn->stat.uid);
free(nn->stat.gid);
free(nn->stat.muid);
pthread_mutex_unlock(&nn->lock);
pthread_mutex_destroy(&nn->lock);
free(node);
}
error_t
p9fs_refresh_node(struct node *node)
{
struct netnode *nn = node->nn;
time_t now = time(NULL);
pthread_mutex_lock(&nn->lock);
if (nn->stat_cache_valid && (now - nn->stat_cache_time) < 10) {
pthread_mutex_unlock(&nn->lock);
return 0;
}
if (nn->fid && p9_stat(nn->fid, &nn->stat) == 0) {
node->nn_stat.st_mode = p9_to_hurd_mode(nn->stat.mode);
node->nn_stat.st_size = nn->stat.length;
node->nn_stat.st_atime = nn->stat.atime;
node->nn_stat.st_mtime = nn->stat.mtime;
node->nn_stat.st_ctime = nn->stat.mtime;
node->nn_stat.st_nlink = S_ISDIR(node->nn_stat.st_mode) ? 2 : 1;
nn->stat_cache_time = now;
nn->stat_cache_valid = 1;
} else {
nn->stat_cache_valid = 0;
}
pthread_mutex_unlock(&nn->lock);
return nn->stat_cache_valid ? 0 : p9_to_hurd_error(p9_errno);
}