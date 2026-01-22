#ifndef __CCACHE_H__
#define __CCACHE_H__
#include "ftpfs.h"
struct ccache
{
struct node *node;
char *image;
off_t size;
off_t max;
size_t alloced;
pthread_mutex_t lock;
pthread_cond_t wakeup;
int fetching_active;
struct ftp_conn *conn;
int data_conn;
off_t data_conn_pos;
};
error_t ccache_read (struct ccache *cc, off_t offs, size_t len, void *data);
error_t ccache_invalidate (struct ccache *cc);
error_t ccache_create (struct node *node, struct ccache **cc);
void ccache_free (struct ccache *cc);
#endif