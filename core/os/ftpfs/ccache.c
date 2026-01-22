#include <unistd.h>
#include <string.h>
#include <sys/mman.h>
#include <hurd/netfs.h>
#include "ccache.h"
#define READ_CHUNK_SIZE   (8*1024)
#define ALLOC_CHUNK_SIZE  (64*1024)
error_t
ccache_read (struct ccache *cc, off_t offs, size_t len, void *data)
{
error_t err = 0;
size_t max = offs + len;
pthread_mutex_lock (&cc->lock);
if (max > cc->size)
max = cc->size;
while (cc->max < max && !err)
{
if (cc->fetching_active)
{
if (pthread_hurd_cond_wait_np (&cc->wakeup, &cc->lock))
err = EINTR;
}
else
{
int re_connected = 0;
struct netnode *nn = cc->node->nn;
cc->fetching_active = 1;
while (cc->max < max && !err)
{
pthread_mutex_unlock (&cc->lock);
if (! cc->conn)
{
err = ftpfs_get_ftp_conn (nn->fs, &cc->conn);
if (! err)
{
err = ftp_conn_start_retrieve (cc->conn, nn->rmt_path,
&cc->data_conn);
if (err == ENOENT)
err = ESTALE;
if (err)
{
ftpfs_release_ftp_conn (nn->fs, cc->conn);
cc->conn = 0;
}
else
cc->data_conn_pos = 0;
}
re_connected = 1;
}
if (! err)
{
size_t new_end = cc->max + READ_CHUNK_SIZE;
if (new_end > cc->size)
new_end = cc->size;
if (new_end > cc->alloced)
{
size_t alloc_end = cc->alloced + ALLOC_CHUNK_SIZE;
if (alloc_end < new_end)
alloc_end = new_end;
else if (alloc_end > cc->size)
alloc_end = cc->size;
if (cc->alloced == 0)
{
vm_address_t addr = 0;
addr = (vm_address_t) mmap (0, alloc_end,
PROT_READ|PROT_WRITE,
MAP_ANON, 0, 0);
err = (addr == -1) ? errno : 0;
if (! err)
cc->image = (char *)addr;
}
else
{
vm_address_t addr =
(vm_address_t)cc->image + cc->alloced;
err = vm_allocate (mach_task_self (),
&addr, alloc_end - cc->alloced,
0);
if (err == EKERN_NO_SPACE)
{
addr = 0;
addr = (vm_address_t) mmap (0, alloc_end,
PROT_READ|PROT_WRITE,
MAP_ANON, 0, 0);
err = (addr == -1) ? errno : 0;
if (! err)
{
bcopy (cc->image, (void *)addr, cc->max);
munmap (cc->image, cc->alloced);
cc->image = (char *)addr;
}
}
}
if (! err)
cc->alloced = alloc_end;
}
if (! err)
{
ssize_t rd =
read (cc->data_conn,
cc->image + cc->data_conn_pos,
new_end - cc->data_conn_pos);
if (rd < 0)
err = errno;
else if (rd == 0)
{
if (re_connected)
err = EIO;
else
{
close (cc->data_conn);
ftp_conn_finish_transfer (cc->conn);
ftpfs_release_ftp_conn (nn->fs, cc->conn);
cc->conn = 0;
}
}
else
{
cc->data_conn_pos += rd;
if (cc->data_conn_pos > cc->max)
cc->max = cc->data_conn_pos;
}
}
if (!err && ports_self_interrupted ())
err = EINTR;
}
pthread_mutex_lock (&cc->lock);
if (cc->max < max && !err)
pthread_cond_broadcast (&cc->wakeup);
}
if (!err && cc->conn && cc->max == cc->size)
{
close (cc->data_conn);
ftp_conn_finish_transfer (cc->conn);
ftpfs_release_ftp_conn (nn->fs, cc->conn);
cc->conn = 0;
}
cc->fetching_active = 0;
pthread_cond_broadcast (&cc->wakeup);
}
}
if (! err)
bcopy (cc->image + offs, data, max - offs);
pthread_mutex_unlock (&cc->lock);
return err;
}
error_t
ccache_invalidate (struct ccache *cc)
{
error_t err = 0;
pthread_mutex_lock (&cc->lock);
while  (cc->fetching_active && !err)
{
if (pthread_hurd_cond_wait_np (&cc->wakeup, &cc->lock))
err = EINTR;
}
if (! err)
{
if (cc->alloced > 0)
{
munmap (cc->image, cc->alloced);
cc->image = 0;
cc->alloced = 0;
cc->max = 0;
}
if (cc->conn)
{
close (cc->data_conn);
ftp_conn_finish_transfer (cc->conn);
ftpfs_release_ftp_conn (cc->node->nn->fs, cc->conn);
cc->conn = 0;
}
}
pthread_mutex_unlock (&cc->lock);
return err;
}
error_t
ccache_create (struct node *node, struct ccache **cc)
{
struct ccache *new = malloc (sizeof (struct ccache));
if (! new)
return ENOMEM;
new->node = node;
new->image = 0;
new->size = node->nn_stat.st_size;
new->max = 0;
new->alloced = 0;
pthread_mutex_init (&new->lock, NULL);
pthread_cond_init (&new->wakeup, NULL);
new->fetching_active = 0;
new->conn = 0;
new->data_conn = -1;
*cc = new;
return 0;
}
void
ccache_free (struct ccache *cc)
{
if (cc->alloced > 0)
munmap (cc->image, cc->alloced);
if (cc->data_conn >= 0)
close (cc->data_conn);
if (cc->conn)
{
ftp_conn_finish_transfer (cc->conn);
ftpfs_release_ftp_conn (cc->node->nn->fs, cc->conn);
}
free (cc);
}