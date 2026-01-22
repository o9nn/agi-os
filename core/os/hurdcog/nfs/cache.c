#include "nfs.h"
#include <string.h>
#include <stdio.h>
#include <netinet/in.h>
static hurd_ihash_key_t
ihash_hash (const void *data)
{
const struct fhandle *handle = (struct fhandle *) data;
return (hurd_ihash_key_t) hurd_ihash_hash32 (handle->data, handle->size, 0);
}
static int
ihash_compare (const void *key1, const void *key2)
{
const struct fhandle *handle1 = (struct fhandle *) key1;
const struct fhandle *handle2 = (struct fhandle *) key2;
return handle1->size == handle2->size &&
memcmp (handle1->data, handle2->data, handle1->size) == 0;
}
static struct hurd_ihash nodehash =
HURD_IHASH_INITIALIZER_GKI (sizeof (struct node)
+ offsetof (struct netnode, slot), NULL, NULL,
ihash_hash, ihash_compare);
pthread_mutex_t nodehash_ihash_lock = PTHREAD_MUTEX_INITIALIZER;
void
lookup_fhandle (struct fhandle *handle, struct node **npp)
{
struct node *np;
struct netnode *nn;
pthread_mutex_lock (&nodehash_ihash_lock);
np = hurd_ihash_find (&nodehash, (hurd_ihash_key_t) handle);
if (np)
{
netfs_nref (np);
pthread_mutex_unlock (&nodehash_ihash_lock);
pthread_mutex_lock (&np->lock);
*npp = np;
return;
}
np = netfs_make_node_alloc (sizeof (struct netnode));
assert_backtrace (np);
nn = netfs_node_netnode (np);
nn->handle.size = handle->size;
memcpy (nn->handle.data, handle->data, handle->size);
nn->stat_updated = 0;
nn->dtrans = NOT_POSSIBLE;
nn->dead_dir = 0;
nn->dead_name = 0;
hurd_ihash_add (&nodehash, (hurd_ihash_key_t) &nn->handle, np);
netfs_nref_light (np);
pthread_mutex_unlock (&nodehash_ihash_lock);
pthread_mutex_lock (&np->lock);
*npp = np;
}
struct fnd
{
struct node *dir;
char *name;
};
void *
forked_node_delete (void *arg)
{
struct fnd *args = arg;
pthread_setname_np (pthread_self (), "node_delete");
pthread_mutex_lock (&args->dir->lock);
netfs_attempt_unlink ((struct iouser *)-1, args->dir, args->name);
netfs_nput (args->dir);
free (args->name);
free (args);
return 0;
};
void
netfs_node_norefs (struct node *np)
{
if (np->nn->dead_dir)
{
struct fnd *args;
pthread_t thread;
error_t err;
args = malloc (sizeof (struct fnd));
assert_backtrace (args);
args->dir = np->nn->dead_dir;
args->name = np->nn->dead_name;
np->nn->dead_dir = 0;
np->nn->dead_name = 0;
err = pthread_create (&thread, NULL, forked_node_delete, args);
if (!err)
pthread_detach (thread);
else
{
errno = err;
perror ("pthread_create");
}
}
else
{
if (np->nn->dtrans == SYMLINK)
free (np->nn->transarg.name);
free (np);
}
}
void
netfs_try_dropping_softrefs (struct node *np)
{
pthread_mutex_lock (&nodehash_ihash_lock);
hurd_ihash_locp_remove (&nodehash, np->nn->slot);
netfs_nrele_light (np);
pthread_mutex_unlock (&nodehash_ihash_lock);
}
int *
recache_handle (int *p, struct node *np)
{
size_t len;
if (protocol_version == 2)
len = NFS2_FHSIZE;
else
{
len = ntohl (*p);
p++;
}
pthread_mutex_lock (&nodehash_ihash_lock);
hurd_ihash_locp_remove (&nodehash, np->nn->slot);
np->nn->handle.size = len;
memcpy (np->nn->handle.data, p, len);
hurd_ihash_add (&nodehash, (hurd_ihash_key_t) &np->nn->handle, np);
pthread_mutex_unlock (&nodehash_ihash_lock);
return p + len / sizeof (int);
}