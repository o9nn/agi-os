#include <hurd/ihash.h>
#include "priv.h"
#define mix_fasthash(h) ({ \
(h) ^= (h) >> 23; \
(h) *= 0x2127599bf4325c37ULL; \
(h) ^= (h) >> 47; })
static hurd_ihash_key_t
hash (const void *key)
{
ino_t i;
i = *(ino_t *) key;
mix_fasthash (i);
return (hurd_ihash_key_t) i;
}
static int
compare (const void *a, const void *b)
{
return *(ino_t *) a == *(ino_t *) b;
}
static struct hurd_ihash nodecache =
HURD_IHASH_INITIALIZER_GKI (offsetof (struct node, slot), NULL, NULL,
hash, compare);
static pthread_rwlock_t nodecache_lock = PTHREAD_RWLOCK_INITIALIZER;
error_t __attribute__ ((weak))
diskfs_cached_lookup (ino_t inum, struct node **npp)
{
return diskfs_cached_lookup_context (inum, npp, NULL);
}
error_t
diskfs_cached_lookup_context (ino_t inum, struct node **npp,
struct lookup_context *ctx)
{
error_t err;
struct node *np, *tmp;
hurd_ihash_locp_t slot;
pthread_rwlock_rdlock (&nodecache_lock);
np = hurd_ihash_locp_find (&nodecache, (hurd_ihash_key_t) &inum, &slot);
if (np)
goto gotit;
pthread_rwlock_unlock (&nodecache_lock);
err = diskfs_user_make_node (&np, ctx);
if (err)
return err;
np->cache_id = inum;
pthread_mutex_lock (&np->lock);
pthread_rwlock_wrlock (&nodecache_lock);
tmp = hurd_ihash_locp_find (&nodecache, (hurd_ihash_key_t) &np->cache_id,
&slot);
if (tmp)
{
diskfs_nput (np);
np = tmp;
goto gotit;
}
err = hurd_ihash_locp_add (&nodecache, slot,
(hurd_ihash_key_t) &np->cache_id, np);
assert_perror_backtrace (err);
diskfs_nref_light (np);
pthread_rwlock_unlock (&nodecache_lock);
err = diskfs_user_read_node (np, ctx);
if (err)
return err;
else
{
*npp = np;
return 0;
}
gotit:
diskfs_nref (np);
pthread_rwlock_unlock (&nodecache_lock);
pthread_mutex_lock (&np->lock);
*npp = np;
return 0;
}
struct node *
diskfs_cached_ifind (ino_t inum)
{
struct node *np;
pthread_rwlock_rdlock (&nodecache_lock);
np = hurd_ihash_find (&nodecache, (hurd_ihash_key_t) &inum);
pthread_rwlock_unlock (&nodecache_lock);
assert_backtrace (np);
return np;
}
void __attribute__ ((weak))
diskfs_try_dropping_softrefs (struct node *np)
{
pthread_rwlock_wrlock (&nodecache_lock);
if (np->slot != NULL)
{
struct references result;
refcounts_references (&np->refcounts, &result);
if (result.hard > 0)
{
pthread_rwlock_unlock (&nodecache_lock);
return;
}
hurd_ihash_locp_remove (&nodecache, np->slot);
np->slot = NULL;
diskfs_node_update (np, diskfs_synchronous);
diskfs_nrele_light (np);
}
pthread_rwlock_unlock (&nodecache_lock);
diskfs_user_try_dropping_softrefs (np);
}
error_t __attribute__ ((weak))
diskfs_node_iterate (error_t (*fun)(struct node *))
{
error_t err = 0;
size_t num_nodes;
struct node *node, **node_list, **p;
pthread_rwlock_rdlock (&nodecache_lock);
num_nodes = nodecache.nr_items;
node_list = malloc (num_nodes * sizeof (struct node *));
if (node_list == NULL)
{
pthread_rwlock_unlock (&nodecache_lock);
return ENOMEM;
}
p = node_list;
HURD_IHASH_ITERATE (&nodecache, i)
{
*p++ = node = i;
refcounts_ref (&node->refcounts, NULL);
}
pthread_rwlock_unlock (&nodecache_lock);
p = node_list;
while (num_nodes-- > 0)
{
node = *p++;
if (!err)
{
pthread_mutex_lock (&node->lock);
err = (*fun)(node);
pthread_mutex_unlock (&node->lock);
}
diskfs_nrele (node);
}
free (node_list);
return err;
}
error_t __attribute__ ((weak))
diskfs_user_make_node (struct node **npp, struct lookup_context *ctx)
{
assert_backtrace (! "diskfs_user_make_node not implemented");
}
error_t __attribute__ ((weak))
diskfs_user_read_node (struct node *np, struct lookup_context *ctx)
{
assert_backtrace (! "diskfs_user_read_node not implemented");
}
void __attribute__ ((weak))
diskfs_user_try_dropping_softrefs (struct node *np)
{
assert_backtrace (! "diskfs_user_try_dropping_softrefs not implemented");
}