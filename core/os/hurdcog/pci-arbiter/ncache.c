#include "ncache.h"
#include <unistd.h>
#include <string.h>
#include <hurd/netfs.h>
#include "pcifs.h"
#include "netfs_impl.h"
void
node_unlink (struct node *node, struct pcifs *fs)
{
struct netnode *nn = node->nn;
if (nn->ncache_next)
nn->ncache_next->nn->ncache_prev = nn->ncache_prev;
if (nn->ncache_prev)
nn->ncache_prev->nn->ncache_next = nn->ncache_next;
if (fs->node_cache_mru == node)
fs->node_cache_mru = nn->ncache_next;
if (fs->node_cache_lru == node)
fs->node_cache_lru = nn->ncache_prev;
nn->ncache_next = 0;
nn->ncache_prev = 0;
fs->node_cache_len--;
}
void
node_cache (struct node *node)
{
struct netnode *nn = node->nn;
pthread_mutex_lock (&fs->node_cache_lock);
if (fs->params.node_cache_max > 0 || fs->node_cache_len > 0)
{
if (fs->node_cache_mru != node)
{
if (nn->ncache_next || nn->ncache_prev)
node_unlink (node, fs);
else
netfs_nref (node);
nn->ncache_next = fs->node_cache_mru;
nn->ncache_prev = 0;
if (fs->node_cache_mru)
fs->node_cache_mru->nn->ncache_prev = node;
if (!fs->node_cache_lru)
fs->node_cache_lru = node;
fs->node_cache_mru = node;
fs->node_cache_len++;
}
while (fs->node_cache_len > fs->params.node_cache_max)
{
struct node *lru = fs->node_cache_lru;
node_unlink (lru, fs);
netfs_nrele (lru);
}
}
pthread_mutex_unlock (&fs->node_cache_lock);
}