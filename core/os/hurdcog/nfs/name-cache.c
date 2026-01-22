#include "nfs.h"
#include <string.h>
#include <cacheq.h>
#define MAXCACHE 200
#define CACHE_NAME_LEN 100
struct lookup_cache
{
struct cacheq_hdr hdr;
char dir_cache_fh[NFS3_FHSIZE];
size_t dir_cache_len;
struct node *np;
char name[CACHE_NAME_LEN];
size_t name_len;
time_t cache_stamp;
int stati;
};
static struct cacheq lookup_cache = { sizeof (struct lookup_cache) };
static pthread_spinlock_t cache_lock = PTHREAD_SPINLOCK_INITIALIZER;
static struct stats
{
long pos_hits;
long neg_hits;
long miss;
long fetch_errors;
} statistics;
#define PARTIAL_THRESH 100
#define NPARTIALS (MAXCACHE / PARTIAL_THRESH)
struct stats partial_stats [NPARTIALS];
static struct lookup_cache *
find_cache (char *dir, size_t len, const char *name, size_t name_len)
{
struct lookup_cache *c;
int i;
for (i = 0, c = lookup_cache.mru;
c && c->name_len;
c = c->hdr.next, i++)
if (c->name_len == name_len
&& c->dir_cache_len == len
&& c->name[0] == name[0]
&& memcmp (c->dir_cache_fh, dir, len) == 0
&& strcmp (c->name, name) == 0)
{
c->stati = i / PARTIAL_THRESH;
return c;
}
return 0;
}
void
enter_lookup_cache (char *dir, size_t len, struct node *np, const char *name)
{
struct lookup_cache *c;
size_t name_len = strlen (name);
if (name_len > CACHE_NAME_LEN - 1)
return;
pthread_spin_lock (&cache_lock);
if (lookup_cache.length == 0)
cacheq_set_length (&lookup_cache, MAXCACHE);
c = find_cache (dir, len, name, name_len) ?: lookup_cache.lru;
memcpy (c->dir_cache_fh, dir, len);
c->dir_cache_len = len;
if (c->np)
netfs_nrele (c->np);
c->np = np;
if (c->np)
netfs_nref (c->np);
strcpy (c->name, name);
c->name_len = name_len;
c->cache_stamp = mapped_time->seconds;
cacheq_make_mru (&lookup_cache, c);
pthread_spin_unlock (&cache_lock);
}
void
purge_lookup_cache (struct node *dp, const char *name, size_t namelen)
{
struct lookup_cache *c, *next;
pthread_spin_lock (&cache_lock);
for (c = lookup_cache.mru; c; c = next)
{
next = c->hdr.next;
if (c->name_len == namelen
&& c->dir_cache_len == dp->nn->handle.size
&& memcmp (c->dir_cache_fh, dp->nn->handle.data,
c->dir_cache_len) == 0
&& strcmp (c->name, name) == 0)
{
if (c->np)
netfs_nrele (c->np);
c->name_len = 0;
c->np = 0;
cacheq_make_lru (&lookup_cache, c);
}
}
pthread_spin_unlock (&cache_lock);
}
void
purge_lookup_cache_node (struct node *np)
{
struct lookup_cache *c, *next;
pthread_spin_lock (&cache_lock);
for (c = lookup_cache.mru; c; c = next)
{
next = c->hdr.next;
if (c->np == np)
{
netfs_nrele (c->np);
c->name_len = 0;
c->np = 0;
cacheq_make_lru (&lookup_cache, c);
}
}
pthread_spin_unlock (&cache_lock);
}
void
register_neg_hit (int n)
{
int i;
statistics.neg_hits++;
for (i = 0; i < n; i++)
partial_stats[i].miss++;
for (; i < NPARTIALS; i++)
partial_stats[i].neg_hits++;
}
void
register_pos_hit (int n)
{
int i;
statistics.pos_hits++;
for (i = 0; i < n; i++)
partial_stats[i].miss++;
for (; i < NPARTIALS; i++)
partial_stats[i].pos_hits++;
}
void
register_miss (void)
{
int i;
statistics.miss++;
for (i = 0; i < NPARTIALS; i++)
partial_stats[i].miss++;
}
struct node *
check_lookup_cache (struct node *dir, const char *name)
{
struct lookup_cache *c;
pthread_spin_lock (&cache_lock);
c = find_cache (dir->nn->handle.data, dir->nn->handle.size,
name, strlen (name));
if (c)
{
int timeout = c->np
? name_cache_timeout
: name_cache_neg_timeout;
if (mapped_time->seconds - c->cache_stamp >= timeout)
{
register_neg_hit (c->stati);
if (c->np)
netfs_nrele (c->np);
c->name_len = 0;
c->np = 0;
cacheq_make_lru (&lookup_cache, c);
pthread_spin_unlock (&cache_lock);
return 0;
}
cacheq_make_mru (&lookup_cache, c);
if (c->np == 0)
{
register_neg_hit (c->stati);
pthread_spin_unlock (&cache_lock);
pthread_mutex_unlock (&dir->lock);
return (struct node *)-1;
}
else
{
struct node *np;
np = c->np;
netfs_nref (np);
register_pos_hit (c->stati);
pthread_spin_unlock (&cache_lock);
pthread_mutex_unlock (&dir->lock);
pthread_mutex_lock (&np->lock);
return np;
}
}
register_miss ();
pthread_spin_unlock (&cache_lock);
return 0;
}