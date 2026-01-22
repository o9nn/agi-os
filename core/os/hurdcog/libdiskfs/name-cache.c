#include "priv.h"
#include <assert-backtrace.h>
#include <hurd/ihash.h>
#include <string.h>
#define CACHE_SIZE	256
#define BUCKET_SIZE	4
#define CACHE_MASK	(CACHE_SIZE - 1)
struct cache_bucket
{
unsigned long name[BUCKET_SIZE];
unsigned long key[BUCKET_SIZE];
ino64_t dir_cache_id[BUCKET_SIZE];
ino64_t node_cache_id[BUCKET_SIZE];
};
static struct cache_bucket name_cache[CACHE_SIZE];
static pthread_mutex_t cache_lock = PTHREAD_MUTEX_INITIALIZER;
static inline char *
charp (unsigned long value)
{
return (char *) (value & ~3L);
}
static inline unsigned long
frequ (unsigned long value)
{
return value & 3;
}
static inline void
add_entry (struct cache_bucket *b, int i,
const char *name, unsigned long key,
ino64_t dir_cache_id, ino64_t node_cache_id)
{
if (b->name[i])
free (charp (b->name[i]));
b->name[i] = (unsigned long) strdup (name);
assert_backtrace ((b->name[i] & 3) == 0);
if (b->name[i] == 0)
return;
b->key[i] = key;
b->dir_cache_id[i] = dir_cache_id;
b->node_cache_id[i] = node_cache_id;
}
static inline void
remove_entry (struct cache_bucket *b, int i)
{
if (b->name[i])
free (charp (b->name[i]));
b->name[i] = 0;
}
static inline int
valid_entry (struct cache_bucket *b, int i)
{
return b->name[i] != 0;
}
static int replace;
static inline int
lookup (ino64_t dir_cache_id, const char *name, unsigned long key,
struct cache_bucket **bucket, int *index)
{
struct cache_bucket *b = *bucket = &name_cache[key & CACHE_MASK];
unsigned long best = 3;
int i;
for (i = 0; i < BUCKET_SIZE; i++)
{
unsigned long f = frequ (b->name[i]);
if (valid_entry (b, i)
&& b->key[i] == key
&& b->dir_cache_id[i] == dir_cache_id
&& strcmp (charp (b->name[i]), name) == 0)
{
if (f < 3)
b->name[i] += 1;
*index = i;
return 1;
}
if (f < best)
{
best = f;
*index = i;
}
}
if (best == 3)
{
*index = replace;
replace = (replace + 1) & (BUCKET_SIZE - 1);
}
return 0;
}
static inline unsigned long
hash (ino64_t dir_cache_id, const char *name)
{
unsigned long h;
h = hurd_ihash_hash32 (&dir_cache_id, sizeof dir_cache_id, 0);
h = hurd_ihash_hash32 (name, strlen (name), h);
return h;
}
void
diskfs_enter_lookup_cache (struct node *dir, struct node *np, const char *name)
{
unsigned long key = hash (dir->cache_id, name);
ino64_t value = np ? np->cache_id : 0;
struct cache_bucket *bucket;
int i = 0, found;
pthread_mutex_lock (&cache_lock);
found = lookup (dir->cache_id, name, key, &bucket, &i);
if (! found)
add_entry (bucket, i, name, key, dir->cache_id, value);
else
if (bucket->node_cache_id[i] != value)
bucket->node_cache_id[i] = value;
pthread_mutex_unlock (&cache_lock);
}
void
diskfs_purge_lookup_cache (struct node *dp, struct node *np)
{
int i;
struct cache_bucket *b;
pthread_mutex_lock (&cache_lock);
for (b = &name_cache[0]; b < &name_cache[CACHE_SIZE]; b++)
for (i = 0; i < BUCKET_SIZE; i++)
if (valid_entry (b, i)
&& b->dir_cache_id[i] == dp->cache_id
&& b->node_cache_id[i] == np->cache_id)
remove_entry (b, i);
pthread_mutex_unlock (&cache_lock);
}
struct node *
diskfs_check_lookup_cache (struct node *dir, const char *name)
{
unsigned long key = hash (dir->cache_id, name);
int lookup_parent = name[0] == '.' && name[1] == '.' && name[2] == '\0';
struct cache_bucket *bucket;
int i, found;
if (lookup_parent && dir == diskfs_root_node)
return NULL;
pthread_mutex_lock (&cache_lock);
found = lookup (dir->cache_id, name, key, &bucket, &i);
if (found)
{
ino64_t id = bucket->node_cache_id[i];
pthread_mutex_unlock (&cache_lock);
if (id == 0)
return (struct node *) -1;
else if (id == dir->cache_id)
{
diskfs_nref (dir);
return dir;
}
else
{
struct node *np;
error_t err;
if (lookup_parent)
{
pthread_mutex_unlock (&dir->lock);
err = diskfs_cached_lookup (id, &np);
pthread_mutex_lock (&dir->lock);
pthread_mutex_lock (&cache_lock);
found = lookup (dir->cache_id, name, key, &bucket, &i);
if (! found
|| bucket->node_cache_id[i] != id)
{
pthread_mutex_unlock (&cache_lock);
diskfs_nput (np);
return 0;
}
pthread_mutex_unlock (&cache_lock);
}
else
err = diskfs_cached_lookup (id, &np);
return err ? 0 : np;
}
}
pthread_mutex_unlock (&cache_lock);
return 0;
}