#include <pthread.h>
#include <linux/malloc.h>
struct kmem_cache_s
{
pthread_mutex_t lock;
void *freelist;
size_t item_size;
void (*ctor) (void *, kmem_cache_t *, unsigned long);
void (*dtor) (void *, kmem_cache_t *, unsigned long);
};
kmem_cache_t *
kmem_cache_create (const char *name, size_t item_size,
size_t something, unsigned long flags,
void (*ctor) (void *, kmem_cache_t *, unsigned long),
void (*dtor) (void *, kmem_cache_t *, unsigned long))
{
kmem_cache_t *new = malloc (sizeof *new);
if (!new)
return 0;
pthread_mutex_init (&new->lock, NULL);
new->freelist = 0;
new->item_size = item_size;
new->ctor = ctor;
new->dtor = dtor;
return new;
}
void *
kmem_cache_alloc (kmem_cache_t *cache, int flags)
{
void *p;
pthread_mutex_lock (&cache->lock);
p = cache->freelist;
if (p != 0) {
cache->freelist = *(void **)(p + cache->item_size);
pthread_mutex_unlock (&cache->lock);
return p;
}
pthread_mutex_unlock (&cache->lock);
p = malloc (cache->item_size + sizeof (void *));
if (p && cache->ctor)
(*cache->ctor) (p, cache, flags);
return p;
}
void
kmem_cache_free (kmem_cache_t *cache, void *p)
{
void **const nextp = (void **) (p + cache->item_size);
pthread_mutex_lock (&cache->lock);
*nextp = cache->freelist;
cache->freelist = p;
pthread_mutex_unlock (&cache->lock);
}