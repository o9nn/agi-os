#include <fshelp.h>
#include <hurd/ports.h>
#include <hurd/ihash.h>
#include <stddef.h>
#include <assert-backtrace.h>
static struct port_class *idclass = 0;
static pthread_mutex_t idlock = PTHREAD_MUTEX_INITIALIZER;
struct idspec
{
struct port_info pi;
hurd_ihash_locp_t id_hashloc;
ino_t cache_id;
};
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
static struct hurd_ihash idhash
= HURD_IHASH_INITIALIZER_GKI (offsetof (struct idspec, id_hashloc),
NULL, NULL, hash, compare);
static void
id_clean (void *cookie)
{
struct idspec *i = cookie;
pthread_mutex_lock (&idlock);
if (refcounts_hard_references(&i->pi.refcounts) == 0
&& i->id_hashloc != NULL)
{
hurd_ihash_locp_remove (&idhash, i->id_hashloc);
i->id_hashloc = NULL;
ports_port_deref_weak (&i->pi);
}
pthread_mutex_unlock (&idlock);
}
static void
id_initialize (void)
{
assert_backtrace (!idclass);
idclass = ports_create_class (NULL, id_clean);
}
error_t
fshelp_get_identity (struct port_bucket *bucket,
ino_t fileno,
mach_port_t *pt)
{
struct idspec *i;
error_t err = 0;
pthread_mutex_lock (&idlock);
if (!idclass)
id_initialize ();
i = hurd_ihash_find (&idhash, (hurd_ihash_key_t) &fileno);
if (i == NULL)
{
err = ports_create_port (idclass, bucket, sizeof (struct idspec), &i);
if (err)
goto lose;
i->cache_id = fileno;
err = hurd_ihash_add (&idhash, (hurd_ihash_key_t) &i->cache_id, i);
if (err)
goto lose_port;
ports_port_ref_weak(&i->pi);
*pt = ports_get_right (i);
ports_port_deref (i);
}
else
*pt = ports_get_right (i);
goto lose;
lose_port:
ports_destroy_right (i);
lose:
pthread_mutex_unlock (&idlock);
return err;
}