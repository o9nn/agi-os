#include "ports.h"
#include <hurd/ihash.h>
error_t
_ports_bucket_class_iterate (struct hurd_ihash *ht,
struct port_class *class,
error_t (*fun)(void *))
{
void **p;
size_t i, n, nr_items;
error_t err;
pthread_rwlock_rdlock (&_ports_htable_lock);
if (ht->nr_items == 0)
{
pthread_rwlock_unlock (&_ports_htable_lock);
return 0;
}
nr_items = ht->nr_items;
p = malloc (nr_items * sizeof *p);
if (p == NULL)
{
pthread_rwlock_unlock (&_ports_htable_lock);
return ENOMEM;
}
n = 0;
HURD_IHASH_ITERATE (ht, arg)
{
struct port_info *const pi = arg;
if (class == 0 || pi->class == class)
{
refcounts_ref (&pi->refcounts, NULL);
p[n] = pi;
n++;
}
}
pthread_rwlock_unlock (&_ports_htable_lock);
if (n != 0 && n != nr_items)
{
void **new = realloc (p, n * sizeof *p);
if (new)
p = new;
}
err = 0;
for (i = 0; i < n; i++)
{
if (!err && !ports_port_is_notify (p[i]))
err = (*fun)(p[i]);
ports_port_deref (p[i]);
}
free (p);
return err;
}
error_t
ports_bucket_iterate (struct port_bucket *bucket,
error_t (*fun)(void *))
{
return _ports_bucket_class_iterate (&bucket->htable, NULL, fun);
}