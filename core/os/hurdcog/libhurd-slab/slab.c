#if HAVE_CONFIG_H
#include <config.h>
#endif
#include <stdlib.h>
#include <errno.h>
#include <sys/mman.h>
#include <assert-backtrace.h>
#include <string.h>
#include <unistd.h>
#include <pthread.h>
#include <stdint.h>
#include "slab.h"
#define SLAB_PAGES 4
static int __hurd_slab_nr_pages;
union hurd_bufctl
{
union hurd_bufctl *next;
struct hurd_slab *slab;
};
struct hurd_slab
{
struct hurd_slab *next;
struct hurd_slab *prev;
int refcount;
union hurd_bufctl *free_list;
};
static error_t
allocate_buffer (struct hurd_slab_space *space, size_t size, void **ptr)
{
if (space->allocate_buffer)
return space->allocate_buffer (space->hook, size, ptr);
else
{
*ptr = mmap (NULL, size, PROT_READ|PROT_WRITE,
MAP_PRIVATE|MAP_ANONYMOUS, 0, 0);
if (*ptr == MAP_FAILED)
return errno;
else
return 0;
}
}
static error_t
deallocate_buffer (struct hurd_slab_space *space, void *buffer, size_t size)
{
if (space->deallocate_buffer)
return space->deallocate_buffer (space->hook, buffer, size);
else
{
if (munmap (buffer, size) == -1)
return errno;
else
return 0;
}
}
static void
insert_slab (struct hurd_slab_space *space, struct hurd_slab *slab)
{
assert_backtrace (slab->refcount == 0);
if (space->slab_first == 0)
space->slab_first = space->slab_last = slab;
else
{
space->slab_last->next = slab;
slab->prev = space->slab_last;
space->slab_last = slab;
}
}
static void
remove_slab (struct hurd_slab_space *space, struct hurd_slab *slab)
{
if (slab != space->slab_first
&& slab != space->slab_last)
{
slab->next->prev = slab->prev;
slab->prev->next = slab->next;
return;
}
if (slab == space->slab_first)
{
space->slab_first = slab->next;
if (space->slab_first)
space->slab_first->prev = NULL;
}
if (slab == space->slab_last)
{
if (slab->prev)
slab->prev->next = NULL;
space->slab_last = slab->prev;
}
}
static error_t
reap (struct hurd_slab_space *space)
{
struct hurd_slab *s, *next, *new_first;
error_t err = 0;
for (s = space->slab_first; s; s = next)
{
next = s->next;
if (!s->refcount)
{
remove_slab (space, s);
if (space->destructor)
{
union hurd_bufctl *bufctl;
for (bufctl = s->free_list; bufctl; bufctl = bufctl->next)
{
void *buffer = (((void *) bufctl)
- (space->size - sizeof *bufctl));
(*space->destructor) (space->hook, buffer);
}
}
err = deallocate_buffer (space, (void *) (((uintptr_t) s)
+ sizeof (struct hurd_slab)
- space->slab_size),
space->slab_size);
if (err)
break;
__hurd_slab_nr_pages--;
}
}
new_first = space->slab_first;
while (new_first)
{
if (new_first->refcount != space->full_refcount)
break;
new_first = new_first->next;
}
space->first_free = new_first;
return err;
}
static void
init_space (hurd_slab_space_t space)
{
size_t size = space->requested_size + sizeof (union hurd_bufctl);
size_t alignment = space->requested_align;
size = (size + alignment - 1) & ~(alignment - 1);
assert_backtrace (size <= (space->slab_size
- sizeof (struct hurd_slab)
- sizeof (union hurd_bufctl)));
space->size = size;
space->full_refcount
= ((space->slab_size - sizeof (struct hurd_slab)) / size);
space->initialized = true;
}
static error_t
grow (struct hurd_slab_space *space)
{
error_t err;
struct hurd_slab *new_slab;
union hurd_bufctl *bufctl;
int nr_objs, i;
void *p;
if (!space->initialized)
init_space (space);
err = allocate_buffer (space, space->slab_size, &p);
if (err)
return err;
__hurd_slab_nr_pages++;
new_slab = (p + space->slab_size - sizeof (struct hurd_slab));
memset (new_slab, 0, sizeof (*new_slab));
nr_objs = ((space->slab_size - sizeof (struct hurd_slab))
/ space->size);
for (i = 0; i < nr_objs; i++, p += space->size)
{
if (space->constructor)
{
error_t err = (*space->constructor) (space->hook, p);
if (err)
{
for (bufctl = new_slab->free_list; bufctl;
bufctl = bufctl->next)
{
void *buffer = (((void *) bufctl)
- (space->size - sizeof *bufctl));
(*space->destructor) (space->hook, buffer);
}
deallocate_buffer (space, p, space->slab_size);
return err;
}
}
bufctl = (p + space->size - sizeof *bufctl);
bufctl->next = new_slab->free_list;
new_slab->free_list = bufctl;
}
insert_slab (space, new_slab);
space->first_free = new_slab;
return 0;
}
error_t
hurd_slab_init (hurd_slab_space_t space, size_t size, size_t alignment,
hurd_slab_allocate_buffer_t allocate_buffer,
hurd_slab_deallocate_buffer_t deallocate_buffer,
hurd_slab_constructor_t constructor,
hurd_slab_destructor_t destructor,
void *hook)
{
error_t err;
memset (space, 0, sizeof (struct hurd_slab_space));
if (!alignment)
alignment = __alignof__ (void *);
space->requested_size = size;
space->requested_align = alignment;
space->slab_size = getpagesize () * SLAB_PAGES;
size = size + sizeof (union hurd_bufctl);
size = (size + alignment - 1) & ~(alignment - 1);
if (size > (space->slab_size - sizeof (struct hurd_slab)
- sizeof (union hurd_bufctl)))
return EINVAL;
err = pthread_mutex_init (&space->lock, NULL);
if (err)
return err;
space->allocate_buffer = allocate_buffer;
space->deallocate_buffer = deallocate_buffer;
space->constructor = constructor;
space->destructor = destructor;
space->hook = hook;
return 0;
}
error_t
hurd_slab_create (size_t size, size_t alignment,
hurd_slab_allocate_buffer_t allocate_buffer,
hurd_slab_deallocate_buffer_t deallocate_buffer,
hurd_slab_constructor_t constructor,
hurd_slab_destructor_t destructor,
void *hook,
hurd_slab_space_t *r_space)
{
hurd_slab_space_t space;
error_t err;
space = malloc (sizeof (struct hurd_slab_space));
if (!space)
return ENOMEM;
err = hurd_slab_init (space, size, alignment,
allocate_buffer, deallocate_buffer,
constructor, destructor, hook);
if (err)
{
free (space);
return err;
}
*r_space = space;
return 0;
}
error_t
hurd_slab_destroy (hurd_slab_space_t space)
{
error_t err;
pthread_mutex_lock (&space->lock);
err = reap (space);
if (err)
{
pthread_mutex_unlock (&space->lock);
return err;
}
if (space->slab_first)
{
pthread_mutex_unlock (&space->lock);
return EBUSY;
}
return 0;
}
error_t
hurd_slab_free (hurd_slab_space_t space)
{
error_t err = hurd_slab_destroy (space);
if (err)
return err;
free (space);
return 0;
}
error_t
hurd_slab_alloc (hurd_slab_space_t space, void **buffer)
{
error_t err;
union hurd_bufctl *bufctl;
pthread_mutex_lock (&space->lock);
if (!space->first_free)
{
err = grow (space);
if (err)
{
pthread_mutex_unlock (&space->lock);
return err;
}
}
bufctl = space->first_free->free_list;
space->first_free->free_list = bufctl->next;
space->first_free->refcount++;
bufctl->slab = space->first_free;
if (space->first_free->refcount == space->full_refcount)
{
struct hurd_slab *new_first = space->slab_first;
while (new_first)
{
if (new_first->refcount != space->full_refcount)
break;
new_first = new_first->next;
}
space->first_free = new_first;
}
*buffer = ((void *) bufctl) - (space->size - sizeof *bufctl);
pthread_mutex_unlock (&space->lock);
return 0;
}
static inline void
put_on_slab_list (struct hurd_slab *slab, union hurd_bufctl *bufctl)
{
bufctl->next = slab->free_list;
slab->free_list = bufctl;
slab->refcount--;
assert_backtrace (slab->refcount >= 0);
}
void
hurd_slab_dealloc (hurd_slab_space_t space, void *buffer)
{
struct hurd_slab *slab;
union hurd_bufctl *bufctl;
assert_backtrace (space->initialized);
pthread_mutex_lock (&space->lock);
bufctl = (buffer + (space->size - sizeof *bufctl));
put_on_slab_list (slab = bufctl->slab, bufctl);
if (!space->first_free
|| slab->refcount < space->first_free->refcount)
space->first_free = slab;
pthread_mutex_unlock (&space->lock);
}