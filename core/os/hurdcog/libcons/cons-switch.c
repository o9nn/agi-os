#include <errno.h>
#include <assert-backtrace.h>
#include <pthread.h>
#include "cons.h"
error_t
cons_switch (vcons_t vcons, int id, int delta, vcons_t *r_vcons)
{
error_t err = 0;
cons_t cons = vcons->cons;
vcons_list_t vcons_entry = NULL;
if (!id && !delta)
return 0;
pthread_mutex_lock (&cons->lock);
if (id)
{
vcons_entry = cons->vcons_list;
while (vcons_entry && vcons_entry->id != id)
vcons_entry = vcons_entry->next;
}
else if (delta > 0)
{
vcons_entry = vcons->vcons_entry;
while (delta-- > 0)
{
vcons_entry = vcons_entry->next;
if (!vcons_entry)
vcons_entry = cons->vcons_list;
}
}
else
{
assert_backtrace (delta < 0);
vcons_entry = vcons->vcons_entry;
while (delta++ < 0)
{
vcons_entry = vcons_entry->prev;
if (!vcons_entry)
vcons_entry = cons->vcons_last;
}
}
if (!vcons_entry)
{
pthread_mutex_unlock (&cons->lock);
return ESRCH;
}
if (vcons_entry->vcons)
{
*r_vcons = vcons_entry->vcons;
pthread_mutex_lock (&vcons_entry->vcons->lock);
}
else
{
err = cons_vcons_open (cons, vcons_entry, r_vcons);
if (!err)
vcons_entry->vcons = *r_vcons;
}
pthread_mutex_unlock (&cons->lock);
return err;
}