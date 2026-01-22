#include "priv.h"
#include "memory_object_S.h"
#include <stdio.h>
#include <string.h>
#include <assert-backtrace.h>
kern_return_t
_pager_do_write_request (struct pager *p,
mach_port_t control,
vm_offset_t offset,
pointer_t data,
vm_size_t length,
int dirty,
int kcopy,
int initializing)
{
short *pm_entries;
int npages, i;
char *notified;
error_t *pagerrs;
struct lock_request *lr;
struct lock_list {struct lock_request *lr;
struct lock_list *next;} *lock_list, *ll;
int wakeup;
unsigned omitdata = 0;
if (!p
|| p->port.class != _pager_class)
return EOPNOTSUPP;
pthread_mutex_lock (&p->interlock);
if (control != p->memobjcntl)
{
printf ("incg data return: wrong control port\n");
goto release_out;
}
if (length % __vm_page_size)
{
printf ("incg data return: bad length size %lu\n", (unsigned long)length);
goto release_out;
}
if (offset % __vm_page_size)
{
printf ("incg data return: misaligned request\n");
goto release_out;
}
if (p->pager_state != NORMAL)
{
printf ("pager in wrong state for write\n");
goto release_out;
}
npages = length / __vm_page_size;
pagerrs = alloca (npages * sizeof (error_t));
notified = alloca (npages * (sizeof *notified));
#ifndef NDEBUG
memset (notified, -1, npages * (sizeof *notified));
#endif
_pager_block_termination (p);
_pager_pagemap_resize (p, offset + length);
pm_entries = &p->pagemap[offset / __vm_page_size];
if (! dirty)
{
munmap ((void *) data, length);
if (!kcopy) {
for (i = 0; i < npages; i++)
notified[i] = (p->notify_on_evict
&& ! (pm_entries[i] & PM_PAGEINWAIT));
goto notify;
}
else {
_pager_allow_termination (p);
goto release_out;
}
}
retry:
for (i = 0; i < npages; i++)
if (pm_entries[i] & PM_PAGINGOUT)
{
pm_entries[i] |= PM_WRITEWAIT;
pthread_cond_wait (&p->wakeup, &p->interlock);
goto retry;
}
if (initializing)
{
assert_backtrace (npages < 32);
for (i = 0; i < npages; i++)
{
if (pm_entries[i] & PM_INIT)
omitdata |= 1U << i;
else
pm_entries[i] |= PM_PAGINGOUT | PM_INIT;
}
}
else
for (i = 0; i < npages; i++)
pm_entries[i] |= PM_PAGINGOUT | PM_INIT;
lock_list = 0;
for (lr = p->lock_requests; lr; lr = lr->next)
if (offset < lr->end && offset + length >= lr->start)
{
ll = alloca (sizeof (struct lock_list));
ll->lr = lr;
ll->next = lock_list;
lock_list = ll;
lr->pending_writes++;
}
pthread_mutex_unlock (&p->interlock);
for (i = 0; i < npages; i++)
if (!(omitdata & (1U << i)))
pagerrs[i] = pager_write_page (p->upi,
offset + (vm_page_size * i),
data + (vm_page_size * i));
pthread_mutex_lock (&p->interlock);
_pager_pagemap_resize (p, offset + length);
pm_entries = &p->pagemap[offset / __vm_page_size];
wakeup = 0;
for (i = 0; i < npages; i++)
{
if (omitdata & (1U << i))
{
notified[i] = 0;
continue;
}
if (pm_entries[i] & PM_WRITEWAIT)
wakeup = 1;
if (pagerrs[i] && ! (pm_entries[i] & PM_PAGEINWAIT))
pm_entries[i] |= PM_INVALID;
if (pm_entries[i] & PM_PAGEINWAIT)
{
memory_object_data_supply (p->memobjcntl,
offset + (vm_page_size * i),
data + (vm_page_size * i),
vm_page_size, 1,
VM_PROT_NONE, 0, MACH_PORT_NULL);
notified[i] = 0;
}
else
{
munmap ((void *) (data + (vm_page_size * i)),
vm_page_size);
notified[i] = (! kcopy && p->notify_on_evict);
if (! kcopy)
pm_entries[i] &= ~PM_INCORE;
}
pm_entries[i] &= ~(PM_PAGINGOUT | PM_PAGEINWAIT | PM_WRITEWAIT);
}
for (ll = lock_list; ll; ll = ll->next)
if (!--ll->lr->pending_writes && !ll->lr->locks_pending)
wakeup = 1;
if (wakeup)
pthread_cond_broadcast (&p->wakeup);
notify:
_pager_allow_termination (p);
pthread_mutex_unlock (&p->interlock);
for (i = 0; i < npages; i++)
{
assert_backtrace (notified[i] == 0 || notified[i] == 1);
if (notified[i])
{
short *pm_entry = &pm_entries[i];
pager_notify_evict (p->upi, offset + (i * vm_page_size));
pthread_mutex_lock (&p->interlock);
*pm_entry = SET_PM_ERROR (SET_PM_NEXTERROR (*pm_entry, 0), 0);
pthread_mutex_unlock (&p->interlock);
}
}
return 0;
release_out:
pthread_mutex_unlock (&p->interlock);
return 0;
}
kern_return_t
_pager_S_memory_object_data_return (struct pager *p,
mach_port_t control,
vm_offset_t offset,
pointer_t data,
mach_msg_type_number_t length,
int dirty,
int kcopy)
{
return _pager_do_write_request (p, control, offset, data,
length, dirty, kcopy, 0);
}