#include "priv.h"
#include "memory_object_S.h"
#include <stdio.h>
#include <string.h>
kern_return_t
_pager_S_memory_object_data_request (struct pager *p,
mach_port_t control,
vm_offset_t offset,
vm_size_t length,
vm_prot_t access)
{
short *pm_entry;
int doread, doerror;
error_t err;
vm_address_t page;
int write_lock;
if (!p
|| p->port.class != _pager_class)
return EOPNOTSUPP;
pthread_mutex_lock (&p->interlock);
if (control != p->memobjcntl)
{
printf ("incg data request: wrong control port\n");
goto release_out;
}
if (length != __vm_page_size)
{
printf ("incg data request: bad length size %lu\n", (unsigned long)length);
goto release_out;
}
if (offset % __vm_page_size)
{
printf ("incg data request: misaligned request\n");
goto release_out;
}
_pager_block_termination (p);
if (p->pager_state != NORMAL)
{
printf ("pager in wrong state for read\n");
goto allow_release_out;
}
err = _pager_pagemap_resize (p, offset + length);
if (err)
goto allow_release_out;
pm_entry = &p->pagemap[offset / __vm_page_size];
if (*pm_entry & PM_PAGINGOUT)
{
doread = 0;
*pm_entry |= PM_PAGEINWAIT;
}
else
doread = 1;
if (*pm_entry & PM_INVALID)
doerror = 1;
else
doerror = 0;
*pm_entry |= PM_INCORE;
if (PM_NEXTERROR (*pm_entry) != PAGE_NOERR && (access & VM_PROT_WRITE))
{
memory_object_data_error (control, offset, length,
_pager_page_errors[PM_NEXTERROR (*pm_entry)]);
_pager_mark_object_error (p, offset, length,
_pager_page_errors[PM_NEXTERROR (*pm_entry)]);
*pm_entry = SET_PM_NEXTERROR (*pm_entry, PAGE_NOERR);
doread = 0;
}
pthread_mutex_unlock (&p->interlock);
if (!doread)
goto allow_term_out;
if (doerror)
goto error_read;
err = pager_read_page (p->upi, offset, &page, &write_lock);
if (err)
goto error_read;
memory_object_data_supply (p->memobjcntl, offset, page, length, 1,
write_lock ? VM_PROT_WRITE : VM_PROT_NONE,
p->notify_on_evict ? 1 : 0,
MACH_PORT_NULL);
pthread_mutex_lock (&p->interlock);
_pager_mark_object_error (p, offset, length, 0);
_pager_allow_termination (p);
pthread_mutex_unlock (&p->interlock);
return 0;
error_read:
memory_object_data_error (p->memobjcntl, offset, length, EIO);
_pager_mark_object_error (p, offset, length, EIO);
allow_term_out:
pthread_mutex_lock (&p->interlock);
_pager_allow_termination (p);
pthread_mutex_unlock (&p->interlock);
return 0;
allow_release_out:
_pager_allow_termination (p);
release_out:
pthread_mutex_unlock (&p->interlock);
return 0;
}