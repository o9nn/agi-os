#include "priv.h"
void
pager_offer_page (struct pager *p,
int precious,
int writelock,
vm_offset_t offset,
vm_address_t buf)
{
pthread_mutex_lock (&p->interlock);
if (_pager_pagemap_resize (p, offset + vm_page_size))
goto release_out;
short *pm_entry = &p->pagemap[offset / vm_page_size];
*pm_entry |= PM_INCORE;
memory_object_data_supply (p->memobjcntl, offset, buf, vm_page_size, 0,
writelock ? VM_PROT_WRITE : VM_PROT_NONE,
precious, MACH_PORT_NULL);
release_out:
pthread_mutex_unlock (&p->interlock);
}