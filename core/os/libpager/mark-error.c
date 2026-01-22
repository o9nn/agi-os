#include "priv.h"
int _pager_page_errors[] = {KERN_SUCCESS, ENOSPC, EIO, EDQUOT};
void
_pager_mark_next_request_error(struct pager *pager,
vm_address_t offset,
vm_size_t length,
error_t error)
{
int page_error;
short *p;
offset /= __vm_page_size;
length /= __vm_page_size;
switch (error)
{
case 0:
page_error = PAGE_NOERR;
break;
case ENOSPC:
page_error = PAGE_ENOSPC;
break;
case EIO:
default:
page_error = PAGE_EIO;
break;
case EDQUOT:
page_error = PAGE_EDQUOT;
break;
}
for (p = pager->pagemap + offset; p < pager->pagemap + offset + length; p++)
*p = SET_PM_NEXTERROR (*p, page_error);
}
void
_pager_mark_object_error(struct pager *pager,
vm_address_t offset,
vm_size_t length,
error_t error)
{
int page_error = 0;
short *p;
offset /= __vm_page_size;
length /= __vm_page_size;
switch (error)
{
case 0:
page_error = PAGE_NOERR;
break;
case ENOSPC:
page_error = PAGE_ENOSPC;
break;
case EIO:
default:
page_error = PAGE_EIO;
break;
case EDQUOT:
page_error = PAGE_EDQUOT;
break;
}
for (p = pager->pagemap + offset; p < pager->pagemap + offset + length; p++)
*p = SET_PM_ERROR (*p, page_error);
}
error_t
pager_get_error (struct pager *p, vm_address_t addr)
{
error_t err;
pthread_mutex_lock (&p->interlock);
addr /= vm_page_size;
err = _pager_pagemap_resize (p, addr);
if (! err)
err = _pager_page_errors[PM_ERROR(p->pagemap[addr])];
pthread_mutex_unlock (&p->interlock);
return err;
}