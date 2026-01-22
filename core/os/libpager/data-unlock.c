#include "priv.h"
#include "memory_object_S.h"
#include <stdio.h>
kern_return_t
_pager_S_memory_object_data_unlock (struct pager *p,
mach_port_t control,
vm_offset_t offset,
vm_size_t length,
vm_prot_t access)
{
volatile int err;
if (!p
|| p->port.class != _pager_class)
return EOPNOTSUPP;
if (p->pager_state != NORMAL)
{
printf ("pager in wrong state for unlock\n");
goto out;
}
if (control != p->memobjcntl)
{
printf ("incg data unlock: wrong control port\n");
goto out;
}
if ((access & VM_PROT_WRITE) == 0)
{
printf ("incg data unlock: not unlock writes\n");
goto out;
}
if (offset % __vm_page_size)
{
printf ("incg data unlock: misaligned request\n");
goto out;
}
if (length != __vm_page_size)
{
printf ("incg data unlock: bad length size %lu\n", (unsigned long)length);
goto out;
}
err = pager_unlock_page (p->upi, offset);
if (!err)
_pager_lock_object (p, offset, length, MEMORY_OBJECT_RETURN_NONE, 0,
VM_PROT_NONE, 0);
else
{
_pager_lock_object (p, offset, length, MEMORY_OBJECT_RETURN_NONE, 1,
VM_PROT_WRITE, 0);
_pager_mark_next_request_error (p, offset, length, err);
}
out:
return 0;
}