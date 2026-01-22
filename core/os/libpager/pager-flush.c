#include "priv.h"
void
pager_flush (struct pager *p, int wait)
{
vm_address_t offset;
vm_size_t len;
pager_report_extent (p->upi, &offset, &len);
_pager_lock_object (p, offset, len, MEMORY_OBJECT_RETURN_NONE, 1,
VM_PROT_NO_CHANGE, wait);
}
void
pager_flush_some (struct pager *p, vm_address_t offset,
vm_size_t size, int wait)
{
_pager_lock_object (p, offset, size, MEMORY_OBJECT_RETURN_NONE, 1,
VM_PROT_NO_CHANGE, wait);
}