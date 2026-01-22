#include "priv.h"
void
pager_sync (struct pager *p, int wait)
{
vm_address_t offset;
vm_size_t len;
pager_report_extent (p->upi, &offset, &len);
_pager_lock_object (p, offset, len, MEMORY_OBJECT_RETURN_ALL, 0,
VM_PROT_NO_CHANGE, wait);
}
void
pager_sync_some (struct pager *p, vm_address_t offset,
vm_size_t size, int wait)
{
_pager_lock_object (p, offset, size, MEMORY_OBJECT_RETURN_ALL, 0,
VM_PROT_NO_CHANGE, wait);
}