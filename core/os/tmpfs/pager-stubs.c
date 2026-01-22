#include <hurd/pager.h>
#include <stdlib.h>
error_t
pager_read_page (struct user_pager_info *pager,
vm_offset_t page,
vm_address_t *buf,
int *write_lock)
{
abort();
return EIEIO;
}
error_t
pager_write_page (struct user_pager_info *pager,
vm_offset_t page,
vm_address_t buf)
{
abort();
return EIEIO;
}
error_t
pager_unlock_page (struct user_pager_info *pager,
vm_offset_t address)
{
abort();
return EIEIO;
}
void
pager_notify_evict (struct user_pager_info *pager,
vm_offset_t page)
{
abort();
}
error_t
pager_report_extent (struct user_pager_info *pager,
vm_address_t *offset,
vm_size_t *size)
{
abort();
return EIEIO;
}
void
pager_clear_user_data (struct user_pager_info *pager)
{
abort();
}
void
pager_dropweak (struct user_pager_info *p)
{
abort();
}