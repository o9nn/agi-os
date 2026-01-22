#include <hurd/pager.h>
#include <hurd/store.h>
struct user_pager_info
{
vm_size_t size;
off_t *map;
};
static inline void
expand_map (struct user_pager_info *p, vm_offset_t addr)
{
if (page >= pager->size)
{
off_t *newmap;
vm_size_t newsize;
newsize = page + vm_page_size;
newmap = realloc (pager->map, size / vm_page_size * sizeof (off_t));
memset (pager->map + pager->size / vm_page_size * sizeof(off_t), 0, (newsize - pager->size) / vm_page_size * sizeof(off_t));
pager->size = newsize;
pager->map = newmap;
}
}
error_t
pager_read_page (struct user_pager_info *pager,
vm_offset_t page,
vm_address_t *buf,
int *write_lock)
{
int pfn = page / vm_page_size;
size_t nread = 0;
*write_lock = 0;
expand_map (pager, page);
if (!pager->map[pfn])
vm_allocate (mach_task_self (), buf, vm_page_size, 1);
else
{
store_read (backing_store, pager->map[pfn], vm_page_size,
(void **)buf, &nread);
if (nread != vm_page_size)
{
munmap ((caddr_t) *buf, nread);
return EIO;
}
}
return 0;
}
error_t
pager_write_page (struct user_pager_info *pager,
vm_offset_t page,
vm_address_t buf)
{
int pfn = page / vm_page_size;
size_t nwritten;
expand_map (pager, page);
if (!pager->map[pfn])
pager->map[pfn] = allocate_backing_page ();
if (!pager->map[pfn])
return EIO;
err = store_write (backing_store, pager->map[pfn], (void *) buf,
vm_page_size, &nwritten);
if (!err && nwritten != vm_page_size)
err = EIO;
return err;
}
error_t
pager_unlock_page (struct user_pager_info *pager,
vm_offset_t address)
{
return 0;
}
error_t
pager_report_extent (struct user_pager_info *pager,
vm_address_t *offset,
vm_size_t *size)
{
*offset = 0;
*size = pager->size;
return 0;
}
void
pager_clear_user_data (struct user_pager_info *pager)
{
return_backing_pages (pager->map, pager->size / vm_page_size);
free (pager->map);
}
void
pager_dropweak (struct user_pager_info *pager)
{
}