#include <hurd.h>
#include <hurd/pager.h>
#include <assert-backtrace.h>
#include <strings.h>
#include <unistd.h>
#include <errno.h>
#include <error.h>
#include <sys/mman.h>
#include <stdio.h>
#include "dev.h"
error_t
pager_read_page (struct user_pager_info *upi,
vm_offset_t page, vm_address_t *buf, int *writelock)
{
error_t err;
size_t read = 0;
int want = vm_page_size;
struct dev *dev = (struct dev *)upi;
struct store *store = dev->store;
if (page + want > store->size)
want = store->size - page;
err = dev_read (dev, page, want, (void **)buf, &read);
if (!err && want < vm_page_size)
memset ((char *)*buf + want, '\0', vm_page_size - want);
*writelock = (store->flags & STORE_READONLY);
if (err || read < want)
return EIO;
else
return 0;
}
error_t
pager_write_page (struct user_pager_info *upi,
vm_offset_t page, vm_address_t buf)
{
struct dev *dev = (struct dev *)upi;
struct store *store = dev->store;
if (store->flags & STORE_READONLY)
return EROFS;
else
{
error_t err;
size_t written;
int want = vm_page_size;
if (page + want > store->size)
want = store->size - page;
err = dev_write (dev, page, (char *)buf, want, &written);
if (err || written < want)
return EIO;
else
return 0;
}
}
error_t
pager_unlock_page (struct user_pager_info *upi, vm_offset_t address)
{
struct dev *dev = (struct dev *)upi;
if (dev->store->flags & STORE_READONLY)
return EROFS;
else
return 0;
}
void
pager_notify_evict (struct user_pager_info *pager,
vm_offset_t page)
{
assert_backtrace (!"unrequested notification on eviction");
}
error_t
pager_report_extent (struct user_pager_info *upi,
vm_address_t *offset, vm_size_t *size)
{
*offset = 0;
*size = ((struct dev *)upi)->store->size;
return 0;
}
void
pager_clear_user_data (struct user_pager_info *upi)
{
struct dev *dev = (struct dev *)upi;
pthread_mutex_lock (&dev->pager_lock);
dev->pager = 0;
pthread_mutex_unlock (&dev->pager_lock);
}
static struct port_bucket *pager_port_bucket = 0;
static struct pager_requests *pager_requests;
static void
init_dev_paging (void)
{
if (! pager_port_bucket)
{
static pthread_mutex_t pager_global_lock = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_lock (&pager_global_lock);
if (pager_port_bucket == NULL)
{
error_t err;
pager_port_bucket = ports_create_bucket ();
err = pager_start_workers (pager_port_bucket, &pager_requests);
if (err)
{
errno = err;
error (0, err, "pager_start_workers");
}
}
pthread_mutex_unlock (&pager_global_lock);
}
}
void
pager_dropweak (struct user_pager_info *upi __attribute__ ((unused)))
{
}
int
dev_stop_paging (struct dev *dev, int nosync)
{
size_t num_pagers = (pager_port_bucket ?
ports_count_bucket (pager_port_bucket) : 0);
if (num_pagers > 0 && !nosync)
{
error_t block_cache (void *arg)
{
struct pager *p = arg;
pager_change_attributes (p, 0, MEMORY_OBJECT_COPY_DELAY, 1);
return 0;
}
error_t enable_cache (void *arg)
{
struct pager *p = arg;
pager_change_attributes (p, 1, MEMORY_OBJECT_COPY_DELAY, 0);
return 0;
}
ports_bucket_iterate (pager_port_bucket, block_cache);
sleep (1);
num_pagers = ports_count_bucket (pager_port_bucket);
if (num_pagers > 0)
ports_bucket_iterate (pager_port_bucket, enable_cache);
}
return num_pagers == 0;
}
error_t
dev_get_memory_object (struct dev *dev, vm_prot_t prot, memory_object_t *memobj)
{
error_t err = store_map (dev->store, prot, memobj);
if (err == EOPNOTSUPP && !dev->inhibit_cache)
{
int created = 0;
init_dev_paging ();
pthread_mutex_lock (&dev->pager_lock);
if (dev->pager == NULL)
{
dev->pager =
pager_create ((struct user_pager_info *)dev, pager_port_bucket,
1, MEMORY_OBJECT_COPY_DELAY, 0);
if (dev->pager == NULL)
{
pthread_mutex_unlock (&dev->pager_lock);
return errno;
}
created = 1;
}
*memobj = pager_get_port (dev->pager);
if (*memobj == MACH_PORT_NULL)
{
dev->pager = 0;
pthread_mutex_unlock (&dev->pager_lock);
return dev_get_memory_object (dev, prot, memobj);
}
else
err =
mach_port_insert_right (mach_task_self (),
*memobj, *memobj, MACH_MSG_TYPE_MAKE_SEND);
if (created)
ports_port_deref (dev->pager);
pthread_mutex_unlock (&dev->pager_lock);
}
return err;
}