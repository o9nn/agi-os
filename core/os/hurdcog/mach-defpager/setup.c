#include <errno.h>
#include <stddef.h>
#include <assert-backtrace.h>
#include <mach.h>
#include <string.h>
#include <strings.h>
#include <stdlib.h>
#include "default_pager.h"
#include "file_io.h"
#include "default_pager_S.h"
int page_aligned (vm_offset_t num)
{
return trunc_page (num) == num;
}
extern mach_port_t default_pager_default_port;
kern_return_t
S_default_pager_paging_storage_new (mach_port_t pager,
mach_port_t device,
const recnum_t *runs, mach_msg_type_number_t nrun,
const_default_pager_filename_t name,
boolean_t add)
{
struct file_direct *fdp;
int sizes[DEV_GET_RECORDS_COUNT];
mach_msg_type_number_t count;
mach_msg_type_number_t i;
error_t err;
recnum_t devsize;
if (pager != default_pager_default_port)
return KERN_INVALID_ARGUMENT;
if (! add)
return remove_paging_file (name);
if (nrun < 2 || nrun % 2 != 0)
return EINVAL;
count = DEV_GET_RECORDS_COUNT;
err = device_get_status (device, DEV_GET_RECORDS, sizes, &count);
if (err)
return err;
if (count < DEV_GET_RECORDS_COUNT || sizes[DEV_GET_RECORDS_RECORD_SIZE] <= 0)
return EINVAL;
devsize = sizes[DEV_GET_RECORDS_DEVICE_RECORDS];
if (vm_page_size % sizes[DEV_GET_RECORDS_RECORD_SIZE] != 0)
return EINVAL;
fdp = malloc (offsetof (struct file_direct, runs[nrun]));
if (fdp == 0)
return ENOMEM;
fdp->device = device;
fdp->bshift = ffs (sizes[DEV_GET_RECORDS_RECORD_SIZE]) - 1;
fdp->fd_bsize = sizes[DEV_GET_RECORDS_RECORD_SIZE];
fdp->nruns = nrun / 2;
fdp->fd_size = 0;
for (i = 0; i < nrun; i += 2)
{
fdp->runs[i].start = runs[i];
fdp->runs[i].length = runs[i + 1];
if (fdp->runs[i].start + fdp->runs[i].length > devsize)
{
free (fdp);
return EINVAL;
}
fdp->fd_size += fdp->runs[i].length;
}
create_paging_partition (name, fdp, 0, -3);
return 0;
}
#ifdef __i386__
kern_return_t
S_default_pager_paging_storage (mach_port_t pager,
mach_port_t device,
const recnum_t *runs, mach_msg_type_number_t nrun,
const_default_pager_filename_t name,
boolean_t add)
{
return S_default_pager_paging_storage_new (pager, device, runs, nrun, name,
add);
}
#endif
int
page_read_file_direct (struct file_direct *fdp,
vm_offset_t offset,
vm_size_t size,
vm_offset_t *addr,
mach_msg_type_number_t *size_read)
{
struct storage_run *r;
error_t err;
char *readloc;
char *page;
mach_msg_type_number_t nread;
assert_backtrace (page_aligned (offset));
assert_backtrace (size == vm_page_size);
offset >>= fdp->bshift;
assert_backtrace (offset + (size >> fdp->bshift) <= fdp->fd_size);
for (r = fdp->runs; offset > r->length; ++r)
offset -= r->length;
if (offset + (size >> fdp->bshift) <= r->length)
return device_read (fdp->device, 0, r->start + offset,
size, (char **) addr, size_read);
err = device_read (fdp->device, 0, r->start + offset,
(r->length - offset) << fdp->bshift,
(char **) addr, &nread);
if (err)
return err;
size -= nread;
readloc = (char *) *addr;
do
{
readloc += nread;
offset += nread >> fdp->bshift;
if (offset > r->length)
offset -= r++->length;
err = device_read (fdp->device, 0, r->start + offset,
(r->length - offset) << fdp->bshift,
&page, &nread);
if (err)
{
vm_deallocate (mach_task_self (),
(vm_address_t) *addr, vm_page_size);
return err;
}
memcpy (readloc, page, nread);
vm_deallocate (mach_task_self (), (vm_address_t) page, vm_page_size);
size -= nread;
} while (size > 0);
*size_read = vm_page_size;
return 0;
}
int
page_write_file_direct(struct file_direct *fdp,
vm_offset_t offset,
vm_offset_t addr,
vm_size_t size,
mach_msg_type_number_t *size_written)
{
struct storage_run *r;
error_t err;
int wrote;
assert_backtrace (page_aligned (offset));
assert_backtrace (size == vm_page_size);
offset >>= fdp->bshift;
assert_backtrace (offset + (size >> fdp->bshift) <= fdp->fd_size);
for (r = fdp->runs; offset > r->length; ++r)
offset -= r->length;
if (offset + (size >> fdp->bshift) <= r->length)
{
err = device_write (fdp->device, 0, r->start + offset,
(char *) addr, size, &wrote);
*size_written = wrote;
return err;
}
err = device_write (fdp->device, 0,
r->start + offset, (char *) addr,
(r->length - offset) << fdp->bshift,
&wrote);
if (err)
return err;
size -= wrote;
do
{
mach_msg_type_number_t segsize;
addr += wrote;
offset += wrote >> fdp->bshift;
if (offset > r->length)
offset -= r++->length;
segsize = (r->length - offset) << fdp->bshift;
if (segsize > size)
segsize = size;
err = device_write (fdp->device, 0, r->start + offset,
(char *) addr, segsize, &wrote);
if (err)
{
vm_deallocate (mach_task_self (),
(vm_address_t) addr, vm_page_size);
return err;
}
size -= wrote;
} while (size > 0);
*size_written = vm_page_size;
return 0;
}
kern_return_t
remove_paging_file (const char *file_name)
{
struct file_direct *fdp = 0;
kern_return_t kr;
kr = destroy_paging_partition(file_name, (void **)&fdp);
if (kr == KERN_SUCCESS && fdp != 0)
{
mach_port_deallocate (mach_task_self (), fdp->device);
free (fdp);
}
return kr;
}