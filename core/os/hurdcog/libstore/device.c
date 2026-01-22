#include <assert-backtrace.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <hurd.h>
#include "store.h"
static inline error_t
dev_error (error_t err)
{
switch (err)
{
case D_IO_ERROR: return EIO;
case D_WOULD_BLOCK: return EAGAIN;
case D_NO_SUCH_DEVICE: return ENXIO;
case D_ALREADY_OPEN: return EBUSY;
case D_DEVICE_DOWN: return ENXIO;
case D_INVALID_OPERATION: return EBADF;
case D_NO_MEMORY: return ENOMEM;
default:
break;
}
return err;
}
static error_t
dev_read (struct store *store, store_offset_t addr,
size_t index, size_t amount,
void **buf, size_t *len)
{
error_t err;
recnum_t recnum = addr;
mach_msg_type_number_t nread;
if (recnum != addr)
return EOVERFLOW;
err = device_read (store->port, 0, recnum, amount,
(io_buf_ptr_t *) buf, &nread);
if (err)
return dev_error (err);
*len = nread;
return 0;
}
static error_t
dev_write (struct store *store, store_offset_t addr,
size_t index, const void *buf,
size_t len, size_t *amount)
{
recnum_t recnum = addr;
error_t err;
int amount_r;
if (recnum != addr)
return EOVERFLOW;
err = dev_error (device_write (store->port, 0, addr,
(io_buf_ptr_t)buf, len,
&amount_r));
*amount = amount_r;
return err;
}
static error_t
dev_set_size (struct store *store, size_t newsize)
{
return EOPNOTSUPP;
}
static error_t
dev_decode (struct store_enc *enc, const struct store_class *const *classes,
struct store **store)
{
return store_std_leaf_decode (enc, _store_device_create, store);
}
static error_t
dev_open (const char *name, int flags,
const struct store_class *const *classes,
struct store **store)
{
return dev_error (store_device_open (name, flags, store));
}
static error_t
dopen (const char *name, device_t *device, int *mod_flags)
{
device_t dev_master;
error_t err = ENODEV;
char *pos;
char *master;
char *rest;
if ( (name[0] == '@') && (pos = strchr (name, ':')) )
{
master = strndup (name+1, pos-(name+1));
rest = pos+1;
if (*mod_flags & STORE_HARD_READONLY)
{
dev_master = file_name_lookup (master, O_READ, 0);
if (dev_master != MACH_PORT_NULL)
{
err = device_open (dev_master, D_READ, rest, device);
if (err)
err = ENODEV;
mach_port_deallocate (mach_task_self (), dev_master);
}
else
err = ENODEV;
}
else
{
dev_master = file_name_lookup (master, O_READ | O_WRITE, 0);
if (dev_master != MACH_PORT_NULL)
{
err = device_open (dev_master, D_READ | D_WRITE, rest, device);
if (err == ED_READ_ONLY)
{
err = device_open (dev_master, D_READ, rest, device);
if (! err)
*mod_flags |= STORE_HARD_READONLY;
else
err = ENODEV;
}
else if (! err)
*mod_flags &= ~STORE_HARD_READONLY;
mach_port_deallocate (mach_task_self (), dev_master);
}
else
err = ENODEV;
}
free (master);
}
if (err)
{
err = get_privileged_ports (0, &dev_master);
if (! err)
{
if (*mod_flags & STORE_HARD_READONLY)
err = device_open (dev_master, D_READ, (char *)name, device);
else
{
err = device_open (dev_master, D_WRITE | D_READ, (char *)name, device);
if (err == ED_READ_ONLY)
{
err = device_open (dev_master, D_READ, (char *)name, device);
if (! err)
*mod_flags |= STORE_HARD_READONLY;
}
else if (! err)
*mod_flags &= ~STORE_HARD_READONLY;
}
mach_port_deallocate (mach_task_self (), dev_master);
}
}
return err;
}
static void
dclose (struct store *store)
{
mach_port_deallocate (mach_task_self (), store->port);
store->port = MACH_PORT_NULL;
}
static error_t
enforced (struct store *store)
{
error_t err;
dev_status_data_t sizes;
mach_msg_type_number_t sizes_len = DEV_STATUS_MAX;
if (store->num_runs != 1 || store->runs[0].start != 0)
return EINVAL;
else
{
#ifdef DEV_GET_RECORDS
err =
device_get_status (store->port, DEV_GET_RECORDS, sizes, &sizes_len);
if (err && err != D_INVALID_OPERATION)
return EINVAL;
if (!err)
{
assert_backtrace (sizes_len == DEV_GET_RECORDS_COUNT);
if (sizes[DEV_GET_RECORDS_RECORD_SIZE] != store->block_size
|| (store->runs[0].length !=
sizes[DEV_GET_RECORDS_DEVICE_RECORDS]))
return EINVAL;
return 0;
}
else
#endif
{
sizes_len = DEV_STATUS_MAX;
err =
device_get_status (store->port, DEV_GET_SIZE, sizes, &sizes_len);
if (err)
return EINVAL;
assert_backtrace (sizes_len == DEV_GET_SIZE_COUNT);
if (sizes[DEV_GET_SIZE_RECORD_SIZE] != store->block_size
|| (store->runs[0].length !=
sizes[DEV_GET_SIZE_DEVICE_SIZE] >> store->log2_block_size))
return EINVAL;
return 0;
}
}
}
static error_t
dev_set_flags (struct store *store, int flags)
{
if ((flags & ~(STORE_INACTIVE | STORE_ENFORCED)) != 0)
return EINVAL;
if (! ((store->flags | flags) & STORE_INACTIVE))
{
error_t err = enforced (store);
if (err)
return err;
}
if (flags & STORE_INACTIVE)
dclose (store);
store->flags |= flags;
return 0;
}
static error_t
dev_clear_flags (struct store *store, int flags)
{
error_t err = 0;
if ((flags & ~(STORE_INACTIVE | STORE_ENFORCED)) != 0)
err = EINVAL;
if (!err && (flags & STORE_INACTIVE))
err = store->name ? dopen (store->name, &store->port, &store->flags) : ENODEV;
if (! err)
store->flags &= ~flags;
return err;
}
static error_t
dev_map (const struct store *store, vm_prot_t prot, mach_port_t *memobj)
{
size_t nruns = store->num_runs;
if (nruns > 1 || (nruns == 1 && store->runs[0].start != 0))
return EOPNOTSUPP;
else
{
error_t err = device_map (store->port, prot,
store->runs[0].start,
store->runs[0].length,
memobj, 0);
if (err == ED_INVALID_OPERATION)
err = EOPNOTSUPP;
return err;
}
}
const struct store_class
store_device_class =
{
STORAGE_DEVICE, "device", dev_read, dev_write, dev_set_size,
store_std_leaf_allocate_encoding, store_std_leaf_encode, dev_decode,
dev_set_flags, dev_clear_flags, 0, 0, 0, dev_open, 0, dev_map
};
STORE_STD_CLASS (device);
error_t
store_device_create (device_t device, int flags, struct store **store)
{
struct store_run run;
size_t block_size = 0;
dev_status_data_t sizes;
mach_msg_type_number_t sizes_len = DEV_STATUS_MAX;
error_t err;
#ifdef DEV_GET_RECORDS
err = device_get_status (device, DEV_GET_RECORDS, sizes, &sizes_len);
if (! err && sizes_len == DEV_GET_RECORDS_COUNT)
{
block_size = sizes[DEV_GET_RECORDS_RECORD_SIZE];
if (block_size)
{
run.start = 0;
run.length = sizes[DEV_GET_RECORDS_DEVICE_RECORDS];
}
}
else
#endif
{
sizes_len = DEV_STATUS_MAX;
err = device_get_status (device, DEV_GET_SIZE, sizes, &sizes_len);
if (! err && sizes_len == DEV_GET_SIZE_COUNT)
{
block_size = sizes[DEV_GET_SIZE_RECORD_SIZE];
if (block_size)
{
run.start = 0;
run.length = sizes[DEV_GET_SIZE_DEVICE_SIZE] / block_size;
if (run.length * block_size != sizes[DEV_GET_SIZE_DEVICE_SIZE])
block_size = 0;
}
}
}
flags |= STORE_ENFORCED;
if (block_size == 0)
return _store_device_create (device, flags, 0, &run, 0, store);
else
return _store_device_create (device, flags, block_size, &run, 1, store);
}
error_t
_store_device_create (device_t device, int flags, size_t block_size,
const struct store_run *runs, size_t num_runs,
struct store **store)
{
return
_store_create (&store_device_class, device, flags, block_size,
runs, num_runs, 0, store);
}
error_t
store_device_open (const char *name, int flags, struct store **store)
{
device_t device;
error_t err = dopen (name, &device, &flags);
if (! err)
{
err = store_device_create (device, flags, store);
if (! err)
{
err = store_set_name (*store, name);
if (err)
store_free (*store);
}
if (err)
mach_port_deallocate (mach_task_self (), device);
}
return err;
}