#include <stdio.h>
#include <string.h>
#include <error.h>
#include <assert.h>
#include <pthread.h>
#include <hurd.h>
#include <mach.h>
#include <device/device.h>
#include "device_S.h"
#include "libports/notify_S.h"
#include "machdev-dev_hdr.h"
#include "machdev.h"
#include "mach_device.h"
struct port_bucket *machdev_device_bucket;
struct port_class *machdev_device_class;
#define MAX_NUM_EMULATION 32
static struct machdev_device_emulation_ops *emulation_list[MAX_NUM_EMULATION];
static int num_emul = 0;
io_return_t
ds_device_open (mach_port_t open_port, mach_port_t reply_port,
mach_msg_type_name_t reply_port_type, dev_mode_t mode,
const_dev_name_t name, device_t *devp,
mach_msg_type_name_t *devicePoly)
{
int i;
mach_port_t dev_master;
io_return_t err = D_NO_SUCH_DEVICE;
if (!machdev_is_master_device (open_port))
return D_INVALID_OPERATION;
if (! MACH_PORT_VALID (reply_port))
return MIG_NO_REPLY;
for (i = 0; i < num_emul; i++)
{
err = (*emulation_list[i]->open) (reply_port, reply_port_type,
mode, name, devp, devicePoly);
if (err != D_NO_SUCH_DEVICE)
break;
}
if (err)
{
err = get_privileged_ports(NULL, &dev_master);
if (!err)
{
err = device_open (dev_master, mode, name, devp);
mach_port_deallocate (mach_task_self (), dev_master);
}
if (!err)
*devicePoly = MACH_MSG_TYPE_MOVE_SEND;
}
return err;
}
io_return_t
ds_device_open_new (mach_port_t open_port, mach_port_t reply_port,
mach_msg_type_name_t reply_port_type, dev_mode_t mode,
const_dev_name_t name, device_t *devp,
mach_msg_type_name_t *devicePoly)
{
return ds_device_open (open_port, reply_port, reply_port_type, mode,
name, devp, devicePoly);
}
io_return_t
ds_device_close (struct mach_device *device)
{
if (device == MACH_DEVICE_NULL)
return D_NO_SUCH_DEVICE;
return (device->dev.emul_ops->close
? (*device->dev.emul_ops->close) (device->dev.emul_data)
: D_SUCCESS);
}
io_return_t
ds_device_write (struct mach_device *device, mach_port_t reply_port,
mach_msg_type_name_t reply_port_type, dev_mode_t mode,
recnum_t recnum, io_buf_ptr_t data, unsigned int count,
int *bytes_written)
{
if (device == MACH_DEVICE_NULL)
return D_NO_SUCH_DEVICE;
if (data == 0)
return D_INVALID_SIZE;
if (! device->dev.emul_ops->write)
return D_INVALID_OPERATION;
return (*device->dev.emul_ops->write) (device->dev.emul_data, reply_port,
reply_port_type, mode, recnum,
data, count, bytes_written);
}
io_return_t
ds_device_write_inband (struct mach_device *device, mach_port_t reply_port,
mach_msg_type_name_t reply_port_type,
dev_mode_t mode, recnum_t recnum,
const io_buf_ptr_inband_t data, unsigned count,
int *bytes_written)
{
if (device == MACH_DEVICE_NULL)
return D_NO_SUCH_DEVICE;
if (data == 0)
return D_INVALID_SIZE;
if (! device->dev.emul_ops->write_inband)
return D_INVALID_OPERATION;
return (*device->dev.emul_ops->write_inband) (device->dev.emul_data,
reply_port, reply_port_type,
mode, recnum,
data, count, bytes_written);
}
io_return_t
ds_device_read (struct mach_device *device, mach_port_t reply_port,
mach_msg_type_name_t reply_port_type, dev_mode_t mode,
recnum_t recnum, int count, io_buf_ptr_t *data,
unsigned *bytes_read)
{
if (device == MACH_DEVICE_NULL)
return D_NO_SUCH_DEVICE;
if (! device->dev.emul_ops->read)
return D_INVALID_OPERATION;
return (*device->dev.emul_ops->read) (device->dev.emul_data, reply_port,
reply_port_type, mode, recnum,
count, data, bytes_read);
}
io_return_t
ds_device_read_inband (struct mach_device *device, mach_port_t reply_port,
mach_msg_type_name_t reply_port_type, dev_mode_t mode,
recnum_t recnum, int count, io_buf_ptr_inband_t data,
unsigned *bytes_read)
{
if (device == MACH_DEVICE_NULL)
return D_NO_SUCH_DEVICE;
if (! device->dev.emul_ops->read_inband)
return D_INVALID_OPERATION;
return (*device->dev.emul_ops->read_inband) (device->dev.emul_data,
reply_port,
reply_port_type, mode, recnum,
count, data, bytes_read);
}
io_return_t
ds_device_set_status (struct mach_device *device, dev_flavor_t flavor,
dev_status_t status, mach_msg_type_number_t status_count)
{
if (device == MACH_DEVICE_NULL)
return D_NO_SUCH_DEVICE;
if (! device->dev.emul_ops->set_status)
return D_INVALID_OPERATION;
return (*device->dev.emul_ops->set_status) (device->dev.emul_data, flavor,
status, status_count);
}
io_return_t
ds_device_get_status (struct mach_device *device, dev_flavor_t flavor,
dev_status_t status,
mach_msg_type_number_t *status_count)
{
if (device == MACH_DEVICE_NULL)
return D_NO_SUCH_DEVICE;
if (! device->dev.emul_ops->get_status)
return D_INVALID_OPERATION;
return (*device->dev.emul_ops->get_status) (device->dev.emul_data, flavor,
status, status_count);
}
io_return_t
ds_device_set_filter (struct mach_device *device, mach_port_t receive_port,
int priority, filter_t *filter, unsigned filter_count)
{
if (device == MACH_DEVICE_NULL)
return D_NO_SUCH_DEVICE;
if (! device->dev.emul_ops->set_filter)
return D_INVALID_OPERATION;
return (*device->dev.emul_ops->set_filter) (device->dev.emul_data,
receive_port,
priority, filter, filter_count);
}
io_return_t
ds_device_map (struct mach_device *device, vm_prot_t prot, vm_offset_t offset,
vm_size_t size, mach_port_t *pager, boolean_t unmap)
{
if (device == MACH_DEVICE_NULL)
return D_NO_SUCH_DEVICE;
if (! device->dev.emul_ops->map)
return D_INVALID_OPERATION;
return (*device->dev.emul_ops->map) (device->dev.emul_data, prot,
offset, size, pager, unmap);
}
kern_return_t
ds_device_intr_register (mach_device_t dev, int id, int flags,
mach_port_t receive_port)
{
return D_INVALID_OPERATION;
}
kern_return_t
ds_device_intr_ack (mach_device_t dev, mach_port_t receive_port)
{
return D_INVALID_OPERATION;
}
error_t
machdev_create_device_port (size_t size, void *result)
{
return ports_create_port (machdev_device_class, machdev_device_bucket,
size, result);
}
void
machdev_device_init(void)
{
int i;
machdev_device_bucket = ports_create_bucket ();
machdev_device_class = ports_create_class (0, 0);
for (i = 0; i < num_emul; i++)
{
if (emulation_list[i]->init)
emulation_list[i]->init();
}
}
void
machdev_device_sync(void)
{
int i;
for (i = 0; i < num_emul; i++)
{
if (emulation_list[i]->sync)
emulation_list[i]->sync();
}
}
int
machdev_demuxer (mach_msg_header_t *inp, mach_msg_header_t *outp)
{
mig_routine_t routine;
if ((routine = device_server_routine (inp)) ||
(routine = ports_notify_server_routine (inp)))
{
(*routine) (inp, outp);
return TRUE;
}
else
return FALSE;
}
void
machdev_register (struct machdev_device_emulation_ops *ops)
{
assert(num_emul < MAX_NUM_EMULATION-1);
emulation_list[num_emul++] = ops;
}
void *
machdev_server(void *arg)
{
pthread_setname_np (pthread_self (), "machdev_server");
do
{
ports_manage_port_operations_one_thread (machdev_device_bucket, machdev_demuxer, 0);
} while (1);
return NULL;
}