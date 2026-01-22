#include <hurd/trivfs.h>
#include <stdio.h>
#include <fcntl.h>
#include "open.h"
#include "dev.h"
#include "libtrivfs/trivfs_fs_S.h"
#include "libtrivfs/trivfs_io_S.h"
kern_return_t
trivfs_S_io_map (struct trivfs_protid *cred,
mach_port_t reply, mach_msg_type_name_t reply_type,
memory_object_t *rd_obj, mach_msg_type_name_t *rd_type,
memory_object_t *wr_obj, mach_msg_type_name_t *wr_type)
{
if (! cred)
return EOPNOTSUPP;
else if (! (cred->po->openmodes & (O_READ|O_WRITE)))
return EBADF;
else
{
mach_port_t memobj;
int flags = cred->po->openmodes;
vm_prot_t prot =
((flags & O_READ) ? VM_PROT_READ : 0)
| ((flags & O_WRITE) ? VM_PROT_WRITE : 0);
struct open *open = (struct open *)cred->po->hook;
error_t err = dev_get_memory_object (open->dev, prot, &memobj);
if (!err)
{
if (flags & O_READ)
*rd_obj = memobj;
else
*rd_obj = MACH_PORT_NULL;
if (flags & O_WRITE)
*wr_obj = memobj;
else
*wr_obj = MACH_PORT_NULL;
if ((flags & (O_READ|O_WRITE)) == (O_READ|O_WRITE)
&& memobj != MACH_PORT_NULL)
mach_port_mod_refs (mach_task_self (), memobj,
MACH_PORT_RIGHT_SEND, 1);
}
*rd_type = *wr_type = MACH_MSG_TYPE_MOVE_SEND;
return err;
}
}
kern_return_t
trivfs_S_io_read (struct trivfs_protid *cred,
mach_port_t reply, mach_msg_type_name_t reply_type,
data_t *data, mach_msg_type_name_t *data_len,
off_t offs, vm_size_t amount)
{
error_t err;
size_t data_size = *data_len;
if (! cred)
return EOPNOTSUPP;
else if (! (cred->po->openmodes & O_READ))
return EBADF;
err = open_read ((struct open *)cred->po->hook,
offs, amount, (void **)data, &data_size);
if (err)
return err;
*data_len = data_size;
return 0;
}
kern_return_t
trivfs_S_io_readable (struct trivfs_protid *cred,
mach_port_t reply, mach_msg_type_name_t reply_type,
vm_size_t *amount)
{
if (! cred)
return EOPNOTSUPP;
else if (! (cred->po->openmodes & O_READ))
return EBADF;
else
{
struct open *open = (struct open *)cred->po->hook;
*amount = open->dev->store->size - open->offs;
return 0;
}
}
kern_return_t
trivfs_S_io_write (struct trivfs_protid *cred,
mach_port_t reply, mach_msg_type_name_t reply_type,
const_data_t data, mach_msg_type_number_t data_len,
off_t offs, vm_size_t *amount)
{
if (! cred)
return EOPNOTSUPP;
else if (! (cred->po->openmodes & O_WRITE))
return EBADF;
else
return open_write ((struct open *)cred->po->hook,
offs, data, data_len, amount);
}
kern_return_t
trivfs_S_io_seek (struct trivfs_protid *cred,
mach_port_t reply, mach_msg_type_name_t reply_type,
off_t offs, int whence, off_t *new_offs)
{
if (! cred)
return EOPNOTSUPP;
else
return open_seek ((struct open *)cred->po->hook, offs, whence, new_offs);
}
kern_return_t
trivfs_S_io_select (struct trivfs_protid *cred,
mach_port_t reply, mach_msg_type_name_t reply_type,
int *type)
{
if (! cred)
return EOPNOTSUPP;
*type &= ~SELECT_URG;
return 0;
}
kern_return_t
trivfs_S_io_select_timeout (struct trivfs_protid *cred,
mach_port_t reply, mach_msg_type_name_t reply_type,
struct timespec ts,
int *type)
{
return trivfs_S_io_select (cred, reply, reply_type, type);
}
kern_return_t
trivfs_S_file_set_size (struct trivfs_protid *cred,
mach_port_t reply, mach_msg_type_name_t reply_type,
off_t size)
{
if (! cred)
return EOPNOTSUPP;
else if (size < 0)
return EINVAL;
else
return 0;
}
kern_return_t
trivfs_S_io_get_openmodes (struct trivfs_protid *cred,
mach_port_t reply, mach_msg_type_name_t reply_type,
int *bits)
{
if (! cred)
return EOPNOTSUPP;
else
{
*bits = cred->po->openmodes;
return 0;
}
}
kern_return_t
trivfs_S_io_set_all_openmodes (struct trivfs_protid *cred,
mach_port_t reply,
mach_msg_type_name_t reply_type,
int mode)
{
if (! cred)
return EOPNOTSUPP;
else
return 0;
}
kern_return_t
trivfs_S_io_set_some_openmodes (struct trivfs_protid *cred,
mach_port_t reply,
mach_msg_type_name_t reply_type,
int bits)
{
if (! cred)
return EOPNOTSUPP;
else
return 0;
}
kern_return_t
trivfs_S_io_clear_some_openmodes (struct trivfs_protid *cred,
mach_port_t reply,
mach_msg_type_name_t reply_type,
int bits)
{
if (! cred)
return EOPNOTSUPP;
else
return 0;
}
kern_return_t
trivfs_S_io_get_owner (struct trivfs_protid *cred,
mach_port_t reply,
mach_msg_type_name_t reply_type,
pid_t *owner)
{
if (! cred)
return EOPNOTSUPP;
else
{
struct open *open = (struct open *)cred->po->hook;
*owner = open->dev->owner;
return 0;
}
}
kern_return_t
trivfs_S_io_mod_owner (struct trivfs_protid *cred,
mach_port_t reply, mach_msg_type_name_t reply_type,
pid_t owner)
{
if (! cred)
return EOPNOTSUPP;
else
{
struct open *open = (struct open *)cred->po->hook;
open->dev->owner = owner;
return 0;
}
}
kern_return_t
trivfs_S_file_sync (struct trivfs_protid *cred,
mach_port_t reply, mach_msg_type_name_t reply_type,
int wait, int omit_metadata)
{
if (cred)
return dev_sync (((struct open *)cred->po->hook)->dev, wait);
else
return EOPNOTSUPP;
}
kern_return_t
trivfs_S_file_syncfs (struct trivfs_protid *cred,
mach_port_t reply, mach_msg_type_name_t reply_type,
int wait, int dochildren)
{
if (cred)
return dev_sync (((struct open *)cred->po->hook)->dev, wait);
else
return EOPNOTSUPP;
}
kern_return_t
trivfs_S_file_get_storage_info (struct trivfs_protid *cred,
mach_port_t reply,
mach_msg_type_name_t reply_type,
mach_port_t **ports,
mach_msg_type_name_t *ports_type,
mach_msg_type_number_t *num_ports,
int **ints, mach_msg_type_number_t *num_ints,
off_t **offsets,
mach_msg_type_number_t *num_offsets,
data_t *data, mach_msg_type_number_t *data_len)
{
*ports_type = MACH_MSG_TYPE_COPY_SEND;
if (! cred || ! cred->po->hook)
return EOPNOTSUPP;
else
{
error_t err;
struct dev *dev = ((struct open *)cred->po->hook)->dev;
struct store *store = dev->store;
if (dev->enforced && !(store->flags & STORE_ENFORCED))
{
size_t name_len = (store->name ? strlen (store->name) + 1 : 0);
int i;
*num_ports = 0;
i = 0;
(*ints)[i++] = STORAGE_OTHER;
(*ints)[i++] = store->flags;
(*ints)[i++] = store->block_size;
(*ints)[i++] = 1;
(*ints)[i++] = name_len;
(*ints)[i++] = 0;
*num_ints = i;
i = 0;
(*offsets)[i++] = 0;
(*offsets)[i++] = store->size;
*num_offsets = i;
if (store->name)
memcpy (*data, store->name, name_len);
*data_len = name_len;
return 0;
}
if (!cred->isroot
&& !store_is_securely_returnable (store, cred->po->openmodes))
{
struct store *clone;
err = store_clone (store, &clone);
if (! err)
{
err = store_set_flags (clone, STORE_INACTIVE);
if (err == EINVAL)
err = EACCES;
else
err = store_return (clone,
ports, num_ports, ints, num_ints,
offsets, num_offsets, data, data_len);
store_free (clone);
}
}
else
err = store_return (store,
ports, num_ports, ints, num_ints,
offsets, num_offsets, data, data_len);
return err;
}
}