#ifndef	DS_ROUTINES_H
#define	DS_ROUTINES_H
#include <vm/vm_map.h>
#include <device/device_types.h>
#include <device/io_req.h>
#include <mach/machine/vm_types.h>
extern vm_map_t		device_io_map;
extern queue_head_t	io_done_list;
kern_return_t	device_read_alloc(io_req_t, vm_size_t);
kern_return_t	device_write_get(io_req_t, boolean_t *);
boolean_t	device_write_dealloc(io_req_t);
void		device_reference(device_t);
boolean_t	ds_notify(mach_msg_header_t *msg);
boolean_t	ds_open_done(io_req_t);
boolean_t	ds_read_done(io_req_t);
boolean_t	ds_write_done(io_req_t);
void		iowait (io_req_t ior);
kern_return_t	device_pager_setup(
const mach_device_t	device,
int			prot,
vm_offset_t		offset,
vm_size_t		size,
mach_port_t		*pager);
extern void mach_device_init(void);
extern void dev_lookup_init(void);
extern void device_pager_init(void);
extern void io_done_thread(void) __attribute__ ((noreturn));
io_return_t ds_device_write_trap(
device_t 	dev,
dev_mode_t 	mode,
rpc_recnum_t 	recnum,
rpc_vm_offset_t 	data,
rpc_vm_size_t 	count);
io_return_t ds_device_writev_trap(
device_t 	dev,
dev_mode_t 	mode,
rpc_recnum_t 	recnum,
rpc_io_buf_vec_t 	*iovec,
rpc_vm_size_t 	count);
#endif