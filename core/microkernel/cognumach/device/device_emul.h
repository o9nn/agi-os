#ifndef _I386AT_DEVICE_EMUL_H_
#define _I386AT_DEVICE_EMUL_H_
#include <mach/notify.h>
#include <device/net_status.h>
struct device_emulation_ops
{
void (*reference) (void *);
void (*dealloc) (void *);
ipc_port_t (*dev_to_port) (void *);
io_return_t (*open) (ipc_port_t, mach_msg_type_name_t,
dev_mode_t, const char *, device_t *);
io_return_t (*close) (void *);
io_return_t (*write) (void *, ipc_port_t, mach_msg_type_name_t,
dev_mode_t, recnum_t, io_buf_ptr_t, unsigned, int *);
io_return_t (*write_inband) (void *, ipc_port_t, mach_msg_type_name_t,
dev_mode_t, recnum_t, const io_buf_ptr_inband_t,
unsigned, int *);
io_return_t (*read) (void *, ipc_port_t, mach_msg_type_name_t,
dev_mode_t, recnum_t, int, io_buf_ptr_t *, unsigned *);
io_return_t (*read_inband) (void *, ipc_port_t, mach_msg_type_name_t,
dev_mode_t, recnum_t, int, char *, unsigned *);
io_return_t (*set_status) (void *, dev_flavor_t, dev_status_t,
mach_msg_type_number_t);
io_return_t (*get_status) (void *, dev_flavor_t, dev_status_t,
mach_msg_type_number_t *);
io_return_t (*set_filter) (void *, ipc_port_t, int, filter_t [], unsigned);
io_return_t (*map) (void *, vm_prot_t, vm_offset_t,
vm_size_t, ipc_port_t *, boolean_t);
void (*no_senders) (mach_no_senders_notification_t *);
io_return_t (*write_trap) (void *, dev_mode_t,
rpc_recnum_t, rpc_vm_offset_t, rpc_vm_size_t);
io_return_t (*writev_trap) (void *, dev_mode_t,
rpc_recnum_t, rpc_io_buf_vec_t *, rpc_vm_size_t);
};
#endif