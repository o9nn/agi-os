#ifndef _IPC_MIG_H_
#define _IPC_MIG_H_
#include <mach/std_types.h>
#include <device/device_types.h>
#include <ipc/ipc_thread.h>
extern mach_msg_return_t mach_msg_send_from_kernel(
mach_msg_header_t   *msg,
mach_msg_size_t     send_size);
extern void mach_msg_abort_rpc (ipc_thread_t);
extern mach_msg_return_t mach_msg_rpc_from_kernel(
const mach_msg_header_t *msg,
mach_msg_size_t send_size,
mach_msg_size_t reply_size);
extern kern_return_t syscall_vm_map(
mach_port_name_t	target_map,
rpc_vm_offset_t	*address,
rpc_vm_size_t	size,
rpc_vm_offset_t	mask,
boolean_t	anywhere,
mach_port_name_t	memory_object,
rpc_vm_offset_t	offset,
boolean_t	copy,
vm_prot_t	cur_protection,
vm_prot_t	max_protection,
vm_inherit_t	inheritance);
extern kern_return_t syscall_vm_allocate(
mach_port_name_t	target_map,
rpc_vm_offset_t		*address,
rpc_vm_size_t		size,
boolean_t		anywhere);
extern kern_return_t syscall_vm_deallocate(
mach_port_name_t	target_map,
rpc_vm_offset_t		start,
rpc_vm_size_t		size);
extern kern_return_t syscall_task_create(
mach_port_name_t	parent_task,
boolean_t	        inherit_memory,
mach_port_name_t	*child_task);
extern kern_return_t syscall_task_terminate(mach_port_name_t task);
extern kern_return_t syscall_task_suspend(mach_port_name_t task);
extern kern_return_t syscall_task_set_special_port(
mach_port_name_t	task,
int		which_port,
mach_port_name_t	port_name);
extern kern_return_t syscall_mach_port_allocate(
mach_port_name_t 		task,
mach_port_right_t 		right,
mach_port_name_t 		*namep);
extern kern_return_t syscall_mach_port_deallocate(
mach_port_name_t task,
mach_port_name_t name);
extern kern_return_t syscall_mach_port_insert_right(
mach_port_name_t task,
mach_port_name_t name,
mach_port_name_t right,
mach_msg_type_name_t rightType);
extern kern_return_t syscall_mach_port_allocate_name(
mach_port_name_t 		task,
mach_port_right_t 		right,
mach_port_name_t 		name);
extern kern_return_t syscall_thread_depress_abort(mach_port_name_t thread);
extern kern_return_t thread_set_self_state(
int		flavor,
thread_state_t	new_state,
natural_t	new_state_count);
extern io_return_t syscall_device_write_request(
mach_port_name_t	device_name,
mach_port_name_t	reply_name,
dev_mode_t	mode,
rpc_recnum_t	recnum,
rpc_vm_offset_t	data,
rpc_vm_size_t	data_count);
io_return_t syscall_device_writev_request(
mach_port_name_t	device_name,
mach_port_name_t	reply_name,
dev_mode_t	mode,
rpc_recnum_t	recnum,
rpc_io_buf_vec_t	*iovec,
rpc_vm_size_t	iocount);
#endif