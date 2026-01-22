#ifndef _MACHDEV_DEVICE_EMUL_H_
#define _MACHDEV_DEVICE_EMUL_H_
#include <mach.h>
#include <mach/notify.h>
#include <device/device_types.h>
#include <device/net_status.h>
#include <errno.h>
struct machdev_device_emulation_ops
{
void (*init) (void);
void (*reference) (void *);
void (*dealloc) (void *);
mach_port_t (*dev_to_port) (void *);
io_return_t (*open) (mach_port_t, mach_msg_type_name_t,
dev_mode_t, const char *, device_t *,
mach_msg_type_name_t *type);
io_return_t (*close) (void *);
io_return_t (*write) (void *, mach_port_t, mach_msg_type_name_t,
dev_mode_t, recnum_t, io_buf_ptr_t, unsigned, int *);
io_return_t (*write_inband) (void *, mach_port_t, mach_msg_type_name_t,
dev_mode_t, recnum_t, const char *,
unsigned, int *);
io_return_t (*read) (void *, mach_port_t, mach_msg_type_name_t,
dev_mode_t, recnum_t, int, io_buf_ptr_t *, unsigned *);
io_return_t (*read_inband) (void *, mach_port_t, mach_msg_type_name_t,
dev_mode_t, recnum_t, int, char *, unsigned *);
io_return_t (*set_status) (void *, dev_flavor_t, dev_status_t,
mach_msg_type_number_t);
io_return_t (*get_status) (void *, dev_flavor_t, dev_status_t,
mach_msg_type_number_t *);
io_return_t (*set_filter) (void *, mach_port_t, int, filter_t [], unsigned);
io_return_t (*map) (void *, vm_prot_t, vm_offset_t,
vm_size_t, mach_port_t *, boolean_t);
void (*no_senders) (mach_no_senders_notification_t *);
io_return_t (*write_trap) (void *, dev_mode_t,
recnum_t, vm_offset_t, vm_size_t);
io_return_t (*writev_trap) (void *, dev_mode_t,
recnum_t, io_buf_vec_t *, vm_size_t);
void (*sync) (void);
};
#endif