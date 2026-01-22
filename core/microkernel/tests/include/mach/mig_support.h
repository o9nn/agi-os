#ifndef	_MACH_MIG_SUPPORT_H_
#define	_MACH_MIG_SUPPORT_H_
#include <string.h>
#include <mach/message.h>
#include <mach/mach_types.h>
#include <syscalls.h>
static inline void mig_init(void *_first)
{}
static inline void mig_allocate(vm_address_t *addr, vm_size_t size)
{
if (syscall_vm_allocate(mach_task_self(), addr, size, 1) != KERN_SUCCESS)
*addr = 0;
}
static inline void mig_deallocate(vm_address_t addr, vm_size_t size)
{
syscall_vm_deallocate (mach_task_self(), addr, size);
}
static inline void mig_dealloc_reply_port(mach_port_t port)
{}
static inline void mig_put_reply_port(mach_port_t port)
{}
static inline mach_port_t mig_get_reply_port(void)
{
return mach_reply_port();
}
static inline void mig_reply_setup(const mach_msg_header_t *_request,
mach_msg_header_t *reply)
{}
static inline vm_size_t mig_strncpy (char *dst, const char *src, vm_size_t len)
{
return dst - strncpy(dst, src, len);
}
#endif