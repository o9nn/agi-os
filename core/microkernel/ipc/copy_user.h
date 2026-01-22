#ifndef COPY_USER_H
#define COPY_USER_H
#include <stdint.h>
#include <sys/types.h>
#include <machine/locore.h>
#include <mach/message.h>
static inline int copyin_32to64(const uint32_t *uaddr, uint64_t *kaddr)
{
uint32_t rkaddr;
int ret;
ret = copyin(uaddr, &rkaddr, sizeof(uint32_t));
if (ret)
return ret;
*kaddr = rkaddr;
return 0;
}
static inline int copyout_64to32(const uint64_t *kaddr, uint32_t *uaddr)
{
uint32_t rkaddr=*kaddr;
return copyout(&rkaddr, uaddr, sizeof(uint32_t));
}
static inline int copyin_address(const rpc_vm_offset_t *uaddr, vm_offset_t *kaddr)
{
#ifdef USER32
return copyin_32to64(uaddr, kaddr);
#else
return copyin(uaddr, kaddr, sizeof(*uaddr));
#endif
}
static inline int copyout_address(const vm_offset_t *kaddr, rpc_vm_offset_t *uaddr)
{
#ifdef USER32
return copyout_64to32(kaddr, uaddr);
#else
return copyout(kaddr, uaddr, sizeof(*kaddr));
#endif
}
static inline int copyin_port(const mach_port_name_t *uaddr, mach_port_t *kaddr)
{
#ifdef __LP64__
return copyin_32to64(uaddr, kaddr);
#else
return copyin(uaddr, kaddr, sizeof(*uaddr));
#endif
}
static inline int copyout_port(const mach_port_t *kaddr, mach_port_name_t *uaddr)
{
#ifdef __LP64__
return copyout_64to32(kaddr, uaddr);
#else
return copyout(kaddr, uaddr, sizeof(*kaddr));
#endif
}
#if defined(__LP64__) && defined(USER32)
size_t msg_usize(const mach_msg_header_t *kmsg);
#else
static inline size_t msg_usize(const mach_msg_header_t *kmsg)
{
return kmsg->msgh_size;
}
#endif
#endif