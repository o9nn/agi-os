#ifndef	_MACH_MIG_SUPPORT_H_
#define	_MACH_MIG_SUPPORT_H_
#include <string.h>
#include <mach/message.h>
#include <mach/mach_types.h>
extern void		mig_init(void *_first);
extern void		mig_allocate(vm_address_t *_addr_p, vm_size_t _size);
extern void		mig_deallocate(vm_address_t _addr, vm_size_t _size);
extern void		mig_dealloc_reply_port(mach_port_t);
extern void		mig_put_reply_port(mach_port_t);
extern mach_port_name_t	mig_get_reply_port(void);
extern void		mig_reply_setup(const mach_msg_header_t *_request,
mach_msg_header_t *reply);
extern vm_size_t	mig_strncpy(char *_dest, const char *_src, vm_size_t _len);
#endif