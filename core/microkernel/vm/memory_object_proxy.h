#ifndef _VM_MEMORY_OBJECT_PROXY_H_
#define _VM_MEMORY_OBJECT_PROXY_H_
#include <ipc/ipc_types.h>
#include <mach/boolean.h>
#include <mach/machine/kern_return.h>
#include <mach/machine/vm_types.h>
#include <mach/message.h>
#include <mach/vm_prot.h>
extern void memory_object_proxy_init (void);
extern boolean_t memory_object_proxy_notify (mach_msg_header_t *msg);
extern kern_return_t memory_object_proxy_lookup (ipc_port_t port,
ipc_port_t *object,
vm_prot_t *max_protection,
vm_offset_t *start,
vm_offset_t *len);
#endif