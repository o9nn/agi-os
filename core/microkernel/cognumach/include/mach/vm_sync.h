#ifndef _MACH_VM_SYNC_H_
#define _MACH_VM_SYNC_H_
typedef int vm_sync_t;
#define VM_SYNC_ASYNCHRONOUS ((vm_sync_t) 0x01)
#define VM_SYNC_SYNCHRONOUS ((vm_sync_t) 0x02)
#define VM_SYNC_INVALIDATE ((vm_sync_t) 0x04)
#if 0
#define VM_SYNC_KILLPAGES ((vm_sync_t) 0x08)
#define VM_SYNC_DEACTIVATE ((vm_sync_t) 0x10)
#define VM_SYNC_CONTIGUOUS ((vm_sync_t) 0x20)
#define VM_SYNC_REUSABLEPAGES ((vm_sync_t) 0x40)
#endif
#endif