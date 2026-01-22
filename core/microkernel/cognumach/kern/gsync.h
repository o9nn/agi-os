#ifndef _KERN_GSYNC_H_
#define _KERN_GSYNC_H_ 1
#define GSYNC_SHARED 0x01
#define GSYNC_QUAD 0x02
#define GSYNC_TIMED 0x04
#define GSYNC_BROADCAST 0x08
#define GSYNC_MUTATE 0x10
#include <mach/mach_types.h>
void gsync_setup (void);
kern_return_t gsync_wait (task_t task, vm_offset_t addr,
unsigned int lo, unsigned int hi, natural_t msec, int flags);
kern_return_t gsync_wake (task_t task,
vm_offset_t addr, unsigned int val, int flags);
kern_return_t gsync_requeue (task_t task, vm_offset_t src_addr,
vm_offset_t dst_addr, boolean_t wake_one, int flags);
#endif