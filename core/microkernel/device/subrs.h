#ifndef _SUBRS_H_
#define _SUBRS_H_
#include <mach/std_types.h>
#include <device/if_hdr.h>
extern void if_init_queues(struct ifnet *ifp);
extern void sleep (vm_offset_t channel, int priority);
extern void wakeup (vm_offset_t channel);
#endif