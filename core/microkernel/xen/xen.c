#include <sys/types.h>
#include <string.h>
#include <mach/xen.h>
#include <machine/xen.h>
#include <machine/ipl.h>
#include <xen/block.h>
#include <xen/console.h>
#include <xen/grant.h>
#include <xen/net.h>
#include <xen/store.h>
#include <xen/time.h>
#include "xen.h"
#include "evt.h"
static void hyp_debug(void)
{
panic("debug");
}
void hyp_init(void)
{
hyp_grant_init();
hyp_store_init();
evtchn_port_t port = hyp_event_channel_bind_virq(VIRQ_DEBUG, 0);
hyp_evt_handler(port, (interrupt_handler_fn)hyp_debug, 0, SPL7);
}
void hyp_dev_init(void)
{
hyp_block_init();
hyp_net_init();
}
extern int int_mask[];
void hyp_idle(void)
{
int cpu = 0;
hyp_shared_info.vcpu_info[cpu].evtchn_upcall_mask = 0xff;
barrier();
if (!hyp_shared_info.vcpu_info[cpu].evtchn_upcall_pending &&
!hyp_shared_info.evtchn_pending[cpu])
hyp_block();
while (1) {
hyp_shared_info.vcpu_info[cpu].evtchn_upcall_mask = 0x00;
barrier();
if (!hyp_shared_info.vcpu_info[cpu].evtchn_upcall_pending &&
!hyp_shared_info.evtchn_pending[cpu])
break;
hyp_shared_info.vcpu_info[cpu].evtchn_upcall_mask = 0xff;
hyp_c_callback(NULL,NULL);
}
}