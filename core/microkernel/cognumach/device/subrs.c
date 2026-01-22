#include <kern/debug.h>
#include <kern/printf.h>
#include <vm/vm_kern.h>
#include <vm/vm_user.h>
#include <device/buf.h>
#include <device/if_hdr.h>
#include <device/if_ether.h>
#include <device/subrs.h>
char *
ether_sprintf(const u_char *ap)
{
int i;
static char etherbuf[18];
char *cp = etherbuf;
static char digits[] = "0123456789abcdef";
for (i = 0; i < 6; i++) {
*cp++ = digits[*ap >> 4];
*cp++ = digits[*ap++ & 0xf];
*cp++ = ':';
}
*--cp = 0;
return (etherbuf);
}
void if_init_queues(struct ifnet *ifp)
{
IFQ_INIT(&ifp->if_snd);
queue_init(&ifp->if_rcv_port_list);
queue_init(&ifp->if_snd_port_list);
simple_lock_init(&ifp->if_rcv_port_list_lock);
simple_lock_init(&ifp->if_snd_port_list_lock);
}
void sleep(vm_offset_t channel, int priority)
{
assert_wait((event_t) channel, FALSE);
thread_block((void (*)()) 0);
}
void wakeup(vm_offset_t channel)
{
thread_wakeup((event_t) channel);
}