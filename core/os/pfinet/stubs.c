#include "pfinet.h"
#include <stdlib.h>
#include <unistd.h>
#include <linux/types.h>
#include <linux/socket.h>
#include <net/sock.h>
#include <net/pkt_sched.h>
int qdisc_restart(struct device *dev)
{
return 0;
}
void qdisc_run_queues(void)
{
}
struct Qdisc_head qdisc_head;
struct Qdisc qdisc_stub;
void
dev_init_scheduler (struct device *dev)
{
dev->qdisc = &qdisc_stub;
}
void dev_shutdown (struct device *)
__attribute__ ((alias ("dev_init_scheduler")));
void dev_activate (struct device *)
__attribute__ ((alias ("dev_init_scheduler")));
void dev_deactivate (struct device *)
__attribute__ ((alias ("dev_init_scheduler")));
void tcp_ioctl (struct device *) __attribute__ ((alias ("dev_init_scheduler")));
__u32 secure_tcp_sequence_number(__u32 saddr, __u32 daddr,
__u16 sport, __u16 dport)
{
static u32 tcp_iss;
static time_t last;
struct timeval now;
do_gettimeofday (&now);
if (now.tv_sec - last > 300)
{
last = now.tv_sec;
srandom (getpid () ^ now.tv_sec ^ now.tv_usec);
tcp_iss = random ();
}
return tcp_iss + (now.tv_sec * 1000000) + now.tv_usec;
}