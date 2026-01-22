#include <asm/uaccess.h>
#include <asm/system.h>
#include <linux/types.h>
#include <linux/kernel.h>
#include <linux/sched.h>
#include <linux/string.h>
#include <linux/config.h>
#include <linux/socket.h>
#include <linux/in.h>
#include <linux/inet.h>
#include <linux/netdevice.h>
#include <linux/timer.h>
#include <net/ip.h>
#include <net/protocol.h>
#include <net/tcp.h>
#include <linux/skbuff.h>
#include <net/sock.h>
#include <net/icmp.h>
#include <net/udp.h>
#include <net/ipip.h>
#include <linux/igmp.h>
#define IPPROTO_PREVIOUS NULL
#ifdef CONFIG_IP_MULTICAST
static struct inet_protocol igmp_protocol =
{
igmp_rcv,
NULL,
IPPROTO_PREVIOUS,
IPPROTO_IGMP,
0,
NULL,
"IGMP"
};
#undef  IPPROTO_PREVIOUS
#define IPPROTO_PREVIOUS &igmp_protocol
#endif
static struct inet_protocol tcp_protocol =
{
tcp_v4_rcv,
tcp_v4_err,
IPPROTO_PREVIOUS,
IPPROTO_TCP,
0,
NULL,
"TCP"
};
#undef  IPPROTO_PREVIOUS
#define IPPROTO_PREVIOUS &tcp_protocol
static struct inet_protocol udp_protocol =
{
udp_rcv,
udp_err,
IPPROTO_PREVIOUS,
IPPROTO_UDP,
0,
NULL,
"UDP"
};
#undef  IPPROTO_PREVIOUS
#define IPPROTO_PREVIOUS &udp_protocol
static struct inet_protocol icmp_protocol =
{
icmp_rcv,
NULL,
IPPROTO_PREVIOUS,
IPPROTO_ICMP,
0,
NULL,
"ICMP"
};
#undef  IPPROTO_PREVIOUS
#define IPPROTO_PREVIOUS &icmp_protocol
struct inet_protocol *inet_protocol_base = IPPROTO_PREVIOUS;
struct inet_protocol *inet_protos[MAX_INET_PROTOS] =
{
NULL
};
struct inet_protocol *inet_get_protocol(unsigned char prot)
{
unsigned char hash;
struct inet_protocol *p;
hash = prot & (MAX_INET_PROTOS - 1);
for (p = inet_protos[hash] ; p != NULL; p=p->next)
{
if (p->protocol == prot)
return((struct inet_protocol *) p);
}
return(NULL);
}
void inet_add_protocol(struct inet_protocol *prot)
{
unsigned char hash;
struct inet_protocol *p2;
hash = prot->protocol & (MAX_INET_PROTOS - 1);
prot ->next = inet_protos[hash];
inet_protos[hash] = prot;
prot->copy = 0;
p2 = (struct inet_protocol *) prot->next;
while(p2 != NULL)
{
if (p2->protocol == prot->protocol)
{
prot->copy = 1;
break;
}
p2 = (struct inet_protocol *) p2->next;
}
}
int inet_del_protocol(struct inet_protocol *prot)
{
struct inet_protocol *p;
struct inet_protocol *lp = NULL;
unsigned char hash;
hash = prot->protocol & (MAX_INET_PROTOS - 1);
if (prot == inet_protos[hash])
{
inet_protos[hash] = (struct inet_protocol *) inet_protos[hash]->next;
return(0);
}
p = (struct inet_protocol *) inet_protos[hash];
while(p != NULL)
{
if (p->next != NULL && p->next == prot)
{
if (p->copy == 0 && lp != NULL)
lp->copy = 0;
p->next = prot->next;
return(0);
}
if (p->next != NULL && p->next->protocol == prot->protocol)
lp = p;
p = (struct inet_protocol *) p->next;
}
return(-1);
}