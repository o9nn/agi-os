#ifndef _LINUX_IGMP_H
#define _LINUX_IGMP_H
struct igmphdr
{
__u8 type;
__u8 code;
__u16 csum;
__u32 group;
};
#define IGMP_HOST_MEMBERSHIP_QUERY	0x11
#define IGMP_HOST_MEMBERSHIP_REPORT	0x12
#define IGMP_DVMRP			0x13
#define IGMP_PIM			0x14
#define IGMP_TRACE			0x15
#define IGMP_HOST_NEW_MEMBERSHIP_REPORT 0x16
#define IGMP_HOST_LEAVE_MESSAGE 	0x17
#define IGMP_MTRACE_RESP		0x1e
#define IGMP_MTRACE			0x1f
#define IGMP_DELAYING_MEMBER		0x01
#define IGMP_IDLE_MEMBER		0x02
#define IGMP_LAZY_MEMBER		0x03
#define IGMP_SLEEPING_MEMBER		0x04
#define IGMP_AWAKENING_MEMBER		0x05
#define IGMP_OLD_ROUTER 		0x00
#define IGMP_NEW_ROUTER 		0x01
#define IGMP_MINLEN			8
#define IGMP_MAX_HOST_REPORT_DELAY	10
#define IGMP_TIMER_SCALE		10
#define IGMP_AGE_THRESHOLD		540
#define IGMP_ALL_HOSTS		htonl(0xE0000001L)
#define IGMP_ALL_ROUTER 	htonl(0xE0000002L)
#define IGMP_LOCAL_GROUP	htonl(0xE0000000L)
#define IGMP_LOCAL_GROUP_MASK	htonl(0xFFFFFF00L)
#ifdef __KERNEL__
struct ip_mc_socklist
{
unsigned long multiaddr[IP_MAX_MEMBERSHIPS];
struct device *multidev[IP_MAX_MEMBERSHIPS];
};
struct ip_mc_list
{
struct device *interface;
unsigned long multiaddr;
struct ip_mc_list *next;
struct timer_list timer;
short tm_running;
short reporter;
int users;
};
struct ip_router_info
{
struct device *dev;
int    type;
int    time;
struct timer_list timer;
struct ip_router_info *next;
};
extern struct ip_mc_list *ip_mc_head;
extern int igmp_rcv(struct sk_buff *, struct device *, struct options *, __u32, unsigned short,
__u32, int , struct inet_protocol *);
extern void ip_mc_drop_device(struct device *dev);
extern int ip_mc_join_group(struct sock *sk, struct device *dev, unsigned long addr);
extern int ip_mc_leave_group(struct sock *sk, struct device *dev,unsigned long addr);
extern void ip_mc_drop_socket(struct sock *sk);
extern void ip_mr_init(void);
#endif
#endif