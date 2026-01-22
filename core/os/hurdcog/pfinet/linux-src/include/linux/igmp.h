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
#define IGMP_MINLEN			8
#define IGMP_MAX_HOST_REPORT_DELAY	10
#define IGMP_TIMER_SCALE		10
#define IGMP_AGE_THRESHOLD		400
#define IGMP_ALL_HOSTS		htonl(0xE0000001L)
#define IGMP_ALL_ROUTER 	htonl(0xE0000002L)
#define IGMP_LOCAL_GROUP	htonl(0xE0000000L)
#define IGMP_LOCAL_GROUP_MASK	htonl(0xFFFFFF00L)
#ifdef __KERNEL__
struct ip_mc_socklist
{
struct ip_mc_socklist	*next;
int			count;
struct ip_mreqn		multi;
};
struct ip_mc_list
{
struct in_device	*interface;
unsigned long		multiaddr;
struct ip_mc_list	*next;
struct timer_list	timer;
int			users;
char			tm_running;
char			reporter;
char			unsolicit_count;
char			loaded;
};
static __inline__ int ip_check_mc(struct device *dev, u32 mc_addr)
{
struct in_device *in_dev = dev->ip_ptr;
struct ip_mc_list *im;
if (in_dev) {
for (im=in_dev->mc_list; im; im=im->next)
if (im->multiaddr == mc_addr)
return 1;
}
return 0;
}
extern int igmp_rcv(struct sk_buff *, unsigned short);
extern int ip_mc_join_group(struct sock *sk, struct ip_mreqn *imr);
extern int ip_mc_leave_group(struct sock *sk, struct ip_mreqn *imr);
extern void ip_mc_drop_socket(struct sock *sk);
extern void ip_mr_init(void);
extern void ip_mc_init_dev(struct in_device *);
extern void ip_mc_destroy_dev(struct in_device *);
extern void ip_mc_up(struct in_device *);
extern void ip_mc_down(struct in_device *);
extern int ip_mc_dec_group(struct in_device *in_dev, u32 addr);
extern void ip_mc_inc_group(struct in_device *in_dev, u32 addr);
#endif
#endif