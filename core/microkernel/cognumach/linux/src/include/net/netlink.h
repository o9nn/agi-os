#ifndef __NET_NETLINK_H
#define __NET_NETLINK_H
#define NET_MAJOR 36
#define MAX_LINKS 11
#define MAX_QBYTES 32768
#include <linux/config.h>
extern int netlink_attach(int unit, int (*function)(struct sk_buff *skb));
extern int netlink_donothing(struct sk_buff *skb);
extern void netlink_detach(int unit);
extern int netlink_post(int unit, struct sk_buff *skb);
extern int init_netlink(void);
#define NETLINK_ROUTE 0
#define NETLINK_SKIP 1
#define NETLINK_USERSOCK 2
#define NETLINK_FIREWALL 3
#define NETLINK_PSI 4
#define NETLINK_ARPD 8
#define NETLINK_NET_PPP 9
#define NETLINK_IPSEC 10
#ifdef CONFIG_RTNETLINK
extern void ip_netlink_msg(unsigned long, __u32, __u32, __u32, short, short, char *);
#else
#define ip_netlink_msg(a,b,c,d,e,f,g)
#endif
#endif