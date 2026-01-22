#ifndef _ICMP_H
#define _ICMP_H
#include <linux/icmp.h>
#include <linux/skbuff.h>
#include <net/sock.h>
#include <net/protocol.h>
extern struct icmp_err icmp_err_convert[];
extern struct icmp_mib icmp_statistics;
extern void icmp_send(struct sk_buff *skb_in, int type, int code,
unsigned long info, struct device *dev);
extern int icmp_rcv(struct sk_buff *skb1, struct device *dev,
struct options *opt, __u32 daddr,
unsigned short len, __u32 saddr,
int redo, struct inet_protocol *protocol);
extern int icmp_ioctl(struct sock *sk, int cmd,
unsigned long arg);
extern void icmp_init(struct proto_ops *ops);
extern int icmp_chkaddr(struct sk_buff *skb);
#endif