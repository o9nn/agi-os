#ifndef _ICMP_H
#define	_ICMP_H
#include <linux/icmp.h>
#include <linux/skbuff.h>
#include <net/sock.h>
#include <net/protocol.h>
extern struct icmp_err icmp_err_convert[];
extern struct icmp_mib icmp_statistics;
extern void	icmp_send(struct sk_buff *skb_in,  int type, int code,
unsigned long info);
extern int	icmp_rcv(struct sk_buff *skb, unsigned short len);
extern int	icmp_ioctl(struct sock *sk, int cmd, unsigned long arg);
extern void	icmp_init(struct net_proto_family *ops);
extern int 	xrlim_allow(struct dst_entry *dst, int timeout);
extern int	icmp_chkaddr(struct sk_buff *skb);
#endif