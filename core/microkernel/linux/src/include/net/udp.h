#ifndef _UDP_H
#define _UDP_H
#include <linux/udp.h>
#define UDP_HTABLE_SIZE		128
extern struct sock *udp_hash[UDP_HTABLE_SIZE];
extern unsigned short udp_good_socknum(void);
#define UDP_NO_CHECK	0
extern struct proto udp_prot;
extern void	udp_err(int type, int code, unsigned char *header, __u32 daddr,
__u32 saddr, struct inet_protocol *protocol, int len);
extern void	udp_send_check(struct udphdr *uh, __u32 saddr,
__u32 daddr, int len, struct sock *sk);
extern int	udp_recvfrom(struct sock *sk, unsigned char *to,
int len, int noblock, unsigned flags,
struct sockaddr_in *sin, int *addr_len);
extern int	udp_read(struct sock *sk, unsigned char *buff,
int len, int noblock, unsigned flags);
extern int	udp_connect(struct sock *sk,
struct sockaddr_in *usin, int addr_len);
extern int	udp_rcv(struct sk_buff *skb, struct device *dev,
struct options *opt, __u32 daddr,
unsigned short len, __u32 saddr, int redo,
struct inet_protocol *protocol);
extern int	udp_ioctl(struct sock *sk, int cmd, unsigned long arg);
extern int	udp_chkaddr(struct sk_buff *skb);
#endif