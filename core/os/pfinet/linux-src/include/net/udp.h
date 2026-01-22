#ifndef _UDP_H
#define _UDP_H
#include <linux/udp.h>
#include <net/sock.h>
#define UDP_HTABLE_SIZE 128
extern struct sock *udp_hash[UDP_HTABLE_SIZE];
#define UDP_NO_CHECK 0
static inline int udp_lport_inuse(u16 num)
{
struct sock *sk = udp_hash[num & (UDP_HTABLE_SIZE - 1)];
for(; sk != NULL; sk = sk->next) {
if(sk->num == num)
return 1;
}
return 0;
}
extern struct proto udp_prot;
extern void udp_err(struct sk_buff *, unsigned char *, int);
extern int udp_connect(struct sock *sk,
struct sockaddr *usin, int addr_len);
extern int udp_sendmsg(struct sock *sk, struct msghdr *msg, int len);
extern int udp_rcv(struct sk_buff *skb, unsigned short len);
extern int udp_ioctl(struct sock *sk, int cmd, unsigned long arg);
extern int udp_chkaddr(struct sk_buff *skb);
#endif