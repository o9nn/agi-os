#ifndef _LINUX_IPSEC_H
#define _LINUX_IPSEC_H
#include <linux/config.h>
#include <linux/socket.h>
#include <net/sock.h>
#include <linux/skbuff.h>
#define IPSEC_LEVEL_NONE	-1
#define IPSEC_LEVEL_DEFAULT	0
#define IPSEC_LEVEL_USE		1
#define IPSEC_LEVEL_REQUIRE	2
#define IPSEC_LEVEL_UNIQUE	2
#ifdef __KERNEL__
#define RCV_SEC			0x0f
#define RCV_AUTH		0x01
#define RCV_CRYPT		0x02
#define RCV_TUNNEL		0x04
#define SND_SEC			0xf0
#define SND_AUTH		0x10
#define SND_CRYPT		0x20
#define SND_TUNNEL		0x40
#ifdef CONFIG_NET_SECURITY
static __inline__ int ipsec_sk_policy(struct sock *sk, struct sk_buff *skb)
{
return ((sk->authentication < IPSEC_LEVEL_REQUIRE) ||
(skb->security & RCV_AUTH)) &&
((sk->encryption < IPSEC_LEVEL_REQUIRE) ||
(skb->security & RCV_CRYPT));
}
#else
static __inline__ int ipsec_sk_policy(struct sock *sk, struct sk_buff *skb)
{
return 1;
}
#endif
#endif
#endif