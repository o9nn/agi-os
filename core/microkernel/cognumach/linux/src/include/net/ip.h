#ifndef _IP_H
#define _IP_H
#include <linux/config.h>
#include <linux/types.h>
#include <linux/socket.h>
#include <linux/ip.h>
#include <linux/netdevice.h>
#include <net/route.h>
#ifndef _SNMP_H
#include <net/snmp.h>
#endif
#include <net/sock.h>
#define IP_CE 0x8000
#define IP_DF 0x4000
#define IP_MF 0x2000
#define IP_OFFSET 0x1FFF
#define IP_FRAG_TIME (30 * HZ)
#ifdef CONFIG_IP_MULTICAST
extern void ip_mc_dropsocket(struct sock *);
extern void ip_mc_dropdevice(struct device *dev);
extern int ip_mc_procinfo(char *, char **, off_t, int, int);
#endif
#include <net/ip_forward.h>
struct ipfrag
{
int offset;
int end;
int len;
struct sk_buff *skb;
unsigned char *ptr;
struct ipfrag *next;
struct ipfrag *prev;
};
struct ipq
{
unsigned char *mac;
struct iphdr *iph;
int len;
short ihlen;
short maclen;
struct timer_list timer;
struct ipfrag *fragments;
struct ipq *next;
struct ipq *prev;
struct device *dev;
};
extern void ip_print(const struct iphdr *ip);
extern int ip_ioctl(struct sock *sk, int cmd, unsigned long arg);
extern void ip_route_check(__u32 daddr);
extern int ip_send(struct rtable *rt, struct sk_buff *skb, __u32 daddr, int len, struct device *dev, __u32 saddr);
extern int ip_build_header(struct sk_buff *skb,
__u32 saddr,
__u32 daddr,
struct device **dev, int type,
struct options *opt, int len,
int tos,int ttl,struct rtable **rp);
extern int ip_rcv(struct sk_buff *skb, struct device *dev,
struct packet_type *pt);
extern int ip_options_echo(struct options * dopt, struct options * sopt,
__u32 daddr, __u32 saddr,
struct sk_buff * skb);
extern int ip_options_compile(struct options * opt, struct sk_buff * skb);
extern void ip_send_check(struct iphdr *ip);
extern int ip_id_count;
extern void ip_queue_xmit(struct sock *sk,
struct device *dev, struct sk_buff *skb,
int free);
extern void ip_init(void);
extern int ip_build_xmit(struct sock *sk,
void getfrag (const void *,
__u32,
char *,
unsigned int,
unsigned int),
const void *frag,
unsigned short int length,
__u32 daddr,
__u32 saddr,
struct options * opt,
int flags,
int type,
int noblock);
extern struct ip_mib ip_statistics;
extern int sysctl_ip_dynaddr;
int ip_rewrite_addrs(struct sock *sk, struct sk_buff *skb, struct device *dev);
struct sk_buff *ip_defrag(struct iphdr *iph, struct sk_buff *skb, struct device *dev);
void ip_fragment(struct sock *sk, struct sk_buff *skb, struct device *dev, int is_frag);
extern int ip_forward(struct sk_buff *skb, struct device *dev, int is_frag, __u32 target_addr);
extern int sysctl_ip_forward;
extern void ip_options_build(struct sk_buff *skb, struct options *opt, __u32 daddr, __u32 saddr, int is_frag);
extern int ip_options_echo(struct options *dopt, struct options *sopt, __u32 daddr, __u32 saddr, struct sk_buff *skb);
extern void ip_options_fragment(struct sk_buff *skb);
extern int ip_options_compile(struct options *opt, struct sk_buff *skb);
extern int ip_setsockopt(struct sock *sk, int level, int optname, char *optval, int optlen);
extern int ip_getsockopt(struct sock *sk, int level, int optname, char *optval, int *optlen);
#endif