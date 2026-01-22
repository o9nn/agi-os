#ifndef _PROTOCOL_H
#define _PROTOCOL_H
#include <linux/config.h>
#include <linux/in6.h>
#if defined(CONFIG_IPV6) || defined (CONFIG_IPV6_MODULE)
#include <linux/ipv6.h>
#endif
#define MAX_INET_PROTOS 32
struct inet_protocol
{
int (*handler)(struct sk_buff *skb, unsigned short len);
void (*err_handler)(struct sk_buff *skb, unsigned char *dp, int len);
struct inet_protocol *next;
unsigned char protocol;
unsigned char copy:1;
void *data;
const char *name;
};
#if defined(CONFIG_IPV6) || defined (CONFIG_IPV6_MODULE)
struct inet6_protocol
{
int (*handler)(struct sk_buff *skb,
unsigned long len);
void (*err_handler)(struct sk_buff *skb, struct ipv6hdr *hdr,
struct inet6_skb_parm *opt,
int type, int code, unsigned char *buff,
__u32 info);
struct inet6_protocol *next;
unsigned char protocol;
unsigned char copy:1;
void *data;
const char *name;
};
#endif
extern struct inet_protocol *inet_protocol_base;
extern struct inet_protocol *inet_protos[MAX_INET_PROTOS];
#if defined(CONFIG_IPV6) || defined (CONFIG_IPV6_MODULE)
extern struct inet6_protocol *inet6_protocol_base;
extern struct inet6_protocol *inet6_protos[MAX_INET_PROTOS];
#endif
extern void inet_add_protocol(struct inet_protocol *prot);
extern int inet_del_protocol(struct inet_protocol *prot);
#if defined(CONFIG_IPV6) || defined (CONFIG_IPV6_MODULE)
extern void inet6_add_protocol(struct inet6_protocol *prot);
extern int inet6_del_protocol(struct inet6_protocol *prot);
#endif
#endif