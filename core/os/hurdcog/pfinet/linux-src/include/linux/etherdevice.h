#ifndef _LINUX_ETHERDEVICE_H
#define _LINUX_ETHERDEVICE_H
#include <linux/config.h>
#include <linux/if_ether.h>
#ifdef __KERNEL__
extern int eth_header(struct sk_buff *skb, struct device *dev,
unsigned short type, void *daddr,
void *saddr, unsigned len);
extern int eth_rebuild_header(struct sk_buff *skb);
extern unsigned short eth_type_trans(struct sk_buff *skb, struct device *dev);
extern void eth_header_cache_update(struct hh_cache *hh, struct device *dev,
unsigned char * haddr);
extern int eth_header_cache(struct neighbour *neigh,
struct hh_cache *hh);
extern int eth_header_parse(struct sk_buff *skb,
unsigned char *haddr);
extern struct device * init_etherdev(struct device *, int);
#ifdef CONFIG_IP_ROUTER
static __inline__ void eth_copy_and_sum (struct sk_buff *dest, unsigned char *src, int len, int base)
{
memcpy (dest->data, src, len);
}
#else
extern void eth_copy_and_sum(struct sk_buff *dest,
unsigned char *src, int length, int base);
#endif
#endif
#endif