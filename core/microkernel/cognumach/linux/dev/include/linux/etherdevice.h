#ifndef _LINUX_ETHERDEVICE_H
#define _LINUX_ETHERDEVICE_H
#include <linux/if_ether.h>
#ifdef __KERNEL__
extern int eth_header(struct sk_buff *skb, struct device *dev,
unsigned short type, void *daddr,
void *saddr, unsigned len);
extern int eth_rebuild_header(void *buff, struct device *dev,
unsigned long dst, struct sk_buff *skb);
#ifdef MACH
#define eth_type_trans(skb, dev) ((unsigned short)0)
#else
extern unsigned short eth_type_trans(struct sk_buff *skb, struct device *dev);
#endif
extern void eth_header_cache_bind(struct hh_cache ** hhp, struct device *dev,
unsigned short htype, __u32 daddr);
extern void eth_header_cache_update(struct hh_cache *hh, struct device *dev, unsigned char * haddr);
#ifdef MACH
#define eth_copy_and_sum(dest, src, length, base) \
memcpy((dest)->data, src, length)
#else
extern void eth_copy_and_sum(struct sk_buff *dest,
unsigned char *src, int length, int base);
#endif
extern struct device * init_etherdev(struct device *, int);
#endif
#endif