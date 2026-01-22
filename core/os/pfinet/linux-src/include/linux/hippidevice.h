#ifndef _LINUX_HIPPIDEVICE_H
#define _LINUX_HIPPIDEVICE_H
#include <linux/if_hippi.h>
#ifdef __KERNEL__
extern int hippi_header(struct sk_buff *skb,
struct device *dev,
unsigned short type,
void *daddr,
void *saddr,
unsigned len);
extern int hippi_rebuild_header(struct sk_buff *skb);
extern unsigned short hippi_type_trans(struct sk_buff *skb,
struct device *dev);
extern void hippi_header_cache_bind(struct hh_cache ** hhp,
struct device *dev,
unsigned short htype,
__u32 daddr);
extern void hippi_header_cache_update(struct hh_cache *hh,
struct device *dev,
unsigned char * haddr);
extern int hippi_header_parse(struct sk_buff *skb, unsigned char *haddr);
extern void hippi_net_init(void);
void hippi_setup(struct device *dev);
extern struct device *init_hippi_dev(struct device *, int);
extern void unregister_hipdev(struct device *dev);
#endif
#endif