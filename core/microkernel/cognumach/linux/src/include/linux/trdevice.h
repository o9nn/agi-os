#ifndef _LINUX_TRDEVICE_H
#define _LINUX_TRDEVICE_H
#include <linux/if_tr.h>
#ifdef __KERNEL__
extern int		tr_header(struct sk_buff *skb, struct device *dev,
unsigned short type, void *daddr,
void *saddr, unsigned len);
extern int		tr_rebuild_header(void *buff, struct device *dev,
unsigned long raddr, struct sk_buff *skb);
extern unsigned short	tr_type_trans(struct sk_buff *skb, struct device *dev);
#endif
#endif