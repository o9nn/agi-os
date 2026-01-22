#ifndef _LINUX_FDDIDEVICE_H
#define _LINUX_FDDIDEVICE_H
#include <linux/if_fddi.h>
#ifdef __KERNEL__
extern int		fddi_header(struct sk_buff *skb,
struct device *dev,
unsigned short type,
void *daddr,
void *saddr,
unsigned len);
extern int		fddi_rebuild_header(struct sk_buff *skb);
extern unsigned short	fddi_type_trans(struct sk_buff *skb,
struct device *dev);
#endif
#endif