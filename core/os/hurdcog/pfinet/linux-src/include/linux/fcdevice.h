#ifndef _LINUX_FCDEVICE_H
#define _LINUX_FCDEVICE_H
#include <linux/if_fc.h>
#ifdef __KERNEL__
extern int		fc_header(struct sk_buff *skb, struct device *dev,
unsigned short type, void *daddr,
void *saddr, unsigned len);
extern int		fc_rebuild_header(struct sk_buff *skb);
extern struct device    * init_fcdev(struct device *, int);
#endif
#endif