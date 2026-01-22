#include <linux/config.h>
#include <linux/kernel.h>
#include <linux/sched.h>
#include <linux/types.h>
#include <linux/fs.h>
#include <linux/malloc.h>
#include <linux/if_ether.h>
#include <linux/string.h>
#include <linux/netdevice.h>
#include <linux/etherdevice.h>
#include <linux/fddidevice.h>
#include <linux/trdevice.h>
#include <linux/if_arp.h>
#ifdef CONFIG_NET_ALIAS
#include <linux/net_alias.h>
#endif
#define MAX_ETH_CARDS 16
static struct device *ethdev_index[MAX_ETH_CARDS];
struct device *
init_etherdev(struct device *dev, int sizeof_priv)
{
int new_device = 0;
int i;
if (dev == NULL) {
int alloc_size = sizeof(struct device) + sizeof("eth%d  ")
+ sizeof_priv + 3;
struct device *cur_dev;
char pname[8];
for (i = 0; i < MAX_ETH_CARDS; ++i)
if (ethdev_index[i] == NULL) {
sprintf(pname, "eth%d", i);
for (cur_dev = dev_base; cur_dev; cur_dev = cur_dev->next)
if (strcmp(pname, cur_dev->name) == 0) {
dev = cur_dev;
dev->init = NULL;
sizeof_priv = (sizeof_priv + 3) & ~3;
dev->priv = sizeof_priv
? kmalloc(sizeof_priv, GFP_KERNEL)
:	NULL;
if (dev->priv) memset(dev->priv, 0, sizeof_priv);
goto found;
}
}
alloc_size &= ~3;
dev = (struct device *)kmalloc(alloc_size, GFP_KERNEL);
memset(dev, 0, alloc_size);
if (sizeof_priv)
dev->priv = (void *) (dev + 1);
dev->name = sizeof_priv + (char *)(dev + 1);
new_device = 1;
}
found:
if (dev->name &&
((dev->name[0] == '\0') || (dev->name[0] == ' '))) {
for (i = 0; i < MAX_ETH_CARDS; ++i)
if (ethdev_index[i] == NULL) {
sprintf(dev->name, "eth%d", i);
ethdev_index[i] = dev;
break;
}
}
ether_setup(dev);
if (new_device) {
struct device **old_devp = &dev_base;
while ((*old_devp)->next)
old_devp = & (*old_devp)->next;
(*old_devp)->next = dev;
dev->next = 0;
}
return dev;
}
static int eth_mac_addr(struct device *dev, void *p)
{
struct sockaddr *addr=p;
if(dev->start)
return -EBUSY;
memcpy(dev->dev_addr, addr->sa_data,dev->addr_len);
return 0;
}
static int eth_change_mtu(struct device *dev, int new_mtu)
{
if ((new_mtu < 68) || (new_mtu > 1500))
return -EINVAL;
dev->mtu = new_mtu;
return 0;
}
#ifdef CONFIG_FDDI
static int fddi_change_mtu(struct device *dev, int new_mtu)
{
if ((new_mtu < FDDI_K_SNAP_HLEN) || (new_mtu > FDDI_K_SNAP_DLEN))
return(-EINVAL);
dev->mtu = new_mtu;
return(0);
}
#endif
void ether_setup(struct device *dev)
{
int i;
for (i = 0; i < DEV_NUMBUFFS; i++)
skb_queue_head_init(&dev->buffs[i]);
if (dev->name && (strncmp(dev->name, "eth", 3) == 0)) {
i = simple_strtoul(dev->name + 3, NULL, 0);
if (ethdev_index[i] == NULL) {
ethdev_index[i] = dev;
}
else if (dev != ethdev_index[i]) {
printk("ether_setup: Ouch! Someone else took %s\n",
dev->name);
}
}
dev->change_mtu		= eth_change_mtu;
dev->hard_header	= eth_header;
dev->rebuild_header 	= eth_rebuild_header;
dev->set_mac_address 	= eth_mac_addr;
dev->header_cache_bind 	= eth_header_cache_bind;
dev->header_cache_update= eth_header_cache_update;
dev->type		= ARPHRD_ETHER;
dev->hard_header_len 	= ETH_HLEN;
dev->mtu		= 1500;
dev->addr_len		= ETH_ALEN;
dev->tx_queue_len	= 100;
memset(dev->broadcast,0xFF, ETH_ALEN);
dev->flags		= IFF_BROADCAST|IFF_MULTICAST;
dev->family		= AF_INET;
dev->pa_addr	= 0;
dev->pa_brdaddr = 0;
dev->pa_mask	= 0;
dev->pa_alen	= 4;
}
#ifdef CONFIG_TR
void tr_setup(struct device *dev)
{
int i;
for (i = 0; i < DEV_NUMBUFFS; i++)
skb_queue_head_init(&dev->buffs[i]);
dev->hard_header	= tr_header;
dev->rebuild_header 	= tr_rebuild_header;
dev->type		= ARPHRD_IEEE802;
dev->hard_header_len 	= TR_HLEN;
dev->mtu		= 2000;
dev->addr_len		= TR_ALEN;
dev->tx_queue_len	= 100;
memset(dev->broadcast,0xFF, TR_ALEN);
dev->flags		= IFF_BROADCAST;
dev->family		= AF_INET;
dev->pa_addr	= 0;
dev->pa_brdaddr = 0;
dev->pa_mask	= 0;
dev->pa_alen	= 4;
}
#endif
#ifdef CONFIG_FDDI
void fddi_setup(struct device *dev)
{
int i;
for (i=0; i < DEV_NUMBUFFS; i++)
skb_queue_head_init(&dev->buffs[i]);
dev->change_mtu			= fddi_change_mtu;
dev->hard_header		= fddi_header;
dev->rebuild_header		= fddi_rebuild_header;
dev->type				= ARPHRD_FDDI;
dev->hard_header_len	= FDDI_K_SNAP_HLEN+3;
dev->mtu				= FDDI_K_SNAP_DLEN;
dev->addr_len			= FDDI_K_ALEN;
dev->tx_queue_len		= 100;
memset(dev->broadcast, 0xFF, FDDI_K_ALEN);
dev->flags		= IFF_BROADCAST | IFF_MULTICAST;
dev->family		= AF_INET;
dev->pa_addr	= 0;
dev->pa_brdaddr = 0;
dev->pa_mask	= 0;
dev->pa_alen	= 4;
return;
}
#endif
int ether_config(struct device *dev, struct ifmap *map)
{
if (map->mem_start != (u_long)(-1))
dev->mem_start = map->mem_start;
if (map->mem_end != (u_long)(-1))
dev->mem_end = map->mem_end;
if (map->base_addr != (u_short)(-1))
dev->base_addr = map->base_addr;
if (map->irq != (u_char)(-1))
dev->irq = map->irq;
if (map->dma != (u_char)(-1))
dev->dma = map->dma;
if (map->port != (u_char)(-1))
dev->if_port = map->port;
return 0;
}
int register_netdev(struct device *dev)
{
struct device *d = dev_base;
unsigned long flags;
int i=MAX_ETH_CARDS;
save_flags(flags);
cli();
if (dev && dev->init) {
if (dev->name &&
((dev->name[0] == '\0') || (dev->name[0] == ' '))) {
for (i = 0; i < MAX_ETH_CARDS; ++i)
if (ethdev_index[i] == NULL) {
sprintf(dev->name, "eth%d", i);
ethdev_index[i] = dev;
break;
}
}
sti();
if (dev->init(dev) != 0) {
if (i < MAX_ETH_CARDS) ethdev_index[i] = NULL;
restore_flags(flags);
return -EIO;
}
cli();
if (dev_base) {
while (d->next)
d = d->next;
d->next = dev;
}
else
dev_base = dev;
dev->next = NULL;
}
restore_flags(flags);
return 0;
}
void unregister_netdev(struct device *dev)
{
struct device *d = dev_base;
unsigned long flags;
int i;
save_flags(flags);
cli();
if (dev == NULL)
{
printk("was NULL\n");
restore_flags(flags);
return;
}
if (dev->start)
printk("ERROR '%s' busy and not MOD_IN_USE.\n", dev->name);
#ifdef CONFIG_NET_ALIAS
if (dev_base == dev)
dev_base = net_alias_nextdev(dev);
else
{
while(d && (net_alias_nextdev(d) != dev))
d = net_alias_nextdev(d);
if (d && (net_alias_nextdev(d) == dev))
{
net_alias_nextdev_set(d, net_alias_nextdev(dev));
}
#else
if (dev_base == dev)
dev_base = dev->next;
else
{
while (d && (d->next != dev))
d = d->next;
if (d && (d->next == dev))
{
d->next = dev->next;
}
#endif
else
{
printk("unregister_netdev: '%s' not found\n", dev->name);
restore_flags(flags);
return;
}
}
for (i = 0; i < MAX_ETH_CARDS; ++i)
{
if (ethdev_index[i] == dev)
{
ethdev_index[i] = NULL;
break;
}
}
restore_flags(flags);
dev_close(dev);
}