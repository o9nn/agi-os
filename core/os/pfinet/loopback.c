#include "pfinet.h"
#include <netinet/in.h>
#include <arpa/inet.h>
#include <linux/socket.h>
#include <linux/net.h>
#include <linux/inet.h>
#include <linux/netdevice.h>
#include <linux/etherdevice.h>
#include <linux/skbuff.h>
#include <net/sock.h>
#include <linux/if_ether.h>
#include <linux/if_arp.h>
#define LOOPBACK_MTU	(vm_page_size - 172)
static int loopback_xmit(struct sk_buff *skb, struct device *dev)
{
struct net_device_stats *stats = (struct net_device_stats *)dev->priv;
if (skb == NULL || dev == NULL)
printk(KERN_DEBUG "loopback fed NULL data - splat\n");
if(atomic_read(&skb->users) != 1)
{
struct sk_buff *skb2=skb;
skb=skb_clone(skb, GFP_ATOMIC);
if(skb==NULL) {
kfree_skb(skb2);
return 0;
}
kfree_skb(skb2);
}
else
skb_orphan(skb);
skb->protocol=eth_type_trans(skb,dev);
skb->dev=dev;
#ifndef LOOPBACK_MUST_CHECKSUM
skb->ip_summed = CHECKSUM_UNNECESSARY;
#endif
netif_rx(skb);
stats->rx_bytes+=skb->len;
stats->tx_bytes+=skb->len;
stats->rx_packets++;
stats->tx_packets++;
return(0);
}
static struct net_device_stats *get_stats(struct device *dev)
{
return (struct net_device_stats *)dev->priv;
}
static int loopback_open(struct device *dev)
{
dev->flags|=IFF_LOOPBACK;
return 0;
}
static int loopback_init(struct device *dev)
{
dev->mtu		= LOOPBACK_MTU;
dev->tbusy		= 0;
dev->hard_start_xmit	= loopback_xmit;
dev->hard_header	= eth_header;
dev->hard_header_cache	= eth_header_cache;
dev->header_cache_update= eth_header_cache_update;
dev->hard_header_len	= ETH_HLEN;
dev->addr_len		= ETH_ALEN;
dev->tx_queue_len	= 0;
dev->type		= ARPHRD_LOOPBACK;
dev->rebuild_header	= eth_rebuild_header;
dev->open		= loopback_open;
dev->flags		= IFF_LOOPBACK;
dev->priv = kmalloc(sizeof(struct net_device_stats), GFP_KERNEL);
if (dev->priv == NULL)
return -ENOMEM;
memset(dev->priv, 0, sizeof(struct net_device_stats));
dev->get_stats = get_stats;
dev_init_buffers(dev);
return(0);
}
struct device loopback_dev = { name: "lo", init: &loopback_init, };
struct device *dev_base = &loopback_dev;