#include <linux/types.h>
#include <linux/kernel.h>
#include <linux/sched.h>
#include <linux/string.h>
#include <linux/mm.h>
#include <linux/socket.h>
#include <linux/in.h>
#include <linux/inet.h>
#include <linux/ip.h>
#include <linux/netdevice.h>
#include <linux/etherdevice.h>
#include <linux/skbuff.h>
#include <linux/errno.h>
#include <linux/config.h>
#include <linux/init.h>
#include <net/dst.h>
#include <net/arp.h>
#include <net/sock.h>
#include <net/ipv6.h>
#include <net/ip.h>
#include <asm/uaccess.h>
#include <asm/system.h>
#include <asm/checksum.h>
__initfunc(void eth_setup(char *str, int *ints))
{
struct device *d = dev_base;
if (!str || !*str)
return;
while (d)
{
if (!strcmp(str,d->name))
{
if (ints[0] > 0)
d->irq=ints[1];
if (ints[0] > 1)
d->base_addr=ints[2];
if (ints[0] > 2)
d->mem_start=ints[3];
if (ints[0] > 3)
d->mem_end=ints[4];
break;
}
d=d->next;
}
}
int eth_header(struct sk_buff *skb, struct device *dev, unsigned short type,
void *daddr, void *saddr, unsigned len)
{
struct ethhdr *eth = (struct ethhdr *)skb_push(skb,ETH_HLEN);
if(type!=ETH_P_802_3)
eth->h_proto = htons(type);
else
eth->h_proto = htons(len);
if(saddr)
memcpy(eth->h_source,saddr,dev->addr_len);
else
memcpy(eth->h_source,dev->dev_addr,dev->addr_len);
if (dev->flags & (IFF_LOOPBACK|IFF_NOARP))
{
memset(eth->h_dest, 0, dev->addr_len);
return(dev->hard_header_len);
}
if(daddr)
{
memcpy(eth->h_dest,daddr,dev->addr_len);
return dev->hard_header_len;
}
return -dev->hard_header_len;
}
int eth_rebuild_header(struct sk_buff *skb)
{
struct ethhdr *eth = (struct ethhdr *)skb->data;
struct device *dev = skb->dev;
switch (eth->h_proto)
{
#ifdef CONFIG_INET
case __constant_htons(ETH_P_IP):
return arp_find(eth->h_dest, skb);
#endif
default:
printk(KERN_DEBUG
"%s: unable to resolve type %X addresses.\n",
dev->name, (int)eth->h_proto);
memcpy(eth->h_source, dev->dev_addr, dev->addr_len);
break;
}
return 0;
}
unsigned short eth_type_trans(struct sk_buff *skb, struct device *dev)
{
struct ethhdr *eth;
unsigned char *rawp;
skb->mac.raw=skb->data;
skb_pull(skb,dev->hard_header_len);
eth= skb->mac.ethernet;
if(*eth->h_dest&1)
{
if(memcmp(eth->h_dest,dev->broadcast, ETH_ALEN)==0)
skb->pkt_type=PACKET_BROADCAST;
else
skb->pkt_type=PACKET_MULTICAST;
}
else if(1 )
{
if(memcmp(eth->h_dest,dev->dev_addr, ETH_ALEN))
skb->pkt_type=PACKET_OTHERHOST;
}
if (ntohs(eth->h_proto) >= 1536)
return eth->h_proto;
rawp = skb->data;
if (*(unsigned short *)rawp == 0xFFFF)
return htons(ETH_P_802_3);
return htons(ETH_P_802_2);
}
int eth_header_parse(struct sk_buff *skb, unsigned char *haddr)
{
struct ethhdr *eth = skb->mac.ethernet;
memcpy(haddr, eth->h_source, ETH_ALEN);
return ETH_ALEN;
}
int eth_header_cache(struct neighbour *neigh, struct hh_cache *hh)
{
unsigned short type = hh->hh_type;
struct ethhdr *eth = (struct ethhdr*)(((u8*)hh->hh_data) + 2);
struct device *dev = neigh->dev;
if (type == __constant_htons(ETH_P_802_3))
return -1;
eth->h_proto = type;
memcpy(eth->h_source, dev->dev_addr, dev->addr_len);
memcpy(eth->h_dest, neigh->ha, dev->addr_len);
return 0;
}
void eth_header_cache_update(struct hh_cache *hh, struct device *dev, unsigned char * haddr)
{
memcpy(((u8*)hh->hh_data) + 2, haddr, dev->addr_len);
}
#ifndef CONFIG_IP_ROUTER
void eth_copy_and_sum(struct sk_buff *dest, unsigned char *src, int length, int base)
{
struct ethhdr *eth;
struct iphdr *iph;
int ip_length;
eth=(struct ethhdr *)src;
if(eth->h_proto!=htons(ETH_P_IP))
{
memcpy(dest->data,src,length);
return;
}
memcpy(dest->data,src,sizeof(struct iphdr)+ETH_HLEN);
length -= sizeof(struct iphdr) + ETH_HLEN;
iph=(struct iphdr*)(src+ETH_HLEN);
ip_length = ntohs(iph->tot_len) - sizeof(struct iphdr);
if ((ip_length <= length) && (ip_length > 7))
length=ip_length;
dest->csum=csum_partial_copy(src+sizeof(struct iphdr)+ETH_HLEN,dest->data+sizeof(struct iphdr)+ETH_HLEN,length,base);
dest->ip_summed=1;
}
#endif