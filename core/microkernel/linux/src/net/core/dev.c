#include <asm/segment.h>
#include <asm/system.h>
#include <asm/bitops.h>
#include <linux/config.h>
#include <linux/types.h>
#include <linux/kernel.h>
#include <linux/sched.h>
#include <linux/string.h>
#include <linux/mm.h>
#include <linux/socket.h>
#include <linux/sockios.h>
#include <linux/in.h>
#include <linux/errno.h>
#include <linux/interrupt.h>
#include <linux/if_ether.h>
#include <linux/inet.h>
#include <linux/netdevice.h>
#include <linux/etherdevice.h>
#include <linux/notifier.h>
#include <net/ip.h>
#include <net/route.h>
#include <linux/skbuff.h>
#include <net/sock.h>
#include <net/arp.h>
#include <net/slhc.h>
#include <linux/proc_fs.h>
#include <linux/stat.h>
#include <net/br.h>
#ifdef CONFIG_NET_ALIAS
#include <linux/net_alias.h>
#endif
#ifdef CONFIG_KERNELD
#include <linux/kerneld.h>
#endif
#ifdef CONFIG_NET_RADIO
#include <linux/wireless.h>
#endif
struct packet_type *ptype_base[16];
struct packet_type *ptype_all = NULL;
int dev_lockct=0;
struct notifier_block *netdev_chain=NULL;
static struct sk_buff_head backlog;
static int backlog_size = 0;
static __inline__ unsigned long min(unsigned long a, unsigned long b)
{
return (a < b)? a : b;
}
static int dev_nit=0;
void dev_add_pack(struct packet_type *pt)
{
int hash;
if(pt->type==htons(ETH_P_ALL))
{
dev_nit++;
pt->next=ptype_all;
ptype_all=pt;
}
else
{
hash=ntohs(pt->type)&15;
pt->next = ptype_base[hash];
ptype_base[hash] = pt;
}
}
void dev_remove_pack(struct packet_type *pt)
{
struct packet_type **pt1;
if(pt->type==htons(ETH_P_ALL))
{
dev_nit--;
pt1=&ptype_all;
}
else
pt1=&ptype_base[ntohs(pt->type)&15];
for(; (*pt1)!=NULL; pt1=&((*pt1)->next))
{
if(pt==(*pt1))
{
*pt1=pt->next;
return;
}
}
printk(KERN_WARNING "dev_remove_pack: %p not found.\n", pt);
}
struct device *dev_get(const char *name)
{
struct device *dev;
for (dev = dev_base; dev != NULL; dev = dev->next)
{
if (strcmp(dev->name, name) == 0)
return(dev);
}
return NULL;
}
#ifdef CONFIG_KERNELD
extern __inline__ void dev_load(const char *name)
{
if(!dev_get(name) && suser()) {
#ifdef CONFIG_NET_ALIAS
const char *sptr;
for (sptr=name ; *sptr ; sptr++) if(*sptr==':') break;
if (!(*sptr && *(sptr+1)))
#endif
request_module(name);
}
}
#endif
int dev_open(struct device *dev)
{
int ret = -ENODEV;
if (dev->open)
ret = dev->open(dev);
if (ret == 0)
{
dev->flags |= (IFF_UP | IFF_RUNNING);
dev_mc_upload(dev);
notifier_call_chain(&netdev_chain, NETDEV_UP, dev);
}
return(ret);
}
int dev_close(struct device *dev)
{
int ct=0;
if ((dev->flags & IFF_UP) && dev->stop)
dev->stop(dev);
dev->flags&=~(IFF_UP|IFF_RUNNING);
notifier_call_chain(&netdev_chain, NETDEV_DOWN, dev);
dev_mc_discard(dev);
while(ct<DEV_NUMBUFFS)
{
struct sk_buff *skb;
while((skb=skb_dequeue(&dev->buffs[ct]))!=NULL)
if(skb->free)
kfree_skb(skb,FREE_WRITE);
ct++;
}
return(0);
}
int register_netdevice_notifier(struct notifier_block *nb)
{
return notifier_chain_register(&netdev_chain, nb);
}
int unregister_netdevice_notifier(struct notifier_block *nb)
{
return notifier_chain_unregister(&netdev_chain,nb);
}
static void do_dev_queue_xmit(struct sk_buff *skb, struct device *dev, int pri)
{
unsigned long flags;
struct sk_buff_head *list;
int retransmission = 0;
if(pri>=0 && !skb_device_locked(skb))
skb_device_lock(skb);
#if CONFIG_SKB_CHECK
IS_SKB(skb);
#endif
skb->dev = dev;
if (pri < 0)
{
pri = -pri-1;
retransmission = 1;
}
#ifdef CONFIG_NET_DEBUG
if (pri >= DEV_NUMBUFFS)
{
printk(KERN_WARNING "bad priority in dev_queue_xmit.\n");
pri = 1;
}
#endif
if (!skb->arp && dev->rebuild_header(skb->data, dev, skb->raddr, skb)) {
return;
}
#ifdef CONFIG_NET_ALIAS
if (net_alias_is(dev))
skb->dev = dev = net_alias_dev_tx(dev);
#endif
#ifdef CONFIG_BRIDGE
if(skb->pkt_bridged!=IS_BRIDGED && br_stats.flags & BR_UP)
{
if(br_tx_frame(skb))
return;
}
#endif
list = dev->buffs + pri;
save_flags(flags);
if (!retransmission) {
if (skb_queue_len(list)) {
if (skb_queue_len(list) > dev->tx_queue_len) {
dev_kfree_skb(skb, FREE_WRITE);
return;
}
}
if (dev_nit) {
struct packet_type *ptype;
skb->stamp=xtime;
for (ptype = ptype_all; ptype!=NULL; ptype = ptype->next)
{
if ((ptype->dev == dev || !ptype->dev) &&
((struct sock *)ptype->data != skb->sk))
{
struct sk_buff *skb2;
if ((skb2 = skb_clone(skb, GFP_ATOMIC)) == NULL)
break;
skb2->h.raw = skb2->data + dev->hard_header_len;
if (dev->flags&IFF_SOFTHEADERS)
skb_pull(skb2,skb2->mac.raw-skb2->data);
skb2->mac.raw = skb2->data;
ptype->func(skb2, skb->dev, ptype);
}
}
}
if (skb_queue_len(list)) {
cli();
skb_device_unlock(skb);
__skb_queue_tail(list, skb);
skb = __skb_dequeue(list);
skb_device_lock(skb);
restore_flags(flags);
}
}
if (dev->hard_start_xmit(skb, dev) == 0) {
return;
}
cli();
skb_device_unlock(skb);
__skb_queue_head(list,skb);
restore_flags(flags);
}
void dev_queue_xmit(struct sk_buff *skb, struct device *dev, int pri)
{
start_bh_atomic();
do_dev_queue_xmit(skb, dev, pri);
end_bh_atomic();
}
void netif_rx(struct sk_buff *skb)
{
static int dropping = 0;
skb->sk = NULL;
skb->free = 1;
if(skb->stamp.tv_sec==0)
skb->stamp = xtime;
if (!backlog_size)
dropping = 0;
else if (backlog_size > 300)
dropping = 1;
if (dropping)
{
kfree_skb(skb, FREE_READ);
return;
}
#if CONFIG_SKB_CHECK
IS_SKB(skb);
#endif
skb_queue_tail(&backlog,skb);
backlog_size++;
mark_bh(NET_BH);
return;
}
static void dev_transmit(void)
{
struct device *dev;
for (dev = dev_base; dev != NULL; dev = dev->next)
{
if (dev->flags != 0 && !dev->tbusy) {
dev_tint(dev);
}
}
}
void net_bh(void)
{
struct packet_type *ptype;
struct packet_type *pt_prev;
unsigned short type;
dev_transmit();
while (!skb_queue_empty(&backlog)) {
struct sk_buff * skb = backlog.next;
cli();
__skb_unlink(skb, &backlog);
backlog_size--;
sti();
#ifdef CONFIG_BRIDGE
if (br_stats.flags & BR_UP)
{
int offset=skb->data-skb->mac.raw;
cli();
skb_push(skb,offset);
if(br_receive_frame(skb))
{
sti();
continue;
}
skb_pull(skb,offset);
sti();
}
#endif
skb->h.raw = skb->data;
type = skb->protocol;
pt_prev = NULL;
for (ptype = ptype_all; ptype!=NULL; ptype=ptype->next)
{
if(!ptype->dev || ptype->dev == skb->dev) {
if(pt_prev) {
struct sk_buff *skb2=skb_clone(skb, GFP_ATOMIC);
if(skb2)
pt_prev->func(skb2,skb->dev, pt_prev);
}
pt_prev=ptype;
}
}
for (ptype = ptype_base[ntohs(type)&15]; ptype != NULL; ptype = ptype->next)
{
if (ptype->type == type && (!ptype->dev || ptype->dev==skb->dev))
{
if(pt_prev)
{
struct sk_buff *skb2;
skb2=skb_clone(skb, GFP_ATOMIC);
if(skb2)
pt_prev->func(skb2, skb->dev, pt_prev);
}
pt_prev=ptype;
}
}
if(pt_prev)
pt_prev->func(skb, skb->dev, pt_prev);
else
kfree_skb(skb, FREE_WRITE);
#ifdef XMIT_EVERY
dev_transmit();
#endif
}
#ifdef XMIT_AFTER
dev_transmit();
#endif
}
void dev_tint(struct device *dev)
{
int i;
unsigned long flags;
struct sk_buff_head * head;
#ifdef CONFIG_NET_ALIAS
if (net_alias_is(dev)) return;
#endif
head = dev->buffs;
save_flags(flags);
cli();
for(i = 0;i < DEV_NUMBUFFS; i++,head++)
{
while (!skb_queue_empty(head)) {
struct sk_buff *skb;
skb = head->next;
__skb_unlink(skb, head);
skb_device_lock(skb);
restore_flags(flags);
do_dev_queue_xmit(skb,dev,-i - 1);
if (dev->tbusy)
return;
cli();
}
}
restore_flags(flags);
}
static int dev_ifconf(char *arg)
{
struct ifconf ifc;
struct ifreq ifr;
struct device *dev;
char *pos;
int len;
int err;
err=verify_area(VERIFY_WRITE, arg, sizeof(struct ifconf));
if(err)
return err;
memcpy_fromfs(&ifc, arg, sizeof(struct ifconf));
len = ifc.ifc_len;
pos = ifc.ifc_buf;
err=verify_area(VERIFY_WRITE,pos,len);
if(err)
return err;
for (dev = dev_base; dev != NULL; dev = dev->next)
{
if(!(dev->flags & IFF_UP))
continue;
if (len < sizeof(struct ifreq))
break;
memset(&ifr, 0, sizeof(struct ifreq));
strcpy(ifr.ifr_name, dev->name);
(*(struct sockaddr_in *) &ifr.ifr_addr).sin_family = dev->family;
(*(struct sockaddr_in *) &ifr.ifr_addr).sin_addr.s_addr = dev->pa_addr;
memcpy_tofs(pos, &ifr, sizeof(struct ifreq));
pos += sizeof(struct ifreq);
len -= sizeof(struct ifreq);
}
ifc.ifc_len = (pos - ifc.ifc_buf);
ifc.ifc_req = (struct ifreq *) ifc.ifc_buf;
memcpy_tofs(arg, &ifc, sizeof(struct ifconf));
return(pos - arg);
}
#ifdef CONFIG_PROC_FS
static int sprintf_stats(char *buffer, struct device *dev)
{
struct enet_statistics *stats = (dev->get_stats ? dev->get_stats(dev): NULL);
int size;
if (stats)
size = sprintf(buffer, "%6s:%7d %4d %4d %4d %4d %8d %4d %4d %4d %5d %4d\n",
dev->name,
stats->rx_packets, stats->rx_errors,
stats->rx_dropped + stats->rx_missed_errors,
stats->rx_fifo_errors,
stats->rx_length_errors + stats->rx_over_errors
+ stats->rx_crc_errors + stats->rx_frame_errors,
stats->tx_packets, stats->tx_errors, stats->tx_dropped,
stats->tx_fifo_errors, stats->collisions,
stats->tx_carrier_errors + stats->tx_aborted_errors
+ stats->tx_window_errors + stats->tx_heartbeat_errors);
else
size = sprintf(buffer, "%6s: No statistics available.\n", dev->name);
return size;
}
int dev_get_info(char *buffer, char **start, off_t offset, int length, int dummy)
{
int len=0;
off_t begin=0;
off_t pos=0;
int size;
struct device *dev;
size = sprintf(buffer, "Inter-|   Receive                  |  Transmit\n"
" face |packets errs drop fifo frame|packets errs drop fifo colls carrier\n");
pos+=size;
len+=size;
for (dev = dev_base; dev != NULL; dev = dev->next)
{
size = sprintf_stats(buffer+len, dev);
len+=size;
pos=begin+len;
if(pos<offset)
{
len=0;
begin=pos;
}
if(pos>offset+length)
break;
}
*start=buffer+(offset-begin);
len-=(offset-begin);
if(len>length)
len=length;
return len;
}
#endif
#ifdef CONFIG_NET_RADIO
#ifdef CONFIG_PROC_FS
static int
sprintf_wireless_stats(char *		buffer,
struct device *	dev)
{
struct iw_statistics *stats = (dev->get_wireless_stats ?
dev->get_wireless_stats(dev) :
(struct iw_statistics *) NULL);
int size;
if(stats != (struct iw_statistics *) NULL)
size = sprintf(buffer,
"%6s: %02x  %3d%c %3d%c  %3d%c %5d %5d %5d\n",
dev->name,
stats->status,
stats->qual.qual,
stats->qual.updated & 1 ? '.' : ' ',
stats->qual.level,
stats->qual.updated & 2 ? '.' : ' ',
stats->qual.noise,
stats->qual.updated & 3 ? '.' : ' ',
stats->discard.nwid,
stats->discard.code,
stats->discard.misc);
else
size = 0;
return size;
}
int
dev_get_wireless_info(char *	buffer,
char **	start,
off_t	offset,
int	length,
int	dummy)
{
int		len = 0;
off_t		begin = 0;
off_t		pos = 0;
int		size;
struct device *	dev;
size = sprintf(buffer,
"Inter-|sta|  Quality       |  Discarded packets\n"
" face |tus|link level noise| nwid crypt  misc\n");
pos+=size;
len+=size;
for(dev = dev_base; dev != NULL; dev = dev->next)
{
size = sprintf_wireless_stats(buffer+len, dev);
len+=size;
pos=begin+len;
if(pos < offset)
{
len=0;
begin=pos;
}
if(pos > offset + length)
break;
}
*start = buffer + (offset - begin);
len -= (offset - begin);
if(len > length)
len = length;
return len;
}
#endif
#endif
static inline int bad_mask(unsigned long mask, unsigned long addr)
{
if (addr & (mask = ~mask))
return 1;
mask = ntohl(mask);
if (mask & (mask+1))
return 1;
return 0;
}
static int dev_ifsioc(void *arg, unsigned int getset)
{
struct ifreq ifr;
struct device *dev;
int ret;
int err=verify_area(VERIFY_WRITE, arg, sizeof(struct ifreq));
if(err)
return err;
memcpy_fromfs(&ifr, arg, sizeof(struct ifreq));
#ifdef CONFIG_KERNELD
dev_load(ifr.ifr_name);
#endif
#ifdef CONFIG_NET_ALIAS
if ((dev = net_alias_dev_get(ifr.ifr_name, getset == SIOCSIFADDR, &err, NULL, NULL)) == NULL)
return(err);
#else
if ((dev = dev_get(ifr.ifr_name)) == NULL)
return(-ENODEV);
#endif
switch(getset)
{
case SIOCGIFFLAGS:
ifr.ifr_flags = (dev->flags & ~IFF_SOFTHEADERS);
goto rarok;
case SIOCSIFFLAGS:
{
int old_flags = dev->flags;
if(securelevel>0)
ifr.ifr_flags&=~IFF_PROMISC;
dev_lock_wait();
dev->flags = (ifr.ifr_flags & (
IFF_BROADCAST | IFF_DEBUG | IFF_LOOPBACK |
IFF_POINTOPOINT | IFF_NOTRAILERS | IFF_RUNNING |
IFF_NOARP | IFF_PROMISC | IFF_ALLMULTI | IFF_SLAVE | IFF_MASTER
| IFF_MULTICAST)) | (dev->flags & (IFF_SOFTHEADERS|IFF_UP));
dev_mc_upload(dev);
if ((old_flags^ifr.ifr_flags)&IFF_UP)
{
if(old_flags&IFF_UP)
ret=dev_close(dev);
else
{
ret=dev_open(dev);
if(ret<0)
dev->flags&=~IFF_UP;
}
}
else
ret=0;
dev_mc_upload(dev);
}
break;
case SIOCGIFADDR:
if(ifr.ifr_addr.sa_family==AF_UNSPEC)
{
memcpy(ifr.ifr_hwaddr.sa_data,dev->dev_addr, MAX_ADDR_LEN);
ifr.ifr_hwaddr.sa_family=dev->type;
goto rarok;
}
else
{
(*(struct sockaddr_in *)
&ifr.ifr_addr).sin_addr.s_addr = dev->pa_addr;
(*(struct sockaddr_in *)
&ifr.ifr_addr).sin_family = dev->family;
(*(struct sockaddr_in *)
&ifr.ifr_addr).sin_port = 0;
}
goto rarok;
case SIOCSIFADDR:
if(ifr.ifr_addr.sa_family==AF_UNSPEC)
{
if(dev->set_mac_address==NULL)
return -EOPNOTSUPP;
if(securelevel>0)
return -EPERM;
ret=dev->set_mac_address(dev,&ifr.ifr_addr);
}
else
{
u32 new_pa_addr = (*(struct sockaddr_in *)
&ifr.ifr_addr).sin_addr.s_addr;
u16 new_family = ifr.ifr_addr.sa_family;
if (new_family == dev->family &&
new_pa_addr == dev->pa_addr) {
ret =0;
break;
}
if (dev->flags & IFF_UP)
notifier_call_chain(&netdev_chain, NETDEV_DOWN, dev);
#ifdef CONFIG_NET_ALIAS
if (net_alias_is(dev))
net_alias_dev_rehash(dev ,&ifr.ifr_addr);
#endif
dev->pa_addr = new_pa_addr;
dev->family = new_family;
#ifdef CONFIG_INET
if (!dev->pa_mask)
dev->pa_mask = ip_get_mask(dev->pa_addr);
#endif
if (!dev->pa_brdaddr)
dev->pa_brdaddr = dev->pa_addr | ~dev->pa_mask;
if (dev->flags & IFF_UP)
notifier_call_chain(&netdev_chain, NETDEV_UP, dev);
ret = 0;
}
break;
case SIOCGIFBRDADDR:
(*(struct sockaddr_in *)
&ifr.ifr_broadaddr).sin_addr.s_addr = dev->pa_brdaddr;
(*(struct sockaddr_in *)
&ifr.ifr_broadaddr).sin_family = dev->family;
(*(struct sockaddr_in *)
&ifr.ifr_broadaddr).sin_port = 0;
goto rarok;
case SIOCSIFBRDADDR:
dev->pa_brdaddr = (*(struct sockaddr_in *)
&ifr.ifr_broadaddr).sin_addr.s_addr;
ret = 0;
break;
case SIOCGIFDSTADDR:
(*(struct sockaddr_in *)
&ifr.ifr_dstaddr).sin_addr.s_addr = dev->pa_dstaddr;
(*(struct sockaddr_in *)
&ifr.ifr_dstaddr).sin_family = dev->family;
(*(struct sockaddr_in *)
&ifr.ifr_dstaddr).sin_port = 0;
goto rarok;
case SIOCSIFDSTADDR:
dev->pa_dstaddr = (*(struct sockaddr_in *)
&ifr.ifr_dstaddr).sin_addr.s_addr;
ret = 0;
break;
case SIOCGIFNETMASK:
(*(struct sockaddr_in *)
&ifr.ifr_netmask).sin_addr.s_addr = dev->pa_mask;
(*(struct sockaddr_in *)
&ifr.ifr_netmask).sin_family = dev->family;
(*(struct sockaddr_in *)
&ifr.ifr_netmask).sin_port = 0;
goto rarok;
case SIOCSIFNETMASK:
{
unsigned long mask = (*(struct sockaddr_in *)
&ifr.ifr_netmask).sin_addr.s_addr;
ret = -EINVAL;
if (bad_mask(mask,0))
break;
dev->pa_mask = mask;
ret = 0;
}
break;
case SIOCGIFMETRIC:
ifr.ifr_metric = dev->metric;
goto  rarok;
case SIOCSIFMETRIC:
dev->metric = ifr.ifr_metric;
ret=0;
break;
case SIOCGIFMTU:
ifr.ifr_mtu = dev->mtu;
goto rarok;
case SIOCSIFMTU:
if (dev->change_mtu)
ret = dev->change_mtu(dev, ifr.ifr_mtu);
else
{
if(ifr.ifr_mtu<68)
return -EINVAL;
dev->mtu = ifr.ifr_mtu;
ret = 0;
}
break;
case SIOCGIFMEM:
ret = -EINVAL;
break;
case SIOCSIFMEM:
ret = -EINVAL;
break;
case SIOCGIFHWADDR:
memcpy(ifr.ifr_hwaddr.sa_data,dev->dev_addr, MAX_ADDR_LEN);
ifr.ifr_hwaddr.sa_family=dev->type;
goto rarok;
case SIOCSIFHWADDR:
if(dev->set_mac_address==NULL)
return -EOPNOTSUPP;
if(securelevel > 0)
return -EPERM;
if(ifr.ifr_hwaddr.sa_family!=dev->type)
return -EINVAL;
ret=dev->set_mac_address(dev,&ifr.ifr_hwaddr);
break;
case SIOCGIFMAP:
ifr.ifr_map.mem_start=dev->mem_start;
ifr.ifr_map.mem_end=dev->mem_end;
ifr.ifr_map.base_addr=dev->base_addr;
ifr.ifr_map.irq=dev->irq;
ifr.ifr_map.dma=dev->dma;
ifr.ifr_map.port=dev->if_port;
goto rarok;
case SIOCSIFMAP:
if(dev->set_config==NULL)
return -EOPNOTSUPP;
return dev->set_config(dev,&ifr.ifr_map);
case SIOCADDMULTI:
if(dev->set_multicast_list==NULL)
return -EINVAL;
if(ifr.ifr_hwaddr.sa_family!=AF_UNSPEC)
return -EINVAL;
dev_mc_add(dev,ifr.ifr_hwaddr.sa_data, dev->addr_len, 1);
return 0;
case SIOCDELMULTI:
if(dev->set_multicast_list==NULL)
return -EINVAL;
if(ifr.ifr_hwaddr.sa_family!=AF_UNSPEC)
return -EINVAL;
dev_mc_delete(dev,ifr.ifr_hwaddr.sa_data,dev->addr_len, 1);
return 0;
default:
if((getset >= SIOCDEVPRIVATE) &&
(getset <= (SIOCDEVPRIVATE + 15))) {
if(dev->do_ioctl==NULL)
return -EOPNOTSUPP;
ret=dev->do_ioctl(dev, &ifr, getset);
memcpy_tofs(arg,&ifr,sizeof(struct ifreq));
break;
}
#ifdef CONFIG_NET_RADIO
if((getset >= SIOCIWFIRST) &&
(getset <= SIOCIWLAST))
{
if(dev->do_ioctl==NULL)
return -EOPNOTSUPP;
ret=dev->do_ioctl(dev, &ifr, getset);
if(IW_IS_GET(getset))
memcpy_tofs(arg, &ifr,
sizeof(struct ifreq));
break;
}
#endif
ret = -EINVAL;
}
return(ret);
rarok:
memcpy_tofs(arg, &ifr, sizeof(struct ifreq));
return 0;
}
int dev_ioctl(unsigned int cmd, void *arg)
{
switch(cmd)
{
case SIOCGIFCONF:
(void) dev_ifconf((char *) arg);
return 0;
case SIOCGIFFLAGS:
case SIOCGIFADDR:
case SIOCGIFDSTADDR:
case SIOCGIFBRDADDR:
case SIOCGIFNETMASK:
case SIOCGIFMETRIC:
case SIOCGIFMTU:
case SIOCGIFMEM:
case SIOCGIFHWADDR:
case SIOCGIFSLAVE:
case SIOCGIFMAP:
return dev_ifsioc(arg, cmd);
case SIOCSIFFLAGS:
case SIOCSIFADDR:
case SIOCSIFDSTADDR:
case SIOCSIFBRDADDR:
case SIOCSIFNETMASK:
case SIOCSIFMETRIC:
case SIOCSIFMTU:
case SIOCSIFMEM:
case SIOCSIFHWADDR:
case SIOCSIFMAP:
case SIOCSIFSLAVE:
case SIOCADDMULTI:
case SIOCDELMULTI:
if (!suser())
return -EPERM;
return dev_ifsioc(arg, cmd);
case SIOCSIFLINK:
return -EINVAL;
default:
if((cmd >= SIOCDEVPRIVATE) &&
(cmd <= (SIOCDEVPRIVATE + 15))) {
return dev_ifsioc(arg, cmd);
}
#ifdef CONFIG_NET_RADIO
if((cmd >= SIOCIWFIRST) &&
(cmd <= SIOCIWLAST))
{
if((IW_IS_SET(cmd)) && (!suser()))
return -EPERM;
return dev_ifsioc(arg, cmd);
}
#endif
return -EINVAL;
}
}
extern int lance_init(void);
extern int pi_init(void);
extern int pt_init(void);
extern int bpq_init(void);
extern void sdla_setup(void);
extern int dlci_setup(void);
extern int sm_init(void);
extern int baycom_init(void);
int net_dev_init(void)
{
struct device *dev, **dp;
skb_queue_head_init(&backlog);
#ifdef CONFIG_BRIDGE
br_init();
#endif
#if defined(CONFIG_PI)
pi_init();
#endif
#if defined(CONFIG_PT)
pt_init();
#endif
#if defined(CONFIG_BPQETHER)
bpq_init();
#endif
#if defined(CONFIG_DLCI)
dlci_setup();
#endif
#if defined(CONFIG_SDLA)
sdla_setup();
#endif
#if defined(CONFIG_BAYCOM)
baycom_init();
#endif
#if defined(CONFIG_SOUNDMODEM)
sm_init();
#endif
#if (defined(CONFIG_SLIP) && defined(CONFIG_SLIP_COMPRESSED)) \
|| defined(CONFIG_PPP) \
|| (defined(CONFIG_ISDN) && defined(CONFIG_ISDN_PPP))
slhc_install();
#endif
dp = &dev_base;
while ((dev = *dp) != NULL)
{
int i;
for (i = 0; i < DEV_NUMBUFFS; i++)  {
skb_queue_head_init(dev->buffs + i);
}
if (dev->init && dev->init(dev))
{
*dp = dev->next;
}
else
{
dp = &dev->next;
}
}
#ifdef CONFIG_PROC_FS
proc_net_register(&(struct proc_dir_entry) {
PROC_NET_DEV, 3, "dev",
S_IFREG | S_IRUGO, 1, 0, 0,
0, &proc_net_inode_operations,
dev_get_info
});
#endif
#ifdef CONFIG_NET_RADIO
#ifdef CONFIG_PROC_FS
proc_net_register(&(struct proc_dir_entry) {
PROC_NET_WIRELESS, 8, "wireless",
S_IFREG | S_IRUGO, 1, 0, 0,
0, &proc_net_inode_operations,
dev_get_wireless_info
});
#endif
#endif
#ifdef CONFIG_NET_ALIAS
net_alias_init();
#endif
init_bh(NET_BH, net_bh);
return 0;
}