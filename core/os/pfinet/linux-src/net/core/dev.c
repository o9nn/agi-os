#include <asm/uaccess.h>
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
#include <linux/errno.h>
#include <linux/interrupt.h>
#include <linux/if_ether.h>
#include <linux/netdevice.h>
#include <linux/etherdevice.h>
#include <linux/notifier.h>
#include <linux/skbuff.h>
#include <net/sock.h>
#include <linux/rtnetlink.h>
#include <net/slhc.h>
#include <linux/proc_fs.h>
#include <linux/stat.h>
#include <net/br.h>
#include <net/dst.h>
#include <net/pkt_sched.h>
#include <net/profile.h>
#include <linux/init.h>
#include <linux/kmod.h>
#ifdef CONFIG_NET_RADIO
#include <linux/wireless.h>
#endif
#ifdef CONFIG_PLIP
extern int plip_init(void);
#endif
NET_PROFILE_DEFINE(dev_queue_xmit)
NET_PROFILE_DEFINE(net_bh)
NET_PROFILE_DEFINE(net_bh_skb)
const char *if_port_text[] = {
"unknown",
"BNC",
"10baseT",
"AUI",
"100baseT",
"100baseTX",
"100baseFX"
};
struct packet_type *ptype_base[16];
struct packet_type *ptype_all = NULL;
atomic_t dev_lockct = ATOMIC_INIT(0);
#ifdef _HURD_
struct notifier_block *netdev_chain=NULL;
#else
static struct notifier_block *netdev_chain=NULL;
#endif
static struct sk_buff_head backlog;
#ifdef CONFIG_NET_FASTROUTE
int netdev_fastroute;
int netdev_fastroute_obstacles;
struct net_fastroute_stats dev_fastroute_stat;
#endif
static void dev_clear_backlog(struct device *dev);
int netdev_nit=0;
void dev_add_pack(struct packet_type *pt)
{
int hash;
#ifdef CONFIG_NET_FASTROUTE
if (pt->data) {
netdev_fastroute_obstacles++;
dev_clear_fastroute(pt->dev);
}
#endif
if(pt->type==htons(ETH_P_ALL))
{
netdev_nit++;
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
netdev_nit--;
pt1=&ptype_all;
}
else
pt1=&ptype_base[ntohs(pt->type)&15];
for(; (*pt1)!=NULL; pt1=&((*pt1)->next))
{
if(pt==(*pt1))
{
*pt1=pt->next;
synchronize_bh();
#ifdef CONFIG_NET_FASTROUTE
if (pt->data)
netdev_fastroute_obstacles--;
#endif
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
struct device * dev_get_by_index(int ifindex)
{
struct device *dev;
for (dev = dev_base; dev != NULL; dev = dev->next)
{
if (dev->ifindex == ifindex)
return(dev);
}
return NULL;
}
struct device *dev_getbyhwaddr(unsigned short type, char *ha)
{
struct device *dev;
for (dev = dev_base; dev != NULL; dev = dev->next)
{
if (dev->type == type &&
memcmp(dev->dev_addr, ha, dev->addr_len) == 0)
return(dev);
}
return(NULL);
}
int dev_alloc_name(struct device *dev, const char *name)
{
int i;
for(i=0;i<100;i++)
{
sprintf(dev->name,name,i);
if(dev_get(dev->name)==NULL)
return i;
}
return -ENFILE;
}
struct device *dev_alloc(const char *name, int *err)
{
struct device *dev=kmalloc(sizeof(struct device)+16, GFP_KERNEL);
if(dev==NULL)
{
*err=-ENOBUFS;
return NULL;
}
dev->name=(char *)(dev+1);
*err=dev_alloc_name(dev,name);
if(*err<0)
{
kfree(dev);
return NULL;
}
return dev;
}
void netdev_state_change(struct device *dev)
{
if (dev->flags&IFF_UP)
notifier_call_chain(&netdev_chain, NETDEV_CHANGE, dev);
}
#ifdef CONFIG_KMOD
void dev_load(const char *name)
{
if(!dev_get(name) && capable(CAP_SYS_MODULE))
request_module(name);
}
#else
extern inline void dev_load(const char *unused){;}
#endif
static int default_rebuild_header(struct sk_buff *skb)
{
printk(KERN_DEBUG "%s: default_rebuild_header called -- BUG!\n", skb->dev ? skb->dev->name : "NULL!!!");
kfree_skb(skb);
return 1;
}
int dev_open(struct device *dev)
{
int ret = 0;
if (dev->flags&IFF_UP)
return 0;
if (dev->open)
ret = dev->open(dev);
if (ret == 0)
{
if (dev->rebuild_header == NULL)
dev->rebuild_header = default_rebuild_header;
dev->flags |= (IFF_UP | IFF_RUNNING);
dev_mc_upload(dev);
dev_activate(dev);
notifier_call_chain(&netdev_chain, NETDEV_UP, dev);
}
return(ret);
}
#ifdef CONFIG_NET_FASTROUTE
static __inline__ void dev_do_clear_fastroute(struct device *dev)
{
if (dev->accept_fastpath) {
int i;
for (i=0; i<=NETDEV_FASTROUTE_HMASK; i++)
dst_release_irqwait(xchg(dev->fastpath+i, NULL));
}
}
void dev_clear_fastroute(struct device *dev)
{
if (dev) {
dev_do_clear_fastroute(dev);
} else {
for (dev = dev_base; dev; dev = dev->next)
dev_do_clear_fastroute(dev);
}
}
#endif
int dev_close(struct device *dev)
{
if (!(dev->flags&IFF_UP))
return 0;
dev_deactivate(dev);
dev_lock_wait();
if (dev->stop)
dev->stop(dev);
if (dev->start)
printk("dev_close: bug %s still running\n", dev->name);
dev_clear_backlog(dev);
dev->flags&=~(IFF_UP|IFF_RUNNING);
#ifdef CONFIG_NET_FASTROUTE
dev_clear_fastroute(dev);
#endif
notifier_call_chain(&netdev_chain, NETDEV_DOWN, dev);
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
void dev_queue_xmit_nit(struct sk_buff *skb, struct device *dev)
{
struct packet_type *ptype;
get_fast_time(&skb->stamp);
for (ptype = ptype_all; ptype!=NULL; ptype = ptype->next)
{
if ((ptype->dev == dev || !ptype->dev) &&
((struct sock *)ptype->data != skb->sk))
{
struct sk_buff *skb2;
if ((skb2 = skb_clone(skb, GFP_ATOMIC)) == NULL)
break;
skb2->mac.raw = skb2->data;
if (skb2->nh.raw < skb2->data || skb2->nh.raw >= skb2->tail) {
if (net_ratelimit())
printk(KERN_DEBUG "protocol %04x is buggy, dev %s\n", skb2->protocol, dev->name);
skb2->nh.raw = skb2->data;
if (dev->hard_header)
skb2->nh.raw += dev->hard_header_len;
}
skb2->h.raw = skb2->nh.raw;
skb2->pkt_type = PACKET_OUTGOING;
ptype->func(skb2, skb->dev, ptype);
}
}
}
void dev_loopback_xmit(struct sk_buff *skb)
{
struct sk_buff *newskb=skb_clone(skb, GFP_ATOMIC);
if (newskb==NULL)
return;
newskb->mac.raw = newskb->data;
skb_pull(newskb, newskb->nh.raw - newskb->data);
newskb->pkt_type = PACKET_LOOPBACK;
newskb->ip_summed = CHECKSUM_UNNECESSARY;
if (newskb->dst==NULL)
printk(KERN_DEBUG "BUG: packet without dst looped back 1\n");
netif_rx(newskb);
}
int dev_queue_xmit(struct sk_buff *skb)
{
struct device *dev = skb->dev;
struct Qdisc *q;
#ifdef CONFIG_NET_PROFILE
start_bh_atomic();
NET_PROFILE_ENTER(dev_queue_xmit);
#endif
start_bh_atomic();
q = dev->qdisc;
if (q->enqueue) {
q->enqueue(skb, q);
qdisc_wakeup(dev);
end_bh_atomic();
#ifdef CONFIG_NET_PROFILE
NET_PROFILE_LEAVE(dev_queue_xmit);
end_bh_atomic();
#endif
return 0;
}
if (dev->flags&IFF_UP) {
if (netdev_nit)
dev_queue_xmit_nit(skb,dev);
if (dev->hard_start_xmit(skb, dev) == 0) {
end_bh_atomic();
#ifdef CONFIG_NET_PROFILE
NET_PROFILE_LEAVE(dev_queue_xmit);
end_bh_atomic();
#endif
return 0;
}
if (net_ratelimit())
printk(KERN_DEBUG "Virtual device %s asks to queue packet!\n", dev->name);
}
end_bh_atomic();
kfree_skb(skb);
#ifdef CONFIG_NET_PROFILE
NET_PROFILE_LEAVE(dev_queue_xmit);
end_bh_atomic();
#endif
return 0;
}
int netdev_dropping = 0;
int netdev_max_backlog = 300;
atomic_t netdev_rx_dropped;
#ifdef CONFIG_CPU_IS_SLOW
int net_cpu_congestion;
#endif
#ifdef CONFIG_NET_HW_FLOWCONTROL
int netdev_throttle_events;
static unsigned long netdev_fc_mask = 1;
unsigned long netdev_fc_xoff = 0;
static struct
{
void (*stimul)(struct device *);
struct device *dev;
} netdev_fc_slots[32];
int netdev_register_fc(struct device *dev, void (*stimul)(struct device *dev))
{
int bit = 0;
unsigned long flags;
save_flags(flags);
cli();
if (netdev_fc_mask != ~0UL) {
bit = ffz(netdev_fc_mask);
netdev_fc_slots[bit].stimul = stimul;
netdev_fc_slots[bit].dev = dev;
set_bit(bit, &netdev_fc_mask);
clear_bit(bit, &netdev_fc_xoff);
}
restore_flags(flags);
return bit;
}
void netdev_unregister_fc(int bit)
{
unsigned long flags;
save_flags(flags);
cli();
if (bit > 0) {
netdev_fc_slots[bit].stimul = NULL;
netdev_fc_slots[bit].dev = NULL;
clear_bit(bit, &netdev_fc_mask);
clear_bit(bit, &netdev_fc_xoff);
}
restore_flags(flags);
}
static void netdev_wakeup(void)
{
unsigned long xoff;
cli();
xoff = netdev_fc_xoff;
netdev_fc_xoff = 0;
netdev_dropping = 0;
netdev_throttle_events++;
while (xoff) {
int i = ffz(~xoff);
xoff &= ~(1<<i);
netdev_fc_slots[i].stimul(netdev_fc_slots[i].dev);
}
sti();
}
#endif
static void dev_clear_backlog(struct device *dev)
{
struct sk_buff *curr;
unsigned long flags;
if (backlog.qlen) {
repeat:
spin_lock_irqsave(&skb_queue_lock, flags);
for (curr = backlog.next;
curr != (struct sk_buff *)(&backlog);
curr = curr->next)
if (curr->dev == dev)
{
__skb_unlink(curr, &backlog);
spin_unlock_irqrestore(&skb_queue_lock, flags);
kfree_skb(curr);
goto repeat;
}
spin_unlock_irqrestore(&skb_queue_lock, flags);
#ifdef CONFIG_NET_HW_FLOWCONTROL
if (netdev_dropping)
netdev_wakeup();
#else
netdev_dropping = 0;
#endif
}
}
void netif_rx(struct sk_buff *skb)
{
#ifndef CONFIG_CPU_IS_SLOW
if(skb->stamp.tv_sec==0)
get_fast_time(&skb->stamp);
#else
skb->stamp = xtime;
#endif
if (backlog.qlen <= netdev_max_backlog) {
if (backlog.qlen) {
if (netdev_dropping == 0) {
skb_queue_tail(&backlog,skb);
mark_bh(NET_BH);
return;
}
atomic_inc(&netdev_rx_dropped);
kfree_skb(skb);
return;
}
#ifdef CONFIG_NET_HW_FLOWCONTROL
if (netdev_dropping)
netdev_wakeup();
#else
netdev_dropping = 0;
#endif
skb_queue_tail(&backlog,skb);
mark_bh(NET_BH);
return;
}
netdev_dropping = 1;
atomic_inc(&netdev_rx_dropped);
kfree_skb(skb);
}
#ifdef CONFIG_BRIDGE
static inline void handle_bridge(struct sk_buff *skb, unsigned short type)
{
if ((br_stats.flags & BR_UP) && br_call_bridge(skb, type))
{
int offset;
skb=skb_clone(skb, GFP_ATOMIC);
if(skb==NULL)
return;
offset=skb->data-skb->mac.raw;
skb_push(skb,offset);
if(br_receive_frame(skb))
return;
kfree_skb(skb);
}
return;
}
#endif
void net_bh(void)
{
struct packet_type *ptype;
struct packet_type *pt_prev;
unsigned short type;
#ifndef _HURD_
unsigned long start_time = jiffies;
#ifdef CONFIG_CPU_IS_SLOW
static unsigned long start_busy = 0;
static unsigned long ave_busy = 0;
if (start_busy == 0)
start_busy = start_time;
net_cpu_congestion = ave_busy>>8;
#endif
#endif
NET_PROFILE_ENTER(net_bh);
if (qdisc_head.forw != &qdisc_head)
qdisc_run_queues();
while (!skb_queue_empty(&backlog))
{
struct sk_buff * skb;
#ifndef _HURD_
if (jiffies - start_time > 1)
goto net_bh_break;
#endif
skb = skb_dequeue(&backlog);
#ifndef _HURD_
#ifdef CONFIG_CPU_IS_SLOW
if (ave_busy > 128*16) {
kfree_skb(skb);
while ((skb = skb_dequeue(&backlog)) != NULL)
kfree_skb(skb);
break;
}
#endif
#endif
#if 0
NET_PROFILE_SKB_PASSED(skb, net_bh_skb);
#endif
#ifdef CONFIG_NET_FASTROUTE
if (skb->pkt_type == PACKET_FASTROUTE) {
dev_queue_xmit(skb);
continue;
}
#endif
skb->h.raw = skb->nh.raw = skb->data;
if (skb->mac.raw < skb->head || skb->mac.raw > skb->data) {
printk(KERN_CRIT "%s: wrong mac.raw ptr, proto=%04x\n", skb->dev->name, skb->protocol);
kfree_skb(skb);
continue;
}
type = skb->protocol;
#ifdef CONFIG_BRIDGE
handle_bridge(skb, type);
#endif
pt_prev = NULL;
for (ptype = ptype_all; ptype!=NULL; ptype=ptype->next)
{
if (!ptype->dev || ptype->dev == skb->dev) {
if(pt_prev)
{
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
else {
kfree_skb(skb);
}
}
if (qdisc_head.forw != &qdisc_head)
qdisc_run_queues();
#ifndef _HURD_
#ifdef CONFIG_CPU_IS_SLOW
if (1) {
unsigned long start_idle = jiffies;
ave_busy += ((start_idle - start_busy)<<3) - (ave_busy>>4);
start_busy = 0;
}
#endif
#endif
#ifdef CONFIG_NET_HW_FLOWCONTROL
if (netdev_dropping)
netdev_wakeup();
#else
netdev_dropping = 0;
#endif
NET_PROFILE_LEAVE(net_bh);
return;
#ifndef _HURD_
net_bh_break:
mark_bh(NET_BH);
NET_PROFILE_LEAVE(net_bh);
return;
#endif
}
static gifconf_func_t * gifconf_list [NPROTO];
int register_gifconf(unsigned int family, gifconf_func_t * gifconf)
{
if (family>=NPROTO)
return -EINVAL;
gifconf_list[family] = gifconf;
return 0;
}
static int dev_ifname(struct ifreq *arg)
{
struct device *dev;
struct ifreq ifr;
int err;
err = copy_from_user(&ifr, arg, sizeof(struct ifreq));
if (err)
return -EFAULT;
dev = dev_get_by_index(ifr.ifr_ifindex);
if (!dev)
return -ENODEV;
strcpy(ifr.ifr_name, dev->name);
err = copy_to_user(arg, &ifr, sizeof(struct ifreq));
return (err)?-EFAULT:0;
}
#ifdef _HURD_
int dev_ifconf(char *arg)
#else
static int dev_ifconf(char *arg)
#endif
{
struct ifconf ifc;
struct device *dev;
char *pos;
int len;
int total;
int i;
if (copy_from_user(&ifc, arg, sizeof(struct ifconf)))
return -EFAULT;
pos = ifc.ifc_buf;
len = ifc.ifc_len;
total = 0;
for (dev = dev_base; dev != NULL; dev = dev->next) {
for (i=0; i<NPROTO; i++) {
if (gifconf_list[i]) {
int done;
if (pos==NULL) {
done = gifconf_list[i](dev, NULL, 0);
} else {
done = gifconf_list[i](dev, pos+total, len-total);
}
if (done<0)
return -EFAULT;
total += done;
}
}
}
ifc.ifc_len = total;
if (copy_to_user(arg, &ifc, sizeof(struct ifconf)))
return -EFAULT;
return 0;
}
#ifdef CONFIG_PROC_FS
static int sprintf_stats(char *buffer, struct device *dev)
{
struct net_device_stats *stats = (dev->get_stats ? dev->get_stats(dev): NULL);
int size;
if (stats)
size = sprintf(buffer, "%6s:%8lu %7lu %4lu %4lu %4lu %5lu %10lu %9lu %8lu %7lu %4lu %4lu %4lu %5lu %7lu %10lu\n",
dev->name,
stats->rx_bytes,
stats->rx_packets, stats->rx_errors,
stats->rx_dropped + stats->rx_missed_errors,
stats->rx_fifo_errors,
stats->rx_length_errors + stats->rx_over_errors
+ stats->rx_crc_errors + stats->rx_frame_errors,
stats->rx_compressed, stats->multicast,
stats->tx_bytes,
stats->tx_packets, stats->tx_errors, stats->tx_dropped,
stats->tx_fifo_errors, stats->collisions,
stats->tx_carrier_errors + stats->tx_aborted_errors
+ stats->tx_window_errors + stats->tx_heartbeat_errors,
stats->tx_compressed);
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
size = sprintf(buffer,
"Inter-|   Receive                                                |  Transmit\n"
" face |bytes    packets errs drop fifo frame compressed multicast|bytes    packets errs drop fifo colls carrier compressed\n");
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
static int dev_proc_stats(char *buffer, char **start, off_t offset,
int length, int *eof, void *data)
{
int len;
len = sprintf(buffer, "%08x %08x %08x %08x %08x\n",
atomic_read(&netdev_rx_dropped),
#ifdef CONFIG_NET_HW_FLOWCONTROL
netdev_throttle_events,
#else
0,
#endif
#ifdef CONFIG_NET_FASTROUTE
dev_fastroute_stat.hits,
dev_fastroute_stat.succeed,
dev_fastroute_stat.deferred
#else
0, 0, 0
#endif
);
len -= offset;
if (len > length)
len = length;
if(len < 0)
len = 0;
*start = buffer + offset;
*eof = 1;
return len;
}
#endif
#ifdef CONFIG_NET_RADIO
#ifdef CONFIG_PROC_FS
static int sprintf_wireless_stats(char *buffer, struct device *dev)
{
struct iw_statistics *stats = (dev->get_wireless_stats ?
dev->get_wireless_stats(dev) :
(struct iw_statistics *) NULL);
int size;
if(stats != (struct iw_statistics *) NULL)
{
size = sprintf(buffer,
"%6s: %04x  %3d%c  %3d%c  %3d%c  %6d %6d %6d\n",
dev->name,
stats->status,
stats->qual.qual,
stats->qual.updated & 1 ? '.' : ' ',
stats->qual.level,
stats->qual.updated & 2 ? '.' : ' ',
stats->qual.noise,
stats->qual.updated & 4 ? '.' : ' ',
stats->discard.nwid,
stats->discard.code,
stats->discard.misc);
stats->qual.updated = 0;
}
else
size = 0;
return size;
}
int dev_get_wireless_info(char * buffer, char **start, off_t offset,
int length, int dummy)
{
int len = 0;
off_t begin = 0;
off_t pos = 0;
int size;
struct device * dev;
size = sprintf(buffer,
"Inter-| sta-|   Quality        |   Discarded packets\n"
" face | tus | link level noise |  nwid  crypt   misc\n"
);
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
void dev_set_promiscuity(struct device *dev, int inc)
{
unsigned short old_flags = dev->flags;
dev->flags |= IFF_PROMISC;
if ((dev->promiscuity += inc) == 0)
dev->flags &= ~IFF_PROMISC;
if (dev->flags^old_flags) {
#ifdef CONFIG_NET_FASTROUTE
if (dev->flags&IFF_PROMISC) {
netdev_fastroute_obstacles++;
dev_clear_fastroute(dev);
} else
netdev_fastroute_obstacles--;
#endif
dev_mc_upload(dev);
printk(KERN_INFO "device %s %s promiscuous mode\n",
dev->name, (dev->flags&IFF_PROMISC) ? "entered" : "left");
}
}
void dev_set_allmulti(struct device *dev, int inc)
{
unsigned short old_flags = dev->flags;
dev->flags |= IFF_ALLMULTI;
if ((dev->allmulti += inc) == 0)
dev->flags &= ~IFF_ALLMULTI;
if (dev->flags^old_flags)
dev_mc_upload(dev);
}
int dev_change_flags(struct device *dev, unsigned flags)
{
int ret;
int old_flags = dev->flags;
dev->flags = (flags & (IFF_DEBUG|IFF_NOTRAILERS|IFF_RUNNING|IFF_NOARP|
IFF_SLAVE|IFF_MASTER|IFF_DYNAMIC|
IFF_MULTICAST|IFF_PORTSEL|IFF_AUTOMEDIA)) |
(dev->flags & (IFF_UP|IFF_VOLATILE|IFF_PROMISC|IFF_ALLMULTI));
dev_mc_upload(dev);
ret = 0;
if ((old_flags^flags)&IFF_UP)
{
ret = ((old_flags & IFF_UP) ? dev_close : dev_open)(dev);
if (ret == 0)
dev_mc_upload(dev);
}
if (dev->flags&IFF_UP &&
((old_flags^dev->flags)&~(IFF_UP|IFF_RUNNING|IFF_PROMISC|IFF_ALLMULTI|IFF_VOLATILE)))
notifier_call_chain(&netdev_chain, NETDEV_CHANGE, dev);
if ((flags^dev->gflags)&IFF_PROMISC) {
int inc = (flags&IFF_PROMISC) ? +1 : -1;
dev->gflags ^= IFF_PROMISC;
dev_set_promiscuity(dev, inc);
}
if ((flags^dev->gflags)&IFF_ALLMULTI) {
int inc = (flags&IFF_ALLMULTI) ? +1 : -1;
dev->gflags ^= IFF_ALLMULTI;
dev_set_allmulti(dev, inc);
}
if (!ret && dev->change_flags)
ret = dev->change_flags(dev, dev->flags);
return ret;
}
#ifdef _HURD_
#define dev_ioctl 0
#else
static int dev_ifsioc(struct ifreq *ifr, unsigned int cmd)
{
struct device *dev;
int err;
if ((dev = dev_get(ifr->ifr_name)) == NULL)
return -ENODEV;
switch(cmd)
{
case SIOCGIFFLAGS:
ifr->ifr_flags = (dev->flags&~(IFF_PROMISC|IFF_ALLMULTI))
|(dev->gflags&(IFF_PROMISC|IFF_ALLMULTI));
return 0;
case SIOCSIFFLAGS:
return dev_change_flags(dev, ifr->ifr_flags);
case SIOCGIFMETRIC:
ifr->ifr_metric = 0;
return 0;
case SIOCSIFMETRIC:
return -EOPNOTSUPP;
case SIOCGIFMTU:
ifr->ifr_mtu = dev->mtu;
return 0;
case SIOCSIFMTU:
if (ifr->ifr_mtu == dev->mtu)
return 0;
if (ifr->ifr_mtu<=0)
return -EINVAL;
if (dev->change_mtu)
err = dev->change_mtu(dev, ifr->ifr_mtu);
else {
dev->mtu = ifr->ifr_mtu;
err = 0;
}
if (!err && dev->flags&IFF_UP)
notifier_call_chain(&netdev_chain, NETDEV_CHANGEMTU, dev);
return err;
case SIOCGIFHWADDR:
memcpy(ifr->ifr_hwaddr.sa_data,dev->dev_addr, MAX_ADDR_LEN);
ifr->ifr_hwaddr.sa_family=dev->type;
return 0;
case SIOCSIFHWADDR:
if(dev->set_mac_address==NULL)
return -EOPNOTSUPP;
if(ifr->ifr_hwaddr.sa_family!=dev->type)
return -EINVAL;
err=dev->set_mac_address(dev,&ifr->ifr_hwaddr);
if (!err)
notifier_call_chain(&netdev_chain, NETDEV_CHANGEADDR, dev);
return err;
case SIOCSIFHWBROADCAST:
if(ifr->ifr_hwaddr.sa_family!=dev->type)
return -EINVAL;
memcpy(dev->broadcast, ifr->ifr_hwaddr.sa_data, MAX_ADDR_LEN);
notifier_call_chain(&netdev_chain, NETDEV_CHANGEADDR, dev);
return 0;
case SIOCGIFMAP:
ifr->ifr_map.mem_start=dev->mem_start;
ifr->ifr_map.mem_end=dev->mem_end;
ifr->ifr_map.base_addr=dev->base_addr;
ifr->ifr_map.irq=dev->irq;
ifr->ifr_map.dma=dev->dma;
ifr->ifr_map.port=dev->if_port;
return 0;
case SIOCSIFMAP:
if (dev->set_config)
return dev->set_config(dev,&ifr->ifr_map);
return -EOPNOTSUPP;
case SIOCADDMULTI:
if(dev->set_multicast_list==NULL ||
ifr->ifr_hwaddr.sa_family!=AF_UNSPEC)
return -EINVAL;
dev_mc_add(dev,ifr->ifr_hwaddr.sa_data, dev->addr_len, 1);
return 0;
case SIOCDELMULTI:
if(dev->set_multicast_list==NULL ||
ifr->ifr_hwaddr.sa_family!=AF_UNSPEC)
return -EINVAL;
dev_mc_delete(dev,ifr->ifr_hwaddr.sa_data,dev->addr_len, 1);
return 0;
case SIOCGIFINDEX:
ifr->ifr_ifindex = dev->ifindex;
return 0;
case SIOCGIFTXQLEN:
ifr->ifr_qlen = dev->tx_queue_len;
return 0;
case SIOCSIFTXQLEN:
if(ifr->ifr_qlen<0)
return -EINVAL;
dev->tx_queue_len = ifr->ifr_qlen;
return 0;
case SIOCSIFNAME:
if (dev->flags&IFF_UP)
return -EBUSY;
if (dev_get(ifr->ifr_newname))
return -EEXIST;
memcpy(dev->name, ifr->ifr_newname, IFNAMSIZ);
dev->name[IFNAMSIZ-1] = 0;
notifier_call_chain(&netdev_chain, NETDEV_CHANGENAME, dev);
return 0;
default:
if(cmd >= SIOCDEVPRIVATE &&
cmd <= SIOCDEVPRIVATE + 15) {
if (dev->do_ioctl)
return dev->do_ioctl(dev, ifr, cmd);
return -EOPNOTSUPP;
}
#ifdef CONFIG_NET_RADIO
if(cmd >= SIOCIWFIRST && cmd <= SIOCIWLAST) {
if (dev->do_ioctl)
return dev->do_ioctl(dev, ifr, cmd);
return -EOPNOTSUPP;
}
#endif
}
return -EINVAL;
}
int dev_ioctl(unsigned int cmd, void *arg)
{
struct ifreq ifr;
int ret;
char *colon;
if (cmd == SIOCGIFCONF) {
rtnl_shlock();
ret = dev_ifconf((char *) arg);
rtnl_shunlock();
return ret;
}
if (cmd == SIOCGIFNAME) {
return dev_ifname((struct ifreq *)arg);
}
if (copy_from_user(&ifr, arg, sizeof(struct ifreq)))
return -EFAULT;
ifr.ifr_name[IFNAMSIZ-1] = 0;
colon = strchr(ifr.ifr_name, ':');
if (colon)
*colon = 0;
switch(cmd)
{
case SIOCGIFFLAGS:
case SIOCGIFMETRIC:
case SIOCGIFMTU:
case SIOCGIFHWADDR:
case SIOCGIFSLAVE:
case SIOCGIFMAP:
case SIOCGIFINDEX:
case SIOCGIFTXQLEN:
dev_load(ifr.ifr_name);
ret = dev_ifsioc(&ifr, cmd);
if (!ret) {
if (colon)
*colon = ':';
if (copy_to_user(arg, &ifr, sizeof(struct ifreq)))
return -EFAULT;
}
return ret;
case SIOCSIFFLAGS:
case SIOCSIFMETRIC:
case SIOCSIFMTU:
case SIOCSIFMAP:
case SIOCSIFHWADDR:
case SIOCSIFSLAVE:
case SIOCADDMULTI:
case SIOCDELMULTI:
case SIOCSIFHWBROADCAST:
case SIOCSIFTXQLEN:
case SIOCSIFNAME:
if (!capable(CAP_NET_ADMIN))
return -EPERM;
dev_load(ifr.ifr_name);
rtnl_lock();
ret = dev_ifsioc(&ifr, cmd);
rtnl_unlock();
return ret;
case SIOCGIFMEM:
case SIOCSIFMEM:
case SIOCSIFLINK:
return -EINVAL;
default:
if (cmd >= SIOCDEVPRIVATE &&
cmd <= SIOCDEVPRIVATE + 15) {
dev_load(ifr.ifr_name);
rtnl_lock();
ret = dev_ifsioc(&ifr, cmd);
rtnl_unlock();
if (!ret && copy_to_user(arg, &ifr, sizeof(struct ifreq)))
return -EFAULT;
return ret;
}
#ifdef CONFIG_NET_RADIO
if (cmd >= SIOCIWFIRST && cmd <= SIOCIWLAST) {
dev_load(ifr.ifr_name);
if (IW_IS_SET(cmd)) {
if (!suser())
return -EPERM;
rtnl_lock();
}
ret = dev_ifsioc(&ifr, cmd);
if (IW_IS_SET(cmd))
rtnl_unlock();
if (!ret && IW_IS_GET(cmd) &&
copy_to_user(arg, &ifr, sizeof(struct ifreq)))
return -EFAULT;
return ret;
}
#endif
return -EINVAL;
}
}
#endif
int dev_new_index(void)
{
static int ifindex;
for (;;) {
if (++ifindex <= 0)
ifindex=1;
if (dev_get_by_index(ifindex) == NULL)
return ifindex;
}
}
static int dev_boot_phase = 1;
int register_netdevice(struct device *dev)
{
struct device *d, **dp;
if (dev_boot_phase) {
printk(KERN_INFO "early initialization of device %s is deferred\n", dev->name);
for (dp=&dev_base; (d=*dp) != NULL; dp=&d->next) {
if (d == dev || strcmp(d->name, dev->name) == 0)
return -EEXIST;
}
dev->next = NULL;
*dp = dev;
return 0;
}
dev->iflink = -1;
if (dev->init && dev->init(dev) != 0)
return -EIO;
for (dp=&dev_base; (d=*dp) != NULL; dp=&d->next) {
if (d == dev || strcmp(d->name, dev->name) == 0)
return -EEXIST;
}
dev->next = NULL;
dev_init_scheduler(dev);
dev->ifindex = dev_new_index();
if (dev->iflink == -1)
dev->iflink = dev->ifindex;
*dp = dev;
notifier_call_chain(&netdev_chain, NETDEV_REGISTER, dev);
return 0;
}
int unregister_netdevice(struct device *dev)
{
struct device *d, **dp;
if (dev_boot_phase == 0) {
if (dev->flags & IFF_UP)
dev_close(dev);
#ifdef CONFIG_NET_FASTROUTE
dev_clear_fastroute(dev);
#endif
dev_shutdown(dev);
notifier_call_chain(&netdev_chain, NETDEV_UNREGISTER, dev);
dev_mc_discard(dev);
dev_lock_wait();
}
for (dp = &dev_base; (d=*dp) != NULL; dp=&d->next) {
if (d == dev) {
*dp = d->next;
synchronize_bh();
d->next = NULL;
if (dev->destructor)
dev->destructor(dev);
return 0;
}
}
return -ENODEV;
}
extern int lance_init(void);
extern int bpq_init(void);
extern int scc_init(void);
extern void sdla_setup(void);
extern void sdla_c_setup(void);
extern void dlci_setup(void);
extern int dmascc_init(void);
extern int sm_init(void);
extern int baycom_ser_fdx_init(void);
extern int baycom_ser_hdx_init(void);
extern int baycom_par_init(void);
extern int lapbeth_init(void);
extern int comx_init(void);
extern void arcnet_init(void);
extern void ip_auto_config(void);
#ifdef CONFIG_8xx
extern int cpm_enet_init(void);
#endif
#ifdef CONFIG_PROC_FS
static struct proc_dir_entry proc_net_dev = {
PROC_NET_DEV, 3, "dev",
S_IFREG | S_IRUGO, 1, 0, 0,
0, &proc_net_inode_operations,
dev_get_info
};
#endif
#ifdef CONFIG_NET_RADIO
#ifdef CONFIG_PROC_FS
static struct proc_dir_entry proc_net_wireless = {
PROC_NET_WIRELESS, 8, "wireless",
S_IFREG | S_IRUGO, 1, 0, 0,
0, &proc_net_inode_operations,
dev_get_wireless_info
};
#endif
#endif
__initfunc(int net_dev_init(void))
{
struct device *dev, **dp;
#ifdef CONFIG_NET_SCHED
pktsched_init();
#endif
skb_queue_head_init(&backlog);
#ifdef CONFIG_BRIDGE
br_init();
#endif
#if defined(CONFIG_SCC)
scc_init();
#endif
#if defined(CONFIG_DMASCC)
dmascc_init();
#endif
#if defined(CONFIG_BPQETHER)
bpq_init();
#endif
#if defined(CONFIG_DLCI)
dlci_setup();
#endif
#if defined(CONFIG_SDLA)
sdla_c_setup();
#endif
#if defined(CONFIG_BAYCOM_PAR)
baycom_par_init();
#endif
#if defined(CONFIG_BAYCOM_SER_FDX)
baycom_ser_fdx_init();
#endif
#if defined(CONFIG_BAYCOM_SER_HDX)
baycom_ser_hdx_init();
#endif
#if defined(CONFIG_SOUNDMODEM)
sm_init();
#endif
#if defined(CONFIG_LAPBETHER)
lapbeth_init();
#endif
#if defined(CONFIG_PLIP)
plip_init();
#endif
#if defined(CONFIG_ARCNET)
arcnet_init();
#endif
#if defined(CONFIG_8xx)
cpm_enet_init();
#endif
#if defined(CONFIG_COMX)
comx_init();
#endif
#ifdef CONFIG_INET
#if (defined(CONFIG_SLIP) && defined(CONFIG_SLIP_COMPRESSED)) \
|| defined(CONFIG_PPP) \
|| (defined(CONFIG_ISDN) && defined(CONFIG_ISDN_PPP))
slhc_install();
#endif
#endif
#ifdef CONFIG_NET_PROFILE
net_profile_init();
NET_PROFILE_REGISTER(dev_queue_xmit);
NET_PROFILE_REGISTER(net_bh);
#if 0
NET_PROFILE_REGISTER(net_bh_skb);
#endif
#endif
dp = &dev_base;
while ((dev = *dp) != NULL)
{
dev->iflink = -1;
if (dev->init && dev->init(dev))
{
*dp = dev->next;
synchronize_bh();
}
else
{
dp = &dev->next;
dev->ifindex = dev_new_index();
if (dev->iflink == -1)
dev->iflink = dev->ifindex;
dev_init_scheduler(dev);
}
}
#ifdef CONFIG_PROC_FS
proc_net_register(&proc_net_dev);
{
struct proc_dir_entry *ent = create_proc_entry("net/dev_stat", 0, 0);
ent->read_proc = dev_proc_stats;
}
#endif
#ifdef CONFIG_NET_RADIO
#ifdef CONFIG_PROC_FS
proc_net_register(&proc_net_wireless);
#endif
#endif
init_bh(NET_BH, net_bh);
dev_boot_phase = 0;
dev_mcast_init();
#ifdef CONFIG_BRIDGE
br_spacedevice_register();
#endif
#ifdef CONFIG_IP_PNP
ip_auto_config();
#endif
return 0;
}