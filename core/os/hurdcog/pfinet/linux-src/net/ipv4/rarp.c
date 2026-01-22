#include <linux/module.h>
#include <linux/types.h>
#include <linux/string.h>
#include <linux/kernel.h>
#include <linux/sched.h>
#include <linux/mm.h>
#include <linux/socket.h>
#include <linux/sockios.h>
#include <linux/errno.h>
#include <linux/netdevice.h>
#include <linux/if_arp.h>
#include <linux/in.h>
#include <linux/config.h>
#include <linux/init.h>
#include <asm/system.h>
#include <asm/uaccess.h>
#include <stdarg.h>
#include <linux/inet.h>
#include <linux/etherdevice.h>
#include <net/ip.h>
#include <net/route.h>
#include <net/protocol.h>
#include <net/tcp.h>
#include <linux/skbuff.h>
#include <net/sock.h>
#include <net/arp.h>
#include <net/rarp.h>
#if defined(CONFIG_AX25) || defined(CONFIG_AX25_MODULE)
#include <net/ax25.h>
#endif
#include <linux/proc_fs.h>
#include <linux/stat.h>
extern int (*rarp_ioctl_hook)(unsigned int,void*);
struct rarp_table
{
struct rarp_table *next;
unsigned long ip;
unsigned char ha[MAX_ADDR_LEN];
unsigned char hlen;
unsigned char htype;
struct device *dev;
};
struct rarp_table *rarp_tables = NULL;
static int rarp_rcv(struct sk_buff *, struct device *, struct packet_type *);
static struct packet_type rarp_packet_type =
{
0,
0,
rarp_rcv,
NULL,
NULL
};
static int initflag = 1;
static inline void rarp_release_entry(struct rarp_table *entry)
{
kfree_s(entry, sizeof(struct rarp_table));
MOD_DEC_USE_COUNT;
return;
}
static void rarp_destroy(unsigned long ip_addr)
{
struct rarp_table *entry;
struct rarp_table **pentry;
start_bh_atomic();
pentry = &rarp_tables;
while ((entry = *pentry) != NULL)
{
if (entry->ip == ip_addr)
{
*pentry = entry->next;
end_bh_atomic();
rarp_release_entry(entry);
return;
}
pentry = &entry->next;
}
end_bh_atomic();
}
static void rarp_destroy_dev(struct device *dev)
{
struct rarp_table *entry;
struct rarp_table **pentry;
start_bh_atomic();
pentry = &rarp_tables;
while ((entry = *pentry) != NULL)
{
if (entry->dev == dev)
{
*pentry = entry->next;
rarp_release_entry(entry);
}
else
pentry = &entry->next;
}
end_bh_atomic();
}
static int rarp_device_event(struct notifier_block *this, unsigned long event, void *ptr)
{
if(event!=NETDEV_DOWN)
return NOTIFY_DONE;
rarp_destroy_dev((struct device *)ptr);
return NOTIFY_DONE;
}
static struct notifier_block rarp_dev_notifier={
rarp_device_event,
NULL,
0
};
static int rarp_pkt_inited=0;
static void rarp_init_pkt (void)
{
rarp_packet_type.type=htons(ETH_P_RARP);
dev_add_pack(&rarp_packet_type);
register_netdevice_notifier(&rarp_dev_notifier);
rarp_pkt_inited=1;
}
#ifdef MODULE
static void rarp_end_pkt(void)
{
if(!rarp_pkt_inited)
return;
dev_remove_pack(&rarp_packet_type);
unregister_netdevice_notifier(&rarp_dev_notifier);
rarp_pkt_inited=0;
}
#endif
static int rarp_rcv(struct sk_buff *skb, struct device *dev, struct packet_type *pt)
{
struct arphdr *rarp = (struct arphdr *) skb->data;
unsigned char *rarp_ptr = skb_pull(skb,sizeof(struct arphdr));
struct rarp_table *entry;
struct in_device *in_dev = dev->ip_ptr;
long sip,tip;
unsigned char *sha,*tha;
if (rarp->ar_hln != dev->addr_len || dev->type != ntohs(rarp->ar_hrd)
|| dev->flags&IFF_NOARP || !in_dev || !in_dev->ifa_list)
{
kfree_skb(skb);
return 0;
}
if (rarp->ar_op != htons(ARPOP_RREQUEST))
{
kfree_skb(skb);
return 0;
}
if (
#if defined(CONFIG_AX25) || defined(CONFIG_AX25_MODULE)
(rarp->ar_pro != htons(AX25_P_IP) && dev->type == ARPHRD_AX25) ||
#endif
(rarp->ar_pro != htons(ETH_P_IP) && dev->type != ARPHRD_AX25)
|| rarp->ar_pln != 4)
{
kfree_skb(skb);
return 0;
}
sha=rarp_ptr;
rarp_ptr+=dev->addr_len;
memcpy(&sip,rarp_ptr,4);
rarp_ptr+=4;
tha=rarp_ptr;
rarp_ptr+=dev->addr_len;
memcpy(&tip,rarp_ptr,4);
for (entry = rarp_tables; entry != NULL; entry = entry->next)
if (!memcmp(entry->ha, tha, rarp->ar_hln))
break;
if (entry != NULL)
{
sip=entry->ip;
arp_send(ARPOP_RREPLY, ETH_P_RARP, sip, dev, in_dev->ifa_list->ifa_address, sha,
dev->dev_addr, sha);
}
kfree_skb(skb);
return 0;
}
static int rarp_req_set(struct arpreq *req)
{
struct arpreq r;
struct rarp_table *entry;
struct sockaddr_in *si;
int htype, hlen;
unsigned long ip;
struct rtable *rt;
struct device * dev;
int err;
err = copy_from_user(&r, req, sizeof(r));
if (err)
return -EFAULT;
if (r.arp_pa.sa_family != AF_INET)
return -EPFNOSUPPORT;
switch (r.arp_ha.sa_family)
{
case ARPHRD_ETHER:
htype = ARPHRD_ETHER;
hlen = ETH_ALEN;
break;
#if defined(CONFIG_AX25) || defined(CONFIG_AX25_MODULE)
case ARPHRD_AX25:
htype = ARPHRD_AX25;
hlen = 7;
break;
#endif
default:
return -EPFNOSUPPORT;
}
si = (struct sockaddr_in *) &r.arp_pa;
ip = si->sin_addr.s_addr;
if (ip == 0)
{
printk(KERN_DEBUG "RARP: SETRARP: requested PA is 0.0.0.0 !\n");
return -EINVAL;
}
err = ip_route_output(&rt, ip, 0, 1, 0);
if (err)
return err;
if (rt->rt_flags&(RTCF_LOCAL|RTCF_BROADCAST|RTCF_MULTICAST|RTCF_DNAT)) {
ip_rt_put(rt);
return -EINVAL;
}
dev = rt->u.dst.dev;
for (entry = rarp_tables; entry != NULL; entry = entry->next)
if (entry->ip == ip)
break;
if (entry == NULL)
{
entry = (struct rarp_table *) kmalloc(sizeof(struct rarp_table),
GFP_ATOMIC);
if (entry == NULL)
{
return -ENOMEM;
}
if (initflag)
{
rarp_init_pkt();
initflag=0;
}
cli();
entry->next = rarp_tables;
rarp_tables = entry;
}
cli();
entry->ip = ip;
entry->hlen = hlen;
entry->htype = htype;
memcpy(&entry->ha, &r.arp_ha.sa_data, hlen);
entry->dev = dev;
sti();
MOD_INC_USE_COUNT;
return 0;
}
static int rarp_req_get(struct arpreq *req)
{
struct arpreq r;
struct rarp_table *entry;
struct sockaddr_in *si;
unsigned long ip;
int err;
err = copy_from_user(&r, req, sizeof(r));
if (err)
return -EFAULT;
if (r.arp_pa.sa_family != AF_INET)
return -EPFNOSUPPORT;
si = (struct sockaddr_in *) &r.arp_pa;
ip = si->sin_addr.s_addr;
for (entry = rarp_tables; entry != NULL; entry = entry->next)
if (entry->ip == ip)
break;
if (entry == NULL)
{
return -ENXIO;
}
memcpy(r.arp_ha.sa_data, &entry->ha, entry->hlen);
r.arp_ha.sa_family = entry->htype;
return copy_to_user(req, &r, sizeof(r)) ? -EFAULT : 0;
}
int rarp_ioctl(unsigned int cmd, void *arg)
{
struct arpreq r;
struct sockaddr_in *si;
int err;
switch(cmd)
{
case SIOCDRARP:
if (!suser())
return -EPERM;
err = copy_from_user(&r, arg, sizeof(r));
if (err)
return -EFAULT;
if (r.arp_pa.sa_family != AF_INET)
return -EPFNOSUPPORT;
si = (struct sockaddr_in *) &r.arp_pa;
rarp_destroy(si->sin_addr.s_addr);
return 0;
case SIOCGRARP:
return rarp_req_get((struct arpreq *)arg);
case SIOCSRARP:
if (!suser())
return -EPERM;
return rarp_req_set((struct arpreq *)arg);
default:
return -EINVAL;
}
return 0;
}
#ifdef CONFIG_PROC_FS
int rarp_get_info(char *buffer, char **start, off_t offset, int length, int dummy)
{
int len=0;
off_t begin=0;
off_t pos=0;
int size;
struct rarp_table *entry;
char ipbuffer[20];
unsigned long netip;
if (initflag)
{
size = sprintf(buffer,"RARP disabled until entries added to cache.\n");
pos+=size;
len+=size;
}
else
{
size = sprintf(buffer,
"IP address       HW type             HW address\n");
pos+=size;
len+=size;
for(entry=rarp_tables; entry!=NULL; entry=entry->next)
{
netip=htonl(entry->ip);
sprintf(ipbuffer,"%d.%d.%d.%d",
(unsigned int)(netip>>24)&255,
(unsigned int)(netip>>16)&255,
(unsigned int)(netip>>8)&255,
(unsigned int)(netip)&255);
size = sprintf(buffer+len,
"%-17s%-20s%02x:%02x:%02x:%02x:%02x:%02x\n",
ipbuffer,
"10Mbps Ethernet",
(unsigned int)entry->ha[0],
(unsigned int)entry->ha[1],
(unsigned int)entry->ha[2],
(unsigned int)entry->ha[3],
(unsigned int)entry->ha[4],
(unsigned int)entry->ha[5]);
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
}
*start = buffer+(offset-begin);
len -= (offset-begin);
if (len>length)
len = length;
return len;
}
struct proc_dir_entry proc_net_rarp = {
PROC_NET_RARP, 4, "rarp",
S_IFREG | S_IRUGO, 1, 0, 0,
0, &proc_net_inode_operations,
rarp_get_info
};
#endif
__initfunc(void
rarp_init(void))
{
#ifdef CONFIG_PROC_FS
proc_net_register(&proc_net_rarp);
#endif
rarp_ioctl_hook = rarp_ioctl;
}
#ifdef MODULE
int init_module(void)
{
rarp_init();
return 0;
}
void cleanup_module(void)
{
struct rarp_table *rt, *rt_next;
#ifdef CONFIG_PROC_FS
proc_net_unregister(PROC_NET_RARP);
#endif
rarp_ioctl_hook = NULL;
cli();
rt = rarp_tables;
rarp_tables = NULL;
sti();
for ( ; rt != NULL; rt = rt_next) {
rt_next = rt->next;
rarp_release_entry(rt);
}
rarp_end_pkt();
}
#endif