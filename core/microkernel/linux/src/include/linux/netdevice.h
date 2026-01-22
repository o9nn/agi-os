#ifndef _LINUX_NETDEVICE_H
#define _LINUX_NETDEVICE_H
#include <linux/config.h>
#include <linux/if.h>
#include <linux/if_ether.h>
#define DEV_NUMBUFFS 3
#define MAX_ADDR_LEN 7
#ifndef CONFIG_AX25
#ifndef CONFIG_AX25_MODULE
#ifndef CONFIG_TR
#if !defined(CONFIG_NET_IPIP) && !defined(CONFIG_NET_IPIP_MODULE)
#define MAX_HEADER 32
#else
#define MAX_HEADER 80
#endif
#else
#define MAX_HEADER 48
#endif
#else
#define MAX_HEADER 96
#endif
#else
#define MAX_HEADER 96
#endif
#define IS_MYADDR 1
#define IS_LOOPBACK 2
#define IS_BROADCAST 3
#define IS_INVBCAST 4
#define IS_MULTICAST 5
#ifdef __KERNEL__
#include <linux/skbuff.h>
struct dev_mc_list
{
struct dev_mc_list *next;
char dmi_addr[MAX_ADDR_LEN];
unsigned short dmi_addrlen;
unsigned short dmi_users;
};
struct hh_cache
{
struct hh_cache *hh_next;
void *hh_arp;
int hh_refcnt;
unsigned short hh_type;
char hh_uptodate;
char hh_data[16];
};
struct device
{
char *name;
unsigned long rmem_end;
unsigned long rmem_start;
unsigned long mem_end;
unsigned long mem_start;
unsigned long base_addr;
unsigned char irq;
volatile unsigned char start,
interrupt;
unsigned long tbusy;
struct device *next;
int (*init)(struct device *dev);
unsigned char if_port;
unsigned char dma;
struct enet_statistics* (*get_stats)(struct device *dev);
unsigned long trans_start;
unsigned long last_rx;
unsigned short flags;
unsigned short family;
unsigned short metric;
unsigned short mtu;
unsigned short type;
unsigned short hard_header_len;
void *priv;
unsigned char broadcast[MAX_ADDR_LEN];
unsigned char pad;
unsigned char dev_addr[MAX_ADDR_LEN];
unsigned char addr_len;
unsigned long pa_addr;
unsigned long pa_brdaddr;
unsigned long pa_dstaddr;
unsigned long pa_mask;
unsigned short pa_alen;
struct dev_mc_list *mc_list;
int mc_count;
struct ip_mc_list *ip_mc_list;
__u32 tx_queue_len;
unsigned long pkt_queue;
struct device *slave;
struct net_alias_info *alias_info;
struct net_alias *my_alias;
struct sk_buff_head buffs[DEV_NUMBUFFS];
int (*open)(struct device *dev);
int (*stop)(struct device *dev);
int (*hard_start_xmit) (struct sk_buff *skb,
struct device *dev);
int (*hard_header) (struct sk_buff *skb,
struct device *dev,
unsigned short type,
void *daddr,
void *saddr,
unsigned len);
int (*rebuild_header)(void *eth, struct device *dev,
unsigned long raddr, struct sk_buff *skb);
#define HAVE_MULTICAST
void (*set_multicast_list)(struct device *dev);
#define HAVE_SET_MAC_ADDR
int (*set_mac_address)(struct device *dev, void *addr);
#define HAVE_PRIVATE_IOCTL
int (*do_ioctl)(struct device *dev, struct ifreq *ifr, int cmd);
#define HAVE_SET_CONFIG
int (*set_config)(struct device *dev, struct ifmap *map);
#define HAVE_HEADER_CACHE
void (*header_cache_bind)(struct hh_cache **hhp, struct device *dev, unsigned short htype, __u32 daddr);
void (*header_cache_update)(struct hh_cache *hh, struct device *dev, unsigned char * haddr);
#define HAVE_CHANGE_MTU
int (*change_mtu)(struct device *dev, int new_mtu);
struct iw_statistics* (*get_wireless_stats)(struct device *dev);
};
struct packet_type {
unsigned short type;
struct device * dev;
int (*func) (struct sk_buff *, struct device *,
struct packet_type *);
void *data;
struct packet_type *next;
};
#include <linux/interrupt.h>
#include <linux/notifier.h>
#define IN_SKBUFF 1
extern volatile unsigned long in_bh;
extern struct device loopback_dev;
extern struct device *dev_base;
extern struct packet_type *ptype_base[16];
extern int ip_addr_match(unsigned long addr1, unsigned long addr2);
extern int ip_chk_addr(unsigned long addr);
extern struct device *ip_dev_bynet(unsigned long daddr, unsigned long mask);
extern unsigned long ip_my_addr(void);
extern unsigned long ip_get_mask(unsigned long addr);
extern struct device *ip_dev_find(unsigned long addr);
extern struct device *dev_getbytype(unsigned short type);
extern void dev_add_pack(struct packet_type *pt);
extern void dev_remove_pack(struct packet_type *pt);
extern struct device *dev_get(const char *name);
extern int dev_open(struct device *dev);
extern int dev_close(struct device *dev);
extern void dev_queue_xmit(struct sk_buff *skb, struct device *dev,
int pri);
#define HAVE_NETIF_RX 1
extern void netif_rx(struct sk_buff *skb);
extern void net_bh(void);
extern void dev_tint(struct device *dev);
extern int dev_get_info(char *buffer, char **start, off_t offset, int length, int dummy);
extern int dev_ioctl(unsigned int cmd, void *);
extern void dev_init(void);
extern int dev_lockct;
extern __inline__ void dev_lock_list(void)
{
unsigned long flags;
save_flags(flags);
cli();
dev_lockct++;
restore_flags(flags);
}
extern __inline__ void dev_unlock_list(void)
{
unsigned long flags;
save_flags(flags);
cli();
dev_lockct--;
restore_flags(flags);
}
extern __inline__ void dev_lock_wait(void)
{
while(dev_lockct)
schedule();
}
extern void ether_setup(struct device *dev);
extern void tr_setup(struct device *dev);
extern void fddi_setup(struct device *dev);
extern int ether_config(struct device *dev, struct ifmap *map);
extern int register_netdev(struct device *dev);
extern void unregister_netdev(struct device *dev);
extern int register_netdevice_notifier(struct notifier_block *nb);
extern int unregister_netdevice_notifier(struct notifier_block *nb);
extern void dev_mc_upload(struct device *dev);
extern void dev_mc_delete(struct device *dev, void *addr, int alen, int all);
extern void dev_mc_add(struct device *dev, void *addr, int alen, int newonly);
extern void dev_mc_discard(struct device *dev);
extern void ip_mc_allhost(struct device *dev);
#endif
#endif