#ifndef _LINUX_NETDEVICE_H
#define _LINUX_NETDEVICE_H
#ifdef __KERNEL__
#include <linux/config.h>
#endif
#include <linux/if.h>
#include <linux/if_ether.h>
#include <linux/if_packet.h>
#include <asm/atomic.h>
#ifdef __KERNEL__
#ifdef CONFIG_NET_PROFILE
#include <net/profile.h>
#endif
#endif
#define MAX_ADDR_LEN 7
#if !defined(CONFIG_AX25) && !defined(CONFIG_AX25_MODULE) && !defined(CONFIG_TR)
#define LL_MAX_HEADER 32
#else
#if defined(CONFIG_AX25) || defined(CONFIG_AX25_MODULE)
#define LL_MAX_HEADER 96
#else
#define LL_MAX_HEADER 48
#endif
#endif
#if !defined(CONFIG_NET_IPIP) && \
!defined(CONFIG_IPV6) && !defined(CONFIG_IPV6_MODULE)
#define MAX_HEADER LL_MAX_HEADER
#else
#define MAX_HEADER (LL_MAX_HEADER + 48)
#endif
struct net_device_stats
{
unsigned long rx_packets;
unsigned long tx_packets;
unsigned long rx_bytes;
unsigned long tx_bytes;
unsigned long rx_errors;
unsigned long tx_errors;
unsigned long rx_dropped;
unsigned long tx_dropped;
unsigned long multicast;
unsigned long collisions;
unsigned long rx_length_errors;
unsigned long rx_over_errors;
unsigned long rx_crc_errors;
unsigned long rx_frame_errors;
unsigned long rx_fifo_errors;
unsigned long rx_missed_errors;
unsigned long tx_aborted_errors;
unsigned long tx_carrier_errors;
unsigned long tx_fifo_errors;
unsigned long tx_heartbeat_errors;
unsigned long tx_window_errors;
unsigned long rx_compressed;
unsigned long tx_compressed;
};
#ifdef CONFIG_NET_FASTROUTE
struct net_fastroute_stats
{
int hits;
int succeed;
int deferred;
int latency_reduction;
};
#endif
enum {
IF_PORT_UNKNOWN = 0,
IF_PORT_10BASE2,
IF_PORT_10BASET,
IF_PORT_AUI,
IF_PORT_100BASET,
IF_PORT_100BASETX,
IF_PORT_100BASEFX
};
#ifdef __KERNEL__
extern const char *if_port_text[];
#include <linux/skbuff.h>
struct neighbour;
struct neigh_parms;
struct sk_buff;
struct dev_mc_list
{
struct dev_mc_list *next;
__u8 dmi_addr[MAX_ADDR_LEN];
unsigned char dmi_addrlen;
int dmi_users;
int dmi_gusers;
};
struct hh_cache
{
struct hh_cache *hh_next;
atomic_t hh_refcnt;
unsigned short hh_type;
int (*hh_output)(struct sk_buff *skb);
rwlock_t hh_lock;
unsigned long hh_data[16/sizeof(unsigned long)];
};
struct device
{
char *name;
unsigned long rmem_end;
unsigned long rmem_start;
unsigned long mem_end;
unsigned long mem_start;
unsigned long base_addr;
unsigned int irq;
volatile unsigned char start;
unsigned long interrupt;
unsigned long tbusy;
struct device *next;
int (*init)(struct device *dev);
void (*destructor)(struct device *dev);
int ifindex;
int iflink;
unsigned char if_port;
unsigned char dma;
struct net_device_stats* (*get_stats)(struct device *dev);
struct iw_statistics* (*get_wireless_stats)(struct device *dev);
unsigned long trans_start;
unsigned long last_rx;
unsigned short flags;
unsigned short gflags;
unsigned mtu;
unsigned short type;
unsigned short hard_header_len;
void *priv;
unsigned char broadcast[MAX_ADDR_LEN];
unsigned char pad;
unsigned char dev_addr[MAX_ADDR_LEN];
unsigned char addr_len;
struct dev_mc_list *mc_list;
int mc_count;
int promiscuity;
int allmulti;
unsigned long pkt_queue;
struct device *slave;
void *atalk_ptr;
void *ip_ptr;
void *dn_ptr;
struct Qdisc *qdisc;
struct Qdisc *qdisc_sleeping;
struct Qdisc *qdisc_list;
unsigned long tx_queue_len;
int bridge_port_id;
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
int (*rebuild_header)(struct sk_buff *skb);
#define HAVE_MULTICAST
void (*set_multicast_list)(struct device *dev);
#define HAVE_SET_MAC_ADDR
int (*set_mac_address)(struct device *dev,
void *addr);
#define HAVE_PRIVATE_IOCTL
int (*do_ioctl)(struct device *dev,
struct ifreq *ifr, int cmd);
#define HAVE_SET_CONFIG
int (*set_config)(struct device *dev,
struct ifmap *map);
#define HAVE_HEADER_CACHE
int (*hard_header_cache)(struct neighbour *neigh,
struct hh_cache *hh);
void (*header_cache_update)(struct hh_cache *hh,
struct device *dev,
unsigned char * haddr);
#define HAVE_CHANGE_MTU
int (*change_mtu)(struct device *dev, int new_mtu);
int (*hard_header_parse)(struct sk_buff *skb,
unsigned char *haddr);
int (*neigh_setup)(struct device *dev, struct neigh_parms *);
int (*accept_fastpath)(struct device *, struct dst_entry*);
#ifdef CONFIG_NET_FASTROUTE
int tx_semaphore;
#define NETDEV_FASTROUTE_HMASK 0xF
struct dst_entry *fastpath[NETDEV_FASTROUTE_HMASK+1];
#endif
int (*change_flags)(struct device *dev, short flags);
};
struct packet_type
{
unsigned short type;
struct device *dev;
int (*func) (struct sk_buff *, struct device *,
struct packet_type *);
void *data;
struct packet_type *next;
};
#include <linux/interrupt.h>
#include <linux/notifier.h>
extern struct device loopback_dev;
extern struct device *dev_base;
extern struct packet_type *ptype_base[16];
extern int netdev_dropping;
extern int net_cpu_congestion;
extern struct device *dev_getbyhwaddr(unsigned short type, char *hwaddr);
extern void dev_add_pack(struct packet_type *pt);
extern void dev_remove_pack(struct packet_type *pt);
extern struct device *dev_get(const char *name);
extern struct device *dev_alloc(const char *name, int *err);
extern int dev_alloc_name(struct device *dev, const char *name);
extern int dev_open(struct device *dev);
extern int dev_close(struct device *dev);
extern int dev_queue_xmit(struct sk_buff *skb);
extern void dev_loopback_xmit(struct sk_buff *skb);
extern int register_netdevice(struct device *dev);
extern int unregister_netdevice(struct device *dev);
extern int register_netdevice_notifier(struct notifier_block *nb);
extern int unregister_netdevice_notifier(struct notifier_block *nb);
extern int dev_new_index(void);
extern struct device *dev_get_by_index(int ifindex);
extern int dev_restart(struct device *dev);
typedef int gifconf_func_t(struct device * dev, char * bufptr, int len);
extern int register_gifconf(unsigned int family, gifconf_func_t * gifconf);
static __inline__ int unregister_gifconf(unsigned int family)
{
return register_gifconf(family, 0);
}
#define HAVE_NETIF_RX 1
extern void netif_rx(struct sk_buff *skb);
extern void net_bh(void);
extern int dev_get_info(char *buffer, char **start, off_t offset, int length, int dummy);
extern int dev_ioctl(unsigned int cmd, void *);
extern int dev_change_flags(struct device *, unsigned);
extern void dev_queue_xmit_nit(struct sk_buff *skb, struct device *dev);
extern void dev_init(void);
extern int netdev_nit;
extern atomic_t dev_lockct;
static __inline__ void dev_lock_list(void)
{
atomic_inc(&dev_lockct);
}
static __inline__ void dev_unlock_list(void)
{
atomic_dec(&dev_lockct);
}
static __inline__ void dev_lock_wait(void)
{
while (atomic_read(&dev_lockct)) {
current->policy |= SCHED_YIELD;
schedule();
}
}
static __inline__ void dev_init_buffers(struct device *dev)
{
}
extern void ether_setup(struct device *dev);
extern void fddi_setup(struct device *dev);
extern void tr_setup(struct device *dev);
extern void fc_setup(struct device *dev);
extern void tr_freedev(struct device *dev);
extern void fc_freedev(struct device *dev);
extern int ether_config(struct device *dev, struct ifmap *map);
extern int register_netdev(struct device *dev);
extern void unregister_netdev(struct device *dev);
extern int register_trdev(struct device *dev);
extern void unregister_trdev(struct device *dev);
extern int register_fcdev(struct device *dev);
extern void unregister_fcdev(struct device *dev);
extern void dev_mc_upload(struct device *dev);
extern int dev_mc_delete(struct device *dev, void *addr, int alen, int all);
extern int dev_mc_add(struct device *dev, void *addr, int alen, int newonly);
extern void dev_mc_discard(struct device *dev);
extern void dev_set_promiscuity(struct device *dev, int inc);
extern void dev_set_allmulti(struct device *dev, int inc);
extern void netdev_state_change(struct device *dev);
extern void dev_load(const char *name);
extern void dev_mcast_init(void);
extern int netdev_register_fc(struct device *dev, void (*stimul)(struct device *dev));
extern void netdev_unregister_fc(int bit);
extern int netdev_dropping;
extern int netdev_max_backlog;
extern atomic_t netdev_rx_dropped;
extern unsigned long netdev_fc_xoff;
#ifdef CONFIG_NET_FASTROUTE
extern int netdev_fastroute;
extern int netdev_fastroute_obstacles;
extern void dev_clear_fastroute(struct device *dev);
extern struct net_fastroute_stats dev_fastroute_stat;
#endif
#endif
#endif