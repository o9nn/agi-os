#ifndef __LINUX_ATALK_H__
#define __LINUX_ATALK_H__
#define SIOCATALKDIFADDR (SIOCPROTOPRIVATE + 0)
#define ATPORT_FIRST 1
#define ATPORT_RESERVED 128
#define ATPORT_LAST 255
#define ATADDR_ANYNET (__u16)0
#define ATADDR_ANYNODE (__u8)0
#define ATADDR_ANYPORT (__u8)0
#define ATADDR_BCAST (__u8)255
#define DDP_MAXSZ 587
struct at_addr
{
__u16 s_net;
__u8 s_node;
};
struct sockaddr_at
{
short sat_family;
__u8 sat_port;
struct at_addr sat_addr;
char sat_zero[ 8 ];
};
struct netrange
{
__u8 nr_phase;
__u16 nr_firstnet;
__u16 nr_lastnet;
};
struct atalk_route
{
struct device *dev;
struct at_addr target;
struct at_addr gateway;
int flags;
struct atalk_route *next;
};
struct atalk_iface
{
struct device *dev;
struct at_addr address;
int status;
#define ATIF_PROBE 1
#define ATIF_PROBE_FAIL 2
struct netrange nets;
struct atalk_iface *next;
};
struct atalk_sock
{
unsigned short dest_net;
unsigned short src_net;
unsigned char dest_node;
unsigned char src_node;
unsigned char dest_port;
unsigned char src_port;
};
#define DDP_MAXHOPS 15
#ifdef __KERNEL__
#include <asm/byteorder.h>
struct ddpehdr
{
#ifdef __LITTLE_ENDIAN_BITFIELD
__u16 deh_len:10, deh_hops:4, deh_pad:2;
#else
__u16 deh_pad:2, deh_hops:4, deh_len:10;
#endif
__u16 deh_sum;
__u16 deh_dnet;
__u16 deh_snet;
__u8 deh_dnode;
__u8 deh_snode;
__u8 deh_dport;
__u8 deh_sport;
};
struct ddpshdr
{
#ifdef __LITTLE_ENDIAN_BITFIELD
__u16 dsh_len:10, dsh_pad:6;
#else
__u16 dsh_pad:6, dsh_len:10;
#endif
__u8 dsh_dport;
__u8 dsh_sport;
};
struct elapaarp
{
__u16 hw_type;
#define AARP_HW_TYPE_ETHERNET 1
#define AARP_HW_TYPE_TOKENRING 2
__u16 pa_type;
__u8 hw_len;
__u8 pa_len;
#define AARP_PA_ALEN 4
__u16 function;
#define AARP_REQUEST 1
#define AARP_REPLY 2
#define AARP_PROBE 3
__u8 hw_src[ETH_ALEN] __attribute__ ((packed));
__u8 pa_src_zero __attribute__ ((packed));
__u16 pa_src_net __attribute__ ((packed));
__u8 pa_src_node __attribute__ ((packed));
__u8 hw_dst[ETH_ALEN] __attribute__ ((packed));
__u8 pa_dst_zero __attribute__ ((packed));
__u16 pa_dst_net __attribute__ ((packed));
__u8 pa_dst_node __attribute__ ((packed));
};
typedef struct sock atalk_socket;
#define AARP_EXPIRY_TIME (5*60*HZ)
#define AARP_HASH_SIZE 16
#define AARP_TICK_TIME (HZ/5)
#define AARP_RETRANSMIT_LIMIT 10
#define AARP_RESOLVE_TIME (10*HZ)
extern struct datalink_proto *ddp_dl, *aarp_dl;
extern void aarp_proto_init(void);
extern struct atalk_iface *atalk_find_dev(struct device *dev);
extern struct at_addr *atalk_find_dev_addr(struct device *dev);
extern int aarp_send_ddp(struct device *dev,struct sk_buff *skb, struct at_addr *sa, void *hwaddr);
extern void aarp_send_probe(struct device *dev, struct at_addr *addr);
#ifdef MODULE
extern void aarp_cleanup_module(void);
#endif
#endif
#endif