#ifndef __LINUX_MROUTE_H
#define __LINUX_MROUTE_H
#include <linux/sockios.h>
#include <linux/in.h>
#define MRT_BASE	200
#define MRT_INIT	(MRT_BASE)
#define MRT_DONE	(MRT_BASE+1)
#define MRT_ADD_VIF	(MRT_BASE+2)
#define MRT_DEL_VIF	(MRT_BASE+3)
#define MRT_ADD_MFC	(MRT_BASE+4)
#define MRT_DEL_MFC	(MRT_BASE+5)
#define MRT_VERSION	(MRT_BASE+6)
#define MRT_ASSERT	(MRT_BASE+7)
#define MRT_PIM		(MRT_BASE+8)
#define SIOCGETVIFCNT	SIOCPROTOPRIVATE
#define SIOCGETSGCNT	(SIOCPROTOPRIVATE+1)
#define SIOCGETRPF	(SIOCPROTOPRIVATE+2)
#define MAXVIFS		32
typedef unsigned long vifbitmap_t;
typedef unsigned short vifi_t;
#define ALL_VIFS	((vifi_t)(-1))
#define VIFM_SET(n,m)	((m)|=(1<<(n)))
#define VIFM_CLR(n,m)	((m)&=~(1<<(n)))
#define VIFM_ISSET(n,m)	((m)&(1<<(n)))
#define VIFM_CLRALL(m)	((m)=0)
#define VIFM_COPY(mfrom,mto)	((mto)=(mfrom))
#define VIFM_SAME(m1,m2)	((m1)==(m2))
struct vifctl {
vifi_t	vifc_vifi;
unsigned char vifc_flags;
unsigned char vifc_threshold;
unsigned int vifc_rate_limit;
struct in_addr vifc_lcl_addr;
struct in_addr vifc_rmt_addr;
};
#define VIFF_TUNNEL	0x1
#define VIFF_SRCRT	0x2
#define VIFF_REGISTER	0x4
struct mfcctl
{
struct in_addr mfcc_origin;
struct in_addr mfcc_mcastgrp;
vifi_t	mfcc_parent;
unsigned char mfcc_ttls[MAXVIFS];
unsigned int mfcc_pkt_cnt;
unsigned int mfcc_byte_cnt;
unsigned int mfcc_wrong_if;
int	     mfcc_expire;
};
struct sioc_sg_req
{
struct in_addr src;
struct in_addr grp;
unsigned long pktcnt;
unsigned long bytecnt;
unsigned long wrong_if;
};
struct sioc_vif_req
{
vifi_t	vifi;
unsigned long icount;
unsigned long ocount;
unsigned long ibytes;
unsigned long obytes;
};
struct igmpmsg
{
__u32 unused1,unused2;
unsigned char im_msgtype;
unsigned char im_mbz;
unsigned char im_vif;
unsigned char unused3;
struct in_addr im_src,im_dst;
};
#ifdef __KERNEL__
extern struct sock *mroute_socket;
extern int ip_mroute_setsockopt(struct sock *, int, char *, int);
extern int ip_mroute_getsockopt(struct sock *, int, char *, int *);
extern int ipmr_ioctl(struct sock *sk, int cmd, unsigned long arg);
extern void mroute_close(struct sock *sk);
extern void ipmr_forward(struct sk_buff *skb, int is_frag);
extern int ip_mr_find_tunnel(__u32, __u32);
extern void ip_mr_init(void);
struct vif_device
{
struct device 	*dev;
unsigned long	bytes_in,bytes_out;
unsigned long	pkt_in,pkt_out;
unsigned long	rate_limit;
unsigned char	threshold;
unsigned short	flags;
__u32		local,remote;
int		link;
};
struct mfc_cache
{
struct mfc_cache *next;
__u32 mfc_mcastgrp;
__u32 mfc_origin;
vifi_t mfc_parent;
struct timer_list mfc_timer;
int mfc_flags;
struct sk_buff_head mfc_unresolved;
int mfc_queuelen;
unsigned long mfc_last_assert;
int mfc_minvif;
int mfc_maxvif;
unsigned long mfc_bytes;
unsigned long mfc_pkt;
unsigned long mfc_wrong_if;
unsigned char mfc_ttls[MAXVIFS];
};
#define MFC_QUEUED		1
#define MFC_RESOLVED		2
#define MFC_NOTIFY		4
#define MFC_LINES		64
#ifdef __BIG_ENDIAN
#define MFC_HASH(a,b)	((((a)>>24)^((b)>>26))&(MFC_LINES-1))
#else
#define MFC_HASH(a,b)	(((a)^((b)>>2))&(MFC_LINES-1))
#endif
#endif
#define MFC_ASSERT_THRESH (3*HZ)
#define IGMPMSG_NOCACHE		1
#define IGMPMSG_WRONGVIF	2
#define IGMPMSG_WHOLEPKT	3
#ifdef __KERNEL__
#define PIM_V1_VERSION		__constant_htonl(0x10000000)
#define PIM_V1_REGISTER		1
#define PIM_VERSION		2
#define PIM_REGISTER		1
#define PIM_NULL_REGISTER	__constant_htonl(0x40000000)
struct pimreghdr
{
__u8	type;
__u8	reserved;
__u16	csum;
__u32	flags;
};
extern int pim_rcv(struct sk_buff * , unsigned short);
extern int pim_rcv_v1(struct sk_buff * , unsigned short len);
struct rtmsg;
extern int ipmr_get_route(struct sk_buff *skb, struct rtmsg *rtm, int nowait);
#endif
#endif