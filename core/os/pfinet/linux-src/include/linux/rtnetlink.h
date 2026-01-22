#ifndef __LINUX_RTNETLINK_H
#define __LINUX_RTNETLINK_H
#ifdef __KERNEL__
#include <linux/config.h>
#endif
#include <linux/netlink.h>
#define RTNL_DEBUG 1
#define RTM_BASE 0x10
#define RTM_NEWLINK (RTM_BASE+0)
#define RTM_DELLINK (RTM_BASE+1)
#define RTM_GETLINK (RTM_BASE+2)
#define RTM_NEWADDR (RTM_BASE+4)
#define RTM_DELADDR (RTM_BASE+5)
#define RTM_GETADDR (RTM_BASE+6)
#define RTM_NEWROUTE (RTM_BASE+8)
#define RTM_DELROUTE (RTM_BASE+9)
#define RTM_GETROUTE (RTM_BASE+10)
#define RTM_NEWNEIGH (RTM_BASE+12)
#define RTM_DELNEIGH (RTM_BASE+13)
#define RTM_GETNEIGH (RTM_BASE+14)
#define RTM_NEWRULE (RTM_BASE+16)
#define RTM_DELRULE (RTM_BASE+17)
#define RTM_GETRULE (RTM_BASE+18)
#define RTM_NEWQDISC (RTM_BASE+20)
#define RTM_DELQDISC (RTM_BASE+21)
#define RTM_GETQDISC (RTM_BASE+22)
#define RTM_NEWTCLASS (RTM_BASE+24)
#define RTM_DELTCLASS (RTM_BASE+25)
#define RTM_GETTCLASS (RTM_BASE+26)
#define RTM_NEWTFILTER (RTM_BASE+28)
#define RTM_DELTFILTER (RTM_BASE+29)
#define RTM_GETTFILTER (RTM_BASE+30)
#define RTM_MAX (RTM_BASE+31)
struct rtattr
{
unsigned short rta_len;
unsigned short rta_type;
};
#define RTA_ALIGNTO 4
#define RTA_ALIGN(len) ( ((len)+RTA_ALIGNTO-1) & ~(RTA_ALIGNTO-1) )
#define RTA_OK(rta,len) ((len) > 0 && (rta)->rta_len >= sizeof(struct rtattr) && \
(rta)->rta_len <= (len))
#define RTA_NEXT(rta,attrlen) ((attrlen) -= RTA_ALIGN((rta)->rta_len), \
(struct rtattr*)(((char*)(rta)) + RTA_ALIGN((rta)->rta_len)))
#define RTA_LENGTH(len) (RTA_ALIGN(sizeof(struct rtattr)) + (len))
#define RTA_SPACE(len) RTA_ALIGN(RTA_LENGTH(len))
#define RTA_DATA(rta) ((void*)(((char*)(rta)) + RTA_LENGTH(0)))
#define RTA_PAYLOAD(rta) ((int)((rta)->rta_len) - RTA_LENGTH(0))
struct rtmsg
{
unsigned char rtm_family;
unsigned char rtm_dst_len;
unsigned char rtm_src_len;
unsigned char rtm_tos;
unsigned char rtm_table;
unsigned char rtm_protocol;
unsigned char rtm_scope;
unsigned char rtm_type;
unsigned rtm_flags;
};
enum
{
RTN_UNSPEC,
RTN_UNICAST,
RTN_LOCAL,
RTN_BROADCAST,
RTN_ANYCAST,
RTN_MULTICAST,
RTN_BLACKHOLE,
RTN_UNREACHABLE,
RTN_PROHIBIT,
RTN_THROW,
RTN_NAT,
RTN_XRESOLVE,
};
#define RTN_MAX RTN_XRESOLVE
#define RTPROT_UNSPEC 0
#define RTPROT_REDIRECT 1
#define RTPROT_KERNEL 2
#define RTPROT_BOOT 3
#define RTPROT_STATIC 4
#define RTPROT_GATED 8
#define RTPROT_RA 9
#define RTPROT_MRT 10
#define RTPROT_ZEBRA 11
#define RTPROT_BIRD 12
enum rt_scope_t
{
RT_SCOPE_UNIVERSE=0,
RT_SCOPE_SITE=200,
RT_SCOPE_LINK=253,
RT_SCOPE_HOST=254,
RT_SCOPE_NOWHERE=255
};
#define RTM_F_NOTIFY 0x100
#define RTM_F_CLONED 0x200
#define RTM_F_EQUALIZE 0x400
enum rt_class_t
{
RT_TABLE_UNSPEC=0,
RT_TABLE_DEFAULT=253,
RT_TABLE_MAIN=254,
RT_TABLE_LOCAL=255
};
#define RT_TABLE_MAX RT_TABLE_LOCAL
enum rtattr_type_t
{
RTA_UNSPEC,
RTA_DST,
RTA_SRC,
RTA_IIF,
RTA_OIF,
RTA_GATEWAY,
RTA_PRIORITY,
RTA_PREFSRC,
RTA_METRICS,
RTA_MULTIPATH,
RTA_PROTOINFO,
RTA_FLOW,
RTA_CACHEINFO
};
#define RTA_MAX RTA_CACHEINFO
#define RTM_RTA(r) ((struct rtattr*)(((char*)(r)) + NLMSG_ALIGN(sizeof(struct rtmsg))))
#define RTM_PAYLOAD(n) NLMSG_PAYLOAD(n,sizeof(struct rtmsg))
struct rtnexthop
{
unsigned short rtnh_len;
unsigned char rtnh_flags;
unsigned char rtnh_hops;
int rtnh_ifindex;
};
#define RTNH_F_DEAD 1
#define RTNH_F_PERVASIVE 2
#define RTNH_F_ONLINK 4
#define RTNH_ALIGNTO 4
#define RTNH_ALIGN(len) ( ((len)+RTNH_ALIGNTO-1) & ~(RTNH_ALIGNTO-1) )
#define RTNH_OK(rtnh,len) ((rtnh)->rtnh_len >= sizeof(struct rtnexthop) && \
((int)(rtnh)->rtnh_len) <= (len))
#define RTNH_NEXT(rtnh) ((struct rtnexthop*)(((char*)(rtnh)) + RTNH_ALIGN((rtnh)->rtnh_len)))
#define RTNH_LENGTH(len) (RTNH_ALIGN(sizeof(struct rtnexthop)) + (len))
#define RTNH_SPACE(len) RTNH_ALIGN(RTNH_LENGTH(len))
#define RTNH_DATA(rtnh) ((struct rtattr*)(((char*)(rtnh)) + RTNH_LENGTH(0)))
struct rta_cacheinfo
{
__u32 rta_clntref;
__u32 rta_lastuse;
__s32 rta_expires;
__u32 rta_error;
__u32 rta_used;
};
enum
{
RTAX_UNSPEC,
RTAX_LOCK,
RTAX_MTU,
RTAX_WINDOW,
RTAX_RTT,
RTAX_HOPS,
RTAX_SSTHRESH,
RTAX_CWND,
};
#define RTAX_MAX RTAX_CWND
struct ifaddrmsg
{
unsigned char ifa_family;
unsigned char ifa_prefixlen;
unsigned char ifa_flags;
unsigned char ifa_scope;
int ifa_index;
};
enum
{
IFA_UNSPEC,
IFA_ADDRESS,
IFA_LOCAL,
IFA_LABEL,
IFA_BROADCAST,
IFA_ANYCAST,
IFA_CACHEINFO
};
#define IFA_MAX IFA_CACHEINFO
#define IFA_F_SECONDARY 0x01
#define IFA_F_DEPRECATED 0x20
#define IFA_F_TENTATIVE 0x40
#define IFA_F_PERMANENT 0x80
struct ifa_cacheinfo
{
__s32 ifa_prefered;
__s32 ifa_valid;
};
#define IFA_RTA(r) ((struct rtattr*)(((char*)(r)) + NLMSG_ALIGN(sizeof(struct ifaddrmsg))))
#define IFA_PAYLOAD(n) NLMSG_PAYLOAD(n,sizeof(struct ifaddrmsg))
struct ndmsg
{
unsigned char ndm_family;
unsigned char ndm_pad1;
unsigned short ndm_pad2;
int ndm_ifindex;
__u16 ndm_state;
__u8 ndm_flags;
__u8 ndm_type;
};
enum
{
NDA_UNSPEC,
NDA_DST,
NDA_LLADDR,
NDA_CACHEINFO
};
#define NDA_MAX NDA_CACHEINFO
#define NDA_RTA(r) ((struct rtattr*)(((char*)(r)) + NLMSG_ALIGN(sizeof(struct ndmsg))))
#define NDA_PAYLOAD(n) NLMSG_PAYLOAD(n,sizeof(struct ndmsg))
#define NTF_PROXY 0x08
#define NTF_ROUTER 0x80
#define NUD_INCOMPLETE 0x01
#define NUD_REACHABLE 0x02
#define NUD_STALE 0x04
#define NUD_DELAY 0x08
#define NUD_PROBE 0x10
#define NUD_FAILED 0x20
#define NUD_NOARP 0x40
#define NUD_PERMANENT 0x80
#define NUD_NONE 0x00
struct nda_cacheinfo
{
__u32 ndm_confirmed;
__u32 ndm_used;
__u32 ndm_updated;
__u32 ndm_refcnt;
};
struct rtgenmsg
{
unsigned char rtgen_family;
};
struct ifinfomsg
{
unsigned char ifi_family;
unsigned char __ifi_pad;
unsigned short ifi_type;
int ifi_index;
unsigned ifi_flags;
unsigned ifi_change;
};
enum
{
IFLA_UNSPEC,
IFLA_ADDRESS,
IFLA_BROADCAST,
IFLA_IFNAME,
IFLA_MTU,
IFLA_LINK,
IFLA_QDISC,
IFLA_STATS
};
#define IFLA_MAX IFLA_STATS
#define IFLA_RTA(r) ((struct rtattr*)(((char*)(r)) + NLMSG_ALIGN(sizeof(struct ifinfomsg))))
#define IFLA_PAYLOAD(n) NLMSG_PAYLOAD(n,sizeof(struct ifinfomsg))
struct tcmsg
{
unsigned char tcm_family;
unsigned char tcm__pad1;
unsigned short tcm__pad2;
int tcm_ifindex;
__u32 tcm_handle;
__u32 tcm_parent;
__u32 tcm_info;
};
enum
{
TCA_UNSPEC,
TCA_KIND,
TCA_OPTIONS,
TCA_STATS,
TCA_XSTATS,
TCA_RATE,
};
#define TCA_MAX TCA_RATE
#define TCA_RTA(r) ((struct rtattr*)(((char*)(r)) + NLMSG_ALIGN(sizeof(struct tcmsg))))
#define TCA_PAYLOAD(n) NLMSG_PAYLOAD(n,sizeof(struct tcmsg))
#define RTATTR_MAX RTA_MAX
#define RTMGRP_LINK 1
#define RTMGRP_NOTIFY 2
#define RTMGRP_NEIGH 4
#define RTMGRP_TC 8
#define RTMGRP_IPV4_IFADDR 0x10
#define RTMGRP_IPV4_MROUTE 0x20
#define RTMGRP_IPV4_ROUTE 0x40
#define RTMGRP_IPV6_IFADDR 0x100
#define RTMGRP_IPV6_MROUTE 0x200
#define RTMGRP_IPV6_ROUTE 0x400
#ifdef __KERNEL__
extern atomic_t rtnl_rlockct;
extern struct wait_queue *rtnl_wait;
static __inline__ int rtattr_strcmp(struct rtattr *rta, char *str)
{
int len = strlen(str) + 1;
return len > rta->rta_len || memcmp(RTA_DATA(rta), str, len);
}
extern int rtattr_parse(struct rtattr *tb[], int maxattr, struct rtattr *rta, int len);
#ifdef CONFIG_RTNETLINK
extern struct sock *rtnl;
struct rtnetlink_link
{
int (*doit)(struct sk_buff *, struct nlmsghdr*, void *attr);
int (*dumpit)(struct sk_buff *, struct netlink_callback *cb);
};
extern struct rtnetlink_link * rtnetlink_links[NPROTO];
extern int rtnetlink_dump_ifinfo(struct sk_buff *skb, struct netlink_callback *cb);
extern int rtnetlink_send(struct sk_buff *skb, u32 pid, u32 group, int echo);
extern void __rta_fill(struct sk_buff *skb, int attrtype, int attrlen, const void *data);
#define RTA_PUT(skb, attrtype, attrlen, data) \
({ if (skb_tailroom(skb) < (int)RTA_SPACE(attrlen)) goto rtattr_failure; \
__rta_fill(skb, attrtype, attrlen, data); })
extern unsigned long rtnl_wlockct;
static __inline__ int rtnl_shlock_nowait(void)
{
atomic_inc(&rtnl_rlockct);
if (test_bit(0, &rtnl_wlockct)) {
atomic_dec(&rtnl_rlockct);
return -EAGAIN;
}
return 0;
}
static __inline__ void rtnl_shlock(void)
{
while (rtnl_shlock_nowait())
sleep_on(&rtnl_wait);
}
static __inline__ int rtnl_exlock_nowait(void)
{
if (atomic_read(&rtnl_rlockct) > 1)
return -EAGAIN;
if (test_and_set_bit(0, &rtnl_wlockct))
return -EAGAIN;
return 0;
}
static __inline__ void rtnl_exlock(void)
{
while (rtnl_exlock_nowait())
sleep_on(&rtnl_wait);
}
#if 0
static __inline__ void rtnl_shunlock(void)
{
atomic_dec(&rtnl_rlockct);
if (atomic_read(&rtnl_rlockct) <= 1) {
wake_up(&rtnl_wait);
if (rtnl && rtnl->receive_queue.qlen)
rtnl->data_ready(rtnl, 0);
}
}
#else
#define rtnl_shunlock() ({ \
atomic_dec(&rtnl_rlockct); \
if (atomic_read(&rtnl_rlockct) <= 1) { \
wake_up(&rtnl_wait); \
if (rtnl && rtnl->receive_queue.qlen) \
rtnl->data_ready(rtnl, 0); \
} \
})
#endif
static __inline__ void rtnl_exunlock(void)
{
clear_bit(0, &rtnl_wlockct);
wake_up(&rtnl_wait);
}
#else
static __inline__ void rtnl_shlock(void)
{
#ifndef _HURD_
while (atomic_read(&rtnl_rlockct))
sleep_on(&rtnl_wait);
atomic_inc(&rtnl_rlockct);
#endif
}
static __inline__ void rtnl_shunlock(void)
{
#ifndef _HURD_
if (atomic_dec_and_test(&rtnl_rlockct))
wake_up(&rtnl_wait);
#endif
}
static __inline__ void rtnl_exlock(void)
{
}
static __inline__ void rtnl_exunlock(void)
{
}
#endif
extern void rtnl_lock(void);
extern void rtnl_unlock(void);
extern void rtnetlink_init(void);
#endif
#endif