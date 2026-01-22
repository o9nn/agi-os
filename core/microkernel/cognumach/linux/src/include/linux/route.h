#ifndef _LINUX_ROUTE_H
#define _LINUX_ROUTE_H
#include <linux/if.h>
struct rtentry
{
unsigned long	rt_hash;
struct sockaddr	rt_dst;
struct sockaddr	rt_gateway;
struct sockaddr	rt_genmask;
short		rt_flags;
short		rt_refcnt;
unsigned long	rt_use;
struct ifnet	*rt_ifp;
short		rt_metric;
char		*rt_dev;
unsigned long	rt_mss;
unsigned long	rt_window;
unsigned short	rt_irtt;
};
#define	RTF_UP		0x0001
#define	RTF_GATEWAY	0x0002
#define	RTF_HOST	0x0004
#define RTF_REINSTATE	0x0008
#define	RTF_DYNAMIC	0x0010
#define	RTF_MODIFIED	0x0020
#define RTF_MSS		0x0040
#define RTF_WINDOW	0x0080
#define RTF_IRTT	0x0100
#define RTF_REJECT	0x0200
#define RTF_NOTCACHED	0x0400
struct netlink_rtinfo
{
unsigned long	rtmsg_type;
struct sockaddr rtmsg_dst;
struct sockaddr rtmsg_gateway;
struct sockaddr rtmsg_genmask;
short 		rtmsg_flags;
short		rtmsg_metric;
char		rtmsg_device[16];
};
#define RTMSG_NEWROUTE		0x01
#define RTMSG_DELROUTE		0x02
#define RTMSG_NEWDEVICE		0x11
#define RTMSG_DELDEVICE		0x12
#endif