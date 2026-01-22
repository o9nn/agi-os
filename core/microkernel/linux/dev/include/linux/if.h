#ifndef _LINUX_IF_H
#define _LINUX_IF_H
#include <linux/types.h>
#include <linux/socket.h>
#ifdef MACH_INCLUDE
#define LINUX_IFF_UP 0x1
#define LINUX_IFF_BROADCAST 0x2
#define LINUX_IFF_DEBUG 0x4
#define LINUX_IFF_LOOPBACK 0x8
#define LINUX_IFF_POINTOPOINT 0x10
#define LINUX_IFF_NOTRAILERS 0x20
#define LINUX_IFF_RUNNING 0x40
#define LINUX_IFF_NOARP 0x80
#define LINUX_IFF_PROMISC 0x100
#define LINUX_IFF_ALLMULTI 0x200
#define LINUX_IFF_MASTER 0x400
#define LINUX_IFF_SLAVE 0x800
#define LINUX_IFF_MULTICAST 0x1000
#define LINUX_IFF_SOFTHEADERS 0x2000
#else
#define IFF_UP 0x1
#define IFF_BROADCAST 0x2
#define IFF_DEBUG 0x4
#define IFF_LOOPBACK 0x8
#define IFF_POINTOPOINT 0x10
#define IFF_NOTRAILERS 0x20
#define IFF_RUNNING 0x40
#define IFF_NOARP 0x80
#define IFF_PROMISC 0x100
#define IFF_ALLMULTI 0x200
#define IFF_MASTER 0x400
#define IFF_SLAVE 0x800
#define IFF_MULTICAST 0x1000
#define IFF_SOFTHEADERS 0x2000
#endif
struct ifaddr
{
struct sockaddr ifa_addr;
union {
struct sockaddr ifu_broadaddr;
struct sockaddr ifu_dstaddr;
} ifa_ifu;
struct iface *ifa_ifp;
struct ifaddr *ifa_next;
};
#define ifa_broadaddr ifa_ifu.ifu_broadaddr
#define ifa_dstaddr ifa_ifu.ifu_dstaddr
struct ifmap
{
unsigned long mem_start;
unsigned long mem_end;
unsigned short base_addr;
unsigned char irq;
unsigned char dma;
unsigned char port;
};
struct ifreq
{
#define IFHWADDRLEN 6
#define IFNAMSIZ 16
union
{
char ifrn_name[IFNAMSIZ];
} ifr_ifrn;
union {
struct sockaddr ifru_addr;
struct sockaddr ifru_dstaddr;
struct sockaddr ifru_broadaddr;
struct sockaddr ifru_netmask;
struct sockaddr ifru_hwaddr;
short ifru_flags;
int ifru_metric;
int ifru_mtu;
struct ifmap ifru_map;
char ifru_slave[IFNAMSIZ];
caddr_t ifru_data;
} ifr_ifru;
};
#define ifr_name ifr_ifrn.ifrn_name
#define ifr_hwaddr ifr_ifru.ifru_hwaddr
#define ifr_addr ifr_ifru.ifru_addr
#define ifr_dstaddr ifr_ifru.ifru_dstaddr
#define ifr_broadaddr ifr_ifru.ifru_broadaddr
#define ifr_netmask ifr_ifru.ifru_netmask
#define ifr_flags ifr_ifru.ifru_flags
#define ifr_metric ifr_ifru.ifru_metric
#define ifr_mtu ifr_ifru.ifru_mtu
#define ifr_map ifr_ifru.ifru_map
#define ifr_slave ifr_ifru.ifru_slave
#define ifr_data ifr_ifru.ifru_data
struct ifconf
{
int ifc_len;
union
{
caddr_t ifcu_buf;
struct ifreq *ifcu_req;
} ifc_ifcu;
};
#define ifc_buf ifc_ifcu.ifcu_buf
#define ifc_req ifc_ifcu.ifcu_req
#endif