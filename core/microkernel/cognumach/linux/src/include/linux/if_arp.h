#ifndef _LINUX_IF_ARP_H
#define _LINUX_IF_ARP_H
#include <linux/netdevice.h>
#define ARPHRD_NETROM	0
#define ARPHRD_ETHER 	1
#define	ARPHRD_EETHER	2
#define	ARPHRD_AX25	3
#define	ARPHRD_PRONET	4
#define	ARPHRD_CHAOS	5
#define	ARPHRD_IEEE802	6
#define	ARPHRD_ARCNET	7
#define	ARPHRD_APPLETLK	8
#define ARPHRD_DLCI	15
#define ARPHRD_METRICOM	23
#define ARPHRD_SLIP	256
#define ARPHRD_CSLIP	257
#define ARPHRD_SLIP6	258
#define ARPHRD_CSLIP6	259
#define ARPHRD_RSRVD	260
#define ARPHRD_ADAPT	264
#define	ARPHRD_ROSE	270
#define ARPHRD_PPP	512
#define ARPHRD_TUNNEL	768
#define ARPHRD_TUNNEL6	769
#define ARPHRD_FRAD	770
#define ARPHRD_SKIP	771
#define ARPHRD_LOOPBACK	772
#define ARPHRD_LOCALTLK 773
#define ARPHRD_FDDI		774
#define	ARPOP_REQUEST	1
#define	ARPOP_REPLY	2
#define	ARPOP_RREQUEST	3
#define	ARPOP_RREPLY	4
struct arpreq {
struct sockaddr	arp_pa;
struct sockaddr	arp_ha;
int			arp_flags;
struct sockaddr       arp_netmask;
char			arp_dev[16];
};
struct arpreq_old {
struct sockaddr	arp_pa;
struct sockaddr	arp_ha;
int			arp_flags;
struct sockaddr       arp_netmask;
};
#define ATF_COM		0x02
#define	ATF_PERM	0x04
#define	ATF_PUBL	0x08
#define	ATF_USETRAILERS	0x10
#define ATF_NETMASK     0x20
struct arphdr
{
unsigned short	ar_hrd;
unsigned short	ar_pro;
unsigned char	ar_hln;
unsigned char	ar_pln;
unsigned short	ar_op;
#if 0
unsigned char		ar_sha[ETH_ALEN];
unsigned char		ar_sip[4];
unsigned char		ar_tha[ETH_ALEN];
unsigned char		ar_tip[4];
#endif
};
#define ARPD_UPDATE	0x01
#define ARPD_LOOKUP	0x02
#define ARPD_FLUSH	0x03
struct arpd_request
{
unsigned short	req;
__u32		ip;
unsigned long	dev;
unsigned long	stamp;
unsigned long	updated;
unsigned char	ha[MAX_ADDR_LEN];
};
#endif