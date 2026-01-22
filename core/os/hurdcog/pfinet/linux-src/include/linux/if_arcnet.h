#ifndef _LINUX_IF_ARCNET_H
#define _LINUX_IF_ARCNET_H
#define ARC_P_IP	212
#define ARC_P_ARP	213
#define ARC_P_RARP	214
#define ARC_P_IPX	250
#define ARC_P_NOVELL_EC	236
#define ARC_P_IP_RFC1051 240
#define ARC_P_ARP_RFC1051 241
#define ARC_P_ETHER	0xE8
#define ARC_P_DATAPOINT_BOOT	0
#define ARC_P_DATAPOINT_MOUNT	1
#define ARC_P_POWERLAN_BEACON	8
#define ARC_P_POWERLAN_BEACON2	243
#define ARC_P_LANSOFT	251
#define ARC_P_ATALK	0xDD
struct archdr
{
u_char	source,
destination,
offset1,
offset2;
};
#endif