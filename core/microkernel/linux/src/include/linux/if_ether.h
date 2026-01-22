#ifndef _LINUX_IF_ETHER_H
#define _LINUX_IF_ETHER_H
#define ETH_ALEN	6
#define ETH_HLEN	14
#define ETH_ZLEN	60
#define ETH_DATA_LEN	1500
#define ETH_FRAME_LEN	1514
#define ETH_P_LOOP	0x0060
#define ETH_P_ECHO	0x0200
#define ETH_P_PUP	0x0400
#define ETH_P_IP	0x0800
#define ETH_P_X25	0x0805
#define ETH_P_ARP	0x0806
#define	ETH_P_BPQ	0x08FF
#define ETH_P_DEC       0x6000
#define ETH_P_DNA_DL    0x6001
#define ETH_P_DNA_RC    0x6002
#define ETH_P_DNA_RT    0x6003
#define ETH_P_LAT       0x6004
#define ETH_P_DIAG      0x6005
#define ETH_P_CUST      0x6006
#define ETH_P_SCA       0x6007
#define ETH_P_RARP      0x8035
#define ETH_P_ATALK	0x809B
#define ETH_P_AARP	0x80F3
#define ETH_P_IPX	0x8137
#define ETH_P_IPV6	0x86DD
#define ETH_P_802_3	0x0001
#define ETH_P_AX25	0x0002
#define ETH_P_ALL	0x0003
#define ETH_P_802_2	0x0004
#define ETH_P_SNAP	0x0005
#define ETH_P_DDCMP     0x0006
#define ETH_P_WAN_PPP   0x0007
#define ETH_P_PPP_MP    0x0008
#define ETH_P_LOCALTALK 0x0009
#define ETH_P_PPPTALK	0x0010
#define ETH_P_TR_802_2	0x0011
struct ethhdr
{
unsigned char	h_dest[ETH_ALEN];
unsigned char	h_source[ETH_ALEN];
unsigned short	h_proto;
};
struct enet_statistics
{
int	rx_packets;
int	tx_packets;
int	rx_errors;
int	tx_errors;
int	rx_dropped;
int	tx_dropped;
int	multicast;
int	collisions;
int	rx_length_errors;
int	rx_over_errors;
int	rx_crc_errors;
int	rx_frame_errors;
int	rx_fifo_errors;
int	rx_missed_errors;
int	tx_aborted_errors;
int	tx_carrier_errors;
int	tx_fifo_errors;
int	tx_heartbeat_errors;
int	tx_window_errors;
};
#endif