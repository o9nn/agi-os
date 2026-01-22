#ifndef _LINUX_IF_TR_H
#define _LINUX_IF_TR_H
#define TR_ALEN 6
#define TR_HLEN (sizeof(struct trh_hdr)+sizeof(struct trllc))
#define AC 0x10
#define LLC_FRAME 0x40
#if 0
#define ETH_HLEN 14
#define ETH_ZLEN 60
#define ETH_DATA_LEN 1500
#define ETH_FRAME_LEN 1514
#endif
#define ETH_P_IP 0x0800
#define ETH_P_ARP 0x0806
#define ETH_P_RARP 0x8035
#define EXTENDED_SAP 0xAA
#define UI_CMD 0x03
struct trh_hdr {
__u8 ac;
__u8 fc;
__u8 daddr[TR_ALEN];
__u8 saddr[TR_ALEN];
__u16 rcf;
__u16 rseg[8];
};
struct trllc {
__u8 dsap;
__u8 ssap;
__u8 llc;
__u8 protid[3];
__u16 ethertype;
};
struct tr_statistics {
int rx_packets;
int tx_packets;
int rx_errors;
int tx_errors;
int rx_dropped;
int tx_dropped;
int multicast;
int transmit_collision;
int line_errors;
int internal_errors;
int burst_errors;
int A_C_errors;
int abort_delimiters;
int lost_frames;
int recv_congest_count;
int frame_copied_errors;
int frequency_errors;
int token_errors;
int dummy1;
};
#define TR_RII 0x80
#define TR_RCF_DIR_BIT 0x80
#define TR_RCF_LEN_MASK 0x1f00
#define TR_RCF_BROADCAST 0x8000
#define TR_RCF_LIMITED_BROADCAST 0xA000
#define TR_RCF_FRAME2K 0x20
#define TR_RCF_BROADCAST_MASK 0xC000
#endif