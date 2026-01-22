#ifndef __NET_SPX_H
#define __NET_SPX_H
struct spxhdr
{
__u8 cctl;
#define CCTL_SPXII_XHD	0x01
#define CCTL_SPX_UNKNOWN 0x02
#define CCTL_SPXII_NEG	0x04
#define CCTL_SPXII	0x08
#define CCTL_EOM	0x10
#define CCTL_URG	0x20
#define CCTL_ACK	0x40
#define CCTL_CTL	0x80
__u8 dtype;
#define SPX_DTYPE_ECONN	0xFE
#define SPX_DTYPE_ECACK	0xFF
__u16 sconn;
__u16 dconn;
__u16 sequence;
__u16 ackseq;
__u16 allocseq;
};
#define IPXTYPE_SPX	5
#endif