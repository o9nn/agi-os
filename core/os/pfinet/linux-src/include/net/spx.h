#ifndef __NET_SPX_H
#define __NET_SPX_H
#include <net/ipx.h>
struct spxhdr
{	__u8	cctl;
__u8	dtype;
#define SPX_DTYPE_ECONN	0xFE
#define SPX_DTYPE_ECACK	0xFF
__u16	sconn;
__u16	dconn;
__u16	sequence;
__u16	ackseq;
__u16	allocseq;
};
struct ipxspxhdr
{	struct ipxhdr	ipx;
struct spxhdr	spx;
};
#define	SPX_SYS_PKT_LEN	(sizeof(struct ipxspxhdr))
#ifdef __KERNEL__
struct spx_opt
{	int	state;
int	sndbuf;
int	retries;
int	retransmits;
int	max_retries;
int	wd_interval;
void	*owner;
__u16	dest_connid;
__u16	source_connid;
__u16	sequence;
__u16	alloc;
__u16	rmt_ack;
__u16	rmt_seq;
__u16	acknowledge;
__u16	rmt_alloc;
ipx_address	dest_addr;
ipx_address	source_addr;
struct timer_list	watchdog;
struct timer_list	retransmit;
struct sk_buff_head     rcv_queue;
struct sk_buff_head	transmit_queue;
struct sk_buff_head     retransmit_queue;
};
#define CCTL_SPXII_XHD  0x01
#define CCTL_SPX_UNKNOWN 0x02
#define CCTL_SPXII_NEG  0x04
#define CCTL_SPXII      0x08
#define CCTL_EOM        0x10
#define CCTL_URG        0x20
#define CCTL_ACK        0x40
#define CCTL_CTL        0x80
#define CCTL_SYS        CCTL_CTL
#define SPX_CLOSED	7
#define	SPX_CONNECTING	8
#define SPX_CONNECTED	9
#define DATA	0
#define ACK	1
#define WDACK	2
#define CONACK	3
#define	CONREQ	4
#define WDREQ	5
#define	DISCON	6
#define	DISACK	7
#define RETRAN	8
#define TQUEUE	9
#define VERIFY_TIMEOUT  3 * HZ
#define ABORT_TIMEOUT   30 * HZ
#define RETRY_COUNT     10
#define RETRY_TIME      1 * HZ
#define MAX_RETRY_DELAY 5 * HZ
#endif
#endif