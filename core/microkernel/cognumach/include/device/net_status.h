#ifndef	_DEVICE_NET_STATUS_H_
#define	_DEVICE_NET_STATUS_H_
#include <device/device_types.h>
#include <mach/message.h>
struct net_status {
int	min_packet_size;
int	max_packet_size;
int	header_format;
int	header_size;
int	address_size;
int	flags;
int	mapped_size;
};
#define	NET_STATUS_COUNT	(sizeof(struct net_status)/sizeof(int))
#define	NET_STATUS		(('n'<<16) + 1)
#define	HDR_ETHERNET		1
#define	HDR_EXP_ETHERNET	2
#define	HDR_PRO_NET		4
#define	HDR_CHAOS		5
#define	HDR_802			6
#define	NET_ADDRESS		(('n'<<16) + 2)
#define	NET_DSTADDR		(('n'<<16) + 3)
#define	NET_FLAGS		(('n'<<16) + 4)
#define	NET_MAX_FILTER		128
#define	NET_FILTER_STACK_DEPTH	32
typedef	unsigned short	filter_t;
typedef filter_t	*filter_array_t;
#define CSPF_BYTES(n) ((n) * sizeof (filter_t))
#define NETF_NBPA	10
#define NETF_NBPO	6
#define	NETF_ARG(word)	((word) & 0x3ff)
#define	NETF_OP(word)	(((word)>>NETF_NBPA)&0x3f)
#define NETF_TYPE_MASK	(((1 << NETF_NBPO) - 1) << NETF_NBPA)
#define NETF_BPF	(1 << NETF_NBPA)
#define NETF_IN		0x1
#define NETF_OUT	0x2
#define NETF_NOP	(0<<NETF_NBPA)
#define NETF_EQ		(1<<NETF_NBPA)
#define NETF_LT		(2<<NETF_NBPA)
#define NETF_LE		(3<<NETF_NBPA)
#define NETF_GT		(4<<NETF_NBPA)
#define NETF_GE		(5<<NETF_NBPA)
#define NETF_AND	(6<<NETF_NBPA)
#define NETF_OR		(7<<NETF_NBPA)
#define NETF_XOR	(8<<NETF_NBPA)
#define NETF_COR	(9<<NETF_NBPA)
#define NETF_CAND	(10<<NETF_NBPA)
#define NETF_CNOR	(11<<NETF_NBPA)
#define NETF_CNAND	(12<<NETF_NBPA)
#define NETF_NEQ	(13<<NETF_NBPA)
#define	NETF_LSH	(14<<NETF_NBPA)
#define	NETF_RSH	(15<<NETF_NBPA)
#define	NETF_ADD	(16<<NETF_NBPA)
#define	NETF_SUB	(17<<NETF_NBPA)
#define NETF_NOPUSH	0
#define NETF_PUSHLIT	1
#define NETF_PUSHZERO	2
#define	NETF_PUSHIND	14
#define	NETF_PUSHHDRIND	15
#define NETF_PUSHWORD	16
#define	NETF_PUSHHDR	960
#define	NETF_PUSHSTK	992
#define	NET_HI_PRI	100
#define	NET_PRI_MAX	255
#include <device/bpf.h>
#define	NET_RCV_MAX	4095
#define	NET_HDW_HDR_MAX	64
#define	NET_RCV_MSG_ID	2999
struct packet_header {
unsigned short	length;
unsigned short	type;
};
struct net_rcv_msg {
mach_msg_header_t msg_hdr;
mach_msg_type_t	header_type;
char		header[NET_HDW_HDR_MAX];
mach_msg_type_t	packet_type;
char		packet[NET_RCV_MAX];
boolean_t	sent;
};
typedef struct net_rcv_msg 	*net_rcv_msg_t;
#define	net_rcv_msg_packet_count packet_type.msgt_number
#endif