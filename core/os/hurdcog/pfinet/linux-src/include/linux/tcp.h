#ifndef _LINUX_TCP_H
#define _LINUX_TCP_H
#include <linux/types.h>
#include <asm/byteorder.h>
struct tcphdr {
__u16	source;
__u16	dest;
__u32	seq;
__u32	ack_seq;
#if defined(__LITTLE_ENDIAN_BITFIELD)
__u16	res1:4,
doff:4,
fin:1,
syn:1,
rst:1,
psh:1,
ack:1,
urg:1,
res2:2;
#elif defined(__BIG_ENDIAN_BITFIELD)
__u16	doff:4,
res1:4,
res2:2,
urg:1,
ack:1,
psh:1,
rst:1,
syn:1,
fin:1;
#else
#error	"Adjust your <asm/byteorder.h> defines"
#endif
__u16	window;
__u16	check;
__u16	urg_ptr;
};
enum {
TCP_ESTABLISHED = 1,
TCP_SYN_SENT,
TCP_SYN_RECV,
TCP_FIN_WAIT1,
TCP_FIN_WAIT2,
TCP_TIME_WAIT,
TCP_CLOSE,
TCP_CLOSE_WAIT,
TCP_LAST_ACK,
TCP_LISTEN,
TCP_CLOSING,
TCP_MAX_STATES
};
#define TCP_STATE_MASK	0xF
#define TCP_ACTION_FIN	(1 << 7)
enum {
TCPF_ESTABLISHED = (1 << 1),
TCPF_SYN_SENT  = (1 << 2),
TCPF_SYN_RECV  = (1 << 3),
TCPF_FIN_WAIT1 = (1 << 4),
TCPF_FIN_WAIT2 = (1 << 5),
TCPF_TIME_WAIT = (1 << 6),
TCPF_CLOSE     = (1 << 7),
TCPF_CLOSE_WAIT = (1 << 8),
TCPF_LAST_ACK  = (1 << 9),
TCPF_LISTEN    = (1 << 10),
TCPF_CLOSING   = (1 << 11)
};
#endif