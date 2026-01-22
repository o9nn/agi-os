#ifndef	_IF_HDR_
#define	_IF_HDR_
#include <kern/lock.h>
#include <kern/queue.h>
struct ifqueue {
queue_head_t	ifq_head;
int		ifq_len;
int		ifq_maxlen;
int		ifq_drops;
decl_simple_lock_data(,
ifq_lock)
};
struct ifnet {
short	if_unit;
short	if_flags;
short	if_timer;
short	if_mtu;
short	if_header_size;
short	if_header_format;
short	if_address_size;
short	if_alloc_size;
char	*if_address;
struct ifqueue if_snd;
queue_head_t if_rcv_port_list;
queue_head_t if_snd_port_list;
decl_simple_lock_data(,
if_rcv_port_list_lock)
decl_simple_lock_data(,
if_snd_port_list_lock)
int	if_ipackets;
int	if_ierrors;
int	if_opackets;
int	if_oerrors;
int	if_collisions;
int	if_rcvdrops;
};
#define	IFF_UP		0x0001
#define	IFF_BROADCAST	0x0002
#define	IFF_DEBUG	0x0004
#define	IFF_LOOPBACK	0x0008
#define	IFF_POINTOPOINT	0x0010
#define	IFF_RUNNING	0x0040
#define	IFF_NOARP	0x0080
#define	IFF_PROMISC	0x0100
#define	IFF_ALLMULTI	0x0200
#define	IFF_BRIDGE	0x0100
#define	IFF_SNAP	0x0200
#define	IFF_CANTCHANGE	(IFF_BROADCAST | IFF_POINTOPOINT | IFF_RUNNING)
#define	IF_QFULL(ifq)		((ifq)->ifq_len >= (ifq)->ifq_maxlen)
#define	IF_DROP(ifq)		((ifq)->ifq_drops++)
#define	IF_ENQUEUE(ifq, ior) \
MACRO_BEGIN \
simple_lock(&(ifq)->ifq_lock); \
enqueue_tail(&(ifq)->ifq_head, (queue_entry_t)ior); \
(ifq)->ifq_len++; \
simple_unlock(&(ifq)->ifq_lock); \
MACRO_END
#define	IF_PREPEND(ifq, ior) \
MACRO_BEGIN \
simple_lock(&(ifq)->ifq_lock); \
enqueue_head(&(ifq)->ifq_head, (queue_entry_t)ior); \
(ifq)->ifq_len++; \
simple_unlock(&(ifq)->ifq_lock); \
MACRO_END
#define	IF_DEQUEUE(ifq, ior) \
MACRO_BEGIN \
simple_lock(&(ifq)->ifq_lock); \
if (((ior) = (io_req_t)dequeue_head(&(ifq)->ifq_head)) != 0) \
(ifq)->ifq_len--; \
simple_unlock(&(ifq)->ifq_lock); \
MACRO_END
#define	IFQ_MAXLEN	50
#define	IFQ_INIT(ifq) \
MACRO_BEGIN \
queue_init(&(ifq)->ifq_head); \
simple_lock_init(&(ifq)->ifq_lock); \
(ifq)->ifq_len = 0; \
(ifq)->ifq_maxlen = IFQ_MAXLEN; \
(ifq)->ifq_drops = 0; \
MACRO_END
#define	IFNET_SLOWHZ	1
#endif