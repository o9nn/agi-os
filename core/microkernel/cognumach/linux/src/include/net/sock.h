#ifndef _SOCK_H
#define _SOCK_H
#include <linux/timer.h>
#include <linux/ip.h>
#include <linux/in.h>
#include <linux/tcp.h>
#include <linux/config.h>
#include <linux/netdevice.h>
#include <linux/skbuff.h>
#include <net/protocol.h>
#if defined(CONFIG_AX25) || defined(CONFIG_AX25_MODULE)
#include <net/ax25.h>
#if defined(CONFIG_NETROM) || defined(CONFIG_NETROM_MODULE)
#include <net/netrom.h>
#endif
#if defined(CONFIG_ROSE) || defined(CONFIG_ROSE_MODULE)
#include <net/rose.h>
#endif
#endif
#if defined(CONFIG_IPX) || defined(CONFIG_IPX_MODULE)
#include <net/ipx.h>
#endif
#if defined(CONFIG_ATALK) || defined(CONFIG_ATALK_MODULE)
#include <linux/atalk.h>
#endif
#include <linux/igmp.h>
#include <asm/atomic.h>
struct unix_opt
{
int 			family;
char *			name;
int  			locks;
struct inode *		inode;
struct semaphore	readsem;
struct sock *		other;
int 			marksweep;
#define MARKED			1
int			inflight;
};
struct inet_packet_opt
{
struct notifier_block	notifier;
struct device		*bound_dev;
unsigned long		dev_stamp;
struct packet_type	*prot_hook;
char			device_name[15];
};
#if defined(CONFIG_IPX) || defined(CONFIG_IPX_MODULE)
struct ipx_opt
{
ipx_address		dest_addr;
ipx_interface		*intrfc;
unsigned short		port;
#ifdef CONFIG_IPX_INTERN
unsigned char           node[IPX_NODE_LEN];
#endif
unsigned short		type;
struct ncp_server       *ncp_server;
unsigned short		ipx_ncp_conn;
};
#endif
#ifdef CONFIG_NUTCP
struct tcp_opt
{
__u32	rcv_nxt;
__u32	rcv_up;
__u32	rcv_wnd;
__u32	snd_nxt;
__u32	snd_una;
__u32	snd_up;
__u32	snd_wl1;
__u32	snd_wl2;
__u32	snd_cwnd;
__u32	snd_ssthresh;
struct timer_list	delack_timer;
struct timer_list	idle_timer;
struct timer_list	completion_timer;
struct timer_list	probe_timer;
struct timer_list	retransmit_timer;
};
#endif
struct sock
{
struct sock		*sklist_next;
struct sock		*sklist_prev;
struct options		*opt;
atomic_t		wmem_alloc;
atomic_t		rmem_alloc;
unsigned long		allocation;
__u32			write_seq;
__u32			sent_seq;
__u32			acked_seq;
__u32			copied_seq;
__u32			rcv_ack_seq;
unsigned short		rcv_ack_cnt;
__u32			window_seq;
__u32			fin_seq;
__u32			urg_seq;
__u32			urg_data;
__u32			syn_seq;
int			users;
volatile char		dead,
urginline,
intr,
blog,
done,
reuse,
keepopen,
linger,
delay_acks,
destroy,
ack_timed,
no_check,
zapped,
broadcast,
nonagle,
bsdism;
struct device           * bound_device;
unsigned long	        lingertime;
int			proc;
struct sock		*next;
struct sock		**pprev;
struct sock		*bind_next;
struct sock		**bind_pprev;
struct sock		*pair;
int			hashent;
struct sock		*prev;
struct sk_buff		* volatile send_head;
struct sk_buff		* volatile send_next;
struct sk_buff		* volatile send_tail;
struct sk_buff_head	back_log;
struct sk_buff		*partial;
struct timer_list	partial_timer;
long			retransmits;
struct sk_buff_head	write_queue,
receive_queue;
struct proto		*prot;
struct wait_queue	**sleep;
__u32			daddr;
__u32			saddr;
__u32			rcv_saddr;
unsigned short		max_unacked;
unsigned short		window;
__u32                   lastwin_seq;
__u32			high_seq;
volatile unsigned long  ato;
volatile unsigned long  lrcvtime;
volatile unsigned long  idletime;
unsigned int		bytes_rcv;
unsigned short		mtu;
volatile unsigned short	mss;
volatile unsigned short	user_mss;
volatile unsigned short	max_window;
unsigned long 		window_clamp;
unsigned int		ssthresh;
unsigned short		num;
volatile unsigned short	cong_window;
volatile unsigned short	cong_count;
volatile unsigned short	packets_out;
volatile unsigned short	shutdown;
volatile unsigned long	rtt;
volatile unsigned long	mdev;
volatile unsigned long	rto;
volatile unsigned short	backoff;
int			err, err_soft;
unsigned char		protocol;
volatile unsigned char	state;
unsigned short		ack_backlog;
unsigned char		priority;
unsigned char		debug;
int			rcvbuf;
int			sndbuf;
unsigned short		type;
unsigned char		localroute;
union
{
struct unix_opt	af_unix;
#if defined(CONFIG_AX25) || defined(CONFIG_AX25_MODULE)
ax25_cb			*ax25;
#if defined(CONFIG_NETROM) || defined(CONFIG_NETROM_MODULE)
nr_cb			*nr;
#endif
#if defined(CONFIG_ROSE) || defined(CONFIG_ROSE_MODULE)
rose_cb			*rose;
#endif
#endif
#if defined(CONFIG_ATALK) || defined(CONFIG_ATALK_MODULE)
struct atalk_sock	af_at;
#endif
#if defined(CONFIG_IPX) || defined(CONFIG_IPX_MODULE)
struct ipx_opt		af_ipx;
#endif
#ifdef CONFIG_INET
struct inet_packet_opt  af_packet;
#ifdef CONFIG_NUTCP
struct tcp_opt		af_tcp;
#endif
#endif
} protinfo;
int			ip_ttl;
int			ip_tos;
struct tcphdr		dummy_th;
struct timer_list	keepalive_timer;
struct timer_list	retransmit_timer;
struct timer_list	delack_timer;
int			ip_xmit_timeout;
struct rtable		*ip_route_cache;
unsigned char		ip_hdrincl;
#ifdef CONFIG_IP_MULTICAST
int			ip_mc_ttl;
int			ip_mc_loop;
char			ip_mc_name[MAX_ADDR_LEN];
struct ip_mc_socklist	*ip_mc_list;
#endif
int			timeout;
struct timer_list	timer;
struct timeval		stamp;
struct socket		*socket;
void			(*state_change)(struct sock *sk);
void			(*data_ready)(struct sock *sk,int bytes);
void			(*write_space)(struct sock *sk);
void			(*error_report)(struct sock *sk);
unsigned short		max_ack_backlog;
struct sock		*listening;
};
struct proto
{
struct sock		*sklist_next;
struct sock		*sklist_prev;
void			(*close)(struct sock *sk, unsigned long timeout);
int			(*build_header)(struct sk_buff *skb,
__u32 saddr,
__u32 daddr,
struct device **dev, int type,
struct options *opt, int len,
int tos, int ttl, struct rtable ** rp);
int			(*connect)(struct sock *sk,
struct sockaddr_in *usin, int addr_len);
struct sock *		(*accept) (struct sock *sk, int flags);
void			(*queue_xmit)(struct sock *sk,
struct device *dev, struct sk_buff *skb,
int free);
void			(*retransmit)(struct sock *sk, int all);
void			(*write_wakeup)(struct sock *sk);
void			(*read_wakeup)(struct sock *sk);
int			(*rcv)(struct sk_buff *buff, struct device *dev,
struct options *opt, __u32 daddr,
unsigned short len, __u32 saddr,
int redo, struct inet_protocol *protocol);
int			(*select)(struct sock *sk, int which,
select_table *wait);
int			(*ioctl)(struct sock *sk, int cmd,
unsigned long arg);
int			(*init)(struct sock *sk);
void			(*shutdown)(struct sock *sk, int how);
int			(*setsockopt)(struct sock *sk, int level, int optname,
char *optval, int optlen);
int			(*getsockopt)(struct sock *sk, int level, int optname,
char *optval, int *option);
int			(*sendmsg)(struct sock *sk, struct msghdr *msg, int len,
int noblock, int flags);
int			(*recvmsg)(struct sock *sk, struct msghdr *msg, int len,
int noblock, int flags, int *addr_len);
int			(*bind)(struct sock *sk, struct sockaddr *uaddr, int addr_len);
void			(*hash)(struct sock *sk);
void			(*unhash)(struct sock *sk);
void			(*rehash)(struct sock *sk);
unsigned short		(*good_socknum)(void);
int			(*verify_bind)(struct sock *sk, unsigned short snum);
unsigned short		max_header;
unsigned long		retransmits;
char			name[32];
int			inuse, highestinuse;
};
#define TIME_WRITE	1
#define TIME_CLOSE	2
#define TIME_KEEPOPEN	3
#define TIME_DESTROY	4
#define TIME_DONE	5
#define TIME_PROBE0	6
#define SOCK_DESTROY_TIME (10*HZ)
#define PROT_SOCK	1024
#define SHUTDOWN_MASK	3
#define RCV_SHUTDOWN	1
#define SEND_SHUTDOWN	2
#define SOCKHASH_LOCK()		start_bh_atomic()
#define SOCKHASH_UNLOCK()	end_bh_atomic()
static __inline__ void add_to_prot_sklist(struct sock *sk)
{
SOCKHASH_LOCK();
if(!sk->sklist_next) {
struct proto *p = sk->prot;
sk->sklist_prev = (struct sock *) p;
sk->sklist_next = p->sklist_next;
p->sklist_next->sklist_prev = sk;
p->sklist_next = sk;
sk->prot->inuse += 1;
if(sk->prot->highestinuse < sk->prot->inuse)
sk->prot->highestinuse = sk->prot->inuse;
}
SOCKHASH_UNLOCK();
}
static __inline__ void del_from_prot_sklist(struct sock *sk)
{
SOCKHASH_LOCK();
if(sk->sklist_next) {
sk->sklist_next->sklist_prev = sk->sklist_prev;
sk->sklist_prev->sklist_next = sk->sklist_next;
sk->sklist_next = NULL;
sk->prot->inuse--;
}
SOCKHASH_UNLOCK();
}
extern void __release_sock(struct sock *sk);
static inline void lock_sock(struct sock *sk)
{
#if 0
if (sk->users) {
__label__ here;
printk("double lock on socket at %p\n", &&here);
here:
}
#endif
sk->users++;
barrier();
}
static inline void release_sock(struct sock *sk)
{
barrier();
#if 0
if (sk->users == 0) {
__label__ here;
sk->users = 1;
printk("trying to unlock unlocked socket at %p\n", &&here);
here:
}
#endif
if ((sk->users = sk->users-1) == 0)
__release_sock(sk);
}
extern struct sock *		sk_alloc(int priority);
extern void			sk_free(struct sock *sk);
extern void			destroy_sock(struct sock *sk);
extern struct sk_buff		*sock_wmalloc(struct sock *sk,
unsigned long size, int force,
int priority);
extern struct sk_buff		*sock_rmalloc(struct sock *sk,
unsigned long size, int force,
int priority);
extern void			sock_wfree(struct sock *sk,
struct sk_buff *skb);
extern void			sock_rfree(struct sock *sk,
struct sk_buff *skb);
extern unsigned long		sock_rspace(struct sock *sk);
extern unsigned long		sock_wspace(struct sock *sk);
extern int			sock_setsockopt(struct sock *sk, int level,
int op, char *optval,
int optlen);
extern int			sock_getsockopt(struct sock *sk, int level,
int op, char *optval,
int *optlen);
extern struct sk_buff 		*sock_alloc_send_skb(struct sock *skb,
unsigned long size,
unsigned long fallback,
int noblock,
int *errcode);
static __inline__ int sock_queue_rcv_skb(struct sock *sk, struct sk_buff *skb)
{
if (sk->rmem_alloc + skb->truesize >= sk->rcvbuf)
return -ENOMEM;
atomic_add(skb->truesize, &sk->rmem_alloc);
skb->sk=sk;
skb_queue_tail(&sk->receive_queue,skb);
if (!sk->dead)
sk->data_ready(sk,skb->len);
return 0;
}
static __inline__ int __sock_queue_rcv_skb(struct sock *sk, struct sk_buff *skb)
{
if (sk->rmem_alloc + skb->truesize >= sk->rcvbuf)
return -ENOMEM;
atomic_add(skb->truesize, &sk->rmem_alloc);
skb->sk=sk;
__skb_queue_tail(&sk->receive_queue,skb);
if (!sk->dead)
sk->data_ready(sk,skb->len);
return 0;
}
static __inline__ int sock_error(struct sock *sk)
{
int err=xchg(&sk->err,0);
return -err;
}
extern struct sock *timer_base;
extern void delete_timer (struct sock *);
extern void reset_timer (struct sock *, int, unsigned long);
extern void net_timer (unsigned long);
#define NETDEBUG(x)	do { } while (0)
#endif