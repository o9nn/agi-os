#ifndef _SOCK_H
#define _SOCK_H
#include <linux/config.h>
#include <linux/timer.h>
#include <linux/in.h>
#if defined(CONFIG_IPV6) || defined (CONFIG_IPV6_MODULE)
#include <linux/in6.h>
#include <linux/ipv6.h>
#include <linux/icmpv6.h>
#include <net/if_inet6.h>
#endif
#if defined(CONFIG_INET) || defined (CONFIG_INET_MODULE)
#include <linux/icmp.h>
#endif
#include <linux/tcp.h>
#include <linux/netdevice.h>
#include <linux/skbuff.h>
#include <net/protocol.h>
#if defined(CONFIG_X25) || defined(CONFIG_X25_MODULE)
#include <net/x25.h>
#endif
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
#if defined(CONFIG_SPX) || defined(CONFIG_SPX_MODULE)
#include <net/spx.h>
#else
#include <net/ipx.h>
#endif
#endif
#if defined(CONFIG_ATALK) || defined(CONFIG_ATALK_MODULE)
#include <linux/atalk.h>
#endif
#if defined(CONFIG_DECNET) || defined(CONFIG_DECNET_MODULE)
#include <net/dn.h>
#endif
#if defined(CONFIG_IRDA) || defined(CONFIG_IRDA_MODULE)
#include <net/irda/irda.h>
#endif
#ifdef CONFIG_FILTER
#include <linux/filter.h>
#endif
#include <asm/atomic.h>
#define MIN_WRITE_SPACE	2048
struct unix_opt {
int 			family;
char *			name;
int  			locks;
struct unix_address	*addr;
struct dentry *		dentry;
struct semaphore	readsem;
struct sock *		other;
struct sock **		list;
struct sock *		gc_tree;
int			inflight;
};
#ifdef CONFIG_NETLINK
struct netlink_callback;
struct netlink_opt {
pid_t			pid;
unsigned		groups;
pid_t			dst_pid;
unsigned		dst_groups;
int			(*handler)(int unit, struct sk_buff *skb);
atomic_t		locks;
struct netlink_callback	*cb;
};
#endif
#if defined(CONFIG_IPX) || defined(CONFIG_IPX_MODULE)
struct ipx_opt {
ipx_address		dest_addr;
ipx_interface		*intrfc;
unsigned short		port;
#ifdef CONFIG_IPX_INTERN
unsigned char           node[IPX_NODE_LEN];
#endif
unsigned short		type;
unsigned short		ipx_ncp_conn;
};
#endif
#if defined(CONFIG_IPV6) || defined (CONFIG_IPV6_MODULE)
struct ipv6_pinfo {
struct in6_addr 	saddr;
struct in6_addr 	rcv_saddr;
struct in6_addr		daddr;
struct in6_addr		*daddr_cache;
__u32			flow_label;
__u32			frag_size;
int			hop_limit;
int			mcast_hops;
int			mcast_oif;
union {
struct {
__u8	srcrt:2,
rxinfo:1,
rxhlim:1,
hopopts:1,
dstopts:1,
authhdr:1,
rxflow:1;
} bits;
__u8		all;
} rxopt;
__u8			mc_loop:1,
recverr:1,
sndflow:1,
pmtudisc:2,
ipv6only:1;
struct ipv6_mc_socklist	*ipv6_mc_list;
struct ipv6_fl_socklist *ipv6_fl_list;
__u32			dst_cookie;
struct ipv6_txoptions	*opt;
struct sk_buff		*pktoptions;
};
struct raw6_opt {
__u32			checksum;
__u32			offset;
struct icmp6_filter	filter;
};
#endif
#if defined(CONFIG_INET) || defined(CONFIG_INET_MODULE)
struct raw_opt {
struct icmp_filter	filter;
};
#endif
struct tcp_sack_block {
__u32	start_seq;
__u32	end_seq;
};
struct tcp_opt {
unsigned int	tcp_header_len;
__u32	pred_flags;
__u32	rcv_nxt;
__u32	snd_nxt;
__u32	snd_una;
__u32	rcv_tstamp;
__u32	lrcvtime;
__u32	srtt;
__u32	ato;
__u32	snd_wl1;
__u32	snd_wl2;
__u32	snd_wnd;
__u32	max_window;
__u32	pmtu_cookie;
__u16	mss_cache;
__u16	mss_clamp;
__u16	ext_header_len;
__u8	pending;
__u8	retransmits;
__u32	last_ack_sent;
__u32	backoff;
__u32	mdev;
__u32	snd_cwnd;
__u32	rto;
__u32	packets_out;
__u32	fackets_out;
__u32	retrans_out;
__u32	high_seq;
__u32	snd_ssthresh;
__u16	snd_cwnd_cnt;
__u8	dup_acks;
__u8	delayed_acks;
__u16	user_mss;
struct timer_list	retransmit_timer;
struct timer_list	delack_timer;
struct sk_buff_head	out_of_order_queue;
struct tcp_func		*af_specific;
struct sk_buff		*send_head;
struct sk_buff		*retrans_head;
__u32	rcv_wnd;
__u32	rcv_wup;
__u32	write_seq;
__u32	copied_seq;
char	tstamp_ok,
wscale_ok,
sack_ok;
char	saw_tstamp;
__u8	snd_wscale;
__u8	rcv_wscale;
__u8	rexmt_done;
__u32	rcv_tsval;
__u32	rcv_tsecr;
__u32	ts_recent;
__u32	ts_recent_stamp;
int	num_sacks;
struct tcp_sack_block selective_acks[4];
struct timer_list	probe_timer;
__u32	window_clamp;
__u32	probes_out;
__u32	syn_seq;
__u32	fin_seq;
__u32	urg_seq;
__u32	urg_data;
__u32	last_seg_size;
__u32	rcv_mss;
__u32 	partial_writers;
struct open_request	*syn_wait_queue;
struct open_request	**syn_wait_last;
int syn_backlog;
};
#define SOCK_DEBUGGING
#ifdef SOCK_DEBUGGING
#define SOCK_DEBUG(sk, msg...) do { if((sk) && ((sk)->debug)) printk(KERN_DEBUG msg); } while (0)
#else
#define SOCK_DEBUG(sk, msg...) do { } while (0)
#endif
struct sock {
struct sock		*sklist_next;
struct sock		*sklist_prev;
struct sock		*bind_next;
struct sock		**bind_pprev;
__u32			daddr;
__u32			rcv_saddr;
__u16			dport;
unsigned short		num;
int			bound_dev_if;
struct sock		*next;
struct sock		**pprev;
volatile unsigned char	state,
zapped;
__u16			sport;
unsigned short		family;
unsigned char		reuse,
nonagle;
atomic_t		sock_readers;
int			rcvbuf;
struct wait_queue	**sleep;
struct dst_entry	*dst_cache;
atomic_t		rmem_alloc;
struct sk_buff_head	receive_queue;
atomic_t		wmem_alloc;
struct sk_buff_head	write_queue;
atomic_t		omem_alloc;
__u32			saddr;
unsigned int		allocation;
int			sndbuf;
struct sock		*prev;
volatile char		dead,
done,
urginline,
keepopen,
linger,
destroy,
no_check,
broadcast,
bsdism;
unsigned char		debug;
int			proc;
unsigned long	        lingertime;
int			hashent;
struct sock		*pair;
struct sk_buff_head	back_log,
error_queue;
struct proto		*prot;
unsigned short		shutdown;
#if defined(CONFIG_IPV6) || defined (CONFIG_IPV6_MODULE)
union {
struct ipv6_pinfo	af_inet6;
} net_pinfo;
#endif
union {
struct tcp_opt		af_tcp;
#if defined(CONFIG_INET) || defined (CONFIG_INET_MODULE)
struct raw_opt		tp_raw4;
#endif
#if defined(CONFIG_IPV6) || defined (CONFIG_IPV6_MODULE)
struct raw6_opt		tp_raw;
#endif
#if defined(CONFIG_SPX) || defined (CONFIG_SPX_MODULE)
struct spx_opt		af_spx;
#endif
} tp_pinfo;
int			err, err_soft;
unsigned short		ack_backlog;
unsigned short		max_ack_backlog;
__u32			priority;
unsigned short		type;
unsigned char		localroute;
unsigned char		protocol;
struct ucred		peercred;
#ifdef CONFIG_FILTER
struct sk_filter      	*filter;
#endif
union {
void *destruct_hook;
struct unix_opt	af_unix;
#if defined(CONFIG_ATALK) || defined(CONFIG_ATALK_MODULE)
struct atalk_sock	af_at;
#endif
#if defined(CONFIG_IPX) || defined(CONFIG_IPX_MODULE)
struct ipx_opt		af_ipx;
#endif
#if defined (CONFIG_DECNET) || defined(CONFIG_DECNET_MODULE)
struct dn_scp           dn;
#endif
#if defined (CONFIG_PACKET) || defined(CONFIG_PACKET_MODULE)
struct packet_opt	*af_packet;
#endif
#if defined(CONFIG_X25) || defined(CONFIG_X25_MODULE)
x25_cb			*x25;
#endif
#if defined(CONFIG_AX25) || defined(CONFIG_AX25_MODULE)
ax25_cb			*ax25;
#endif
#if defined(CONFIG_NETROM) || defined(CONFIG_NETROM_MODULE)
nr_cb			*nr;
#endif
#if defined(CONFIG_ROSE) || defined(CONFIG_ROSE_MODULE)
rose_cb			*rose;
#endif
#ifdef CONFIG_NETLINK
struct netlink_opt	af_netlink;
#endif
#if defined(CONFIG_ECONET) || defined(CONFIG_ECONET_MODULE)
struct econet_opt	*af_econet;
#endif
#if defined(CONFIG_IRDA) || defined(CONFIG_IRDA_MODULE)
struct irda_sock        *irda;
#endif
} protinfo;
int			ip_ttl;
int			ip_tos;
unsigned	   	ip_cmsg_flags;
struct ip_options	*opt;
unsigned char		ip_hdrincl;
__u8			ip_mc_ttl;
__u8			ip_mc_loop;
__u8			ip_recverr;
__u8			ip_pmtudisc;
int			ip_mc_index;
__u32			ip_mc_addr;
struct ip_mc_socklist	*ip_mc_list;
int			timeout;
struct timer_list	timer;
struct timeval		stamp;
struct socket		*socket;
void			*user_data;
void			(*state_change)(struct sock *sk);
void			(*data_ready)(struct sock *sk,int bytes);
void			(*write_space)(struct sock *sk);
void			(*error_report)(struct sock *sk);
int			(*backlog_rcv) (struct sock *sk,
struct sk_buff *skb);
void                    (*destruct)(struct sock *sk);
};
struct proto {
struct sock		*sklist_next;
struct sock		*sklist_prev;
void			(*close)(struct sock *sk,
long timeout);
int			(*connect)(struct sock *sk,
struct sockaddr *uaddr,
int addr_len);
struct sock *		(*accept) (struct sock *sk, int flags);
void			(*retransmit)(struct sock *sk, int all);
void			(*write_wakeup)(struct sock *sk);
void			(*read_wakeup)(struct sock *sk);
unsigned int		(*poll)(struct file * file, struct socket *sock,
struct poll_table_struct *wait);
int			(*ioctl)(struct sock *sk, int cmd,
unsigned long arg);
int			(*init)(struct sock *sk);
int			(*destroy)(struct sock *sk);
void			(*shutdown)(struct sock *sk, int how);
int			(*setsockopt)(struct sock *sk, int level,
int optname, char *optval, int optlen);
int			(*getsockopt)(struct sock *sk, int level,
int optname, char *optval,
int *option);
int			(*sendmsg)(struct sock *sk, struct msghdr *msg,
int len);
int			(*recvmsg)(struct sock *sk, struct msghdr *msg,
int len, int noblock, int flags,
int *addr_len);
int			(*bind)(struct sock *sk,
struct sockaddr *uaddr, int addr_len);
int			(*backlog_rcv) (struct sock *sk,
struct sk_buff *skb);
void			(*hash)(struct sock *sk);
void			(*unhash)(struct sock *sk);
int			(*get_port)(struct sock *sk, unsigned short snum);
unsigned short		max_header;
unsigned long		retransmits;
char			name[32];
int			inuse, highestinuse;
};
#define TIME_WRITE	1
#define TIME_RETRANS	2
#define TIME_DACK	3
#define TIME_CLOSE	4
#define TIME_KEEPOPEN	5
#define TIME_DESTROY	6
#define TIME_DONE	7
#define TIME_PROBE0	8
#define SOCK_DESTROY_TIME (10*HZ)
#define PROT_SOCK	1024
#define SHUTDOWN_MASK	3
#define RCV_SHUTDOWN	1
#define SEND_SHUTDOWN	2
#define SOCKHASH_LOCK()		start_bh_atomic()
#define SOCKHASH_UNLOCK()	end_bh_atomic()
static __inline__ void __add_to_prot_sklist(struct sock *sk)
{
struct proto *p = sk->prot;
sk->sklist_prev = (struct sock *) p;
sk->sklist_next = p->sklist_next;
p->sklist_next->sklist_prev = sk;
p->sklist_next = sk;
sk->prot->inuse += 1;
if(sk->prot->highestinuse < sk->prot->inuse)
sk->prot->highestinuse = sk->prot->inuse;
}
static __inline__ void add_to_prot_sklist(struct sock *sk)
{
SOCKHASH_LOCK();
if(!sk->sklist_next)
__add_to_prot_sklist(sk);
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
if (atomic_read(&sk->sock_readers)) {
printk("double lock on socket at %p\n", gethere());
here:
}
#endif
atomic_inc(&sk->sock_readers);
synchronize_bh();
}
static inline void release_sock(struct sock *sk)
{
barrier();
if (atomic_dec_and_test(&sk->sock_readers))
__release_sock(sk);
}
static __inline__ int min(unsigned int a, unsigned int b)
{
if (a > b)
a = b;
return a;
}
static __inline__ int max(unsigned int a, unsigned int b)
{
if (a < b)
a = b;
return a;
}
extern struct sock *		sk_alloc(int family, int priority, int zero_it);
extern void			sk_free(struct sock *sk);
extern void			destroy_sock(struct sock *sk);
extern struct sk_buff		*sock_wmalloc(struct sock *sk,
unsigned long size, int force,
int priority);
extern struct sk_buff		*sock_rmalloc(struct sock *sk,
unsigned long size, int force,
int priority);
extern void			sock_wfree(struct sk_buff *skb);
extern void			sock_rfree(struct sk_buff *skb);
extern unsigned long		sock_rspace(struct sock *sk);
extern int			sock_setsockopt(struct socket *sock, int level,
int op, char *optval,
int optlen);
extern int			sock_getsockopt(struct socket *sock, int level,
int op, char *optval,
int *optlen);
extern struct sk_buff 		*sock_alloc_send_skb(struct sock *sk,
unsigned long size,
unsigned long fallback,
int noblock,
int *errcode);
extern void *sock_kmalloc(struct sock *sk, int size, int priority);
extern void sock_kfree_s(struct sock *sk, void *mem, int size);
extern int                      sock_no_dup(struct socket *, struct socket *);
extern int                      sock_no_release(struct socket *,
struct socket *);
extern int                      sock_no_bind(struct socket *,
struct sockaddr *, int);
extern int                      sock_no_connect(struct socket *,
struct sockaddr *, int, int);
extern int                      sock_no_socketpair(struct socket *,
struct socket *);
extern int                      sock_no_accept(struct socket *,
struct socket *, int);
extern int                      sock_no_getname(struct socket *,
struct sockaddr *, int *, int);
extern unsigned int             sock_no_poll(struct file *, struct socket *,
struct poll_table_struct *);
extern int                      sock_no_ioctl(struct socket *, unsigned int,
unsigned long);
extern int			sock_no_listen(struct socket *, int);
extern int                      sock_no_shutdown(struct socket *, int);
extern int			sock_no_getsockopt(struct socket *, int , int,
char *, int *);
extern int			sock_no_setsockopt(struct socket *, int, int,
char *, int);
extern int 			sock_no_fcntl(struct socket *,
unsigned int, unsigned long);
extern int                      sock_no_sendmsg(struct socket *,
struct msghdr *, int,
struct scm_cookie *);
extern int                      sock_no_recvmsg(struct socket *,
struct msghdr *, int,
struct scm_cookie *);
extern void sock_def_callback1(struct sock *);
extern void sock_def_callback2(struct sock *, int);
extern void sock_def_callback3(struct sock *);
extern void sock_def_destruct(struct sock *);
extern void sock_init_data(struct socket *sock, struct sock *sk);
extern void sklist_remove_socket(struct sock **list, struct sock *sk);
extern void sklist_insert_socket(struct sock **list, struct sock *sk);
extern void sklist_destroy_socket(struct sock **list, struct sock *sk);
#ifdef CONFIG_FILTER
static __inline__ int sk_filter(struct sk_buff *skb, struct sk_filter *filter)
{
int pkt_len;
pkt_len = sk_run_filter(skb, filter->insns, filter->len);
if(!pkt_len)
return 1;
else
skb_trim(skb, pkt_len);
return 0;
}
static __inline__ void sk_filter_release(struct sock *sk, struct sk_filter *fp)
{
unsigned int size = sk_filter_len(fp);
atomic_sub(size, &sk->omem_alloc);
if (atomic_dec_and_test(&fp->refcnt))
kfree_s(fp, size);
}
static __inline__ void sk_filter_charge(struct sock *sk, struct sk_filter *fp)
{
atomic_inc(&fp->refcnt);
atomic_add(sk_filter_len(fp), &sk->omem_alloc);
}
#endif
static __inline__ void skb_set_owner_w(struct sk_buff *skb, struct sock *sk)
{
skb->sk = sk;
skb->destructor = sock_wfree;
atomic_add(skb->truesize, &sk->wmem_alloc);
}
static __inline__ void skb_set_owner_r(struct sk_buff *skb, struct sock *sk)
{
skb->sk = sk;
skb->destructor = sock_rfree;
atomic_add(skb->truesize, &sk->rmem_alloc);
}
static __inline__ int sock_queue_rcv_skb(struct sock *sk, struct sk_buff *skb)
{
#ifdef CONFIG_FILTER
struct sk_filter *filter;
#endif
if (atomic_read(&sk->rmem_alloc) + skb->truesize >= (unsigned)sk->rcvbuf)
return -ENOMEM;
#ifdef CONFIG_FILTER
if ((filter = sk->filter) != NULL && sk_filter(skb, filter))
return -EPERM;
#endif
skb_set_owner_r(skb, sk);
skb_queue_tail(&sk->receive_queue, skb);
if (!sk->dead)
sk->data_ready(sk,skb->len);
return 0;
}
static __inline__ int sock_queue_err_skb(struct sock *sk, struct sk_buff *skb)
{
if (atomic_read(&sk->rmem_alloc) + skb->truesize >= (unsigned)sk->rcvbuf)
return -ENOMEM;
skb_set_owner_r(skb, sk);
skb_queue_tail(&sk->error_queue,skb);
if (!sk->dead)
sk->data_ready(sk,skb->len);
return 0;
}
static __inline__ int sock_error(struct sock *sk)
{
int err=xchg(&sk->err,0);
return -err;
}
static __inline__ unsigned long sock_wspace(struct sock *sk)
{
int amt = 0;
if (!(sk->shutdown & SEND_SHUTDOWN)) {
amt = sk->sndbuf - atomic_read(&sk->wmem_alloc);
if (amt < 0)
amt = 0;
}
return amt;
}
static __inline__ int sock_writeable(struct sock *sk)
{
return sock_wspace(sk) >= MIN_WRITE_SPACE;
}
extern struct sock *timer_base;
extern void net_delete_timer (struct sock *);
extern void net_reset_timer (struct sock *, int, unsigned long);
extern void net_timer (unsigned long);
static __inline__ int gfp_any(void)
{
return in_interrupt() ? GFP_ATOMIC : GFP_KERNEL;
}
#if 1
#define NETDEBUG(x)	do { } while (0)
#else
#define NETDEBUG(x)	do { x; } while (0)
#endif
#endif