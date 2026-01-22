#ifndef _TCP_H
#define _TCP_H
#include <linux/config.h>
#include <linux/tcp.h>
#include <linux/slab.h>
#include <net/checksum.h>
extern unsigned int tcp_ehash_size;
extern struct sock **tcp_ehash;
#define TCP_LHTABLE_SIZE	32
extern struct sock *tcp_listening_hash[TCP_LHTABLE_SIZE];
struct tcp_bind_bucket {
unsigned short		port;
unsigned short		fastreuse;
struct tcp_bind_bucket	*next;
struct sock		*owners;
struct tcp_bind_bucket	**pprev;
};
extern unsigned int tcp_bhash_size;
extern struct tcp_bind_bucket **tcp_bhash;
extern kmem_cache_t *tcp_bucket_cachep;
extern struct tcp_bind_bucket *tcp_bucket_create(unsigned short snum);
extern void tcp_bucket_unlock(struct sock *sk);
#define TCP_NUM_REGS		32
extern struct sock *tcp_regs[TCP_NUM_REGS];
#define TCP_RHASH_FN(__fport) \
((((__fport) >> 7) ^ (__fport)) & (TCP_NUM_REGS - 1))
#define TCP_RHASH(__fport)	tcp_regs[TCP_RHASH_FN((__fport))]
#define TCP_SK_RHASH_FN(__sock)	TCP_RHASH_FN((__sock)->dport)
#define TCP_SK_RHASH(__sock)	tcp_regs[TCP_SK_RHASH_FN((__sock))]
static __inline__ void tcp_reg_zap(struct sock *sk)
{
struct sock **rpp;
rpp = &(TCP_SK_RHASH(sk));
if(*rpp == sk)
*rpp = NULL;
}
static __inline__ int tcp_bhashfn(__u16 lport)
{
return (lport & (tcp_bhash_size - 1));
}
struct tcp_tw_bucket {
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
unsigned char		state,
zapped;
__u16			sport;
unsigned short		family;
unsigned char		reuse,
nonagle;
__u32			rcv_nxt, snd_nxt;
__u16			window;
struct tcp_func		*af_specific;
struct tcp_bind_bucket	*tb;
struct tcp_tw_bucket	*next_death;
struct tcp_tw_bucket	**pprev_death;
int			death_slot;
#if defined(CONFIG_IPV6) || defined(CONFIG_IPV6_MODULE)
struct in6_addr		v6_daddr;
struct in6_addr		v6_rcv_saddr;
#endif
};
extern kmem_cache_t *tcp_timewait_cachep;
#ifdef __BIG_ENDIAN
#define TCP_COMBINED_PORTS(__sport, __dport) \
(((__u32)(__sport)<<16) | (__u32)(__dport))
#else
#define TCP_COMBINED_PORTS(__sport, __dport) \
(((__u32)(__dport)<<16) | (__u32)(__sport))
#endif
#if (BITS_PER_LONG == 64)
#ifdef __BIG_ENDIAN
#define TCP_V4_ADDR_COOKIE(__name, __saddr, __daddr) \
__u64 __name = (((__u64)(__saddr))<<32)|((__u64)(__daddr));
#else
#define TCP_V4_ADDR_COOKIE(__name, __saddr, __daddr) \
__u64 __name = (((__u64)(__daddr))<<32)|((__u64)(__saddr));
#endif
#define TCP_IPV4_MATCH(__sk, __cookie, __saddr, __daddr, __ports, __dif)\
(((*((__u64 *)&((__sk)->daddr)))== (__cookie))	&&		\
((*((__u32 *)&((__sk)->dport)))== (__ports))   &&		\
(!((__sk)->bound_dev_if) || ((__sk)->bound_dev_if == (__dif))))
#else
#define TCP_V4_ADDR_COOKIE(__name, __saddr, __daddr)
#define TCP_IPV4_MATCH(__sk, __cookie, __saddr, __daddr, __ports, __dif)\
(((__sk)->daddr			== (__saddr))	&&		\
((__sk)->rcv_saddr		== (__daddr))	&&		\
((*((__u32 *)&((__sk)->dport)))== (__ports))   &&		\
(!((__sk)->bound_dev_if) || ((__sk)->bound_dev_if == (__dif))))
#endif
#define TCP_IPV6_MATCH(__sk, __saddr, __daddr, __ports, __dif)			   \
(((*((__u32 *)&((__sk)->dport)))== (__ports))   			&& \
((__sk)->family		== AF_INET6)				&& \
!ipv6_addr_cmp(&(__sk)->net_pinfo.af_inet6.daddr, (__saddr))		&& \
!ipv6_addr_cmp(&(__sk)->net_pinfo.af_inet6.rcv_saddr, (__daddr))	&& \
(!((__sk)->bound_dev_if) || ((__sk)->bound_dev_if == (__dif))))
static __inline__ int tcp_lhashfn(unsigned short num)
{
return num & (TCP_LHTABLE_SIZE - 1);
}
static __inline__ int tcp_sk_listen_hashfn(struct sock *sk)
{
return tcp_lhashfn(sk->num);
}
#define NETHDR_SIZE	(sizeof(struct iphdr) + 40)
#define MAX_SYN_SIZE	(NETHDR_SIZE + sizeof(struct tcphdr) + 20 + MAX_HEADER + 15)
#define MAX_FIN_SIZE	(NETHDR_SIZE + sizeof(struct tcphdr) + MAX_HEADER + 15)
#define BASE_ACK_SIZE	(NETHDR_SIZE + MAX_HEADER + 15)
#define MAX_ACK_SIZE	(NETHDR_SIZE + sizeof(struct tcphdr) + MAX_HEADER + 15)
#define MAX_RESET_SIZE	(NETHDR_SIZE + sizeof(struct tcphdr) + MAX_HEADER + 15)
#define MAX_TCPHEADER_SIZE (NETHDR_SIZE + sizeof(struct tcphdr) + 20 + MAX_HEADER + 15)
#define MAX_WINDOW	32767
#define MIN_WINDOW	2048
#define MAX_ACK_BACKLOG	2
#define MAX_DELAY_ACK	2
#define TCP_WINDOW_DIFF	2048
#define URG_VALID	0x0100
#define URG_NOTYET	0x0200
#define URG_READ	0x0400
#define TCP_RETR1	7
#define TCP_RETR2	15
#define TCP_TIMEOUT_LEN	(15*60*HZ)
#define TCP_TIMEWAIT_LEN (60*HZ)
#define TCP_FIN_TIMEOUT (3*60*HZ)
#define TCP_ACK_TIME	(3*HZ)
#define TCP_DONE_TIME	(5*HZ/2)
#define TCP_WRITE_TIME	(30*HZ)
#define TCP_TIMEOUT_INIT (3*HZ)
#define TCP_SYN_RETRIES	 10
#define TCP_PROBEWAIT_LEN (1*HZ)
#define TCP_KEEPALIVE_TIME (120*60*HZ)
#define TCP_KEEPALIVE_PROBES	9
#define TCP_KEEPALIVE_PERIOD ((75*HZ)>>2)
#define TCP_SYNACK_PERIOD	(HZ/2)
#define TCP_QUICK_TRIES		8
#define TCP_BUCKETGC_PERIOD	(HZ)
#define TCP_TWKILL_SLOTS	8
#define TCP_TWKILL_PERIOD	((HZ*60)/TCP_TWKILL_SLOTS)
#define TCPOPT_NOP		1
#define TCPOPT_EOL		0
#define TCPOPT_MSS		2
#define TCPOPT_WINDOW		3
#define TCPOPT_SACK_PERM        4
#define TCPOPT_SACK             5
#define TCPOPT_TIMESTAMP	8
#define TCPOLEN_MSS            4
#define TCPOLEN_WINDOW         3
#define TCPOLEN_SACK_PERM      2
#define TCPOLEN_TIMESTAMP      10
#define TCPOLEN_TSTAMP_ALIGNED		12
#define TCPOLEN_WSCALE_ALIGNED		4
#define TCPOLEN_SACKPERM_ALIGNED	4
#define TCPOLEN_SACK_BASE		2
#define TCPOLEN_SACK_BASE_ALIGNED	4
#define TCPOLEN_SACK_PERBLOCK		8
struct open_request;
struct or_calltable {
void (*rtx_syn_ack)	(struct sock *sk, struct open_request *req);
void (*destructor)	(struct open_request *req);
void (*send_reset)	(struct sk_buff *skb);
};
struct tcp_v4_open_req {
__u32			loc_addr;
__u32			rmt_addr;
struct ip_options	*opt;
};
#if defined(CONFIG_IPV6) || defined (CONFIG_IPV6_MODULE)
struct tcp_v6_open_req {
struct in6_addr		loc_addr;
struct in6_addr		rmt_addr;
struct sk_buff		*pktopts;
int			iif;
};
#endif
struct open_request {
struct open_request	*dl_next;
__u32			rcv_isn;
__u32			snt_isn;
__u16			rmt_port;
__u16			mss;
__u8			retrans;
__u8			__pad;
unsigned snd_wscale : 4,
rcv_wscale : 4,
tstamp_ok : 1,
sack_ok : 1,
wscale_ok : 1;
__u32			window_clamp;
__u32			rcv_wnd;
__u32			ts_recent;
unsigned long		expires;
struct or_calltable	*class;
struct sock		*sk;
union {
struct tcp_v4_open_req v4_req;
#if defined(CONFIG_IPV6) || defined (CONFIG_IPV6_MODULE)
struct tcp_v6_open_req v6_req;
#endif
} af;
#ifdef CONFIG_IP_TRANSPARENT_PROXY
__u16			lcl_port;
#endif
};
extern kmem_cache_t *tcp_openreq_cachep;
#define tcp_openreq_alloc()	kmem_cache_alloc(tcp_openreq_cachep, SLAB_ATOMIC)
#define tcp_openreq_free(req)	kmem_cache_free(tcp_openreq_cachep, req)
struct tcp_func {
void			(*queue_xmit)		(struct sk_buff *skb);
void			(*send_check)		(struct sock *sk,
struct tcphdr *th,
int len,
struct sk_buff *skb);
int			(*rebuild_header)	(struct sock *sk);
int			(*conn_request)		(struct sock *sk,
struct sk_buff *skb,
__u32 isn);
struct sock *		(*syn_recv_sock)	(struct sock *sk,
struct sk_buff *skb,
struct open_request *req,
struct dst_entry *dst);
struct sock *		(*get_sock)		(struct sk_buff *skb,
struct tcphdr *th);
__u16			net_header_len;
int			(*setsockopt)		(struct sock *sk,
int level,
int optname,
char *optval,
int optlen);
int			(*getsockopt)		(struct sock *sk,
int level,
int optname,
char *optval,
int *optlen);
void			(*addr2sockaddr)	(struct sock *sk,
struct sockaddr *);
int sockaddr_len;
};
static __inline int before(__u32 seq1, __u32 seq2)
{
return (__s32)(seq1-seq2) < 0;
}
static __inline int after(__u32 seq1, __u32 seq2)
{
return (__s32)(seq2-seq1) < 0;
}
static __inline int between(__u32 seq1, __u32 seq2, __u32 seq3)
{
return seq3 - seq2 >= seq1 - seq2;
}
extern struct proto tcp_prot;
extern struct tcp_mib tcp_statistics;
extern void			tcp_put_port(struct sock *sk);
extern void			__tcp_put_port(struct sock *sk);
extern void			tcp_inherit_port(struct sock *sk, struct sock *child);
extern void			tcp_v4_err(struct sk_buff *skb,
unsigned char *, int);
extern void			tcp_shutdown (struct sock *sk, int how);
extern int			tcp_v4_rcv(struct sk_buff *skb,
unsigned short len);
extern int			tcp_do_sendmsg(struct sock *sk, struct msghdr *msg);
extern int			tcp_ioctl(struct sock *sk,
int cmd,
unsigned long arg);
extern int			tcp_rcv_state_process(struct sock *sk,
struct sk_buff *skb,
struct tcphdr *th,
unsigned len);
extern int			tcp_rcv_established(struct sock *sk,
struct sk_buff *skb,
struct tcphdr *th,
unsigned len);
enum tcp_tw_status {
TCP_TW_SUCCESS = 0,
TCP_TW_RST = 1,
TCP_TW_ACK = 2
};
extern enum tcp_tw_status tcp_timewait_state_process(struct tcp_tw_bucket *tw,
struct sk_buff *skb,
struct tcphdr *th,
unsigned len);
extern void			tcp_close(struct sock *sk,
long timeout);
extern struct sock *		tcp_accept(struct sock *sk, int flags);
extern unsigned int		tcp_poll(struct file * file, struct socket *sock, struct poll_table_struct *wait);
extern void			tcp_write_space(struct sock *sk);
extern int			tcp_getsockopt(struct sock *sk, int level,
int optname, char *optval,
int *optlen);
extern int			tcp_setsockopt(struct sock *sk, int level,
int optname, char *optval,
int optlen);
extern void			tcp_set_keepalive(struct sock *sk, int val);
extern int			tcp_recvmsg(struct sock *sk,
struct msghdr *msg,
int len, int nonblock,
int flags, int *addr_len);
extern void			tcp_parse_options(struct sock *sk, struct tcphdr *th,
struct tcp_opt *tp, int no_fancy);
extern int		       	tcp_v4_rebuild_header(struct sock *sk);
extern int		       	tcp_v4_build_header(struct sock *sk,
struct sk_buff *skb);
extern void		       	tcp_v4_send_check(struct sock *sk,
struct tcphdr *th, int len,
struct sk_buff *skb);
extern int			tcp_v4_conn_request(struct sock *sk,
struct sk_buff *skb,
__u32 isn);
extern struct sock *		tcp_create_openreq_child(struct sock *sk,
struct open_request *req,
struct sk_buff *skb);
extern struct sock *		tcp_v4_syn_recv_sock(struct sock *sk,
struct sk_buff *skb,
struct open_request *req,
struct dst_entry *dst);
extern int			tcp_v4_do_rcv(struct sock *sk,
struct sk_buff *skb);
extern int			tcp_v4_connect(struct sock *sk,
struct sockaddr *uaddr,
int addr_len);
extern void			tcp_connect(struct sock *sk,
struct sk_buff *skb,
int est_mss);
extern struct sk_buff *		tcp_make_synack(struct sock *sk,
struct dst_entry *dst,
struct open_request *req,
int mss);
extern struct sock *cookie_v4_check(struct sock *sk, struct sk_buff *skb,
struct ip_options *opt);
extern __u32 cookie_v4_init_sequence(struct sock *sk, struct sk_buff *skb,
__u16 *mss);
extern void tcp_read_wakeup(struct sock *);
extern void tcp_write_xmit(struct sock *);
extern void tcp_time_wait(struct sock *);
extern int tcp_retransmit_skb(struct sock *, struct sk_buff *);
extern void tcp_fack_retransmit(struct sock *);
extern void tcp_xmit_retransmit_queue(struct sock *);
extern void tcp_simple_retransmit(struct sock *);
extern void tcp_send_probe0(struct sock *);
extern void tcp_send_partial(struct sock *);
extern void tcp_write_wakeup(struct sock *);
extern void tcp_send_fin(struct sock *sk);
extern void tcp_send_active_reset(struct sock *sk);
extern int  tcp_send_synack(struct sock *);
extern void tcp_transmit_skb(struct sock *, struct sk_buff *);
extern void tcp_send_skb(struct sock *, struct sk_buff *, int force_queue);
extern void tcp_send_ack(struct sock *sk);
extern void tcp_send_delayed_ack(struct tcp_opt *tp, int max_timeout);
extern int tcp_chkaddr(struct sk_buff *);
#define     tcp_reset_msl_timer(x,y,z)	net_reset_timer(x,y,z)
extern void tcp_reset_xmit_timer(struct sock *, int, unsigned long);
extern void tcp_init_xmit_timers(struct sock *);
extern void tcp_clear_xmit_timers(struct sock *);
extern void tcp_retransmit_timer(unsigned long);
extern void tcp_delack_timer(unsigned long);
extern void tcp_probe_timer(unsigned long);
extern struct sock *tcp_check_req(struct sock *sk, struct sk_buff *skb,
struct open_request *req);
extern struct timer_list	tcp_slow_timer;
struct tcp_sl_timer {
atomic_t	count;
unsigned long	period;
unsigned long	last;
void (*handler)	(unsigned long);
};
#define TCP_SLT_SYNACK		0
#define TCP_SLT_KEEPALIVE	1
#define TCP_SLT_TWKILL		2
#define TCP_SLT_MAX		3
extern struct tcp_sl_timer tcp_slt_array[TCP_SLT_MAX];
extern int tcp_sync_mss(struct sock *sk, u32 pmtu);
static __inline__ unsigned int tcp_current_mss(struct sock *sk)
{
struct tcp_opt *tp = &sk->tp_pinfo.af_tcp;
struct dst_entry *dst = sk->dst_cache;
int mss_now = tp->mss_cache;
if (dst && dst->pmtu != tp->pmtu_cookie)
mss_now = tcp_sync_mss(sk, dst->pmtu);
if(tp->sack_ok && tp->num_sacks)
mss_now -= (TCPOLEN_SACK_BASE_ALIGNED +
(tp->num_sacks * TCPOLEN_SACK_PERBLOCK));
return mss_now > 8 ? mss_now : 8;
}
static __inline__ u32 tcp_receive_window(struct tcp_opt *tp)
{
s32 win = tp->rcv_wup + tp->rcv_wnd - tp->rcv_nxt;
if (win < 0)
win = 0;
return (u32) win;
}
extern u32	__tcp_select_window(struct sock *sk);
static __inline__ u16 tcp_select_window(struct sock *sk)
{
struct tcp_opt *tp = &(sk->tp_pinfo.af_tcp);
u32 cur_win = tcp_receive_window(tp);
u32 new_win = __tcp_select_window(sk);
if(new_win < cur_win) {
new_win = cur_win;
} else {
tp->rcv_wnd = new_win;
tp->rcv_wup = tp->rcv_nxt;
}
return new_win >> tp->rcv_wscale;
}
static __inline__ int tcp_raise_window(struct sock *sk)
{
struct tcp_opt *tp = &(sk->tp_pinfo.af_tcp);
u32 cur_win = tcp_receive_window(tp);
u32 new_win = __tcp_select_window(sk);
return (new_win && (new_win > (cur_win << 1)));
}
static __inline__ __u32 tcp_recalc_ssthresh(struct tcp_opt *tp)
{
__u32 snd_wnd_packets = tp->snd_wnd / max(tp->mss_cache, 1);
return max(min(snd_wnd_packets, tp->snd_cwnd) >> 1, 2);
}
#define tcp_time_stamp		((__u32)(jiffies))
struct tcp_skb_cb {
union {
struct inet_skb_parm	h4;
#if defined(CONFIG_IPV6) || defined (CONFIG_IPV6_MODULE)
struct inet6_skb_parm	h6;
#endif
} header;
__u32		seq;
__u32		end_seq;
__u32		when;
__u8		flags;
#define TCPCB_FLAG_FIN		0x01
#define TCPCB_FLAG_SYN		0x02
#define TCPCB_FLAG_RST		0x04
#define TCPCB_FLAG_PSH		0x08
#define TCPCB_FLAG_ACK		0x10
#define TCPCB_FLAG_URG		0x20
__u8		sacked;
#define TCPCB_SACKED_ACKED	0x01
#define TCPCB_SACKED_RETRANS	0x02
__u16		urg_ptr;
__u32		ack_seq;
};
#define TCP_SKB_CB(__skb)	((struct tcp_skb_cb *)&((__skb)->cb[0]))
static __inline__ int tcp_packets_in_flight(struct tcp_opt *tp)
{
return tp->packets_out - tp->fackets_out + tp->retrans_out;
}
static __inline__ int tcp_snd_test(struct sock *sk, struct sk_buff *skb)
{
struct tcp_opt *tp = &(sk->tp_pinfo.af_tcp);
int nagle_check = 1;
if ((sk->nonagle == 2 && (skb->len < tp->mss_cache)) ||
(!sk->nonagle &&
skb->len < (tp->mss_cache >> 1) &&
tp->packets_out &&
!(TCP_SKB_CB(skb)->flags & (TCPCB_FLAG_URG|TCPCB_FLAG_FIN))))
nagle_check = 0;
return (nagle_check &&
((tcp_packets_in_flight(tp) < tp->snd_cwnd) ||
(TCP_SKB_CB(skb)->flags & TCPCB_FLAG_FIN)) &&
!after(TCP_SKB_CB(skb)->end_seq, tp->snd_una + tp->snd_wnd) &&
tp->retransmits == 0);
}
static __inline__ void tcp_push_pending_frames(struct sock *sk, struct tcp_opt *tp)
{
if(tp->send_head) {
if(tcp_snd_test(sk, tp->send_head))
tcp_write_xmit(sk);
else if(tp->packets_out == 0 && !tp->pending) {
tp->pending = TIME_PROBE0;
tcp_reset_xmit_timer(sk, TIME_PROBE0, tp->rto);
}
}
}
#define tcp_enter_quickack_mode(__tp)	((__tp)->ato |= (1U<<31))
#define tcp_exit_quickack_mode(__tp)	((__tp)->ato &= ~(1U<<31))
#define tcp_in_quickack_mode(__tp)	(((__tp)->ato & (1U << 31)) != 0)
static __inline const int tcp_connected(const int state)
{
return ((1 << state) &
(TCPF_ESTABLISHED|TCPF_CLOSE_WAIT|TCPF_FIN_WAIT1|
TCPF_FIN_WAIT2|TCPF_SYN_RECV));
}
static __inline__ u16 tcp_v4_check(struct tcphdr *th, int len,
unsigned long saddr, unsigned long daddr,
unsigned long base)
{
return csum_tcpudp_magic(saddr,daddr,len,IPPROTO_TCP,base);
}
#undef STATE_TRACE
#ifdef STATE_TRACE
static char *statename[]={
"Unused","Established","Syn Sent","Syn Recv",
"Fin Wait 1","Fin Wait 2","Time Wait", "Close",
"Close Wait","Last ACK","Listen","Closing"
};
#endif
static __inline__ void tcp_set_state(struct sock *sk, int state)
{
int oldstate = sk->state;
sk->state = state;
#ifdef STATE_TRACE
SOCK_DEBUG(sk, "TCP sk=%p, State %s -> %s\n",sk, statename[oldstate],statename[state]);
#endif
switch (state) {
case TCP_ESTABLISHED:
if (oldstate != TCP_ESTABLISHED)
tcp_statistics.TcpCurrEstab++;
break;
case TCP_CLOSE:
{
struct tcp_opt *tp = &sk->tp_pinfo.af_tcp;
net_reset_timer(sk, TIME_DONE, min(tp->srtt * 2, TCP_DONE_TIME));
sk->prot->unhash(sk);
}
default:
if (oldstate==TCP_ESTABLISHED)
tcp_statistics.TcpCurrEstab--;
}
}
static __inline__ void tcp_build_and_update_options(__u32 *ptr, struct tcp_opt *tp, __u32 tstamp)
{
if (tp->tstamp_ok) {
*ptr++ = __constant_htonl((TCPOPT_NOP << 24) |
(TCPOPT_NOP << 16) |
(TCPOPT_TIMESTAMP << 8) |
TCPOLEN_TIMESTAMP);
*ptr++ = htonl(tstamp);
*ptr++ = htonl(tp->ts_recent);
}
if(tp->sack_ok && tp->num_sacks) {
int this_sack;
*ptr++ = __constant_htonl((TCPOPT_NOP << 24) |
(TCPOPT_NOP << 16) |
(TCPOPT_SACK << 8) |
(TCPOLEN_SACK_BASE +
(tp->num_sacks * TCPOLEN_SACK_PERBLOCK)));
for(this_sack = 0; this_sack < tp->num_sacks; this_sack++) {
*ptr++ = htonl(tp->selective_acks[this_sack].start_seq);
*ptr++ = htonl(tp->selective_acks[this_sack].end_seq);
}
}
}
static __inline__ void tcp_syn_build_options(__u32 *ptr, int mss, int ts, int sack,
int offer_wscale, int wscale, __u32 tstamp, __u32 ts_recent)
{
*ptr++ = htonl((TCPOPT_MSS << 24) | (TCPOLEN_MSS << 16) | mss);
if (ts) {
if(sack)
*ptr++ = __constant_htonl((TCPOPT_SACK_PERM << 24) | (TCPOLEN_SACK_PERM << 16) |
(TCPOPT_TIMESTAMP << 8) | TCPOLEN_TIMESTAMP);
else
*ptr++ = __constant_htonl((TCPOPT_NOP << 24) | (TCPOPT_NOP << 16) |
(TCPOPT_TIMESTAMP << 8) | TCPOLEN_TIMESTAMP);
*ptr++ = htonl(tstamp);
*ptr++ = htonl(ts_recent);
} else if(sack)
*ptr++ = __constant_htonl((TCPOPT_NOP << 24) | (TCPOPT_NOP << 16) |
(TCPOPT_SACK_PERM << 8) | TCPOLEN_SACK_PERM);
if (offer_wscale)
*ptr++ = htonl((TCPOPT_NOP << 24) | (TCPOPT_WINDOW << 16) | (TCPOLEN_WINDOW << 8) | (wscale));
}
static __inline__ void tcp_select_initial_window(__u32 space, __u16 mss,
__u32 *rcv_wnd,
__u32 *window_clamp,
int wscale_ok,
__u8 *rcv_wscale)
{
if (*window_clamp == 0)
(*window_clamp) = (65535<<14);
space = min(*window_clamp,space);
if (space > mss)
space = (space/mss)*mss;
(*rcv_wnd) = min(space, MAX_WINDOW);
(*rcv_wscale) = 0;
if (wscale_ok) {
while (space > 65535 && (*rcv_wscale) < 14) {
space >>= 1;
(*rcv_wscale)++;
}
}
(*window_clamp) = min(65535<<(*rcv_wscale),*window_clamp);
}
static __inline__ void tcp_synq_unlink(struct tcp_opt *tp, struct open_request *req, struct open_request *prev)
{
if(!req->dl_next)
tp->syn_wait_last = (struct open_request **)prev;
prev->dl_next = req->dl_next;
}
static __inline__ void tcp_synq_queue(struct tcp_opt *tp, struct open_request *req)
{
req->dl_next = NULL;
*tp->syn_wait_last = req;
tp->syn_wait_last = &req->dl_next;
}
static __inline__ void tcp_synq_init(struct tcp_opt *tp)
{
tp->syn_wait_queue = NULL;
tp->syn_wait_last = &tp->syn_wait_queue;
}
extern void __tcp_inc_slow_timer(struct tcp_sl_timer *slt);
static __inline__ void tcp_inc_slow_timer(int timer)
{
struct tcp_sl_timer *slt = &tcp_slt_array[timer];
if (atomic_read(&slt->count) == 0)
{
__tcp_inc_slow_timer(slt);
}
atomic_inc(&slt->count);
}
static __inline__ void tcp_dec_slow_timer(int timer)
{
struct tcp_sl_timer *slt = &tcp_slt_array[timer];
atomic_dec(&slt->count);
}
extern const char timer_bug_msg[];
static inline void tcp_clear_xmit_timer(struct sock *sk, int what)
{
struct tcp_opt *tp = &sk->tp_pinfo.af_tcp;
struct timer_list *timer;
switch (what) {
case TIME_RETRANS:
timer = &tp->retransmit_timer;
break;
case TIME_DACK:
timer = &tp->delack_timer;
break;
case TIME_PROBE0:
timer = &tp->probe_timer;
break;
default:
printk("%s", timer_bug_msg);
return;
};
if(timer->prev != NULL)
del_timer(timer);
}
static inline int tcp_timer_is_set(struct sock *sk, int what)
{
struct tcp_opt *tp = &sk->tp_pinfo.af_tcp;
switch (what) {
case TIME_RETRANS:
return tp->retransmit_timer.prev != NULL;
break;
case TIME_DACK:
return tp->delack_timer.prev != NULL;
break;
case TIME_PROBE0:
return tp->probe_timer.prev != NULL;
break;
default:
printk("%s", timer_bug_msg);
};
return 0;
}
#endif