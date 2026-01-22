#include <linux/config.h>
#include <linux/mm.h>
#include <linux/sysctl.h>
#include <net/tcp.h>
#include <linux/ipsec.h>
#ifdef CONFIG_SYSCTL
#define SYNC_INIT 0
#else
#define SYNC_INIT 1
#endif
extern int sysctl_tcp_fin_timeout;
int sysctl_tcp_timestamps = 1;
int sysctl_tcp_window_scaling = 1;
int sysctl_tcp_sack = 1;
int sysctl_tcp_syncookies = SYNC_INIT;
int sysctl_tcp_stdurg;
int sysctl_tcp_rfc1337;
static int prune_queue(struct sock *sk);
static void tcp_delack_estimator(struct tcp_opt *tp)
{
if(tp->ato == 0) {
tp->lrcvtime = tcp_time_stamp;
tp->ato = 1;
tcp_enter_quickack_mode(tp);
} else {
int m = tcp_time_stamp - tp->lrcvtime;
tp->lrcvtime = tcp_time_stamp;
if(m <= 0)
m = 1;
if(m > tp->rto)
tp->ato = tp->rto;
else {
tp->ato = ((tp->ato << 1) >> 2) + m;
}
}
}
static __inline__ void tcp_remember_ack(struct tcp_opt *tp, struct tcphdr *th,
struct sk_buff *skb)
{
tp->delayed_acks++;
if(th->psh && (skb->len < (tp->mss_cache >> 1))) {
if((tp->ato & 0x7fffffff) > HZ/50)
tp->ato = ((tp->ato & 0x80000000) |
(HZ/50));
}
}
static __inline__ void tcp_rtt_estimator(struct tcp_opt *tp, __u32 mrtt)
{
long m = mrtt;
if(m == 0)
m = 1;
if (tp->srtt != 0) {
m -= (tp->srtt >> 3);
tp->srtt += m;
if (m < 0)
m = -m;
m -= (tp->mdev >> 2);
tp->mdev += m;
} else {
tp->srtt = m<<3;
tp->mdev = m<<2;
}
}
static __inline__ void tcp_set_rto(struct tcp_opt *tp)
{
tp->rto = (tp->srtt >> 3) + tp->mdev;
tp->rto += (tp->rto >> 2) + (tp->rto >> (tp->snd_cwnd-1));
}
static __inline__ void tcp_bound_rto(struct tcp_opt *tp)
{
if (tp->rto > 120*HZ)
tp->rto = 120*HZ;
if (tp->rto < HZ/5)
tp->rto = HZ/5;
}
static __inline__ void tcp_replace_ts_recent(struct sock *sk, struct tcp_opt *tp,
__u32 start_seq, __u32 end_seq)
{
if (!after(start_seq, tp->last_ack_sent) &&
!before(end_seq, tp->rcv_nxt)) {
if((s32)(tp->rcv_tsval - tp->ts_recent) >= 0) {
tp->ts_recent = tp->rcv_tsval;
tp->ts_recent_stamp = tcp_time_stamp;
}
}
}
#define PAWS_24DAYS (HZ * 60 * 60 * 24 * 24)
static __inline__ int tcp_paws_discard(struct tcp_opt *tp, struct tcphdr *th, unsigned len)
{
return ((s32)(tp->rcv_tsval - tp->ts_recent) < 0 &&
(s32)(tcp_time_stamp - tp->ts_recent_stamp) < PAWS_24DAYS &&
len != (th->doff * 4));
}
static int __tcp_sequence(struct tcp_opt *tp, u32 seq, u32 end_seq)
{
u32 end_window = tp->rcv_wup + tp->rcv_wnd;
if (tp->rcv_wnd &&
after(end_seq, tp->rcv_nxt) &&
before(seq, end_window))
return 1;
if (seq != end_window)
return 0;
return (seq == end_seq);
}
static __inline__ int tcp_sequence(struct tcp_opt *tp, u32 seq, u32 end_seq)
{
if (seq == tp->rcv_nxt)
return (tp->rcv_wnd || (end_seq == seq));
return __tcp_sequence(tp, seq, end_seq);
}
static void tcp_reset(struct sock *sk)
{
sk->zapped = 1;
switch (sk->state) {
case TCP_SYN_SENT:
sk->err = ECONNREFUSED;
break;
case TCP_CLOSE_WAIT:
sk->err = EPIPE;
break;
default:
sk->err = ECONNRESET;
};
tcp_set_state(sk, TCP_CLOSE);
sk->shutdown = SHUTDOWN_MASK;
if (!sk->dead)
sk->state_change(sk);
}
static void tcp_sacktag_write_queue(struct sock *sk, struct tcp_sack_block *sp, int nsacks)
{
struct tcp_opt *tp = &(sk->tp_pinfo.af_tcp);
int i = nsacks;
while(i--) {
struct sk_buff *skb = skb_peek(&sk->write_queue);
__u32 start_seq = ntohl(sp->start_seq);
__u32 end_seq = ntohl(sp->end_seq);
int fack_count = 0;
while((skb != NULL) &&
(skb != tp->send_head) &&
(skb != (struct sk_buff *)&sk->write_queue)) {
if(after(TCP_SKB_CB(skb)->seq, end_seq))
break;
fack_count++;
if(!after(start_seq, TCP_SKB_CB(skb)->seq) &&
!before(end_seq, TCP_SKB_CB(skb)->end_seq)) {
if((TCP_SKB_CB(skb)->sacked & TCPCB_SACKED_RETRANS) &&
tp->retrans_out)
tp->retrans_out--;
TCP_SKB_CB(skb)->sacked |= TCPCB_SACKED_ACKED;
if(fack_count > tp->fackets_out)
tp->fackets_out = fack_count;
}
skb = skb->next;
}
sp++;
}
}
void tcp_parse_options(struct sock *sk, struct tcphdr *th, struct tcp_opt *tp, int no_fancy)
{
unsigned char *ptr;
int length=(th->doff*4)-sizeof(struct tcphdr);
int saw_mss = 0;
ptr = (unsigned char *)(th + 1);
tp->saw_tstamp = 0;
while(length>0) {
int opcode=*ptr++;
int opsize;
switch (opcode) {
case TCPOPT_EOL:
return;
case TCPOPT_NOP:
length--;
continue;
default:
opsize=*ptr++;
if (opsize < 2)
return;
if (opsize > length)
break;
switch(opcode) {
case TCPOPT_MSS:
if(opsize==TCPOLEN_MSS && th->syn) {
u16 in_mss = ntohs(*(__u16 *)ptr);
if (in_mss == 0)
in_mss = 536;
if (tp->mss_clamp > in_mss)
tp->mss_clamp = in_mss;
saw_mss = 1;
}
break;
case TCPOPT_WINDOW:
if(opsize==TCPOLEN_WINDOW && th->syn)
if (!no_fancy && sysctl_tcp_window_scaling) {
tp->wscale_ok = 1;
tp->snd_wscale = *(__u8 *)ptr;
if(tp->snd_wscale > 14) {
if(net_ratelimit())
printk("tcp_parse_options: Illegal window "
"scaling value %d >14 received.",
tp->snd_wscale);
tp->snd_wscale = 14;
}
}
break;
case TCPOPT_TIMESTAMP:
if(opsize==TCPOLEN_TIMESTAMP) {
if (sysctl_tcp_timestamps && !no_fancy) {
tp->tstamp_ok = 1;
tp->saw_tstamp = 1;
tp->rcv_tsval = ntohl(*(__u32 *)ptr);
tp->rcv_tsecr = ntohl(*(__u32 *)(ptr+4));
}
}
break;
case TCPOPT_SACK_PERM:
if(opsize==TCPOLEN_SACK_PERM && th->syn) {
if (sysctl_tcp_sack && !no_fancy) {
tp->sack_ok = 1;
tp->num_sacks = 0;
}
}
break;
case TCPOPT_SACK:
if((opsize >= (TCPOLEN_SACK_BASE + TCPOLEN_SACK_PERBLOCK)) &&
sysctl_tcp_sack && (sk != NULL) && !th->syn) {
int sack_bytes = opsize - TCPOLEN_SACK_BASE;
if(!(sack_bytes % TCPOLEN_SACK_PERBLOCK)) {
int num_sacks = sack_bytes >> 3;
struct tcp_sack_block *sackp;
sackp = (struct tcp_sack_block *)ptr;
tcp_sacktag_write_queue(sk, sackp, num_sacks);
}
}
};
ptr+=opsize-2;
length-=opsize;
};
}
if(th->syn && saw_mss == 0)
tp->mss_clamp = 536;
}
static __inline__ int tcp_fast_parse_options(struct sock *sk, struct tcphdr *th, struct tcp_opt *tp)
{
if (tp->tcp_header_len == sizeof(struct tcphdr))
return 0;
if (th->doff == sizeof(struct tcphdr)>>2) {
tp->saw_tstamp = 0;
return 0;
} else if (th->doff == (sizeof(struct tcphdr)>>2)+(TCPOLEN_TSTAMP_ALIGNED>>2)) {
__u32 *ptr = (__u32 *)(th + 1);
if (*ptr == __constant_ntohl((TCPOPT_NOP << 24) | (TCPOPT_NOP << 16)
| (TCPOPT_TIMESTAMP << 8) | TCPOLEN_TIMESTAMP)) {
tp->saw_tstamp = 1;
tp->rcv_tsval = ntohl(*++ptr);
tp->rcv_tsecr = ntohl(*++ptr);
return 1;
}
}
tcp_parse_options(sk, th, tp, 0);
return 1;
}
#define FLAG_DATA 0x01
#define FLAG_WIN_UPDATE 0x02
#define FLAG_DATA_ACKED 0x04
#define FLAG_RETRANS_DATA_ACKED 0x08
static __inline__ void clear_fast_retransmit(struct tcp_opt *tp)
{
if (tp->dup_acks > 3)
tp->snd_cwnd = (tp->snd_ssthresh);
tp->dup_acks = 0;
}
static void tcp_fast_retrans(struct sock *sk, u32 ack, int not_dup)
{
struct tcp_opt *tp = &(sk->tp_pinfo.af_tcp);
if (ack == tp->snd_una && tp->packets_out && (not_dup == 0)) {
if (tp->high_seq == 0 || after(ack, tp->high_seq)) {
tp->dup_acks++;
if ((tp->fackets_out > 3) || (tp->dup_acks == 3)) {
tp->snd_ssthresh = tcp_recalc_ssthresh(tp);
tp->snd_cwnd = (tp->snd_ssthresh + 3);
tp->high_seq = tp->snd_nxt;
if(!tp->fackets_out)
tcp_retransmit_skb(sk,
skb_peek(&sk->write_queue));
else
tcp_fack_retransmit(sk);
tcp_reset_xmit_timer(sk, TIME_RETRANS, tp->rto);
}
} else if (++tp->dup_acks > 3) {
if(!tp->fackets_out) {
tp->snd_cwnd++;
} else {
tcp_fack_retransmit(sk);
}
}
} else if (tp->high_seq != 0) {
if (!before(ack, tp->high_seq) || (not_dup & FLAG_DATA) != 0) {
clear_fast_retransmit(tp);
if (!before(ack, tp->high_seq)) {
tp->high_seq = 0;
tp->fackets_out = 0;
}
} else if (tp->dup_acks >= 3) {
if (!tp->fackets_out) {
if (ack != tp->snd_una && before(ack, tp->high_seq)) {
tcp_retransmit_skb(sk,
skb_peek(&sk->write_queue));
tcp_reset_xmit_timer(sk, TIME_RETRANS, tp->rto);
}
} else {
tcp_fack_retransmit(sk);
}
}
}
}
static __inline__ void tcp_cong_avoid(struct tcp_opt *tp)
{
if (tp->snd_cwnd <= tp->snd_ssthresh) {
tp->snd_cwnd++;
} else {
if (tp->snd_cwnd_cnt >= tp->snd_cwnd) {
tp->snd_cwnd++;
tp->snd_cwnd_cnt=0;
} else
tp->snd_cwnd_cnt++;
}
}
static int tcp_clean_rtx_queue(struct sock *sk, __u32 ack,
__u32 *seq, __u32 *seq_rtt)
{
struct tcp_opt *tp = &(sk->tp_pinfo.af_tcp);
struct sk_buff *skb;
__u32 now = tcp_time_stamp;
int acked = 0;
if (tp->retrans_head != NULL &&
!before(ack, TCP_SKB_CB(tp->retrans_head)->end_seq))
tp->retrans_head = NULL;
while((skb=skb_peek(&sk->write_queue)) && (skb != tp->send_head)) {
struct tcp_skb_cb *scb = TCP_SKB_CB(skb);
__u8 sacked = scb->sacked;
if (after(scb->end_seq, ack))
break;
if((sacked & TCPCB_SACKED_RETRANS) && tp->retrans_out)
tp->retrans_out--;
if(!(scb->flags & TCPCB_FLAG_SYN)) {
acked |= FLAG_DATA_ACKED;
if(sacked & TCPCB_SACKED_RETRANS)
acked |= FLAG_RETRANS_DATA_ACKED;
if(tp->fackets_out)
tp->fackets_out--;
} else {
tp->retrans_head = NULL;
}
tp->packets_out--;
*seq = scb->seq;
*seq_rtt = now - scb->when;
__skb_unlink(skb, skb->list);
kfree_skb(skb);
}
return acked;
}
static void tcp_ack_probe(struct sock *sk, __u32 ack)
{
struct tcp_opt *tp = &(sk->tp_pinfo.af_tcp);
tp->probes_out = 0;
if (tp->send_head != NULL &&
!before (ack + tp->snd_wnd, TCP_SKB_CB(tp->send_head)->end_seq)) {
tp->backoff = 0;
tp->pending = 0;
tcp_clear_xmit_timer(sk, TIME_PROBE0);
} else {
tcp_reset_xmit_timer(sk, TIME_PROBE0,
min(tp->rto << tp->backoff, 120*HZ));
}
}
static __inline__ int should_advance_cwnd(struct tcp_opt *tp, int flag)
{
if ((flag & FLAG_DATA_ACKED) == 0)
return 0;
if ((flag & FLAG_RETRANS_DATA_ACKED) != 0) {
if (tp->fackets_out != 0 ||
tp->retransmits != 0)
return 1;
return 0;
}
return 1;
}
static void tcp_ack_saw_tstamp(struct sock *sk, struct tcp_opt *tp,
u32 seq, u32 ack, int flag)
{
__u32 seq_rtt;
if (!(flag & FLAG_DATA_ACKED))
return;
seq_rtt = tcp_time_stamp - tp->rcv_tsecr;
tcp_rtt_estimator(tp, seq_rtt);
if (tp->retransmits) {
if (tp->packets_out == 0) {
tp->retransmits = 0;
tp->fackets_out = 0;
tp->retrans_out = 0;
tp->backoff = 0;
tcp_set_rto(tp);
} else {
tcp_set_rto(tp);
tp->rto = tp->rto << tp->backoff;
}
} else {
tcp_set_rto(tp);
}
tcp_bound_rto(tp);
}
static __inline__ void tcp_ack_packets_out(struct sock *sk, struct tcp_opt *tp)
{
struct sk_buff *skb = skb_peek(&sk->write_queue);
if (tp->retransmits) {
tcp_xmit_retransmit_queue(sk);
tcp_reset_xmit_timer(sk, TIME_RETRANS, tp->rto);
} else {
__u32 when = tp->rto - (tcp_time_stamp - TCP_SKB_CB(skb)->when);
if ((__s32)when < 0)
when = 1;
tcp_reset_xmit_timer(sk, TIME_RETRANS, when);
}
}
static int tcp_ack(struct sock *sk, struct tcphdr *th,
u32 ack_seq, u32 ack, int len)
{
struct tcp_opt *tp = &(sk->tp_pinfo.af_tcp);
int flag = 0;
u32 seq = 0;
u32 seq_rtt = 0;
if(sk->zapped)
return(1);
if (tp->pending == TIME_KEEPOPEN)
tp->probes_out = 0;
tp->rcv_tstamp = tcp_time_stamp;
if (after(ack, tp->snd_nxt) || before(ack, tp->snd_una))
goto uninteresting_ack;
if (len != th->doff*4) {
flag |= FLAG_DATA;
tcp_delack_estimator(tp);
}
if (before(tp->snd_wl1, ack_seq) ||
(tp->snd_wl1 == ack_seq && !after(tp->snd_wl2, ack))) {
u32 nwin = ntohs(th->window) << tp->snd_wscale;
if ((tp->snd_wl2 != ack) || (nwin > tp->snd_wnd)) {
flag |= FLAG_WIN_UPDATE;
tp->snd_wnd = nwin;
tp->snd_wl1 = ack_seq;
tp->snd_wl2 = ack;
if (nwin > tp->max_window)
tp->max_window = nwin;
}
}
sk->err_soft = 0;
if (tp->pending == TIME_PROBE0)
tcp_ack_probe(sk, ack);
flag |= tcp_clean_rtx_queue(sk, ack, &seq, &seq_rtt);
if (should_advance_cwnd(tp, flag))
tcp_cong_avoid(tp);
if (tp->saw_tstamp) {
tcp_ack_saw_tstamp(sk, tp, seq, ack, flag);
} else {
if (tp->retransmits) {
if (tp->packets_out == 0) {
tp->retransmits = 0;
tp->fackets_out = 0;
tp->retrans_out = 0;
}
} else {
if (flag & FLAG_DATA_ACKED) {
if(!(flag & FLAG_RETRANS_DATA_ACKED)) {
tp->backoff = 0;
tcp_rtt_estimator(tp, seq_rtt);
tcp_set_rto(tp);
tcp_bound_rto(tp);
}
}
}
}
if (tp->packets_out) {
if (flag & FLAG_DATA_ACKED)
tcp_ack_packets_out(sk, tp);
} else {
tcp_clear_xmit_timer(sk, TIME_RETRANS);
}
flag &= (FLAG_DATA | FLAG_WIN_UPDATE);
if ((ack == tp->snd_una && tp->packets_out && flag == 0) ||
(tp->high_seq != 0)) {
tcp_fast_retrans(sk, ack, flag);
} else {
tp->dup_acks = 0;
}
if (ack != tp->snd_una || (flag == 0 && !th->fin))
dst_confirm(sk->dst_cache);
tp->snd_una = ack;
return 1;
uninteresting_ack:
SOCK_DEBUG(sk, "Ack ignored %u %u\n", ack, tp->snd_nxt);
return 0;
}
extern void tcp_tw_schedule(struct tcp_tw_bucket *tw);
extern void tcp_tw_reschedule(struct tcp_tw_bucket *tw);
extern void tcp_tw_deschedule(struct tcp_tw_bucket *tw);
void tcp_timewait_kill(struct tcp_tw_bucket *tw)
{
struct tcp_bind_bucket *tb = tw->tb;
if(tw->bind_next)
tw->bind_next->bind_pprev = tw->bind_pprev;
*(tw->bind_pprev) = tw->bind_next;
if (tb->owners == NULL) {
if (tb->next)
tb->next->pprev = tb->pprev;
*(tb->pprev) = tb->next;
kmem_cache_free(tcp_bucket_cachep, tb);
}
if(tw->next)
tw->next->pprev = tw->pprev;
*tw->pprev = tw->next;
tw->sklist_next->sklist_prev = tw->sklist_prev;
tw->sklist_prev->sklist_next = tw->sklist_next;
kmem_cache_free(tcp_timewait_cachep, tw);
}
enum tcp_tw_status
tcp_timewait_state_process(struct tcp_tw_bucket *tw, struct sk_buff *skb,
struct tcphdr *th, unsigned len)
{
if(th->syn && !th->rst && after(TCP_SKB_CB(skb)->seq, tw->rcv_nxt)) {
struct sock *sk;
struct tcp_func *af_specific = tw->af_specific;
__u32 isn;
isn = tw->snd_nxt + 128000;
if(isn == 0)
isn++;
tcp_tw_deschedule(tw);
tcp_timewait_kill(tw);
sk = af_specific->get_sock(skb, th);
if(sk == NULL ||
!ipsec_sk_policy(sk,skb) ||
atomic_read(&sk->sock_readers) != 0)
return 0;
skb_set_owner_r(skb, sk);
af_specific = sk->tp_pinfo.af_tcp.af_specific;
if(af_specific->conn_request(sk, skb, isn) < 0)
return TCP_TW_RST;
return 0;
}
if(th->rst || th->syn) {
if(sysctl_tcp_rfc1337 == 0) {
tcp_tw_deschedule(tw);
tcp_timewait_kill(tw);
}
if(!th->rst)
return TCP_TW_RST;
return 0;
} else {
if(th->ack)
tcp_tw_reschedule(tw);
}
if (!after(TCP_SKB_CB(skb)->end_seq, tw->rcv_nxt) &&
(th->doff * 4) > len)
return TCP_TW_ACK;
return 0;
}
static __inline__ void tcp_tw_hashdance(struct sock *sk, struct tcp_tw_bucket *tw)
{
struct sock **head, *sktw;
if(sk->next)
sk->next->pprev = sk->pprev;
*sk->pprev = sk->next;
sk->pprev = NULL;
tcp_reg_zap(sk);
tw->tb = (struct tcp_bind_bucket *)sk->prev;
if((tw->bind_next = sk->bind_next) != NULL)
sk->bind_next->bind_pprev = &tw->bind_next;
tw->bind_pprev = sk->bind_pprev;
*sk->bind_pprev = (struct sock *)tw;
sk->prev = NULL;
(tw->sklist_next = sk->sklist_next)->sklist_prev = (struct sock *)tw;
(tw->sklist_prev = sk->sklist_prev)->sklist_next = (struct sock *)tw;
sk->sklist_next = NULL;
sk->prot->inuse--;
head = &tcp_ehash[sk->hashent + (tcp_ehash_size/2)];
sktw = (struct sock *)tw;
if((sktw->next = *head) != NULL)
(*head)->pprev = &sktw->next;
*head = sktw;
sktw->pprev = head;
}
void tcp_time_wait(struct sock *sk)
{
struct tcp_tw_bucket *tw;
tw = kmem_cache_alloc(tcp_timewait_cachep, SLAB_ATOMIC);
if(tw != NULL) {
tw->daddr = sk->daddr;
tw->rcv_saddr = sk->rcv_saddr;
tw->bound_dev_if= sk->bound_dev_if;
tw->num = sk->num;
tw->state = TCP_TIME_WAIT;
tw->sport = sk->sport;
tw->dport = sk->dport;
tw->family = sk->family;
tw->reuse = sk->reuse;
tw->rcv_nxt = sk->tp_pinfo.af_tcp.rcv_nxt;
tw->snd_nxt = sk->tp_pinfo.af_tcp.snd_nxt;
tw->window = tcp_select_window(sk);
tw->af_specific = sk->tp_pinfo.af_tcp.af_specific;
#if defined(CONFIG_IPV6) || defined(CONFIG_IPV6_MODULE)
if(tw->family == PF_INET6) {
memcpy(&tw->v6_daddr,
&sk->net_pinfo.af_inet6.daddr,
sizeof(struct in6_addr));
memcpy(&tw->v6_rcv_saddr,
&sk->net_pinfo.af_inet6.rcv_saddr,
sizeof(struct in6_addr));
}
#endif
tcp_tw_hashdance(sk, tw);
tcp_tw_schedule(tw);
if(sk->state == TCP_ESTABLISHED)
tcp_statistics.TcpCurrEstab--;
sk->state = TCP_CLOSE;
net_reset_timer(sk, TIME_DONE,
min(sk->tp_pinfo.af_tcp.srtt * 2, TCP_DONE_TIME));
} else {
tcp_set_state(sk, TCP_CLOSE);
}
sk->shutdown = SHUTDOWN_MASK;
if(!sk->dead)
sk->state_change(sk);
}
static void tcp_fin(struct sk_buff *skb, struct sock *sk, struct tcphdr *th)
{
sk->tp_pinfo.af_tcp.fin_seq = TCP_SKB_CB(skb)->end_seq;
tcp_send_ack(sk);
sk->shutdown |= RCV_SHUTDOWN;
sk->done = 1;
if (!sk->dead) {
sk->state_change(sk);
sock_wake_async(sk->socket, 1);
}
switch(sk->state) {
case TCP_SYN_RECV:
case TCP_ESTABLISHED:
tcp_set_state(sk, TCP_CLOSE_WAIT);
if (th->rst)
sk->shutdown = SHUTDOWN_MASK;
break;
case TCP_CLOSE_WAIT:
case TCP_CLOSING:
break;
case TCP_LAST_ACK:
break;
case TCP_FIN_WAIT1:
tcp_set_state(sk, TCP_CLOSING);
break;
case TCP_FIN_WAIT2:
tcp_time_wait(sk);
break;
default:
printk("tcp_fin: Impossible, sk->state=%d\n", sk->state);
break;
};
}
static void tcp_sack_maybe_coalesce(struct tcp_opt *tp, struct tcp_sack_block *sp)
{
int this_sack, num_sacks = tp->num_sacks;
struct tcp_sack_block *swalk = &tp->selective_acks[0];
if(num_sacks != 1) {
for(this_sack = 0; this_sack < num_sacks; this_sack++, swalk++) {
if(swalk == sp)
continue;
if(between(sp->start_seq, swalk->start_seq, swalk->end_seq)) {
sp->start_seq = swalk->start_seq;
goto coalesce;
}
if(between(sp->end_seq, swalk->start_seq, swalk->end_seq)) {
sp->end_seq = swalk->end_seq;
goto coalesce;
}
}
}
return;
coalesce:
for(; this_sack < num_sacks-1; this_sack++, swalk++) {
struct tcp_sack_block *next = (swalk + 1);
swalk->start_seq = next->start_seq;
swalk->end_seq = next->end_seq;
}
tp->num_sacks--;
}
static __inline__ void tcp_sack_swap(struct tcp_sack_block *sack1, struct tcp_sack_block *sack2)
{
__u32 tmp;
tmp = sack1->start_seq;
sack1->start_seq = sack2->start_seq;
sack2->start_seq = tmp;
tmp = sack1->end_seq;
sack1->end_seq = sack2->end_seq;
sack2->end_seq = tmp;
}
static void tcp_sack_new_ofo_skb(struct sock *sk, struct sk_buff *skb)
{
struct tcp_opt *tp = &(sk->tp_pinfo.af_tcp);
struct tcp_sack_block *sp = &tp->selective_acks[0];
int cur_sacks = tp->num_sacks;
if (!cur_sacks)
goto new_sack;
if(sp->end_seq == TCP_SKB_CB(skb)->seq) {
sp->end_seq = TCP_SKB_CB(skb)->end_seq;
tcp_sack_maybe_coalesce(tp, sp);
} else if(sp->start_seq == TCP_SKB_CB(skb)->end_seq) {
sp->start_seq = TCP_SKB_CB(skb)->seq;
tcp_sack_maybe_coalesce(tp, sp);
} else {
struct tcp_sack_block *swap = sp + 1;
int this_sack, max_sacks = (tp->tstamp_ok ? 3 : 4);
for(this_sack = 1; this_sack < cur_sacks; this_sack++, swap++) {
if((swap->end_seq == TCP_SKB_CB(skb)->seq) ||
(swap->start_seq == TCP_SKB_CB(skb)->end_seq)) {
if(swap->end_seq == TCP_SKB_CB(skb)->seq)
swap->end_seq = TCP_SKB_CB(skb)->end_seq;
else
swap->start_seq = TCP_SKB_CB(skb)->seq;
tcp_sack_swap(sp, swap);
tcp_sack_maybe_coalesce(tp, sp);
return;
}
}
if (cur_sacks >= max_sacks) {
cur_sacks--;
tp->num_sacks--;
}
while(cur_sacks >= 1) {
struct tcp_sack_block *this = &tp->selective_acks[cur_sacks];
struct tcp_sack_block *prev = (this - 1);
this->start_seq = prev->start_seq;
this->end_seq = prev->end_seq;
cur_sacks--;
}
new_sack:
sp->start_seq = TCP_SKB_CB(skb)->seq;
sp->end_seq = TCP_SKB_CB(skb)->end_seq;
tp->num_sacks++;
}
}
static void tcp_sack_remove_skb(struct tcp_opt *tp, struct sk_buff *skb)
{
struct tcp_sack_block *sp = &tp->selective_acks[0];
int num_sacks = tp->num_sacks;
int this_sack;
for(this_sack = 0; this_sack < num_sacks; this_sack++, sp++) {
if(!before(sp->start_seq, TCP_SKB_CB(skb)->seq) &&
before(sp->start_seq, TCP_SKB_CB(skb)->end_seq))
break;
}
if(this_sack >= num_sacks)
return;
sp->start_seq = TCP_SKB_CB(skb)->end_seq;
if(!before(sp->start_seq, sp->end_seq)) {
for(this_sack += 1; this_sack < num_sacks; this_sack++, sp++) {
struct tcp_sack_block *next = (sp + 1);
sp->start_seq = next->start_seq;
sp->end_seq = next->end_seq;
}
tp->num_sacks--;
}
}
static void tcp_sack_extend(struct tcp_opt *tp, struct sk_buff *old_skb, struct sk_buff *new_skb)
{
struct tcp_sack_block *sp = &tp->selective_acks[0];
int num_sacks = tp->num_sacks;
int this_sack;
for(this_sack = 0; this_sack < num_sacks; this_sack++, sp++) {
if(sp->end_seq == TCP_SKB_CB(old_skb)->end_seq)
break;
}
if(this_sack >= num_sacks)
return;
sp->end_seq = TCP_SKB_CB(new_skb)->end_seq;
}
static void tcp_ofo_queue(struct sock *sk)
{
struct sk_buff *skb;
struct tcp_opt *tp = &(sk->tp_pinfo.af_tcp);
while ((skb = skb_peek(&tp->out_of_order_queue))) {
if (after(TCP_SKB_CB(skb)->seq, tp->rcv_nxt))
break;
if (!after(TCP_SKB_CB(skb)->end_seq, tp->rcv_nxt)) {
SOCK_DEBUG(sk, "ofo packet was already received \n");
__skb_unlink(skb, skb->list);
kfree_skb(skb);
continue;
}
SOCK_DEBUG(sk, "ofo requeuing : rcv_next %X seq %X - %X\n",
tp->rcv_nxt, TCP_SKB_CB(skb)->seq,
TCP_SKB_CB(skb)->end_seq);
if(tp->sack_ok)
tcp_sack_remove_skb(tp, skb);
__skb_unlink(skb, skb->list);
__skb_queue_tail(&sk->receive_queue, skb);
tp->rcv_nxt = TCP_SKB_CB(skb)->end_seq;
if(skb->h.th->fin)
tcp_fin(skb, sk, skb->h.th);
}
}
static void tcp_data_queue(struct sock *sk, struct sk_buff *skb)
{
struct sk_buff *skb1;
struct tcp_opt *tp = &(sk->tp_pinfo.af_tcp);
if (TCP_SKB_CB(skb)->seq == tp->rcv_nxt) {
queue_and_out:
dst_confirm(sk->dst_cache);
__skb_queue_tail(&sk->receive_queue, skb);
tp->rcv_nxt = TCP_SKB_CB(skb)->end_seq;
if(skb->h.th->fin) {
tcp_fin(skb, sk, skb->h.th);
} else {
tcp_remember_ack(tp, skb->h.th, skb);
}
if(tp->sack_ok && tp->num_sacks)
tcp_sack_remove_skb(tp, skb);
tcp_ofo_queue(sk);
if (skb_queue_len(&tp->out_of_order_queue) == 0)
tp->pred_flags = htonl(((tp->tcp_header_len >> 2) << 28) |
(0x10 << 16) |
tp->snd_wnd);
return;
}
if (!after(TCP_SKB_CB(skb)->end_seq, tp->rcv_nxt)) {
SOCK_DEBUG(sk, "retransmit received: seq %X\n", TCP_SKB_CB(skb)->seq);
tcp_enter_quickack_mode(tp);
kfree_skb(skb);
return;
}
if (before(TCP_SKB_CB(skb)->seq, tp->rcv_nxt)) {
SOCK_DEBUG(sk, "partial packet: rcv_next %X seq %X - %X\n",
tp->rcv_nxt, TCP_SKB_CB(skb)->seq,
TCP_SKB_CB(skb)->end_seq);
goto queue_and_out;
}
tp->delayed_acks++;
tcp_enter_quickack_mode(tp);
tp->pred_flags = 0;
SOCK_DEBUG(sk, "out of order segment: rcv_next %X seq %X - %X\n",
tp->rcv_nxt, TCP_SKB_CB(skb)->seq, TCP_SKB_CB(skb)->end_seq);
if (skb_peek(&tp->out_of_order_queue) == NULL) {
if(tp->sack_ok) {
tp->num_sacks = 1;
tp->selective_acks[0].start_seq = TCP_SKB_CB(skb)->seq;
tp->selective_acks[0].end_seq = TCP_SKB_CB(skb)->end_seq;
}
__skb_queue_head(&tp->out_of_order_queue,skb);
} else {
for(skb1=tp->out_of_order_queue.prev; ; skb1 = skb1->prev) {
if (TCP_SKB_CB(skb)->seq == TCP_SKB_CB(skb1)->seq) {
if (skb->len >= skb1->len) {
if(tp->sack_ok)
tcp_sack_extend(tp, skb1, skb);
__skb_append(skb1, skb);
__skb_unlink(skb1, skb1->list);
kfree_skb(skb1);
} else {
kfree_skb(skb);
}
break;
}
if (after(TCP_SKB_CB(skb)->seq, TCP_SKB_CB(skb1)->seq)) {
__skb_append(skb1, skb);
if(tp->sack_ok)
tcp_sack_new_ofo_skb(sk, skb);
break;
}
if (skb1 == skb_peek(&tp->out_of_order_queue)) {
__skb_queue_head(&tp->out_of_order_queue,skb);
if(tp->sack_ok)
tcp_sack_new_ofo_skb(sk, skb);
break;
}
}
}
}
static int tcp_data(struct sk_buff *skb, struct sock *sk, unsigned int len)
{
struct tcphdr *th;
struct tcp_opt *tp = &(sk->tp_pinfo.af_tcp);
th = skb->h.th;
skb_pull(skb, th->doff*4);
skb_trim(skb, len - (th->doff*4));
if (skb->len == 0 && !th->fin)
return(0);
if (atomic_read(&sk->rmem_alloc) > sk->rcvbuf) {
if (prune_queue(sk) < 0) {
return 0;
}
}
tcp_data_queue(sk, skb);
if (before(tp->rcv_nxt, tp->copied_seq)) {
printk(KERN_DEBUG "*** tcp.c:tcp_data bug acked < copied\n");
tp->rcv_nxt = tp->copied_seq;
}
if (!sk->dead) {
sk->data_ready(sk,0);
}
return(1);
}
static void __tcp_data_snd_check(struct sock *sk, struct sk_buff *skb)
{
struct tcp_opt *tp = &(sk->tp_pinfo.af_tcp);
if (!after(TCP_SKB_CB(skb)->end_seq, tp->snd_una + tp->snd_wnd) &&
tcp_packets_in_flight(tp) < tp->snd_cwnd) {
tcp_write_xmit(sk);
} else if (tp->packets_out == 0 && !tp->pending) {
tcp_reset_xmit_timer(sk, TIME_PROBE0, tp->rto);
}
}
static __inline__ void tcp_data_snd_check(struct sock *sk)
{
struct sk_buff *skb = sk->tp_pinfo.af_tcp.send_head;
if (skb != NULL)
__tcp_data_snd_check(sk, skb);
}
static __inline__ void tcp_measure_rcv_mss(struct sock *sk, struct sk_buff *skb)
{
struct tcp_opt *tp = &(sk->tp_pinfo.af_tcp);
unsigned int len = skb->len, lss;
if (len > tp->rcv_mss)
tp->rcv_mss = len;
lss = tp->last_seg_size;
tp->last_seg_size = 0;
if (len >= 536) {
if (len == lss)
tp->rcv_mss = len;
tp->last_seg_size = len;
}
}
static __inline__ void __tcp_ack_snd_check(struct sock *sk)
{
struct tcp_opt *tp = &(sk->tp_pinfo.af_tcp);
if (((tp->rcv_nxt - tp->rcv_wup) >= tp->rcv_mss * MAX_DELAY_ACK) ||
tcp_raise_window(sk) ||
tcp_in_quickack_mode(tp) ||
(skb_peek(&tp->out_of_order_queue) != NULL)) {
tcp_send_ack(sk);
} else {
tcp_send_delayed_ack(tp, HZ/2);
}
}
static __inline__ void tcp_ack_snd_check(struct sock *sk)
{
struct tcp_opt *tp = &(sk->tp_pinfo.af_tcp);
if (tp->delayed_acks == 0) {
return;
}
__tcp_ack_snd_check(sk);
}
static void tcp_check_urg(struct sock * sk, struct tcphdr * th)
{
struct tcp_opt *tp = &(sk->tp_pinfo.af_tcp);
u32 ptr = ntohs(th->urg_ptr);
if (ptr && !sysctl_tcp_stdurg)
ptr--;
ptr += ntohl(th->seq);
if (after(tp->copied_seq, ptr))
return;
if (tp->urg_data && !after(ptr, tp->urg_seq))
return;
if (sk->proc != 0) {
if (sk->proc > 0)
kill_proc(sk->proc, SIGURG, 1);
else
kill_pg(-sk->proc, SIGURG, 1);
}
if (tp->urg_seq == tp->copied_seq)
tp->copied_seq++;
tp->urg_data = URG_NOTYET;
tp->urg_seq = ptr;
tp->pred_flags = 0;
}
static inline void tcp_urg(struct sock *sk, struct tcphdr *th, unsigned long len)
{
struct tcp_opt *tp = &(sk->tp_pinfo.af_tcp);
if (th->urg)
tcp_check_urg(sk,th);
if (tp->urg_data == URG_NOTYET) {
u32 ptr = tp->urg_seq - ntohl(th->seq) + (th->doff*4);
if (ptr < len) {
tp->urg_data = URG_VALID | *(ptr + (unsigned char *) th);
if (!sk->dead)
sk->data_ready(sk,0);
}
}
}
static int prune_queue(struct sock *sk)
{
struct tcp_opt *tp = &sk->tp_pinfo.af_tcp;
struct sk_buff * skb;
SOCK_DEBUG(sk, "prune_queue: c=%x\n", tp->copied_seq);
net_statistics.PruneCalled++;
skb = __skb_dequeue_tail(&tp->out_of_order_queue);
if(skb != NULL) {
do { net_statistics.OfoPruned += skb->len;
kfree_skb(skb);
skb = __skb_dequeue_tail(&tp->out_of_order_queue);
} while(skb != NULL);
if(tp->sack_ok)
tp->num_sacks = 0;
}
if(atomic_read(&sk->rmem_alloc) < (sk->rcvbuf << 1))
return 0;
return -1;
}
int tcp_rcv_established(struct sock *sk, struct sk_buff *skb,
struct tcphdr *th, unsigned len)
{
struct tcp_opt *tp = &(sk->tp_pinfo.af_tcp);
int queued;
u32 flg;
if (tcp_fast_parse_options(sk, th, tp)) {
if (tp->saw_tstamp) {
if (tcp_paws_discard(tp, th, len)) {
tcp_statistics.TcpInErrs++;
if (!th->rst) {
tcp_send_ack(sk);
goto discard;
}
}
tcp_replace_ts_recent(sk, tp,
TCP_SKB_CB(skb)->seq,
TCP_SKB_CB(skb)->end_seq);
}
}
flg = *(((u32 *)th) + 3) & ~htonl(0xFC8 << 16);
if (flg == tp->pred_flags && TCP_SKB_CB(skb)->seq == tp->rcv_nxt) {
if (len <= th->doff*4) {
if (len == th->doff*4) {
tcp_ack(sk, th, TCP_SKB_CB(skb)->seq,
TCP_SKB_CB(skb)->ack_seq, len);
kfree_skb(skb);
tcp_data_snd_check(sk);
return 0;
} else {
tcp_statistics.TcpInErrs++;
goto discard;
}
} else if (TCP_SKB_CB(skb)->ack_seq == tp->snd_una &&
atomic_read(&sk->rmem_alloc) <= sk->rcvbuf) {
__skb_pull(skb,th->doff*4);
tcp_measure_rcv_mss(sk, skb);
__skb_queue_tail(&sk->receive_queue, skb);
tp->rcv_nxt = TCP_SKB_CB(skb)->end_seq;
sk->data_ready(sk, 0);
tcp_delack_estimator(tp);
tcp_remember_ack(tp, th, skb);
__tcp_ack_snd_check(sk);
return 0;
}
}
if (!tcp_sequence(tp, TCP_SKB_CB(skb)->seq, TCP_SKB_CB(skb)->end_seq)) {
if (th->rst)
goto discard;
if (after(TCP_SKB_CB(skb)->seq, tp->rcv_nxt)) {
SOCK_DEBUG(sk, "seq:%d end:%d wup:%d wnd:%d\n",
TCP_SKB_CB(skb)->seq, TCP_SKB_CB(skb)->end_seq,
tp->rcv_wup, tp->rcv_wnd);
}
tcp_send_ack(sk);
goto discard;
}
if(th->syn && TCP_SKB_CB(skb)->seq != tp->syn_seq) {
SOCK_DEBUG(sk, "syn in established state\n");
tcp_statistics.TcpInErrs++;
tcp_reset(sk);
return 1;
}
if(th->rst) {
tcp_reset(sk);
goto discard;
}
if(th->ack)
tcp_ack(sk, th, TCP_SKB_CB(skb)->seq, TCP_SKB_CB(skb)->ack_seq, len);
tcp_urg(sk, th, len);
queued = tcp_data(skb, sk, len);
tcp_measure_rcv_mss(sk, skb);
if(sk->state != TCP_CLOSE) {
tcp_data_snd_check(sk);
tcp_ack_snd_check(sk);
}
if (!queued) {
discard:
kfree_skb(skb);
}
return 0;
}
struct sock *tcp_check_req(struct sock *sk, struct sk_buff *skb,
struct open_request *req)
{
struct tcp_opt *tp = &(sk->tp_pinfo.af_tcp);
u32 flg;
flg = *(((u32 *)skb->h.th) + 3);
flg &= __constant_htonl(0x00170000);
if (flg == __constant_htonl(0x00020000)) {
if (TCP_SKB_CB(skb)->seq == req->rcv_isn) {
req->class->rtx_syn_ack(sk, req);
return NULL;
} else {
return sk;
}
}
if (req->sk) {
sk = req->sk;
} else {
if (!between(TCP_SKB_CB(skb)->ack_seq, req->snt_isn, req->snt_isn+1) ||
!between(TCP_SKB_CB(skb)->seq, req->rcv_isn,
req->rcv_isn+1+req->rcv_wnd)) {
req->class->send_reset(skb);
return NULL;
}
sk = tp->af_specific->syn_recv_sock(sk, skb, req, NULL);
tcp_dec_slow_timer(TCP_SLT_SYNACK);
if (sk == NULL)
return NULL;
req->expires = 0UL;
req->sk = sk;
}
skb_orphan(skb);
skb_set_owner_r(skb, sk);
return sk;
}
int tcp_rcv_state_process(struct sock *sk, struct sk_buff *skb,
struct tcphdr *th, unsigned len)
{
struct tcp_opt *tp = &(sk->tp_pinfo.af_tcp);
int queued = 0;
switch (sk->state) {
case TCP_CLOSE:
return 1;
case TCP_LISTEN:
if(th->ack)
return 1;
if(th->syn) {
if(tp->af_specific->conn_request(sk, skb, 0) < 0)
return 1;
goto discard;
}
goto discard;
break;
case TCP_SYN_SENT:
if(th->ack) {
if (sk->zapped ||
TCP_SKB_CB(skb)->ack_seq != tp->snd_nxt)
return 1;
if (th->rst) {
tcp_reset(sk);
goto discard;
}
if (!th->syn)
goto discard;
tp->snd_wl1 = TCP_SKB_CB(skb)->seq;
tcp_ack(sk,th, TCP_SKB_CB(skb)->seq,
TCP_SKB_CB(skb)->ack_seq, len);
tp->rcv_nxt = TCP_SKB_CB(skb)->seq+1;
tp->rcv_wup = TCP_SKB_CB(skb)->seq+1;
tp->snd_wnd = htons(th->window);
tp->snd_wl1 = TCP_SKB_CB(skb)->seq;
tp->snd_wl2 = TCP_SKB_CB(skb)->ack_seq;
tp->fin_seq = TCP_SKB_CB(skb)->seq;
tcp_set_state(sk, TCP_ESTABLISHED);
tcp_parse_options(sk, th, tp, 0);
if (tp->wscale_ok == 0) {
tp->snd_wscale = tp->rcv_wscale = 0;
tp->window_clamp = min(tp->window_clamp,65535);
}
if (tp->tstamp_ok) {
tp->tcp_header_len =
sizeof(struct tcphdr) + TCPOLEN_TSTAMP_ALIGNED;
} else
tp->tcp_header_len = sizeof(struct tcphdr);
if (tp->saw_tstamp) {
tp->ts_recent = tp->rcv_tsval;
tp->ts_recent_stamp = tcp_time_stamp;
}
tcp_send_ack(sk);
sk->dport = th->source;
tp->copied_seq = tp->rcv_nxt;
if(!sk->dead) {
sk->state_change(sk);
sock_wake_async(sk->socket, 0);
}
} else {
if(th->syn && !th->rst) {
tcp_set_state(sk, TCP_SYN_RECV);
tcp_parse_options(sk, th, tp, 0);
if (tp->saw_tstamp) {
tp->ts_recent = tp->rcv_tsval;
tp->ts_recent_stamp = tcp_time_stamp;
}
tp->rcv_nxt = TCP_SKB_CB(skb)->seq + 1;
tp->rcv_wup = TCP_SKB_CB(skb)->seq + 1;
tp->snd_wnd = htons(th->window);
tp->snd_wl1 = TCP_SKB_CB(skb)->seq;
tcp_send_synack(sk);
} else
break;
}
tcp_sync_mss(sk, tp->pmtu_cookie);
tp->rcv_mss = tp->mss_cache;
if (sk->state == TCP_SYN_RECV)
goto discard;
goto step6;
}
if (tcp_fast_parse_options(sk, th, tp)) {
if (tp->saw_tstamp) {
if (tcp_paws_discard(tp, th, len)) {
tcp_statistics.TcpInErrs++;
if (!th->rst) {
tcp_send_ack(sk);
goto discard;
}
}
tcp_replace_ts_recent(sk, tp,
TCP_SKB_CB(skb)->seq,
TCP_SKB_CB(skb)->end_seq);
}
}
if (!tcp_sequence(tp, TCP_SKB_CB(skb)->seq, TCP_SKB_CB(skb)->end_seq) &&
!(th->fin && TCP_SKB_CB(skb)->end_seq == tp->rcv_nxt)) {
if (!th->rst) {
tcp_send_ack(sk);
}
goto discard;
}
if(th->rst) {
tcp_reset(sk);
goto discard;
}
if (th->syn && TCP_SKB_CB(skb)->seq != tp->syn_seq) {
tcp_reset(sk);
return 1;
}
if (th->ack) {
int acceptable = tcp_ack(sk, th, TCP_SKB_CB(skb)->seq,
TCP_SKB_CB(skb)->ack_seq, len);
switch(sk->state) {
case TCP_SYN_RECV:
if (acceptable) {
tcp_set_state(sk, TCP_ESTABLISHED);
sk->dport = th->source;
tp->copied_seq = tp->rcv_nxt;
if(!sk->dead)
sk->state_change(sk);
tp->snd_una = TCP_SKB_CB(skb)->ack_seq;
tp->snd_wnd = htons(th->window) << tp->snd_wscale;
tp->snd_wl1 = TCP_SKB_CB(skb)->seq;
tp->snd_wl2 = TCP_SKB_CB(skb)->ack_seq;
} else {
SOCK_DEBUG(sk, "bad ack\n");
return 1;
}
break;
case TCP_FIN_WAIT1:
if (tp->snd_una == tp->write_seq) {
sk->shutdown |= SEND_SHUTDOWN;
tcp_set_state(sk, TCP_FIN_WAIT2);
if (!sk->dead)
sk->state_change(sk);
else
tcp_reset_msl_timer(sk, TIME_CLOSE, sysctl_tcp_fin_timeout);
}
break;
case TCP_CLOSING:
if (tp->snd_una == tp->write_seq) {
tcp_time_wait(sk);
goto discard;
}
break;
case TCP_LAST_ACK:
if (tp->snd_una == tp->write_seq) {
sk->shutdown = SHUTDOWN_MASK;
tcp_set_state(sk,TCP_CLOSE);
if (!sk->dead)
sk->state_change(sk);
goto discard;
}
break;
}
} else
goto discard;
step6:
tcp_urg(sk, th, len);
switch (sk->state) {
case TCP_CLOSE_WAIT:
case TCP_CLOSING:
if (!before(TCP_SKB_CB(skb)->seq, tp->fin_seq))
break;
case TCP_FIN_WAIT1:
case TCP_FIN_WAIT2:
if ((sk->shutdown & RCV_SHUTDOWN) && sk->dead) {
if (after(TCP_SKB_CB(skb)->end_seq - th->fin, tp->rcv_nxt)) {
tcp_reset(sk);
return 1;
}
}
case TCP_ESTABLISHED:
queued = tcp_data(skb, sk, len);
tcp_measure_rcv_mss(sk, skb);
break;
}
tcp_data_snd_check(sk);
tcp_ack_snd_check(sk);
if (!queued) {
discard:
kfree_skb(skb);
}
return 0;
}