#include <net/tcp.h>
extern int sysctl_tcp_timestamps;
extern int sysctl_tcp_window_scaling;
extern int sysctl_tcp_sack;
int sysctl_tcp_retrans_collapse = 1;
static __inline__ void clear_delayed_acks(struct sock * sk)
{
struct tcp_opt *tp = &(sk->tp_pinfo.af_tcp);
tp->delayed_acks = 0;
if(tcp_in_quickack_mode(tp))
tcp_exit_quickack_mode(tp);
tcp_clear_xmit_timer(sk, TIME_DACK);
}
static __inline__ void update_send_head(struct sock *sk)
{
struct tcp_opt *tp = &sk->tp_pinfo.af_tcp;
tp->send_head = tp->send_head->next;
if (tp->send_head == (struct sk_buff *) &sk->write_queue)
tp->send_head = NULL;
}
void tcp_transmit_skb(struct sock *sk, struct sk_buff *skb)
{
if(skb != NULL) {
struct tcp_opt *tp = &(sk->tp_pinfo.af_tcp);
struct tcp_skb_cb *tcb = TCP_SKB_CB(skb);
int tcp_header_size = tp->tcp_header_len;
struct tcphdr *th;
int sysctl_flags;
#define SYSCTL_FLAG_TSTAMPS	0x1
#define SYSCTL_FLAG_WSCALE	0x2
#define SYSCTL_FLAG_SACK	0x4
sysctl_flags = 0;
if(tcb->flags & TCPCB_FLAG_SYN) {
tcp_header_size = sizeof(struct tcphdr) + TCPOLEN_MSS;
if(sysctl_tcp_timestamps) {
tcp_header_size += TCPOLEN_TSTAMP_ALIGNED;
sysctl_flags |= SYSCTL_FLAG_TSTAMPS;
}
if(sysctl_tcp_window_scaling) {
tcp_header_size += TCPOLEN_WSCALE_ALIGNED;
sysctl_flags |= SYSCTL_FLAG_WSCALE;
}
if(sysctl_tcp_sack) {
sysctl_flags |= SYSCTL_FLAG_SACK;
if(!(sysctl_flags & SYSCTL_FLAG_TSTAMPS))
tcp_header_size += TCPOLEN_SACKPERM_ALIGNED;
}
} else if(tp->sack_ok && tp->num_sacks) {
tcp_header_size += (TCPOLEN_SACK_BASE_ALIGNED +
(tp->num_sacks * TCPOLEN_SACK_PERBLOCK));
}
th = (struct tcphdr *) skb_push(skb, tcp_header_size);
skb->h.th = th;
skb_set_owner_w(skb, sk);
th->source		= sk->sport;
th->dest		= sk->dport;
th->seq			= htonl(TCP_SKB_CB(skb)->seq);
th->ack_seq		= htonl(tp->rcv_nxt);
th->doff		= (tcp_header_size >> 2);
th->res1		= 0;
*(((__u8 *)th) + 13)	= tcb->flags;
if(!(tcb->flags & TCPCB_FLAG_SYN))
th->window	= htons(tcp_select_window(sk));
th->check		= 0;
th->urg_ptr		= ntohs(tcb->urg_ptr);
if(tcb->flags & TCPCB_FLAG_SYN) {
th->window	= htons(tp->rcv_wnd);
tcp_syn_build_options((__u32 *)(th + 1), tp->mss_clamp,
(sysctl_flags & SYSCTL_FLAG_TSTAMPS),
(sysctl_flags & SYSCTL_FLAG_SACK),
(sysctl_flags & SYSCTL_FLAG_WSCALE),
tp->rcv_wscale,
TCP_SKB_CB(skb)->when,
tp->ts_recent);
} else {
tcp_build_and_update_options((__u32 *)(th + 1),
tp, TCP_SKB_CB(skb)->when);
}
tp->af_specific->send_check(sk, th, skb->len, skb);
clear_delayed_acks(sk);
tp->last_ack_sent = tp->rcv_nxt;
tcp_statistics.TcpOutSegs++;
tp->af_specific->queue_xmit(skb);
}
#undef SYSCTL_FLAG_TSTAMPS
#undef SYSCTL_FLAG_WSCALE
#undef SYSCTL_FLAG_SACK
}
void tcp_send_skb(struct sock *sk, struct sk_buff *skb, int force_queue)
{
struct tcp_opt *tp = &(sk->tp_pinfo.af_tcp);
tp->write_seq += (TCP_SKB_CB(skb)->end_seq - TCP_SKB_CB(skb)->seq);
__skb_queue_tail(&sk->write_queue, skb);
if (!force_queue && tp->send_head == NULL && tcp_snd_test(sk, skb)) {
TCP_SKB_CB(skb)->when = tcp_time_stamp;
tp->snd_nxt = TCP_SKB_CB(skb)->end_seq;
tp->packets_out++;
tcp_transmit_skb(sk, skb_clone(skb, GFP_KERNEL));
if(!tcp_timer_is_set(sk, TIME_RETRANS))
tcp_reset_xmit_timer(sk, TIME_RETRANS, tp->rto);
} else {
if (tp->send_head == NULL)
tp->send_head = skb;
if (!force_queue && tp->packets_out == 0 && !tp->pending) {
tp->pending = TIME_PROBE0;
tcp_reset_xmit_timer(sk, TIME_PROBE0, tp->rto);
}
}
}
static int tcp_fragment(struct sock *sk, struct sk_buff *skb, u32 len)
{
struct sk_buff *buff;
int nsize = skb->len - len;
u16 flags;
buff = sock_wmalloc(sk,
(nsize + MAX_HEADER + sk->prot->max_header),
1, GFP_ATOMIC);
if (buff == NULL)
return -1;
skb_reserve(buff, MAX_HEADER + sk->prot->max_header);
TCP_SKB_CB(buff)->seq = TCP_SKB_CB(skb)->seq + len;
TCP_SKB_CB(buff)->end_seq = TCP_SKB_CB(skb)->end_seq;
flags = TCP_SKB_CB(skb)->flags;
TCP_SKB_CB(skb)->flags = flags & ~(TCPCB_FLAG_FIN | TCPCB_FLAG_PSH);
if(flags & TCPCB_FLAG_URG) {
u16 old_urg_ptr = TCP_SKB_CB(skb)->urg_ptr;
if(old_urg_ptr > len) {
TCP_SKB_CB(skb)->flags &= ~(TCPCB_FLAG_URG);
TCP_SKB_CB(skb)->urg_ptr = 0;
TCP_SKB_CB(buff)->urg_ptr = old_urg_ptr - len;
} else {
flags &= ~(TCPCB_FLAG_URG);
}
}
if(!(flags & TCPCB_FLAG_URG))
TCP_SKB_CB(buff)->urg_ptr = 0;
TCP_SKB_CB(buff)->flags = flags;
TCP_SKB_CB(buff)->sacked = 0;
buff->csum = csum_partial_copy(skb->data + len, skb_put(buff, nsize),
nsize, 0);
TCP_SKB_CB(skb)->end_seq = TCP_SKB_CB(buff)->seq;
skb_trim(skb, len);
skb->csum = csum_partial(skb->data, skb->len, 0);
TCP_SKB_CB(buff)->when = TCP_SKB_CB(skb)->when;
__skb_append(skb, buff);
return 0;
}
int tcp_sync_mss(struct sock *sk, u32 pmtu)
{
struct tcp_opt *tp = &sk->tp_pinfo.af_tcp;
int mss_now;
mss_now = pmtu - tp->af_specific->net_header_len - sizeof(struct tcphdr);
if (mss_now > tp->mss_clamp)
mss_now = tp->mss_clamp;
mss_now -= tp->tcp_header_len - sizeof(struct tcphdr);
mss_now -= tp->ext_header_len;
if (mss_now < 8)
mss_now = 8;
tp->pmtu_cookie = pmtu;
tp->mss_cache = mss_now;
return mss_now;
}
void tcp_write_xmit(struct sock *sk)
{
struct tcp_opt *tp = &(sk->tp_pinfo.af_tcp);
unsigned int mss_now;
mss_now = tcp_current_mss(sk);
if(!sk->zapped) {
struct sk_buff *skb;
int sent_pkts = 0;
while((skb = tp->send_head) && tcp_snd_test(sk, skb)) {
if (skb->len > mss_now) {
if (tcp_fragment(sk, skb, mss_now))
break;
}
update_send_head(sk);
TCP_SKB_CB(skb)->when = tcp_time_stamp;
tp->snd_nxt = TCP_SKB_CB(skb)->end_seq;
tp->packets_out++;
tcp_transmit_skb(sk, skb_clone(skb, GFP_ATOMIC));
sent_pkts = 1;
}
if (sent_pkts && !tcp_timer_is_set(sk, TIME_RETRANS))
tcp_reset_xmit_timer(sk, TIME_RETRANS, tp->rto);
}
}
u32 __tcp_select_window(struct sock *sk)
{
struct tcp_opt *tp = &sk->tp_pinfo.af_tcp;
unsigned int mss = tp->mss_cache;
int free_space;
u32 window;
free_space = (sk->rcvbuf - atomic_read(&sk->rmem_alloc)) / 2;
if (tp->window_clamp) {
if (free_space > ((int) tp->window_clamp))
free_space = tp->window_clamp;
mss = min(tp->window_clamp, mss);
} else {
printk("tcp_select_window: tp->window_clamp == 0.\n");
}
if (mss < 1) {
mss = 1;
printk("tcp_select_window: sk->mss fell to 0.\n");
}
if ((free_space < (sk->rcvbuf/4)) && (free_space < ((int) (mss/2)))) {
window = 0;
tp->pred_flags = 0;
} else {
window = tp->rcv_wnd;
if ((((int) window) <= (free_space - ((int) mss))) ||
(((int) window) > free_space))
window = (((unsigned int) free_space)/mss)*mss;
}
return window;
}
static void tcp_retrans_try_collapse(struct sock *sk, struct sk_buff *skb, int mss_now)
{
struct sk_buff *next_skb = skb->next;
if(!skb_cloned(skb) && !skb_cloned(next_skb)) {
int skb_size = skb->len, next_skb_size = next_skb->len;
u16 flags = TCP_SKB_CB(skb)->flags;
if(flags & TCPCB_FLAG_URG)
return;
if(TCP_SKB_CB(next_skb)->sacked & TCPCB_SACKED_ACKED)
return;
if ((next_skb_size > skb_tailroom(skb)) ||
((skb_size + next_skb_size) > mss_now))
return;
__skb_unlink(next_skb, next_skb->list);
if(skb->len % 4) {
memcpy(skb_put(skb, next_skb_size), next_skb->data, next_skb_size);
skb->csum = csum_partial(skb->data, skb->len, 0);
} else {
skb->csum = csum_partial_copy(next_skb->data,
skb_put(skb, next_skb_size),
next_skb_size, skb->csum);
}
TCP_SKB_CB(skb)->end_seq = TCP_SKB_CB(next_skb)->end_seq;
flags |= TCP_SKB_CB(next_skb)->flags;
if(flags & TCPCB_FLAG_URG) {
u16 urgptr = TCP_SKB_CB(next_skb)->urg_ptr;
TCP_SKB_CB(skb)->urg_ptr = urgptr + skb_size;
}
TCP_SKB_CB(skb)->flags = flags;
kfree_skb(next_skb);
sk->tp_pinfo.af_tcp.packets_out--;
}
}
void tcp_simple_retransmit(struct sock *sk)
{
struct tcp_opt *tp = &(sk->tp_pinfo.af_tcp);
struct sk_buff *skb, *old_next_skb;
unsigned int mss = tcp_current_mss(sk);
tp->dup_acks = 0;
tp->high_seq = tp->snd_nxt;
tp->retrans_head = NULL;
for (old_next_skb = skb = skb_peek(&sk->write_queue);
((skb != tp->send_head) &&
(skb != (struct sk_buff *)&sk->write_queue));
skb = skb->next) {
int resend_skb = 0;
if (old_next_skb != skb || skb->len > mss)
resend_skb = 1;
old_next_skb = skb->next;
if (resend_skb != 0)
tcp_retransmit_skb(sk, skb);
}
}
static __inline__ void update_retrans_head(struct sock *sk)
{
struct tcp_opt *tp = &sk->tp_pinfo.af_tcp;
tp->retrans_head = tp->retrans_head->next;
if((tp->retrans_head == tp->send_head) ||
(tp->retrans_head == (struct sk_buff *) &sk->write_queue)) {
tp->retrans_head = NULL;
tp->rexmt_done = 1;
}
}
int tcp_retransmit_skb(struct sock *sk, struct sk_buff *skb)
{
struct tcp_opt *tp = &(sk->tp_pinfo.af_tcp);
unsigned int cur_mss = tcp_current_mss(sk);
if(skb->len > cur_mss) {
if(tcp_fragment(sk, skb, cur_mss))
return 1;
tp->packets_out++;
}
if(!(TCP_SKB_CB(skb)->flags & TCPCB_FLAG_SYN) &&
(skb->len < (cur_mss >> 1)) &&
(skb->next != tp->send_head) &&
(skb->next != (struct sk_buff *)&sk->write_queue) &&
(sysctl_tcp_retrans_collapse != 0))
tcp_retrans_try_collapse(sk, skb, cur_mss);
if(tp->af_specific->rebuild_header(sk))
return 1;
if(skb->len > 0 &&
(TCP_SKB_CB(skb)->flags & TCPCB_FLAG_FIN) &&
tp->snd_una == (TCP_SKB_CB(skb)->end_seq - 1)) {
TCP_SKB_CB(skb)->seq = TCP_SKB_CB(skb)->end_seq - 1;
skb_trim(skb, 0);
skb->csum = 0;
}
TCP_SKB_CB(skb)->sacked |= TCPCB_SACKED_RETRANS;
tp->retrans_out++;
TCP_SKB_CB(skb)->when = tcp_time_stamp;
if(skb_cloned(skb))
skb = skb_copy(skb, GFP_ATOMIC);
else
skb = skb_clone(skb, GFP_ATOMIC);
tcp_transmit_skb(sk, skb);
sk->prot->retransmits++;
tcp_statistics.TcpRetransSegs++;
return 0;
}
void tcp_xmit_retransmit_queue(struct sock *sk)
{
struct tcp_opt *tp = &(sk->tp_pinfo.af_tcp);
struct sk_buff *skb;
if (tp->retrans_head == NULL &&
tp->rexmt_done == 0)
tp->retrans_head = skb_peek(&sk->write_queue);
if (tp->retrans_head == tp->send_head)
tp->retrans_head = NULL;
while ((skb = tp->retrans_head) != NULL) {
if(!(TCP_SKB_CB(skb)->sacked & TCPCB_SACKED_ACKED)) {
if(tcp_retransmit_skb(sk, skb))
break;
update_retrans_head(sk);
if (tp->retrans_out >= tp->snd_cwnd)
break;
} else {
update_retrans_head(sk);
}
}
}
void tcp_fack_retransmit(struct sock *sk)
{
struct tcp_opt *tp = &(sk->tp_pinfo.af_tcp);
struct sk_buff *skb = skb_peek(&sk->write_queue);
int packet_cnt = 0;
while((skb != NULL) &&
(skb != tp->send_head) &&
(skb != (struct sk_buff *)&sk->write_queue)) {
__u8 sacked = TCP_SKB_CB(skb)->sacked;
if(sacked & (TCPCB_SACKED_ACKED | TCPCB_SACKED_RETRANS))
goto next_packet;
if(tcp_retransmit_skb(sk, skb))
break;
if(tcp_packets_in_flight(tp) >= tp->snd_cwnd)
break;
next_packet:
packet_cnt++;
if(packet_cnt >= tp->fackets_out)
break;
skb = skb->next;
}
}
void tcp_send_fin(struct sock *sk)
{
struct tcp_opt *tp = &(sk->tp_pinfo.af_tcp);
struct sk_buff *skb = skb_peek_tail(&sk->write_queue);
unsigned int mss_now;
mss_now = tcp_current_mss(sk);
if((tp->send_head != NULL) && (skb->len < mss_now)) {
TCP_SKB_CB(skb)->flags |= TCPCB_FLAG_FIN;
TCP_SKB_CB(skb)->end_seq++;
tp->write_seq++;
if(tp->send_head == skb &&
!sk->nonagle &&
skb->len < (tp->mss_cache >> 1) &&
tp->packets_out &&
!(TCP_SKB_CB(skb)->flags & TCPCB_FLAG_URG)) {
update_send_head(sk);
TCP_SKB_CB(skb)->when = tcp_time_stamp;
tp->snd_nxt = TCP_SKB_CB(skb)->end_seq;
tp->packets_out++;
tcp_transmit_skb(sk, skb_clone(skb, GFP_ATOMIC));
if(!tcp_timer_is_set(sk, TIME_RETRANS))
tcp_reset_xmit_timer(sk, TIME_RETRANS, tp->rto);
}
} else {
do {
skb = sock_wmalloc(sk,
(MAX_HEADER +
sk->prot->max_header),
1, GFP_KERNEL);
} while (skb == NULL);
skb_reserve(skb, MAX_HEADER + sk->prot->max_header);
skb->csum = 0;
TCP_SKB_CB(skb)->flags = (TCPCB_FLAG_ACK | TCPCB_FLAG_FIN);
TCP_SKB_CB(skb)->sacked = 0;
TCP_SKB_CB(skb)->urg_ptr = 0;
TCP_SKB_CB(skb)->seq = tp->write_seq;
TCP_SKB_CB(skb)->end_seq = TCP_SKB_CB(skb)->seq + 1;
tcp_send_skb(sk, skb, 0);
}
}
void tcp_send_active_reset(struct sock *sk)
{
struct tcp_opt *tp = &(sk->tp_pinfo.af_tcp);
struct sk_buff *skb;
skb = alloc_skb(MAX_HEADER + sk->prot->max_header, GFP_KERNEL);
if (!skb)
return;
skb_reserve(skb, MAX_HEADER + sk->prot->max_header);
skb->csum = 0;
TCP_SKB_CB(skb)->flags = (TCPCB_FLAG_ACK | TCPCB_FLAG_RST);
TCP_SKB_CB(skb)->sacked = 0;
TCP_SKB_CB(skb)->urg_ptr = 0;
TCP_SKB_CB(skb)->seq = tp->write_seq;
TCP_SKB_CB(skb)->end_seq = TCP_SKB_CB(skb)->seq;
TCP_SKB_CB(skb)->when = tcp_time_stamp;
tcp_transmit_skb(sk, skb);
}
int tcp_send_synack(struct sock *sk)
{
struct tcp_opt* tp = &(sk->tp_pinfo.af_tcp);
struct sk_buff* skb;
skb = sock_wmalloc(sk, (MAX_HEADER + sk->prot->max_header),
1, GFP_ATOMIC);
if (skb == NULL)
return -ENOMEM;
skb_reserve(skb, MAX_HEADER + sk->prot->max_header);
skb->csum = 0;
TCP_SKB_CB(skb)->flags = (TCPCB_FLAG_ACK | TCPCB_FLAG_SYN);
TCP_SKB_CB(skb)->sacked = 0;
TCP_SKB_CB(skb)->urg_ptr = 0;
TCP_SKB_CB(skb)->seq = tp->snd_una;
TCP_SKB_CB(skb)->end_seq = TCP_SKB_CB(skb)->seq + 1;
__skb_queue_tail(&sk->write_queue, skb);
TCP_SKB_CB(skb)->when = tcp_time_stamp;
tp->packets_out++;
tcp_transmit_skb(sk, skb_clone(skb, GFP_ATOMIC));
return 0;
}
struct sk_buff * tcp_make_synack(struct sock *sk, struct dst_entry *dst,
struct open_request *req, int mss)
{
struct tcphdr *th;
int tcp_header_size;
struct sk_buff *skb;
skb = sock_wmalloc(sk, MAX_HEADER + sk->prot->max_header, 1, GFP_ATOMIC);
if (skb == NULL)
return NULL;
skb_reserve(skb, MAX_HEADER + sk->prot->max_header);
skb->dst = dst_clone(dst);
req->mss = min(mss, req->mss);
if (req->mss < 8) {
printk(KERN_DEBUG "initial req->mss below 8\n");
req->mss = 8;
}
tcp_header_size = (sizeof(struct tcphdr) + TCPOLEN_MSS +
(req->tstamp_ok ? TCPOLEN_TSTAMP_ALIGNED : 0) +
(req->wscale_ok ? TCPOLEN_WSCALE_ALIGNED : 0) +
((req->sack_ok && !req->tstamp_ok) ? TCPOLEN_SACKPERM_ALIGNED : 0));
skb->h.th = th = (struct tcphdr *) skb_push(skb, tcp_header_size);
memset(th, 0, sizeof(struct tcphdr));
th->syn = 1;
th->ack = 1;
th->source = sk->sport;
th->dest = req->rmt_port;
TCP_SKB_CB(skb)->seq = req->snt_isn;
TCP_SKB_CB(skb)->end_seq = TCP_SKB_CB(skb)->seq + 1;
th->seq = htonl(TCP_SKB_CB(skb)->seq);
th->ack_seq = htonl(req->rcv_isn + 1);
if (req->rcv_wnd == 0) {
__u8 rcv_wscale;
req->window_clamp = skb->dst->window;
tcp_select_initial_window(sock_rspace(sk)/2,req->mss,
&req->rcv_wnd,
&req->window_clamp,
req->wscale_ok,
&rcv_wscale);
req->rcv_wscale = rcv_wscale;
}
th->window = htons(req->rcv_wnd);
TCP_SKB_CB(skb)->when = tcp_time_stamp;
tcp_syn_build_options((__u32 *)(th + 1), req->mss, req->tstamp_ok,
req->sack_ok, req->wscale_ok, req->rcv_wscale,
TCP_SKB_CB(skb)->when,
req->ts_recent);
skb->csum = 0;
th->doff = (tcp_header_size >> 2);
tcp_statistics.TcpOutSegs++;
return skb;
}
void tcp_connect(struct sock *sk, struct sk_buff *buff, int mtu)
{
struct dst_entry *dst = sk->dst_cache;
struct tcp_opt *tp = &(sk->tp_pinfo.af_tcp);
skb_reserve(buff, MAX_HEADER + sk->prot->max_header);
tp->snd_wnd = 0;
tp->snd_wl1 = 0;
tp->snd_wl2 = tp->write_seq;
tp->snd_una = tp->write_seq;
tp->rcv_nxt = 0;
sk->err = 0;
sk->done = 0;
tp->tcp_header_len = sizeof(struct tcphdr) +
(sysctl_tcp_timestamps ? TCPOLEN_TSTAMP_ALIGNED : 0);
if (tp->user_mss)
tp->mss_clamp = tp->user_mss;
tcp_sync_mss(sk, mtu);
if (tp->mss_cache + tp->tcp_header_len - sizeof(struct tcphdr) < tp->mss_clamp )
tp->mss_clamp = tp->mss_cache + tp->tcp_header_len - sizeof(struct tcphdr);
TCP_SKB_CB(buff)->flags = TCPCB_FLAG_SYN;
TCP_SKB_CB(buff)->sacked = 0;
TCP_SKB_CB(buff)->urg_ptr = 0;
buff->csum = 0;
TCP_SKB_CB(buff)->seq = tp->write_seq++;
TCP_SKB_CB(buff)->end_seq = tp->write_seq;
tp->snd_nxt = TCP_SKB_CB(buff)->end_seq;
tp->window_clamp = dst->window;
tcp_select_initial_window(sock_rspace(sk)/2,tp->mss_clamp,
&tp->rcv_wnd,
&tp->window_clamp,
sysctl_tcp_window_scaling,
&tp->rcv_wscale);
lock_sock(sk);
tcp_set_state(sk,TCP_SYN_SENT);
sk->prot->hash(sk);
tp->rto = dst->rtt;
tcp_init_xmit_timers(sk);
tp->retransmits = 0;
tp->fackets_out = 0;
tp->retrans_out = 0;
__skb_queue_tail(&sk->write_queue, buff);
TCP_SKB_CB(buff)->when = tcp_time_stamp;
tp->packets_out++;
tcp_transmit_skb(sk, skb_clone(buff, GFP_KERNEL));
tcp_statistics.TcpActiveOpens++;
tcp_reset_xmit_timer(sk, TIME_RETRANS, tp->rto);
release_sock(sk);
}
void tcp_send_delayed_ack(struct tcp_opt *tp, int max_timeout)
{
unsigned long timeout;
timeout = tp->ato;
if (timeout > max_timeout)
timeout = max_timeout;
timeout += jiffies;
if (!tp->delack_timer.prev) {
tp->delack_timer.expires = timeout;
add_timer(&tp->delack_timer);
} else {
if (time_before(timeout, tp->delack_timer.expires))
mod_timer(&tp->delack_timer, timeout);
}
}
void tcp_send_ack(struct sock *sk)
{
if(!sk->zapped) {
struct tcp_opt *tp = &(sk->tp_pinfo.af_tcp);
struct sk_buff *buff;
buff = alloc_skb(MAX_HEADER + sk->prot->max_header, GFP_ATOMIC);
if (buff == NULL) {
if(tcp_in_quickack_mode(tp))
tcp_exit_quickack_mode(tp);
tcp_send_delayed_ack(tp, HZ/2);
return;
}
skb_reserve(buff, MAX_HEADER + sk->prot->max_header);
buff->csum = 0;
TCP_SKB_CB(buff)->flags = TCPCB_FLAG_ACK;
TCP_SKB_CB(buff)->sacked = 0;
TCP_SKB_CB(buff)->urg_ptr = 0;
TCP_SKB_CB(buff)->seq = TCP_SKB_CB(buff)->end_seq = tp->snd_nxt;
TCP_SKB_CB(buff)->when = tcp_time_stamp;
tcp_transmit_skb(sk, buff);
}
}
void tcp_write_wakeup(struct sock *sk)
{
if (!sk->zapped) {
struct tcp_opt *tp = &(sk->tp_pinfo.af_tcp);
struct sk_buff *skb;
if ((1 << sk->state) &
~(TCPF_ESTABLISHED|TCPF_CLOSE_WAIT|TCPF_FIN_WAIT1|
TCPF_LAST_ACK|TCPF_CLOSING))
return;
if (before(tp->snd_nxt, tp->snd_una + tp->snd_wnd) &&
((skb = tp->send_head) != NULL)) {
unsigned long win_size;
win_size = tp->snd_wnd - (tp->snd_nxt - tp->snd_una);
if (win_size < TCP_SKB_CB(skb)->end_seq - TCP_SKB_CB(skb)->seq) {
if (tcp_fragment(sk, skb, win_size))
return;
}
update_send_head(sk);
TCP_SKB_CB(skb)->when = tcp_time_stamp;
tp->snd_nxt = TCP_SKB_CB(skb)->end_seq;
tp->packets_out++;
tcp_transmit_skb(sk, skb_clone(skb, GFP_ATOMIC));
if (!tcp_timer_is_set(sk, TIME_RETRANS))
tcp_reset_xmit_timer(sk, TIME_RETRANS, tp->rto);
} else {
skb = alloc_skb(MAX_HEADER + sk->prot->max_header,
GFP_ATOMIC);
if (skb == NULL)
return;
skb_reserve(skb, MAX_HEADER + sk->prot->max_header);
skb->csum = 0;
TCP_SKB_CB(skb)->flags = TCPCB_FLAG_ACK;
TCP_SKB_CB(skb)->sacked = 0;
TCP_SKB_CB(skb)->urg_ptr = 0;
TCP_SKB_CB(skb)->seq = tp->snd_nxt - 1;
TCP_SKB_CB(skb)->end_seq = TCP_SKB_CB(skb)->seq;
TCP_SKB_CB(skb)->when = tcp_time_stamp;
tcp_transmit_skb(sk, skb);
}
}
}
void tcp_send_probe0(struct sock *sk)
{
struct tcp_opt *tp = &(sk->tp_pinfo.af_tcp);
tcp_write_wakeup(sk);
tp->pending = TIME_PROBE0;
tp->backoff++;
tp->probes_out++;
tcp_reset_xmit_timer (sk, TIME_PROBE0,
min(tp->rto << tp->backoff, 120*HZ));
}