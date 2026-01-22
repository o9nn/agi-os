#ifndef BPF_IMPL_H
#define BPF_IMPL_H
#include <device/bpf.h>
#include "queue.h"
typedef struct
{
queue_head_t if_rcv_port_list;
queue_head_t if_snd_port_list;
}if_filter_list_t;
typedef	unsigned short	filter_t;
typedef filter_t	*filter_array_t;
#define	NET_MAX_FILTER		128
#define NET_HASH_SIZE   256
#define N_NET_HASH      4
#define N_NET_HASH_KEYS 4
#ifndef BPF_ALIGN
#define EXTRACT_SHORT(p)	((u_short)ntohs(*(u_short *)p))
#define EXTRACT_LONG(p)		(ntohl(*(u_int *)p))
#else
#define EXTRACT_SHORT(p)\
((u_short)\
((u_short)*((u_char *)p+0)<<8|\
(u_short)*((u_char *)p+1)<<0))
#define EXTRACT_LONG(p)\
((u_int)*((u_char *)p+0)<<24|\
(u_int)*((u_char *)p+1)<<16|\
(u_int)*((u_char *)p+2)<<8|\
(u_int)*((u_char *)p+3)<<0)
#endif
#define HASH_ITERATE(head, elt) (elt) = (net_hash_entry_t) (head); do {
#define HASH_ITERATE_END(head, elt) \
(elt) = (net_hash_entry_t) queue_next((queue_entry_t) (elt));	   \
} while ((elt) != (head));
#define FILTER_ITERATE(if_port_list, fp, nextfp, chain)	\
for ((fp) = (net_rcv_port_t) queue_first(if_port_list);	\
!queue_end(if_port_list, (queue_entry_t)(fp));	\
(fp) = (nextfp)) {					\
(nextfp) = (net_rcv_port_t) queue_next(chain);
#define FILTER_ITERATE_END }
#define ENQUEUE_DEAD(dead, entry_p, chain) {			\
queue_next(&(entry_p)->chain) = (queue_entry_t) (dead);	\
(dead) = (queue_entry_t)(entry_p);			\
}
#define CSPF_BYTES(n) ((n) * sizeof (filter_t))
struct net_rcv_port {
queue_chain_t	input;
queue_chain_t	output;
mach_port_t	rcv_port;
int		rcv_count;
int		priority;
filter_t	*filter_end;
filter_t	filter[NET_MAX_FILTER];
};
typedef struct net_rcv_port *net_rcv_port_t;
struct net_hash_entry {
queue_chain_t   chain;
#define he_next chain.next
#define he_prev chain.prev
mach_port_t      rcv_port;
unsigned int	keys[N_NET_HASH_KEYS];
};
typedef struct net_hash_entry *net_hash_entry_t;
struct net_hash_header {
struct net_rcv_port rcv;
int n_keys;
int ref_count;
net_hash_entry_t table[NET_HASH_SIZE];
};
typedef struct net_hash_header *net_hash_header_t;
int bpf_do_filter(net_rcv_port_t infp, char *p,	unsigned int wirelen,
char *header, unsigned int hlen, net_hash_entry_t **hash_headpp,
net_hash_entry_t *entpp);
io_return_t net_set_filter(if_filter_list_t *ifp, mach_port_t rcv_port,
int priority, filter_t *filter, unsigned int filter_count);
int bpf_validate(bpf_insn_t f, int bytes, bpf_insn_t *match);
int bpf_eq (bpf_insn_t f1, bpf_insn_t f2, int bytes);
unsigned int bpf_hash (int n, unsigned int *keys);
int bpf_match (net_hash_header_t hash, int n_keys, unsigned int *keys,
net_hash_entry_t **hash_headpp, net_hash_entry_t *entpp);
int hash_ent_remove (if_filter_list_t *ifp, net_hash_header_t hp, int used,
net_hash_entry_t *head, net_hash_entry_t entp, queue_entry_t *dead_p);
void net_free_dead_infp (queue_entry_t dead_infp);
void net_free_dead_entp (queue_entry_t dead_entp);
void remove_dead_filter (if_filter_list_t *ifp,
queue_head_t *if_port_list, mach_port_t dead_port);
void destroy_filters (if_filter_list_t *ifp);
#endif