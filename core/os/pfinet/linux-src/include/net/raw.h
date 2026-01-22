#ifndef _RAW_H
#define _RAW_H
extern struct proto raw_prot;
extern void 	raw_err(struct sock *, struct sk_buff *);
extern int 	raw_rcv(struct sock *, struct sk_buff *);
#define RAWV4_HTABLE_SIZE	MAX_INET_PROTOS
extern struct sock *raw_v4_htable[RAWV4_HTABLE_SIZE];
extern struct sock *raw_v4_lookup(struct sock *sk, unsigned short num,
unsigned long raddr, unsigned long laddr,
int dif);
#endif