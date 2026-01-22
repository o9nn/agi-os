#ifndef _RAW_H
#define _RAW_H
extern struct proto raw_prot;
extern void	raw_err(int type, int code, unsigned char *header, __u32 daddr,
__u32 saddr, struct inet_protocol *protocol);
extern int	raw_recvfrom(struct sock *sk, unsigned char *to,
int len, int noblock, unsigned flags,
struct sockaddr_in *sin, int *addr_len);
extern int	raw_read(struct sock *sk, unsigned char *buff,
int len, int noblock, unsigned flags);
extern int 	raw_rcv(struct sock *, struct sk_buff *, struct device *,
__u32, __u32);
#define RAWV4_HTABLE_SIZE	MAX_INET_PROTOS
extern struct sock *raw_v4_htable[RAWV4_HTABLE_SIZE];
extern struct sock *raw_v4_lookup(struct sock *sk, unsigned short num,
unsigned long raddr, unsigned long laddr);
#endif