#ifndef _LINUX_INET_H
#define _LINUX_INET_H
#ifdef __KERNEL__
extern void		inet_proto_init(struct net_proto *pro);
extern char		*in_ntoa(unsigned long in);
extern unsigned long	in_aton(const char *str);
#endif
#endif