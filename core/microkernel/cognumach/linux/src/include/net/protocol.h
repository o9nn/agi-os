#ifndef _PROTOCOL_H
#define _PROTOCOL_H
#define MAX_INET_PROTOS	32
struct inet_protocol {
int			(*handler)(struct sk_buff *skb, struct device *dev,
struct options *opt, __u32 daddr,
unsigned short len, __u32 saddr,
int redo, struct inet_protocol *protocol);
void			(*err_handler)(int type, int code, unsigned char *buff,
__u32 daddr,
__u32 saddr,
struct inet_protocol *protocol, int len);
struct inet_protocol *next;
unsigned char		protocol;
unsigned char		copy:1;
void			*data;
const char		*name;
};
extern struct inet_protocol *inet_protocol_base;
extern struct inet_protocol *inet_protos[MAX_INET_PROTOS];
extern void		inet_add_protocol(struct inet_protocol *prot);
extern int		inet_del_protocol(struct inet_protocol *prot);
#endif