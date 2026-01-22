#ifndef __LINUX_NETLINK_H
#define __LINUX_NETLINK_H
#define NETLINK_ROUTE 0
#define NETLINK_SKIP 1
#define NETLINK_USERSOCK 2
#define NETLINK_FIREWALL 3
#define NETLINK_ARPD 8
#define NETLINK_ROUTE6 11
#define NETLINK_IP6_FW 13
#define NETLINK_TAPBASE 16
#define MAX_LINKS 32
struct sockaddr_nl
{
sa_family_t nl_family;
unsigned short nl_pad;
__u32 nl_pid;
__u32 nl_groups;
};
struct nlmsghdr
{
__u32 nlmsg_len;
__u16 nlmsg_type;
__u16 nlmsg_flags;
__u32 nlmsg_seq;
__u32 nlmsg_pid;
};
#define NLM_F_REQUEST 1
#define NLM_F_MULTI 2
#define NLM_F_ACK 4
#define NLM_F_ECHO 8
#define NLM_F_ROOT 0x100
#define NLM_F_MATCH 0x200
#define NLM_F_ATOMIC 0x400
#define NLM_F_DUMP (NLM_F_ROOT|NLM_F_MATCH)
#define NLM_F_REPLACE 0x100
#define NLM_F_EXCL 0x200
#define NLM_F_CREATE 0x400
#define NLM_F_APPEND 0x800
#define NLMSG_ALIGNTO 4
#define NLMSG_ALIGN(len) ( ((len)+NLMSG_ALIGNTO-1) & ~(NLMSG_ALIGNTO-1) )
#define NLMSG_LENGTH(len) ((len)+NLMSG_ALIGN(sizeof(struct nlmsghdr)))
#define NLMSG_SPACE(len) NLMSG_ALIGN(NLMSG_LENGTH(len))
#define NLMSG_DATA(nlh) ((void*)(((char*)nlh) + NLMSG_LENGTH(0)))
#define NLMSG_NEXT(nlh,len) ((len) -= NLMSG_ALIGN((nlh)->nlmsg_len), \
(struct nlmsghdr*)(((char*)(nlh)) + NLMSG_ALIGN((nlh)->nlmsg_len)))
#define NLMSG_OK(nlh,len) ((len) > 0 && (nlh)->nlmsg_len >= sizeof(struct nlmsghdr) && \
(nlh)->nlmsg_len <= (len))
#define NLMSG_PAYLOAD(nlh,len) ((nlh)->nlmsg_len - NLMSG_SPACE((len)))
#define NLMSG_NOOP 0x1
#define NLMSG_ERROR 0x2
#define NLMSG_DONE 0x3
#define NLMSG_OVERRUN 0x4
struct nlmsgerr
{
int error;
struct nlmsghdr msg;
};
#define NET_MAJOR 36
#ifdef __KERNEL__
struct netlink_skb_parms
{
struct ucred creds;
__u32 pid;
__u32 groups;
__u32 dst_pid;
__u32 dst_groups;
kernel_cap_t eff_cap;
};
#define NETLINK_CB(skb) (*(struct netlink_skb_parms*)&((skb)->cb))
#define NETLINK_CREDS(skb) (&NETLINK_CB((skb)).creds)
extern int netlink_attach(int unit, int (*function)(int,struct sk_buff *skb));
extern void netlink_detach(int unit);
extern int netlink_post(int unit, struct sk_buff *skb);
extern int init_netlink(void);
extern struct sock *netlink_kernel_create(int unit, void (*input)(struct sock *sk, int len));
extern void netlink_ack(struct sk_buff *in_skb, struct nlmsghdr *nlh, int err);
extern int netlink_unicast(struct sock *ssk, struct sk_buff *skb, __u32 pid, int nonblock);
extern void netlink_broadcast(struct sock *ssk, struct sk_buff *skb, __u32 pid,
__u32 group, int allocation);
extern void netlink_set_err(struct sock *ssk, __u32 pid, __u32 group, int code);
#define NLMSG_GOODSIZE (PAGE_SIZE - ((sizeof(struct sk_buff)+0xF)&~0xF))
struct netlink_callback
{
struct sk_buff *skb;
struct nlmsghdr *nlh;
int (*dump)(struct sk_buff * skb, struct netlink_callback *cb);
int (*done)(struct netlink_callback *cb);
int family;
long args[4];
};
static __inline__ struct nlmsghdr *
__nlmsg_put(struct sk_buff *skb, u32 pid, u32 seq, int type, int len)
{
struct nlmsghdr *nlh;
int size = NLMSG_LENGTH(len);
nlh = (struct nlmsghdr*)skb_put(skb, NLMSG_ALIGN(size));
nlh->nlmsg_type = type;
nlh->nlmsg_len = size;
nlh->nlmsg_flags = 0;
nlh->nlmsg_pid = pid;
nlh->nlmsg_seq = seq;
return nlh;
}
#define NLMSG_PUT(skb, pid, seq, type, len) \
({ if (skb_tailroom(skb) < (int)NLMSG_SPACE(len)) goto nlmsg_failure; \
__nlmsg_put(skb, pid, seq, type, len); })
extern int netlink_dump_start(struct sock *ssk, struct sk_buff *skb,
struct nlmsghdr *nlh,
int (*dump)(struct sk_buff *skb, struct netlink_callback*),
int (*done)(struct netlink_callback*));
extern void netlink_proto_init(struct net_proto *pro);
#endif
#endif