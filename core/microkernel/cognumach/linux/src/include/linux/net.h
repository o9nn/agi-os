#ifndef _LINUX_NET_H
#define _LINUX_NET_H
#include <linux/wait.h>
#include <linux/socket.h>
#define NPROTO		16
#define SYS_SOCKET	1
#define SYS_BIND	2
#define SYS_CONNECT	3
#define SYS_LISTEN	4
#define SYS_ACCEPT	5
#define SYS_GETSOCKNAME	6
#define SYS_GETPEERNAME	7
#define SYS_SOCKETPAIR	8
#define SYS_SEND	9
#define SYS_RECV	10
#define SYS_SENDTO	11
#define SYS_RECVFROM	12
#define SYS_SHUTDOWN	13
#define SYS_SETSOCKOPT	14
#define SYS_GETSOCKOPT	15
#define SYS_SENDMSG	16
#define SYS_RECVMSG	17
typedef enum {
SS_FREE = 0,
SS_UNCONNECTED,
SS_CONNECTING,
SS_CONNECTED,
SS_DISCONNECTING
} socket_state;
#define SO_ACCEPTCON	(1<<16)
#define SO_WAITDATA	(1<<17)
#define SO_NOSPACE	(1<<18)
#ifdef __KERNEL__
struct socket {
short			type;
socket_state		state;
long			flags;
struct proto_ops	*ops;
void			*data;
struct socket		*conn;
struct socket		*iconn;
struct socket		*next;
struct wait_queue	**wait;
struct inode		*inode;
struct fasync_struct  *fasync_list;
struct file		*file;
};
#define SOCK_INODE(S)	((S)->inode)
struct proto_ops {
int	family;
int	(*create)	(struct socket *sock, int protocol);
int	(*dup)		(struct socket *newsock, struct socket *oldsock);
int	(*release)	(struct socket *sock, struct socket *peer);
int	(*bind)		(struct socket *sock, struct sockaddr *umyaddr,
int sockaddr_len);
int	(*connect)	(struct socket *sock, struct sockaddr *uservaddr,
int sockaddr_len, int flags);
int	(*socketpair)	(struct socket *sock1, struct socket *sock2);
int	(*accept)	(struct socket *sock, struct socket *newsock,
int flags);
int	(*getname)	(struct socket *sock, struct sockaddr *uaddr,
int *usockaddr_len, int peer);
int	(*select)	(struct socket *sock, int sel_type,
select_table *wait);
int	(*ioctl)	(struct socket *sock, unsigned int cmd,
unsigned long arg);
int	(*listen)	(struct socket *sock, int len);
int	(*shutdown)	(struct socket *sock, int flags);
int	(*setsockopt)	(struct socket *sock, int level, int optname,
char *optval, int optlen);
int	(*getsockopt)	(struct socket *sock, int level, int optname,
char *optval, int *optlen);
int	(*fcntl)	(struct socket *sock, unsigned int cmd,
unsigned long arg);
int   (*sendmsg)	(struct socket *sock, struct msghdr *m, int total_len, int nonblock, int flags);
int   (*recvmsg)	(struct socket *sock, struct msghdr *m, int total_len, int nonblock, int flags, int *addr_len);
};
struct net_proto {
const char *name;
void (*init_func)(struct net_proto *);
};
extern int	sock_wake_async(struct socket *sock, int how);
extern int	sock_register(int family, struct proto_ops *ops);
extern int	sock_unregister(int family);
extern struct socket *sock_alloc(void);
extern void	sock_release(struct socket *sock);
#endif
#endif