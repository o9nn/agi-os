#ifndef LWIP_HURD_H
#define LWIP_HURD_H
#include <sys/socket.h>
#include <hurd/ports.h>
#include <hurd/trivfs.h>
#include <refcount.h>
extern struct port_bucket *lwip_bucket;
extern struct port_class *socketport_class;
extern struct port_class *addrport_class;
extern struct port_class *shutdown_notify_class;
extern struct port_class *lwip_protid_portclasses[2];
extern struct port_class *lwip_cntl_portclasses[2];
extern int lwip_bootstrap_portclass;
extern mach_port_t fsys_identity;
extern struct trivfs_control *lwipcntl;
enum
{
PORTCLASS_INET,
PORTCLASS_INET6,
};
struct socket
{
int sockno;
mach_port_t identity;
refcount_t refcnt;
};
struct sock_user
{
struct port_info pi;
int isroot;
struct socket *sock;
};
struct sock_addr
{
struct port_info pi;
union
{
struct sockaddr_storage storage;
struct sockaddr sa;
} address;
};
extern uid_t lwip_owner;
extern uid_t lwip_group;
struct socket *sock_alloc (void);
void sock_release (struct socket *);
void clean_addrport (void *);
void clean_socketport (void *);
struct sock_user *make_sock_user (struct socket *, int, int, int);
error_t make_sockaddr_port (int, int, mach_port_t *, mach_msg_type_name_t *);
void init_ifs (void *);
void translator_bind (int portclass, const char *name);
#endif