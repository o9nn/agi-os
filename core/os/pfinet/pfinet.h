#ifndef PFINET_H_
#define PFINET_H_
#include <device/device.h>
#include <hurd/ports.h>
#include <hurd/trivfs.h>
#include <sys/mman.h>
#include <sys/socket.h>
#include <pthread.h>
#define _ROUTE_H
#include <net/route.h>
#undef _ROUTE_H
extern pthread_mutex_t global_lock;
extern pthread_mutex_t net_bh_lock;
extern struct port_bucket *pfinet_bucket;
extern struct port_class *addrport_class;
extern struct port_class *socketport_class;
extern mach_port_t fsys_identity;
extern struct device *dev_base;
extern struct device loopback_dev;
struct sock_user
{
struct port_info pi;
int isroot;
struct socket *sock;
};
struct sock_addr
{
struct port_info pi;
struct sockaddr address;
};
extern struct trivfs_control *pfinetctl;
extern uid_t pfinet_owner;
extern uid_t pfinet_group;
void ethernet_initialize (void);
int ethernet_demuxer (mach_msg_header_t *, mach_msg_header_t *);
void setup_ethernet_device (char *, struct device **);
void setup_dummy_device (char *, struct device **);
void setup_tunnel_device (char *, struct device **);
struct sock_user *make_sock_user (struct socket *, int, int, int);
error_t make_sockaddr_port (struct socket *, int,
mach_port_t *, mach_msg_type_name_t *);
void init_devices (void);
void *net_bh_worker (void *);
void init_time (void);
int get_routing_table(int start, int count, ifrtreq_t *routes);
struct sock;
error_t tcp_tiocinq (struct sock *sk, mach_msg_type_number_t *amount);
void clean_addrport (void *);
void clean_socketport (void *);
enum {
PORTCLASS_INET,
PORTCLASS_INET6,
};
extern struct port_class *pfinet_protid_portclasses[2];
extern struct port_class *pfinet_cntl_portclasses[2];
extern int pfinet_bootstrap_portclass;
void pfinet_bind (int portclass, const char *name);
#endif