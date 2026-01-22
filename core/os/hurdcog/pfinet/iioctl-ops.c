#include "pfinet.h"
#include <linux/netdevice.h>
#include <linux/notifier.h>
#include <linux/inetdevice.h>
#include <linux/ip.h>
#include <linux/route.h>
#include <linux/rtnetlink.h>
#include "iioctl_S.h"
#include <netinet/in.h>
#include <arpa/inet.h>
#include <fcntl.h>
#include <string.h>
#include <unistd.h>
#include <mach/notify.h>
#include <sys/mman.h>
#include <hurd/fshelp.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <net/if.h>
#include <net/sock.h>
#include <hurd/ioctl_types.h>
#include <net/route.h>
#include <net/ip_fib.h>
#include <net/addrconf.h>
extern struct notifier_block *netdev_chain;
extern error_t configure_device (struct device *dev, uint32_t addr,
uint32_t netmask, uint32_t peer,
uint32_t broadcast);
extern void inquire_device (struct device *dev, uint32_t *addr,
uint32_t *netmask, uint32_t *peer,
uint32_t *broadcast);
struct device *get_dev (const char *name)
{
char ifname[IFNAMSIZ];
struct device *dev;
memcpy (ifname, name, IFNAMSIZ-1);
ifname[IFNAMSIZ-1] = 0;
pthread_mutex_lock (&global_lock);
for (dev = dev_base; dev; dev = dev->next)
if (strcmp (dev->name, ifname) == 0)
break;
return dev;
}
struct rt_req
{
struct nlmsghdr nlh;
struct rtmsg rtm;
};
static error_t
prepare_rt_req(struct rt_req *req, struct device *dev, in_addr_t dst, in_addr_t mask, in_addr_t gw)
{
if (bad_mask (mask, dst))
return EINVAL;
if (!dev->name)
return ENODEV;
memset (req, 0, sizeof *req);
req->nlh.nlmsg_pid = 0;
req->nlh.nlmsg_seq = 0;
req->nlh.nlmsg_len = NLMSG_LENGTH (sizeof req->rtm);
req->rtm.rtm_scope = RT_SCOPE_UNIVERSE;
req->rtm.rtm_type = RTN_UNICAST;
req->rtm.rtm_protocol = RTPROT_BOOT;
req->rtm.rtm_dst_len = inet_mask_len(mask);
req->rtm.rtm_table = RT_TABLE_MAIN;
return 0;
}
static error_t
delete_gateway(struct device *dev, in_addr_t dst, in_addr_t mask, in_addr_t gw)
{
error_t err;
struct kern_rta rta;
struct rt_req req;
struct fib_table *tb;
err = prepare_rt_req(&req, dev, dst, mask, gw);
if (err)
return err;
req.nlh.nlmsg_type = RTM_DELROUTE;
req.nlh.nlmsg_flags = 0;
req.rtm.rtm_scope = RT_SCOPE_UNIVERSE;
memset (&rta, 0, sizeof rta);
rta.rta_oif = &dev->ifindex;
rta.rta_dst = &dst;
rta.rta_gw = &gw;
tb = fib_get_table (req.rtm.rtm_table);
if (tb)
{
err = - (*tb->tb_delete)
(tb, &req.rtm, &rta, &req.nlh, 0);
if (err && err != ESRCH)
return err;
err = 0;
}
return err;
}
static error_t
add_gateway(struct device *dev, in_addr_t dst, in_addr_t mask, in_addr_t gw)
{
error_t err;
struct kern_rta rta;
struct rt_req req;
struct fib_table *tb;
err = prepare_rt_req(&req, dev, dst, mask, gw);
if (err)
return err;
req.nlh.nlmsg_type = RTM_NEWROUTE;
req.nlh.nlmsg_flags = NLM_F_REQUEST | NLM_F_CREATE;
req.rtm.rtm_scope = RT_SCOPE_UNIVERSE;
memset (&rta, 0, sizeof rta);
rta.rta_oif = &dev->ifindex;
rta.rta_dst = &dst;
rta.rta_gw = &gw;
tb = fib_new_table (req.rtm.rtm_table);
err = (!tb ? ENOBUFS
: - (*tb->tb_insert) (tb, &req.rtm, &rta, &req.nlh, 0));
return err;
}
static error_t
add_static_route(struct device *dev, in_addr_t dst, in_addr_t mask)
{
error_t err;
struct kern_rta rta;
struct rt_req req;
struct fib_table *tb;
err = prepare_rt_req(&req, dev, dst, mask, INADDR_ANY);
if (err)
return err;
req.nlh.nlmsg_type = RTM_NEWROUTE;
req.nlh.nlmsg_flags = NLM_F_REQUEST | NLM_F_CREATE | NLM_F_APPEND;
req.rtm.rtm_scope = RT_SCOPE_LINK;
memset (&rta, 0, sizeof rta);
rta.rta_dst = &dst;
rta.rta_oif = &dev->ifindex;
tb = fib_new_table (req.rtm.rtm_table);
if (tb)
err = tb->tb_insert (tb, &req.rtm, &rta, &req.nlh, NULL);
else
err = ENOBUFS;
return err;
}
static error_t
delete_static_route(struct device *dev, in_addr_t dst, in_addr_t mask)
{
error_t err;
struct kern_rta rta;
struct rt_req req;
struct fib_table *tb;
err = prepare_rt_req(&req, dev, dst, mask, INADDR_ANY);
if (err)
return err;
req.nlh.nlmsg_type = RTM_DELROUTE;
req.nlh.nlmsg_flags = 0;
req.rtm.rtm_scope = RT_SCOPE_LINK;
memset (&rta, 0, sizeof rta);
rta.rta_dst = &dst;
rta.rta_oif = &dev->ifindex;
tb = fib_get_table (req.rtm.rtm_table);
if (tb)
{
err = - (*tb->tb_delete)
(tb, &req.rtm, &rta, &req.nlh, 0);
if (err && err != ESRCH)
return err;
err = 0;
}
return err;
}
error_t
add_route (struct device *dev, const struct srtentry *r)
{
error_t err;
if (!r)
return EINVAL;
if (r->rt_flags & RTF_GATEWAY)
err = add_gateway(dev, r->rt_dest, r->rt_mask, r->rt_gateway);
else
err = add_static_route(dev, r->rt_dest, r->rt_mask);
return err;
}
error_t
delete_route (struct device *dev, const struct srtentry *r)
{
error_t err;
if (!r)
return EINVAL;
if (r->rt_flags & RTF_GATEWAY)
err = delete_gateway(dev, r->rt_dest, r->rt_mask, r->rt_gateway);
else
err = delete_static_route(dev, r->rt_dest, r->rt_mask);
return err;
}
enum siocgif_type
{
ADDR,
NETMASK,
DSTADDR,
BRDADDR
};
#define SIOCGIF(name, type) \
kern_return_t \
S_iioctl_siocgif##name (struct sock_user *user, \
ifname_t ifnam, \
sockaddr_t *addr) \
{ \
return siocgifXaddr (user, ifnam, addr, type); \
}
static kern_return_t
siocgifXaddr (struct sock_user *user,
ifname_t ifnam,
sockaddr_t *addr,
enum siocgif_type type)
{
error_t err = 0;
struct device *dev;
struct sockaddr_in *sin = (struct sockaddr_in *) addr;
uint32_t addrs[4];
if (!user)
return EOPNOTSUPP;
dev = get_dev (ifnam);
if (!dev)
err = ENODEV;
else if (user->sock->sk->family != AF_INET)
err = EINVAL;
else
{
sin->sin_family = AF_INET;
inquire_device (dev, &addrs[0], &addrs[1], &addrs[2], &addrs[3]);
sin->sin_addr.s_addr = addrs[type];
}
pthread_mutex_unlock (&global_lock);
return err;
}
#define SIOCSIF(name, type) \
kern_return_t \
S_iioctl_siocsif##name (struct sock_user *user, \
const ifname_t ifnam, \
sockaddr_t addr) \
{ \
return siocsifXaddr (user, ifnam, &addr, type); \
}
static kern_return_t
siocsifXaddr (struct sock_user *user,
const ifname_t ifnam,
sockaddr_t *addr,
enum siocgif_type type)
{
error_t err = 0;
struct device *dev;
struct sockaddr_in *sin = (struct sockaddr_in *) addr;
uint32_t addrs[4];
if (!user)
return EOPNOTSUPP;
dev = get_dev (ifnam);
if (!user->isroot)
err = EPERM;
else if (!dev)
err = ENODEV;
else if (sin->sin_family != AF_INET)
err = EINVAL;
else if (user->sock->sk->family != AF_INET)
err = EINVAL;
else
{
inquire_device (dev, &addrs[0], &addrs[1], &addrs[2], &addrs[3]);
addrs[type] = sin->sin_addr.s_addr;
err = configure_device (dev, addrs[0], addrs[1], addrs[2], addrs[3]);
}
pthread_mutex_unlock (&global_lock);
return err;
}
kern_return_t
S_rioctl_siocaddrt (struct sock_user *user,
const ifname_t ifnam,
const struct srtentry route)
{
error_t err = 0;
struct device *dev;
if (!user)
return EOPNOTSUPP;
dev = get_dev (ifnam);
if (!dev)
err = ENODEV;
else if (user->sock->sk->family != AF_INET)
err = EINVAL;
else
err = add_route (dev, &route);
pthread_mutex_unlock (&global_lock);
return err;
}
kern_return_t
S_rioctl_siocdelrt (struct sock_user *user,
const ifname_t ifnam,
const struct srtentry route)
{
error_t err = 0;
struct device *dev;
if (!user)
return EOPNOTSUPP;
dev = get_dev (ifnam);
if (!dev)
err = ENODEV;
else if (user->sock->sk->family != AF_INET)
err = EINVAL;
else
err = delete_route (dev, &route);
pthread_mutex_unlock (&global_lock);
return err;
}
SIOCSIF (addr, ADDR);
SIOCSIF (dstaddr, DSTADDR);
kern_return_t
S_iioctl_siocsifflags (struct sock_user *user,
const ifname_t ifnam,
short flags)
{
error_t err = 0;
struct device *dev;
if (!user)
return EOPNOTSUPP;
dev = get_dev (ifnam);
if (!user->isroot)
err = EPERM;
else if (!dev)
err = ENODEV;
else
err = dev_change_flags (dev, flags);
pthread_mutex_unlock (&global_lock);
return err;
}
kern_return_t
S_iioctl_siocgifflags (struct sock_user *user,
ifname_t name,
short *flags)
{
error_t err = 0;
struct device *dev;
dev = get_dev (name);
if (!dev)
err = ENODEV;
else
{
*flags = dev->flags;
}
pthread_mutex_unlock (&global_lock);
return err;
}
SIOCSIF (brdaddr, BRDADDR);
SIOCSIF (netmask, NETMASK);
kern_return_t
S_iioctl_siocgifmetric (struct sock_user *user,
ifname_t ifnam,
int *metric)
{
error_t err = 0;
struct device *dev;
dev = get_dev (ifnam);
if (!dev)
err = ENODEV;
else
{
*metric = 0;
}
pthread_mutex_unlock (&global_lock);
return err;
}
kern_return_t
S_iioctl_siocsifmetric (struct sock_user *user,
const ifname_t ifnam,
int metric)
{
return EOPNOTSUPP;
}
kern_return_t
S_iioctl_siocdifaddr (struct sock_user *user,
const ifname_t ifnam,
sockaddr_t addr)
{
return EOPNOTSUPP;
}
SIOCGIF (addr, ADDR);
SIOCGIF (dstaddr, DSTADDR);
SIOCGIF (brdaddr, BRDADDR);
SIOCGIF (netmask, NETMASK);
kern_return_t
S_iioctl_siocgifhwaddr (struct sock_user *user,
ifname_t ifname,
sockaddr_t *addr)
{
error_t err = 0;
struct device *dev;
if (!user)
return EOPNOTSUPP;
dev = get_dev (ifname);
if (!dev)
err = ENODEV;
else
{
memcpy (addr->sa_data, dev->dev_addr, dev->addr_len);
addr->sa_family = dev->type;
}
pthread_mutex_unlock (&global_lock);
return err;
}
kern_return_t
S_iioctl_siocgifmtu (struct sock_user *user,
ifname_t ifnam,
int *mtu)
{
error_t err = 0;
struct device *dev;
dev = get_dev (ifnam);
if (!dev)
err = ENODEV;
else
{
*mtu = dev->mtu;
}
pthread_mutex_unlock (&global_lock);
return err;
}
kern_return_t
S_iioctl_siocsifmtu (struct sock_user *user,
const ifname_t ifnam,
int mtu)
{
error_t err = 0;
struct device *dev;
if (!user)
return EOPNOTSUPP;
dev = get_dev (ifnam);
if (!user->isroot)
err = EPERM;
if (!dev)
err = ENODEV;
else if (mtu <= 0)
err = EINVAL;
else
{
if (dev->change_mtu)
dev->change_mtu (dev, mtu);
else
dev->mtu = mtu;
notifier_call_chain (&netdev_chain, NETDEV_CHANGEMTU, dev);
}
pthread_mutex_unlock (&global_lock);
return err;
}
kern_return_t
S_iioctl_siocgifindex (struct sock_user *user,
ifname_t ifnam,
int *index)
{
error_t err = 0;
struct device *dev;
dev = get_dev (ifnam);
if (!dev)
err = ENODEV;
else
{
*index = dev->ifindex;
}
pthread_mutex_unlock (&global_lock);
return err;
}
kern_return_t
S_iioctl_siocgifname (struct sock_user *user,
ifname_t ifnam,
int *index)
{
error_t err = 0;
struct device *dev;
pthread_mutex_lock (&global_lock);
dev = dev_get_by_index (*index);
if (!dev)
err = ENODEV;
else
{
strncpy (ifnam, dev->name, IFNAMSIZ);
ifnam[IFNAMSIZ-1] = '\0';
}
pthread_mutex_unlock (&global_lock);
return err;
}