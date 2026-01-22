#include <stdlib.h>
#include <string.h>
#include <hurd.h>
#include <argp.h>
#include <argz.h>
#include <error.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <hurd/ioctl_types.h>
#include "pfinet.h"
#include <linux/netdevice.h>
#include <linux/inetdevice.h>
#include <linux/ip.h>
#include <linux/route.h>
#include <linux/rtnetlink.h>
#include <net/route.h>
#include <net/sock.h>
#include <net/ip_fib.h>
#include <net/ip6_fib.h>
#include <net/ip6_route.h>
#include <net/addrconf.h>
extern error_t find_device (char *name, struct device **device);
extern error_t enumerate_devices (error_t (*fun) (struct device *dev));
extern error_t configure_device (struct device *dev, uint32_t addr,
uint32_t netmask, uint32_t peer,
uint32_t broadcast);
extern void inquire_device (struct device *dev, uint32_t *addr,
uint32_t *netmask, uint32_t *peer,
uint32_t *broadcast);
extern struct inet6_dev *ipv6_find_idev (struct device *dev);
extern int inet6_addr_add (int ifindex, struct in6_addr *pfx, int plen);
extern int inet6_addr_del (int ifindex, struct in6_addr *pfx, int plen);
extern error_t add_route (struct device *dev, struct srtentry *r);
extern error_t delete_route (struct device *dev, struct srtentry *r);
#ifdef CONFIG_IPV6
static struct rt6_info * ipv6_get_dflt_router (void);
#endif
static const struct argp_option options[] =
{
{"interface", 'i', "DEVICE", 0, "Network interface to use", 1},
{0,0,0,0,"These apply to a given interface:", 2},
{"address", 'a', "ADDRESS", OPTION_ARG_OPTIONAL, "Set the network address"},
{"netmask", 'm', "MASK", OPTION_ARG_OPTIONAL, "Set the netmask"},
{"peer", 'p', "ADDRESS", OPTION_ARG_OPTIONAL, "Set the peer address"},
{"gateway", 'g', "ADDRESS", OPTION_ARG_OPTIONAL, "Set the default gateway"},
{"ipv4", '4', "NAME", 0, "Put active IPv4 translator on NAME"},
#ifdef CONFIG_IPV6
{"ipv6", '6', "NAME", 0, "Put active IPv6 translator on NAME"},
{"address6", 'A', "ADDR/LEN", OPTION_ARG_OPTIONAL, "Set the global IPv6 address"},
{"gateway6", 'G', "ADDRESS", OPTION_ARG_OPTIONAL, "Set the IPv6 default gateway"},
#endif
{0}
};
static const char doc[] = "Interface-specific options before the first \
interface specification apply to the first following interface; otherwise \
they apply to the previously specified interface.";
struct parse_interface
{
struct device *device;
uint32_t address, netmask, peer, gateway;
#ifdef CONFIG_IPV6
struct inet6_ifaddr address6;
struct in6_addr gateway6;
#endif
};
struct parse_hook
{
struct parse_interface *interfaces;
size_t num_interfaces;
struct parse_interface *curint;
};
static void
parse_interface_copy_device(struct device *src,
struct parse_interface *dst)
{
uint32_t broad;
struct rt_key key = { 0 };
struct inet6_dev *idev = NULL;
struct fib_result res;
inquire_device (src, &dst->address, &dst->netmask,
&dst->peer, &broad);
dst->gateway = INADDR_NONE;
key.oif = src->ifindex;
if (! main_table->tb_lookup (main_table, &key, &res)
&& FIB_RES_GW(res) != INADDR_ANY)
dst->gateway = FIB_RES_GW (res);
#ifdef CONFIG_IPV6
if (pfinet_protid_portclasses[PORTCLASS_INET6] != MACH_PORT_NULL)
idev = ipv6_find_idev(src);
if (idev)
{
struct inet6_ifaddr *ifa = idev->addr_list;
struct rt6_info *rt6i = ipv6_get_dflt_router();
if (rt6i->rt6i_dev == src)
memcpy (&dst->gateway6, &rt6i->rt6i_gateway, sizeof (struct in6_addr));
do
{
if (!IN6_IS_ADDR_LINKLOCAL (&ifa->addr))
{
memcpy (&dst->address6, ifa, sizeof (struct inet6_ifaddr));
break;
}
}
while ((ifa = ifa->if_next));
}
#endif
}
static error_t
parse_hook_add_interface (struct parse_hook *h)
{
struct parse_interface *new =
realloc (h->interfaces,
(h->num_interfaces + 1) * sizeof (struct parse_interface));
if (! new)
return ENOMEM;
h->interfaces = new;
h->num_interfaces++;
h->curint = new + h->num_interfaces - 1;
h->curint->device = 0;
h->curint->address = INADDR_NONE;
h->curint->netmask = INADDR_NONE;
h->curint->peer = INADDR_NONE;
h->curint->gateway = INADDR_NONE;
#ifdef CONFIG_IPV6
memset (&h->curint->address6, 0, sizeof (struct inet6_ifaddr));
memset (&h->curint->gateway6, 0, sizeof (struct in6_addr));
#endif
return 0;
}
#ifdef CONFIG_IPV6
static struct rt6_info *
ipv6_get_dflt_router (void)
{
struct in6_addr daddr = { 0 };
struct fib6_node *fib = fib6_lookup
(&ip6_routing_table, &daddr, NULL);
return fib->leaf;
}
#endif
static error_t
parse_opt (int opt, char *arg, struct argp_state *state)
{
error_t err = 0;
struct parse_hook *h = state->hook;
#define RETURN(_err) \
do { if (opt == ARGP_KEY_SUCCESS) \
{ err = (_err); goto free_hook; } \
else \
return _err; } while (0)
#define PERR(err, fmt, args...) \
do { argp_error (state, fmt , ##args); RETURN (err); } while (0)
#define FAIL(rerr, status, perr, fmt, args...) \
do{ argp_failure (state, status, perr, fmt , ##args); RETURN (rerr); } while(0)
#undef ADDR
#define ADDR(str, type) \
({ unsigned long addr = inet_addr (str); \
if (addr == INADDR_NONE) PERR (EINVAL, "Malformed %s", type); \
addr; })
if (!arg && state->next < state->argc
&& (*state->argv[state->next] != '-'))
{
arg = state->argv[state->next];
state->next ++;
}
switch (opt)
{
struct parse_interface *in, *gw4_in;
#ifdef CONFIG_IPV6
struct parse_interface *gw6_in;
char *ptr;
#endif
case 'i':
err = 0;
if (h->curint->device)
{
for (in = h->interfaces; in < h->interfaces + h->num_interfaces; in++)
if (strcmp (in->device->name, arg) == 0)
{
h->curint = in;
return 0;
}
err = parse_hook_add_interface (h);
}
in = h->curint;
if (! err)
err = find_device (arg, &in->device);
if (err)
FAIL (err, 10, err, "%s", arg);
parse_interface_copy_device (in->device, in);
break;
case 'a':
if (arg)
{
h->curint->address = ADDR (arg, "address");
if (!IN_CLASSA (ntohl (h->curint->address))
&& !IN_CLASSB (ntohl (h->curint->address))
&& !IN_CLASSC (ntohl (h->curint->address)))
{
if (IN_MULTICAST (ntohl (h->curint->address)))
FAIL (EINVAL, 1, 0,
"%s: Cannot set interface address to multicast address",
arg);
else
FAIL (EINVAL, 1, 0,
"%s: Illegal or undefined network address", arg);
}
}
else
{
h->curint->address = ADDR ("0.0.0.0", "address");
h->curint->netmask = ADDR ("255.0.0.0", "netmask");
h->curint->gateway = INADDR_NONE;
}
break;
case 'm':
if (arg)
h->curint->netmask = ADDR (arg, "netmask");
else
h->curint->netmask = INADDR_NONE;
break;
case 'p':
if (arg)
h->curint->peer = ADDR (arg, "peer");
else
h->curint->peer = INADDR_NONE;
break;
case 'g':
if (arg)
{
for (in = h->interfaces; in < h->interfaces + h->num_interfaces;
in++)
in->gateway = INADDR_NONE;
h->curint->gateway = ADDR (arg, "gateway");
}
else
h->curint->gateway = INADDR_NONE;
break;
case '4':
pfinet_bind (PORTCLASS_INET, arg);
pfinet_bootstrap_portclass = PORTCLASS_INET6;
break;
#ifdef CONFIG_IPV6
case '6':
pfinet_bind (PORTCLASS_INET6, arg);
break;
case 'A':
if (arg)
{
if ((ptr = strchr (arg, '/')))
{
h->curint->address6.prefix_len = atoi (ptr + 1);
if (h->curint->address6.prefix_len > 128)
FAIL (EINVAL, 1, 0, "%s: The prefix-length is invalid", arg);
*ptr = 0;
}
else
{
h->curint->address6.prefix_len = 64;
fprintf (stderr, "No prefix-length given, "
"defaulting to %s/64.\n", arg);
}
if (inet_pton (AF_INET6, arg, &h->curint->address6.addr) <= 0)
PERR (EINVAL, "Malformed address");
if (IN6_IS_ADDR_MULTICAST (&h->curint->address6.addr))
FAIL (EINVAL, 1, 0, "%s: Cannot set interface address to "
"multicast address", arg);
}
else
memset (&h->curint->address6, 0, sizeof (struct inet6_ifaddr));
break;
case 'G':
if (arg)
{
if (inet_pton (AF_INET6, arg, &h->curint->gateway6) <= 0)
PERR (EINVAL, "Malformed gateway");
if (IN6_IS_ADDR_MULTICAST (&h->curint->gateway6))
FAIL (EINVAL, 1, 0, "%s: Cannot set gateway to "
"multicast address", arg);
}
else
memset (&h->curint->gateway6, 0, sizeof (struct in6_addr));
break;
#endif
case ARGP_KEY_INIT:
h = malloc (sizeof (struct parse_hook));
if (! h)
FAIL (ENOMEM, 11, ENOMEM, "option parsing");
h->interfaces = 0;
h->num_interfaces = 0;
err = parse_hook_add_interface (h);
if (err)
FAIL (err, 12, err, "option parsing");
state->hook = h;
break;
case ARGP_KEY_SUCCESS:
in = h->curint;
if (! in->device)
if (in->address != INADDR_NONE || in->netmask != INADDR_NONE
|| in->gateway != INADDR_NONE)
{
err = find_device (0, &in->device);
if (err)
FAIL (err, 13, 0, "No default interface");
}
#if 0
for (in = h->interfaces; in < h->interfaces + h->num_interfaces; in++)
if (in->netmask != INADDR_NONE
&& in->address == INADDR_NONE && in->device->pa_addr == 0)
FAIL (EDESTADDRREQ, 14, 0, "Cannot set netmask");
#endif
#ifdef CONFIG_IPV6
gw6_in = NULL;
#endif
gw4_in = NULL;
for (in = h->interfaces; in < h->interfaces + h->num_interfaces; in++)
{
if (! ( (h->curint->address & h->curint->netmask)
== (h->curint->gateway & h->curint->netmask)))
h->curint->gateway = INADDR_NONE;
if (in->gateway != INADDR_NONE)
gw4_in = in;
#ifdef CONFIG_IPV6
if (!IN6_IS_ADDR_UNSPECIFIED (&in->gateway6))
{
if (gw6_in != NULL)
FAIL (err, 15, 0, "Cannot have multiple IPv6 "
"default gateways");
gw6_in = in;
}
#endif
}
pthread_mutex_lock (&global_lock);
for (in = h->interfaces; in < h->interfaces + h->num_interfaces; in++)
{
#ifdef CONFIG_IPV6
struct inet6_dev *idev = NULL;
if (pfinet_protid_portclasses[PORTCLASS_INET6] != MACH_PORT_NULL
&& in->device)
idev = ipv6_find_idev(in->device);
#endif
if (in->address == INADDR_NONE && in->netmask == INADDR_NONE)
{
h->curint->address = ADDR ("0.0.0.0", "address");
h->curint->netmask = ADDR ("255.0.0.0", "netmask");
}
if (in->device)
err = configure_device (in->device, in->address, in->netmask,
in->peer, INADDR_NONE);
if (err)
{
pthread_mutex_unlock (&global_lock);
FAIL (err, 16, 0, "cannot configure interface");
}
#ifdef CONFIG_IPV6
if (!idev)
continue;
struct inet6_ifaddr *ifa = idev->addr_list;
while (ifa)
{
struct inet6_ifaddr *c_ifa = ifa;
ifa = ifa->if_next;
if (!IN6_IS_ADDR_UNSPECIFIED (&in->address6.addr)
&& IN6_ARE_ADDR_EQUAL (&c_ifa->addr, &in->address6.addr))
memset (&in->address6, 0, sizeof (struct inet6_ifaddr));
else if (!IN6_IS_ADDR_LINKLOCAL (&c_ifa->addr)
&& !IN6_IS_ADDR_SITELOCAL (&c_ifa->addr))
inet6_addr_del (in->device->ifindex, &c_ifa->addr,
c_ifa->prefix_len);
}
if (!IN6_IS_ADDR_UNSPECIFIED (&in->address6.addr))
{
inet6_addr_add (in->device->ifindex, &in->address6.addr,
in->address6.prefix_len);
}
#endif
}
{
struct srtentry route = {0};
route.rt_flags = RTF_GATEWAY;
route.rt_mask = INADDR_ANY;
route.rt_dest = INADDR_ANY;
route.rt_gateway = h->curint->gateway;
if (gw4_in)
{
err = add_route (gw4_in->device, &route);
if (err)
{
pthread_mutex_unlock (&global_lock);
FAIL (err, 17, 0, "cannot set default gateway");
}
}
}
#ifdef CONFIG_IPV6
if (pfinet_protid_portclasses[PORTCLASS_INET6] != MACH_PORT_NULL)
{
struct rt6_info *rt6i = ipv6_get_dflt_router ();
if (!gw6_in || rt6i->rt6i_dev != gw6_in->device
|| !IN6_ARE_ADDR_EQUAL (&rt6i->rt6i_gateway, &gw6_in->gateway6))
{
for (in = h->interfaces; in < h->interfaces
+ h->num_interfaces; in++)
if (rt6i->rt6i_dev == in->device || gw6_in )
rt6_purge_dflt_routers (0);
if (gw6_in)
rt6_add_dflt_router (&gw6_in->gateway6, gw6_in->device);
}
}
#endif
for (in = h->interfaces; in < h->interfaces + h->num_interfaces; in++)
{
if (!in->device)
continue;
struct srtentry route = {0};
route.rt_flags = 0;
route.rt_dest = INADDR_ANY;
err = add_route (in->device, &route);
if (err)
{
pthread_mutex_unlock (&global_lock);
FAIL (err, 17, 0, "cannot add route");
}
}
pthread_mutex_unlock (&global_lock);
case ARGP_KEY_ERROR:
free_hook:
free (h->interfaces);
free (h);
break;
default:
return ARGP_ERR_UNKNOWN;
}
return err;
}
struct argp
pfinet_argp = { options, parse_opt, 0, doc };
struct argp *trivfs_runtime_argp = &pfinet_argp;
error_t
trivfs_append_args (struct trivfs_control *fsys, char **argz, size_t *argz_len)
{
error_t add_dev_opts (struct device *dev)
{
error_t err = 0;
uint32_t addr, mask, peer, broad;
struct rt_key key = { 0 };
struct fib_result res;
inquire_device (dev, &addr, &mask, &peer, &broad);
#define ADD_OPT(fmt, args...) \
do { char buf[100]; \
if (! err) { \
snprintf (buf, sizeof buf, fmt , ##args); \
err = argz_add (argz, argz_len, buf); } } while (0)
#define ADD_ADDR_OPT(name, addr) \
do { struct in_addr i; \
i.s_addr = (addr); \
ADD_OPT ("--%s=%s", name, inet_ntoa (i)); } while (0)
ADD_OPT ("--interface=%s", dev->name);
if (addr != INADDR_NONE)
ADD_ADDR_OPT ("address", addr);
if (mask != INADDR_NONE)
ADD_ADDR_OPT ("netmask", mask);
if (peer != addr)
ADD_ADDR_OPT ("peer", peer);
key.oif = dev->ifindex;
if (! main_table->tb_lookup (main_table, &key, &res)
&& FIB_RES_GW(res) != INADDR_ANY)
ADD_ADDR_OPT ("gateway", FIB_RES_GW (res));
#undef ADD_ADDR_OPT
#ifdef CONFIG_IPV6
struct inet6_dev *idev = NULL;
if (pfinet_protid_portclasses[PORTCLASS_INET6] != MACH_PORT_NULL)
idev = ipv6_find_idev(dev);
if (idev)
{
struct inet6_ifaddr *ifa;
static char addr_buf[INET6_ADDRSTRLEN];
for (ifa = idev->addr_list; ifa; ifa = ifa->if_next)
{
inet_ntop (AF_INET6, &ifa->addr, addr_buf, INET6_ADDRSTRLEN);
ADD_OPT ("--address6=%s/%d", addr_buf, ifa->prefix_len);
}
struct rt6_info *rt6i = ipv6_get_dflt_router ();
if(rt6i->rt6i_dev == dev)
{
inet_ntop (AF_INET6, &rt6i->rt6i_gateway, addr_buf,
INET6_ADDRSTRLEN);
ADD_OPT ("--gateway6=%s", addr_buf);
}
}
#endif
#undef ADD_OPT
return err;
}
return enumerate_devices (add_dev_opts);
}