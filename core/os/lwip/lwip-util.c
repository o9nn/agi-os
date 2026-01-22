#include <lwip-util.h>
#include <error.h>
#include <net/if_arp.h>
#include <lwip/sockets.h>
#include <lwip/inet.h>
#include <lwip/tcpip.h>
#include <lwip-hurd.h>
#include <options.h>
#include <netif/hurdethif.h>
#include <netif/hurdtunif.h>
#include <netif/hurdloopif.h>
static error_t
create_netif_state (char *name, struct ifcommon *ifc)
{
char *base_name;
memset (ifc, 0, sizeof (struct ifcommon));
base_name = strrchr (name, '/');
if (base_name)
base_name++;
else
base_name = name;
if (strncmp (base_name, "tun", 3) == 0)
ifc->init = hurdtunif_device_init;
else
ifc->init = hurdethif_device_init;
ifc->devname = strndup (name, strlen (name));
return errno;
}
static int
ipv4config_is_valid (uint32_t addr, uint32_t netmask,
uint32_t gateway, uint32_t broadcast)
{
if (netmask != INADDR_NONE && !ip4_addr_netmask_valid (netmask))
{
error (0, 0, "Error: Invalid network mask.\n");
return 0;
}
if (gateway != INADDR_NONE && (gateway & netmask) != (addr & netmask))
{
error (0, 0,
"Error: the gateway is not in the same network as the address.\n");
return 0;
}
if (broadcast != INADDR_NONE
&& netmask != INADDR_NONE && broadcast != (addr | ~netmask))
{
error (0, 0,
"Error: the broadcast address doesn't match the network mask.\n");
return 0;
}
return 1;
}
static void
init_loopback (void)
{
struct ifcommon ifc;
memset (&ifc, 0, sizeof (struct ifcommon));
ifc.init = hurdloopif_device_init;
netif_list->state = &ifc;
if_init (netif_list);
}
void
remove_ifs (void)
{
struct netif *netif;
netif = netif_list;
while (netif != 0)
{
if (netif_get_state (netif)->type == ARPHRD_LOOPBACK)
{
netif = netif->next;
continue;
}
if_terminate (netif);
netif_remove (netif);
free (netif);
netif = netif_list;
}
return;
}
void
init_ifs (void *arg)
{
struct parse_interface *in;
struct parse_hook *ifs;
struct netif *netif;
struct ifcommon ifc;
int8_t ipv6_addr_idx;
ip6_addr_t *address6;
int i;
if (netif_list == 0)
netif_list = calloc (1, sizeof (struct netif));
if (netif_list->next == 0)
init_loopback ();
else
remove_ifs ();
ifs = (struct parse_hook *) arg;
for (in = ifs->interfaces + ifs->num_interfaces - 1;
in >= ifs->interfaces; in--)
{
if (!in->dev_name[0])
continue;
if (!ipv4config_is_valid (in->address.addr, in->netmask.addr,
in->gateway.addr, INADDR_NONE))
continue;
netif = calloc (1, sizeof (struct netif));
create_netif_state (in->dev_name, &ifc);
if (!netif_add (netif, &in->address, &in->netmask, &in->gateway, &ifc,
if_init, tcpip_input))
{
if (netif->state != &ifc)
mem_free (netif->state);
free (netif);
continue;
}
netif->ip6_autoconfig_enabled = 1;
netif_create_ip6_linklocal_address (netif, 1);
for (i = 0; i < LWIP_IPV6_NUM_ADDRESSES; i++)
{
address6 = (ip6_addr_t *) & in->addr6[i];
if (!ip6_addr_isany (address6) && !ip6_addr_ismulticast (address6))
{
netif_add_ip6_address (netif, address6, &ipv6_addr_idx);
if (ipv6_addr_idx >= 0)
netif_ip6_addr_set_state (netif, ipv6_addr_idx,
IP6_ADDR_TENTATIVE);
else
error (0, 0, "No free slot for IPv6 address: %s\n",
ip6addr_ntoa (address6));
}
}
netif_set_up (netif);
if (in->gateway.addr != INADDR_NONE)
{
netif_set_default (netif);
}
}
free (ifs->interfaces);
free (ifs);
return;
}
struct update_if_args
{
struct netif *netif;
uint32_t addr;
uint32_t netmask;
uint32_t peer;
uint32_t broadcast;
uint32_t gateway;
uint32_t *addr6;
uint8_t *addr6_prefix_len;
};
static void
update_if (void *arg)
{
int i;
struct update_if_args *args = arg;
netif_set_addr (args->netif, (ip4_addr_t *) & args->addr,
(ip4_addr_t *) & args->netmask,
(ip4_addr_t *) & args->gateway);
if (args->addr6)
for (i = 0; i < LWIP_IPV6_NUM_ADDRESSES; i++)
{
ip6_addr_t *laddr6 = ((ip6_addr_t *) args->addr6 + i);
if (!ip6_addr_isany (laddr6))
{
netif_ip6_addr_set (args->netif, i, laddr6);
if (!ip6_addr_islinklocal (laddr6))
netif_ip6_addr_set_state (args->netif, i, IP6_ADDR_TENTATIVE);
}
}
if (args->addr6_prefix_len)
for (i = 0; i < LWIP_IPV6_NUM_ADDRESSES; i++)
*(args->addr6_prefix_len + i) = 64;
free (args);
return;
}
void
inquire_device (struct netif *netif, uint32_t * addr, uint32_t * netmask,
uint32_t * peer, uint32_t * broadcast, uint32_t * gateway,
uint32_t * addr6, uint8_t * addr6_prefix_len)
{
int i;
if (netif)
{
if (addr)
*addr = netif_ip4_addr (netif)->addr;
if (netmask)
*netmask = netif_ip4_netmask (netif)->addr;
if (peer)
*peer = INADDR_NONE;
if (broadcast)
*broadcast =
netif_ip4_addr (netif)->addr | ~netif_ip4_netmask (netif)->addr;
if (gateway)
*gateway = netif_ip4_gw (netif)->addr;
if (addr6)
for (i = 0; i < LWIP_IPV6_NUM_ADDRESSES; i++)
{
*(addr6 + i * 4 + 0) = netif_ip6_addr (netif, i)->addr[0];
*(addr6 + i * 4 + 1) = netif_ip6_addr (netif, i)->addr[1];
*(addr6 + i * 4 + 2) = netif_ip6_addr (netif, i)->addr[2];
*(addr6 + i * 4 + 3) = netif_ip6_addr (netif, i)->addr[3];
}
if (addr6_prefix_len)
for (i = 0; i < LWIP_IPV6_NUM_ADDRESSES; i++)
*(addr6_prefix_len + i) = 64;
}
}
error_t
configure_device (struct netif *netif, uint32_t addr, uint32_t netmask,
uint32_t peer, uint32_t broadcast, uint32_t gateway,
uint32_t * addr6, uint8_t * addr6_prefix_len)
{
error_t err = 0;
if (addr == INADDR_ANY || addr == INADDR_NONE)
{
addr = INADDR_NONE;
netmask = INADDR_NONE;
peer = INADDR_NONE;
broadcast = INADDR_NONE;
gateway = INADDR_NONE;
}
if (netmask != INADDR_NONE)
if ((netif->flags & NETIF_FLAG_BROADCAST)
&& ip4_addr_netmask_valid (netmask) && netmask <= 0xfffffffc)
broadcast = (addr | ~netmask);
if (!ipv4config_is_valid (addr, netmask, gateway, broadcast))
err = EINVAL;
else
{
struct update_if_args *arg = calloc (1, sizeof (struct update_if_args));
arg->netif = netif;
arg->addr = addr;
arg->netmask = netmask;
arg->peer = peer;
arg->broadcast = broadcast;
arg->gateway = gateway;
arg->addr6 = addr6;
arg->addr6_prefix_len = addr6_prefix_len;
err = err_to_errno(tcpip_callback (update_if, arg));
}
return err;
}