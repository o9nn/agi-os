#include <options.h>
#include <stdlib.h>
#include <argp.h>
#include <argz.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <net/if_arp.h>
#include <error.h>
#include <lwip/netif.h>
#include <lwip/tcpip.h>
#include <lwip-hurd.h>
#include <lwip-util.h>
#include <netif/ifcommon.h>
static error_t
parse_hook_add_interface (struct parse_hook *h)
{
int i;
struct parse_interface *new = realloc (h->interfaces,
(h->num_interfaces +
1) *
sizeof (struct parse_interface));
if (!new)
return ENOMEM;
h->interfaces = new;
h->num_interfaces++;
h->curint = new + h->num_interfaces - 1;
memset (&h->curint->dev_name, 0, DEV_NAME_LEN);
h->curint->address.addr = INADDR_NONE;
h->curint->netmask.addr = INADDR_NONE;
h->curint->peer.addr = INADDR_NONE;
h->curint->gateway.addr = INADDR_NONE;
for (i = 0; i < LWIP_IPV6_NUM_ADDRESSES; i++)
ip6_addr_set_zero ((ip6_addr_t *) & h->curint->addr6[i]);
return 0;
}
static error_t
parse_opt (int opt, char *arg, struct argp_state *state)
{
error_t err = 0;
struct parse_hook *h = state->hook;
int i;
#define RETURN(_err)                          \
do { return _err; } while (0)
#define PERR(err, fmt, args...)               \
do { argp_error (state, fmt , ##args); RETURN (err); } while (0)
#define FAIL(rerr, status, perr, fmt, args...)  \
do{ argp_failure (state, status, perr, fmt , ##args); RETURN (rerr); } while(0)
#undef	ADDR
#define ADDR(str, type)                         \
({ unsigned long addr = inet_addr (str);      \
if (addr == INADDR_NONE) PERR (EINVAL, "Malformed %s", type);  \
addr; })
if (!arg && state->next < state->argc && (*state->argv[state->next] != '-'))
{
arg = state->argv[state->next];
state->next++;
}
switch (opt)
{
struct parse_interface *in;
uint8_t addr6_prefix_len;
ip6_addr_t *address6;
char *ptr;
case 'i':
err = 0;
for (in = h->interfaces; in < h->interfaces + h->num_interfaces; in++)
if (strcmp (in->dev_name, arg) == 0)
{
h->curint = in;
return 0;
}
if (h->curint->dev_name[0])
{
err = parse_hook_add_interface (h);
}
in = h->curint;
strncpy (in->dev_name, arg, sizeof(in->dev_name)-1);
break;
case 'a':
if (arg)
{
h->curint->address.addr = ADDR (arg, "address");
if (!IN_CLASSA (ntohl (h->curint->address.addr))
&& !IN_CLASSB (ntohl (h->curint->address.addr))
&& !IN_CLASSC (ntohl (h->curint->address.addr)))
{
if (IN_MULTICAST (ntohl (h->curint->address.addr)))
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
h->curint->address.addr = ADDR ("0.0.0.0", "address");
h->curint->netmask.addr = ADDR ("255.0.0.0", "netmask");
h->curint->gateway.addr = INADDR_NONE;
}
break;
case 'm':
if (arg)
h->curint->netmask.addr = ADDR (arg, "netmask");
else
h->curint->netmask.addr = INADDR_NONE;
break;
case 'p':
if (arg)
h->curint->peer.addr = ADDR (arg, "peer");
else
h->curint->peer.addr = INADDR_NONE;
break;
case 'g':
if (arg)
{
h->curint->gateway.addr = ADDR (arg, "gateway");
}
else
h->curint->gateway.addr = INADDR_NONE;
break;
case '4':
translator_bind (PORTCLASS_INET, arg);
lwip_bootstrap_portclass = PORTCLASS_INET6;
break;
case '6':
translator_bind (PORTCLASS_INET6, arg);
break;
case 'A':
if (arg)
{
if ((ptr = strchr (arg, '/')))
{
addr6_prefix_len = atoi (ptr + 1);
if (addr6_prefix_len > 128)
FAIL (EINVAL, 1, 0, "%s: The prefix-length is invalid", arg);
*ptr = 0;
if (addr6_prefix_len != 64)
{
error (0, 0,
"The only supported value for the prefix-length"
" is /64. Defaulting to %s/64.\n", arg);
}
}
else
{
error (0, 0, "No prefix-length given, "
"defaulting to %s/64.\n", arg);
}
for (i = 0; i < LWIP_IPV6_NUM_ADDRESSES; i++)
{
address6 = (ip6_addr_t *) & h->curint->addr6[i];
if (!ip6_addr_isany (address6))
continue;
if (ip6addr_aton (arg, address6) <= 0)
PERR (EINVAL, "Malformed address");
break;
}
}
break;
case ARGP_KEY_INIT:
h = malloc (sizeof (struct parse_hook));
if (!h)
FAIL (ENOMEM, 11, ENOMEM, "option parsing");
h->interfaces = 0;
h->num_interfaces = 0;
err = parse_hook_add_interface (h);
if (err)
FAIL (err, 12, err, "option parsing");
state->hook = h;
break;
case ARGP_KEY_SUCCESS:
if (netif_list == 0)
{
tcpip_init (init_ifs, h);
}
else
{
tcpip_callback (init_ifs, h);
}
break;
case ARGP_KEY_ERROR:
free (h->interfaces);
free (h);
break;
default:
return ARGP_ERR_UNKNOWN;
}
return err;
}
error_t
trivfs_append_args (struct trivfs_control * fsys, char **argz,
size_t * argz_len)
{
error_t err = 0;
struct netif *netif;
int i;
uint32_t addr, netmask, gateway;
uint32_t addr6[LWIP_IPV6_NUM_ADDRESSES][4];
uint8_t addr6_prefix_len[LWIP_IPV6_NUM_ADDRESSES];
#define ADD_OPT(fmt, args...)           \
do { char buf[100];                   \
if (! err) {                     \
snprintf (buf, sizeof buf, fmt , ##args);      \
err = argz_add (argz, argz_len, buf); } } while (0)
#define ADD_ADDR_OPT(name, addr)        \
do { struct in_addr i;                \
i.s_addr = (addr);               \
ADD_OPT ("--%s=%s", name, inet_ntoa (i)); } while (0)
NETIF_FOREACH(netif)
{
if (netif_get_state (netif)->type == ARPHRD_LOOPBACK)
{
continue;
}
inquire_device (netif, &addr, &netmask, 0, 0, &gateway,
(uint32_t *) addr6, addr6_prefix_len);
ADD_OPT ("--interface=%s", netif_get_state (netif)->devname);
if (addr != INADDR_NONE)
ADD_ADDR_OPT ("address", addr);
if (netmask != INADDR_NONE)
ADD_ADDR_OPT ("netmask", netmask);
if (gateway != INADDR_NONE)
ADD_ADDR_OPT ("gateway", gateway);
for (i = 0; i < LWIP_IPV6_NUM_ADDRESSES; i++)
if (!ip6_addr_isany (((ip6_addr_t *) & addr6[i])))
ADD_OPT ("--address6=%s/%d",
ip6addr_ntoa (((ip6_addr_t *) & addr6[i])),
addr6_prefix_len[i]);
}
#undef ADD_ADDR_OPT
#undef ADD_OPT
return err;
}
struct argp lwip_argp = { options, parse_opt, 0, doc };
struct argp *trivfs_runtime_argp = &lwip_argp;