#include <lwip_pfinet_S.h>
#include <string.h>
#include <sys/ioctl.h>
#include <net/if.h>
#include <lwip/netif.h>
#include <lwip/sockets.h>
#include <lwip/inet.h>
#include <sys/mman.h>
#include <lwip-util.h>
#include <netif/hurdethif.h>
static void
dev_ifconf (struct ifconf *ifc)
{
struct netif *netif;
struct ifreq *ifr;
struct sockaddr_in *saddr;
int len;
ifr = ifc->ifc_req;
len = ifc->ifc_len;
saddr = (struct sockaddr_in *) &ifr->ifr_addr;
NETIF_FOREACH(netif)
{
if (ifc->ifc_req != 0)
{
if (len < (int) sizeof (struct ifreq))
break;
memset (ifr, 0, sizeof (struct ifreq));
strncpy (ifr->ifr_name, netif_get_state (netif)->devname,
sizeof (ifr->ifr_name)-1);
saddr->sin_len = sizeof (struct sockaddr_in);
saddr->sin_family = AF_INET;
saddr->sin_addr.s_addr = netif_ip4_addr (netif)->addr;
len -= sizeof (struct ifreq);
}
ifr++;
}
ifc->ifc_len = (uintptr_t) ifr - (uintptr_t) ifc->ifc_req;
}
error_t
lwip_S_pfinet_siocgifconf (io_t port,
vm_size_t amount,
char **ifr, mach_msg_type_number_t * len)
{
struct ifconf ifc;
if (amount == (vm_size_t) - 1)
{
ifc.ifc_buf = 0;
ifc.ifc_len = 0;
dev_ifconf (&ifc);
amount = ifc.ifc_len;
}
else
ifc.ifc_len = amount;
if (amount > 0)
{
if (*len < amount)
ifc.ifc_buf = (char *) mmap (0, amount, PROT_READ | PROT_WRITE,
MAP_ANON, 0, 0);
else
ifc.ifc_buf = *ifr;
dev_ifconf (&ifc);
}
*len = ifc.ifc_len;
*ifr = ifc.ifc_buf;
return 0;
}
error_t
lwip_S_pfinet_getroutes (io_t port,
vm_size_t amount,
data_t *routes,
mach_msg_type_number_t *len,
boolean_t *dealloc_data)
{
return EOPNOTSUPP;
}