#include <netif/hurdloopif.h>
#include <net/if.h>
#include <net/if_arp.h>
#include <string.h>
#include <errno.h>
#include <lwip-util.h>
static error_t
hurdloopif_device_set_flags (struct netif *netif, uint16_t flags)
{
error_t err = 0;
hurdloopif *loopif;
loopif = netif_get_state (netif);
loopif->flags = flags;
return err;
}
static error_t
hurdloopif_device_update_mtu (struct netif *netif, uint32_t mtu)
{
error_t err = 0;
netif->mtu = mtu;
return err;
}
static error_t
hurdloopif_device_terminate (struct netif *netif)
{
free (netif_get_state (netif)->devname);
free (netif_get_state (netif));
return 0;
}
err_t
hurdloopif_device_init (struct netif *netif)
{
hurdloopif *loopif;
loopif = calloc (1, sizeof (hurdloopif));
if (loopif == NULL)
{
LWIP_DEBUGF (NETIF_DEBUG, ("hurdloopif_init: out of memory\n"));
return ERR_MEM;
}
memcpy (loopif, netif_get_state (netif), sizeof (struct ifcommon));
netif->state = loopif;
loopif->devname = LOOP_DEV_NAME;
loopif->type = ARPHRD_LOOPBACK;
netif->mtu = TCP_MSS + 20 + 20;
hurdloopif_device_set_flags (netif, IFF_UP | IFF_RUNNING | IFF_LOOPBACK);
loopif->open = 0;
loopif->close = 0;
loopif->terminate = hurdloopif_device_terminate;
loopif->update_mtu = hurdloopif_device_update_mtu;
loopif->change_flags = hurdloopif_device_set_flags;
return ERR_OK;
}