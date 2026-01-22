#include <netif/ifcommon.h>
#include <net/if.h>
#include <errno.h>
#include <lwip/tcpip.h>
static error_t
if_open (struct netif *netif)
{
error_t err = 0;
struct ifcommon *ifc = netif_get_state (netif);
if (ifc->open)
err = ifc->open (netif);
if (!err)
{
ifc->flags |= IFF_UP | IFF_RUNNING;
netif_set_up (netif);
}
return err;
}
static error_t
if_close (struct netif *netif)
{
error_t err = 0;
struct ifcommon *ifc = netif_get_state (netif);
if (ifc->close)
err = ifc->close (netif);
if (!err)
{
ifc->flags &= ~(IFF_UP | IFF_RUNNING);
netif_set_down (netif);
}
return err;
}
err_t
if_init (struct netif *netif)
{
struct ifcommon *ifc = netif_get_state (netif);
if (ifc == NULL)
return -1;
return ifc->init (netif);
}
error_t
if_terminate (struct netif * netif)
{
error_t err;
struct ifcommon *ifc = netif_get_state (netif);
if (ifc == NULL)
return -1;
err = if_close (netif);
if (err)
return err;
return ifc->terminate (netif);
}
struct if_change_flags_args
{
struct netif *netif;
uint16_t flags;
error_t err;
};
static void
_if_change_flags (void *arg)
{
error_t err;
struct ifcommon *ifc;
uint16_t oldflags;
struct if_change_flags_args *args = arg;
ifc = netif_get_state (args->netif);
if (ifc == NULL)
{
errno = EINVAL;
return;
}
oldflags = ifc->flags;
err = ifc->change_flags (args->netif, args->flags);
if (!err && ((oldflags ^ args->flags) & IFF_UP))
err = ((oldflags & IFF_UP) ? if_close : if_open) (args->netif);
args->err = err;
return;
}
error_t
if_change_flags (struct netif * netif, uint16_t flags)
{
error_t err;
struct if_change_flags_args *args =
calloc (1, sizeof (struct if_change_flags_args));
args->netif = netif;
args->flags = flags;
err = tcpip_callback_wait(_if_change_flags, args);
if(!err)
err = args->err;
free (args);
return err;
}