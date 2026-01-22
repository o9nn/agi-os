#ifndef LWIP_IFCOMMON_H
#define LWIP_IFCOMMON_H
#include <stdint.h>
#include <sys/types.h>
#include <device/device.h>
#include <errno.h>
#include <lwip/netif.h>
struct ifcommon
{
uint16_t type;
device_t ether_port;
struct port_info *readpt;
mach_port_t readptname;
char *devname;
uint16_t flags;
err_t (*init) (struct netif * netif);
error_t (*terminate) (struct netif * netif);
error_t (*open) (struct netif * netif);
error_t (*close) (struct netif * netif);
error_t (*update_mtu) (struct netif * netif, uint32_t mtu);
error_t (*change_flags) (struct netif * netif, uint16_t flags);
};
err_t if_init (struct netif *netif);
error_t if_terminate (struct netif *netif);
error_t if_change_flags (struct netif *netif, uint16_t flags);
#define netif_get_state(netif) ((struct ifcommon *)netif->state)
#endif