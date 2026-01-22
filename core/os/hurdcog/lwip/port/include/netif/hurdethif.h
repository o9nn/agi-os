#ifndef LWIP_HURDETHIF_H
#define LWIP_HURDETHIF_H
#include <hurd/ports.h>
#include <lwip/netif.h>
#include <netif/ifcommon.h>
typedef struct ifcommon hurdethif;
err_t hurdethif_device_init (struct netif *netif);
error_t hurdethif_module_init (void);
#endif