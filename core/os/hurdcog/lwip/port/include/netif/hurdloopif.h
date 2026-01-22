#ifndef LWIP_HURDLOOPIF_H
#define LWIP_HURDLOOPIF_H
#include <hurd/ports.h>
#include <lwip/netif.h>
#include <netif/ifcommon.h>
typedef struct ifcommon hurdloopif;
err_t hurdloopif_device_init (struct netif *netif);
#endif