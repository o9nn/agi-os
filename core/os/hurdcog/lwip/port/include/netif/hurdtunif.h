#ifndef LWIP_HURDTUNIF_H
#define LWIP_HURDTUNIF_H
#include <hurd/ports.h>
#include <lwip/netif.h>
#include <netif/ifcommon.h>
struct pbufqueue
{
struct pbuf *head;
struct pbuf **tail;
uint8_t len;
};
struct hurdtunif
{
struct ifcommon comm;
struct trivfs_control *cntl;
file_t underlying;
struct iouser *user;
struct pbufqueue queue;
pthread_mutex_t lock;
pthread_cond_t read;
pthread_cond_t select;
uint8_t read_blocked;
};
extern struct port_class *tunnel_cntlclass;
extern struct port_class *tunnel_class;
err_t hurdtunif_device_init (struct netif *netif);
error_t hurdtunif_module_init (void);
#endif