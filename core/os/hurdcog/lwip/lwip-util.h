#ifndef LWIP_UTIL_H
#define LWIP_UTIL_H
#define LOOP_DEV_NAME "lo"
#include <errno.h>
#include <lwip/netif.h>
void init_ifs (void *arg);
void inquire_device (struct netif *netif, uint32_t * addr, uint32_t * netmask,
uint32_t * peer, uint32_t * broadcast,
uint32_t * gateway, uint32_t * addr6,
uint8_t * addr6_prefix_len);
error_t configure_device (struct netif *netif, uint32_t addr,
uint32_t netmask, uint32_t peer, uint32_t broadcast,
uint32_t gateway, uint32_t * addr6,
uint8_t * addr6_prefix_len);
#endif