#ifndef _HURD_IOCTL_TYPES_H
#define _HURD_IOCTL_TYPES_H
#include <termios.h>
typedef tcflag_t modes_t[4];
typedef speed_t speeds_t[2];
typedef cc_t ccs_t[NCCS];
#include <sys/ioctl.h>
typedef struct winsize winsize_t;
#include <net/if.h>
typedef struct sockaddr sockaddr_t;
typedef char ifname_t[16];
#include <stdint.h>
struct srtentry {
uint32_t rt_dest;
uint32_t rt_mask;
uint32_t rt_gateway;
int rt_flags;
int rt_metric;
int rt_mtu;
int rt_window;
int rt_irtt;
int rt_tos;
int rt_class;
};
typedef struct srtentry srtentry_t;
#endif