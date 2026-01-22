#ifndef __XEN_PUBLIC_SCHED_H__
#define __XEN_PUBLIC_SCHED_H__
#include "event_channel.h"
#define SCHEDOP_yield       0
#define SCHEDOP_block       1
#define SCHEDOP_shutdown    2
struct sched_shutdown {
unsigned int reason;
};
typedef struct sched_shutdown sched_shutdown_t;
DEFINE_XEN_GUEST_HANDLE(sched_shutdown_t);
#define SCHEDOP_poll        3
struct sched_poll {
XEN_GUEST_HANDLE(evtchn_port_t) ports;
unsigned int nr_ports;
uint64_t timeout;
};
typedef struct sched_poll sched_poll_t;
DEFINE_XEN_GUEST_HANDLE(sched_poll_t);
#define SCHEDOP_remote_shutdown        4
struct sched_remote_shutdown {
domid_t domain_id;
unsigned int reason;
};
typedef struct sched_remote_shutdown sched_remote_shutdown_t;
DEFINE_XEN_GUEST_HANDLE(sched_remote_shutdown_t);
#define SHUTDOWN_poweroff   0
#define SHUTDOWN_reboot     1
#define SHUTDOWN_suspend    2
#define SHUTDOWN_crash      3
#endif