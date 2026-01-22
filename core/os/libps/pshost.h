#ifndef __PSHOST_H__
#define __PSHOST_H__
#include <mach/mach_types.h>
#include <mach/host_info.h>
host_t ps_get_host();
error_t ps_host_basic_info(host_basic_info_t *host_info);
error_t ps_host_sched_info(host_sched_info_t *host_info);
error_t ps_host_load_info(host_load_info_t *host_info);
#endif