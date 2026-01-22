#include <hurd.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert-backtrace.h>
#include "ps.h"
#include "common.h"
mach_port_t
ps_get_host (void)
{
static mach_port_t host = MACH_PORT_NULL;
if (host == MACH_PORT_NULL)
host = mach_host_self ();
return host;
}
error_t
ps_host_basic_info (host_basic_info_t *info)
{
static int initialized;
static host_basic_info_data_t buf;
if (!initialized)
{
mach_msg_type_number_t size = sizeof (buf);
error_t err = host_info (ps_get_host (), HOST_BASIC_INFO,
(host_info_t) &buf, &size);
if (err)
return err;
initialized = 1;
}
*info = &buf;
return 0;
}
error_t
ps_host_sched_info (host_sched_info_t *info)
{
static int initialized;
static host_sched_info_data_t buf;
if (!initialized)
{
mach_msg_type_number_t size = sizeof (buf);
error_t err = host_info (ps_get_host (), HOST_SCHED_INFO,
(host_info_t) &buf, &size);
if (err)
return err;
initialized = 1;
}
*info = &buf;
return 0;
}
error_t
ps_host_load_info (host_load_info_t *info)
{
static host_load_info_data_t buf;
mach_msg_type_number_t size = sizeof (buf);
error_t err = host_info (ps_get_host (), HOST_LOAD_INFO,
(host_info_t) &buf, &size);
if (err)
return err;
*info = &buf;
return 0;
}