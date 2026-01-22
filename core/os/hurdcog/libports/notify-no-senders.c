#include "ports.h"
#include "notify_S.h"
kern_return_t
ports_do_mach_notify_no_senders (struct port_info *pi,
mach_port_mscount_t count)
{
error_t err;
mach_port_status_t stat;
if (!pi)
return EOPNOTSUPP;
err = mach_port_get_receive_status (mach_task_self (),
pi->port_right, &stat);
if (err)
return err;
if (stat.mps_srights)
return EAGAIN;
ports_no_senders (pi, stat.mps_mscount);
return 0;
}