#include "netfs.h"
#include "io_S.h"
#include "fs_S.h"
#include "../libports/notify_S.h"
#include "fsys_S.h"
#include "../libports/interrupt_S.h"
#include "ifsock_S.h"
int
netfs_demuxer (mach_msg_header_t *inp,
mach_msg_header_t *outp)
{
mig_routine_t routine;
if ((routine = netfs_io_server_routine (inp)) ||
(routine = netfs_fs_server_routine (inp)) ||
(routine = ports_notify_server_routine (inp)) ||
(routine = netfs_fsys_server_routine (inp)) ||
(routine = ports_interrupt_server_routine (inp)) ||
(routine = netfs_ifsock_server_routine (inp)))
{
(*routine) (inp, outp);
return TRUE;
}
else
return FALSE;
}