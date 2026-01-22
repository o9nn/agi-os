#include "priv.h"
#include "io_S.h"
#include "fs_S.h"
#include "../libports/notify_S.h"
#include "fsys_S.h"
#include "../libports/interrupt_S.h"
#include "ifsock_S.h"
#include "startup_notify_S.h"
#include "exec_startup_S.h"
int
diskfs_demuxer (mach_msg_header_t *inp,
mach_msg_header_t *outp)
{
mig_routine_t routine;
if ((routine = diskfs_io_server_routine (inp)) ||
(routine = diskfs_fs_server_routine (inp)) ||
(routine = ports_notify_server_routine (inp)) ||
(routine = diskfs_fsys_server_routine (inp)) ||
(routine = ports_interrupt_server_routine (inp)) ||
(diskfs_shortcut_ifsock ?
(routine = diskfs_ifsock_server_routine (inp)) : 0) ||
(routine = diskfs_startup_notify_server_routine (inp)) ||
(routine = diskfs_exec_startup_server_routine (inp)))
{
(*routine) (inp, outp);
return TRUE;
}
else
return FALSE;
}