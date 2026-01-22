#include "priv.h"
#include "trivfs_io_S.h"
#include "trivfs_fs_S.h"
#include "../libports/notify_S.h"
#include "trivfs_fsys_S.h"
#include "../libports/interrupt_S.h"
int
trivfs_demuxer (mach_msg_header_t *inp,
mach_msg_header_t *outp)
{
mig_routine_t routine;
if ((routine = trivfs_io_server_routine (inp)) ||
(routine = trivfs_fs_server_routine (inp)) ||
(routine = ports_notify_server_routine (inp)) ||
(routine = trivfs_fsys_server_routine (inp)) ||
(routine = ports_interrupt_server_routine (inp)))
{
(*routine) (inp, outp);
return TRUE;
}
else
return FALSE;
}