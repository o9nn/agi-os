#include <version.h>
#include <error.h>
#include <stdbool.h>
#include <argp.h>
#include <hurd/trivfs.h>
#include <hurd/ports.h>
#include <hurd/rtc.h>
#include <sys/io.h>
#include "rtc_pioctl_S.h"
const char *argp_program_version = STANDARD_HURD_VERSION (rtc);
static struct trivfs_control *rtccntl;
int trivfs_fstype = FSTYPE_DEV;
int trivfs_fsid = 0;
int trivfs_support_read = 0;
int trivfs_support_write = 0;
int trivfs_support_exec = 0;
int trivfs_allow_open = O_READ | O_WRITE;
static const struct argp rtc_argp =
{ NULL, NULL, NULL, "Real-Time Clock device" };
static int
demuxer (mach_msg_header_t *inp, mach_msg_header_t *outp)
{
mig_routine_t routine;
if ((routine = rtc_pioctl_server_routine (inp)) ||
(routine = NULL, trivfs_demuxer (inp, outp)))
{
if (routine)
(*routine) (inp, outp);
return TRUE;
}
else
return FALSE;
}
int
main (int argc, char **argv)
{
error_t err;
mach_port_t bootstrap;
argp_parse (&rtc_argp, argc, argv, 0, 0, 0);
task_get_bootstrap_port (mach_task_self (), &bootstrap);
if (bootstrap == MACH_PORT_NULL)
error (1, 0, "Must be started as a translator");
err = ioperm (0x70, 2, true);
if (err)
error (1, err, "Request IO permission failed");
err = trivfs_startup (bootstrap, O_NORW, NULL, NULL, NULL, NULL, &rtccntl);
mach_port_deallocate (mach_task_self (), bootstrap);
if (err)
error (1, err, "trivfs_startup failed");
ports_manage_port_operations_one_thread (rtccntl->pi.bucket, demuxer,
2 * 60 * 1000);
return 0;
}
void
trivfs_modify_stat (struct trivfs_protid *cred, struct stat *st)
{
}
error_t
trivfs_goaway (struct trivfs_control *fsys, int flags)
{
exit (EXIT_SUCCESS);
}