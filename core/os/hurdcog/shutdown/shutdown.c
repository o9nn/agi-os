#include <argp.h>
#include <assert-backtrace.h>
#include <errno.h>
#include <error.h>
#include <stdlib.h>
#include <string.h>
#include <hurd.h>
#include <hurd/fs.h>
#include <hurd/ports.h>
#include <hurd/trivfs.h>
#include <hurd/paths.h>
#include <device/device.h>
#include <sys/file.h>
#include <version.h>
#include "shutdown_S.h"
#include "acpi_U.h"
#define SLEEP_STATE_S5	5
struct port_bucket *port_bucket;
int trivfs_fstype = FSTYPE_MISC;
int trivfs_fsid = 0;
int trivfs_support_read = 0;
int trivfs_support_write = 0;
int trivfs_support_exec = 0;
int trivfs_allow_open = O_READ | O_WRITE;
struct port_class *trivfs_protid_class;
struct port_class *trivfs_control_class;
static mach_port_t
get_acpi(void)
{
kern_return_t err;
mach_port_t tryacpi, device_master;
err = get_privileged_ports (0, &device_master);
if (!err)
{
err = device_open (device_master, D_READ | D_WRITE, "acpi", &tryacpi);
mach_port_deallocate (mach_task_self (), device_master);
if (!err)
return tryacpi;
}
tryacpi = file_name_lookup (_SERVERS_ACPI, O_RDWR, 0);
return tryacpi;
}
kern_return_t
S_shutdown_shutdown(trivfs_protid_t server)
{
kern_return_t err;
mach_port_t acpi;
acpi = get_acpi();
if (acpi == MACH_PORT_NULL)
return EIO;
err = acpi_sleep(acpi, SLEEP_STATE_S5);
return err;
}
static int
shutdown_demuxer (mach_msg_header_t *inp,
mach_msg_header_t *outp)
{
mig_routine_t routine;
if ((routine = shutdown_server_routine (inp)) ||
(routine = NULL, trivfs_demuxer (inp, outp)))
{
if (routine)
(*routine) (inp, outp);
return TRUE;
}
else
return FALSE;
}
void
trivfs_modify_stat (struct trivfs_protid *cred, struct stat *st)
{
}
error_t
trivfs_goaway (struct trivfs_control *fsys, int flags)
{
int count;
ports_inhibit_class_rpcs (trivfs_control_class);
ports_inhibit_class_rpcs (trivfs_protid_class);
count = ports_count_class (trivfs_protid_class);
if (count > 0 && !(flags & FSYS_GOAWAY_FORCE))
{
ports_enable_class (trivfs_protid_class);
ports_resume_class_rpcs (trivfs_control_class);
ports_resume_class_rpcs (trivfs_protid_class);
return EBUSY;
}
exit (0);
}
int
main (int argc, char *argv[])
{
error_t err;
mach_port_t bootstrap;
struct trivfs_control *fsys;
task_get_bootstrap_port (mach_task_self (), &bootstrap);
if (bootstrap == MACH_PORT_NULL)
error (1, 0, "must be started as a translator");
err = trivfs_add_port_bucket (&port_bucket);
if (err)
error (1, 0, "error creating port bucket");
err = trivfs_add_control_port_class (&trivfs_control_class);
if (err)
error (1, 0, "error creating control port class");
err = trivfs_add_protid_port_class (&trivfs_protid_class);
if (err)
error (1, 0, "error creating protid port class");
err = trivfs_startup (bootstrap, 0,
trivfs_control_class, port_bucket,
trivfs_protid_class, port_bucket,
&fsys);
mach_port_deallocate (mach_task_self (), bootstrap);
if (err)
error (3, err, "Contacting parent");
do
ports_manage_port_operations_multithread (port_bucket, shutdown_demuxer,
2 * 60 * 1000,
10 * 60 * 1000,
0);
while (1);
return 0;
}