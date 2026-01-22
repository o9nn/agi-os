#include "priv.h"
#include <error.h>
#include <device/device.h>
#include <hurd/paths.h>
#include <hurd/startup.h>
#include <argp.h>
#include <argz.h>
#include <version.h>
#include <pids.h>
const char *argp_program_version = STANDARD_HURD_VERSION (exec);
int trivfs_fstype = FSTYPE_MISC;
int trivfs_fsid = 0;
int trivfs_support_read = 0;
int trivfs_support_write = 0;
int trivfs_allow_open = 0;
struct port_class *trivfs_protid_class;
struct port_class *trivfs_control_class;
struct port_bucket *port_bucket;
struct port_class *execboot_portclass;
struct trivfs_control *fsys;
char **save_argv;
mach_port_t opt_device_master;
#include "exec_S.h"
#include "exec_startup_S.h"
static int
exec_demuxer (mach_msg_header_t *inp, mach_msg_header_t *outp)
{
mig_routine_t routine;
if ((routine = exec_server_routine (inp)) ||
(routine = NULL, trivfs_demuxer (inp, outp)) ||
(routine = exec_startup_server_routine (inp)))
{
if (routine)
(*routine) (inp, outp);
return TRUE;
}
else
return FALSE;
}
void
deadboot (void *p)
{
struct bootinfo *boot = p;
size_t i;
munmap (boot->argv, boot->argvlen);
munmap (boot->envp, boot->envplen);
for (i = 0; i < boot->dtablesize; ++i)
mach_port_deallocate (mach_task_self (), boot->dtable[i]);
for (i = 0; i < boot->nports; ++i)
mach_port_deallocate (mach_task_self (), boot->portarray[i]);
munmap (boot->portarray, boot->nports * sizeof (mach_port_t));
munmap (boot->intarray, boot->nints * sizeof (int));
if (ports_count_class (trivfs_control_class) == 0)
{
if (ports_count_class (trivfs_protid_class) == 0)
{
if (ports_count_class (execboot_portclass) == 0)
exit (0);
ports_enable_class (execboot_portclass);
}
ports_enable_class (trivfs_protid_class);
}
ports_enable_class (trivfs_control_class);
}
#define OPT_DEVICE_MASTER_PORT	(-1)
static const struct argp_option options[] =
{
{"device-master-port", OPT_DEVICE_MASTER_PORT, "PORT", 0,
"If specified, a boot-time exec server can print "
"diagnostic messages earlier.", 0},
{0}
};
static error_t
parse_opt (int opt, char *arg, struct argp_state *state)
{
switch (opt)
{
default:
return ARGP_ERR_UNKNOWN;
case ARGP_KEY_INIT:
case ARGP_KEY_SUCCESS:
case ARGP_KEY_ERROR:
break;
case OPT_DEVICE_MASTER_PORT:
opt_device_master = atoi (arg);
break;
}
return 0;
}
error_t
trivfs_append_args (struct trivfs_control *fsys,
char **argz, size_t *argz_len)
{
error_t err = 0;
char *opt;
if (MACH_PORT_VALID (opt_device_master))
{
asprintf (&opt, "--device-master-port=%u", opt_device_master);
if (opt)
{
err = argz_add (argz, argz_len, opt);
free (opt);
}
}
return err;
}
static struct argp argp =
{ options, parse_opt, 0, "Hurd standard exec server." };
struct argp *trivfs_runtime_argp = &argp;
error_t
open_console (mach_port_t device_master)
{
static int got_console = 0;
mach_port_t cons;
error_t err;
if (got_console)
return 0;
err = device_open (device_master, D_READ|D_WRITE, "console", &cons);
if (err)
return err;
stdin = mach_open_devstream (cons, "r");
stdout = stderr = mach_open_devstream (cons, "w");
setlinebuf (stderr);
got_console = 1;
mach_port_deallocate (mach_task_self (), cons);
return 0;
}
int
main (int argc, char **argv)
{
error_t err;
mach_port_t bootstrap;
argp_parse (&argp, argc, argv, 0, 0, 0);
if (MACH_PORT_VALID (opt_device_master))
{
err = open_console (opt_device_master);
assert_perror_backtrace (err);
mach_port_deallocate (mach_task_self (), opt_device_master);
}
save_argv = argv;
task_get_bootstrap_port (mach_task_self (), &bootstrap);
if (bootstrap == MACH_PORT_NULL)
error (2, 0, "Must be started as a translator");
procserver = getproc ();
err = trivfs_add_port_bucket (&port_bucket);
if (err)
error (1, 0, "error creating port bucket");
err = trivfs_add_control_port_class (&trivfs_control_class);
if (err)
error (1, 0, "error creating control port class");
err = trivfs_add_protid_port_class (&trivfs_protid_class);
if (err)
error (1, 0, "error creating protid port class");
execboot_portclass = ports_create_class (deadboot, NULL);
err = trivfs_startup (bootstrap, 0,
trivfs_control_class, port_bucket,
trivfs_protid_class, port_bucket,
&fsys);
mach_port_deallocate (mach_task_self (), bootstrap);
if (err)
error (3, err, "Contacting parent");
ports_manage_port_operations_multithread (port_bucket, exec_demuxer,
2 * 60 * 1000, 0, 0);
return 0;
}
void
trivfs_modify_stat (struct trivfs_protid *cred, struct stat *st)
{
st->st_fstype = FSTYPE_MISC;
}
error_t
trivfs_goaway (struct trivfs_control *fsys, int flags)
{
int count;
ports_inhibit_class_rpcs (trivfs_control_class);
ports_inhibit_class_rpcs (trivfs_protid_class);
count = ports_count_class (trivfs_protid_class);
if (count == 0 || (flags & FSYS_GOAWAY_FORCE))
{
mach_port_deallocate (mach_task_self (), fsys->underlying);
count = ports_count_class (execboot_portclass);
if (count == 0)
exit (0);
ports_enable_class (execboot_portclass);
ports_destroy_right (fsys);
return 0;
}
else
{
ports_enable_class (trivfs_protid_class);
ports_resume_class_rpcs (trivfs_control_class);
ports_resume_class_rpcs (trivfs_protid_class);
return EBUSY;
}
}
kern_return_t
S_exec_init (struct trivfs_protid *protid,
auth_t auth, process_t proc)
{
mach_port_t host_priv, device_master, startup;
error_t err;
if (! protid || ! protid->isroot)
return EPERM;
_hurd_port_set (&_hurd_ports[INIT_PORT_PROC], proc);
_hurd_port_set (&_hurd_ports[INIT_PORT_AUTH], auth);
_hurd_proc_init (save_argv, NULL, 0);
procserver = getproc ();
{
struct iouser *user;
struct trivfs_protid *cred;
mach_port_t right;
err = iohelp_create_empty_iouser (&user);
assert_perror_backtrace (err);
err = trivfs_open (fsys, user, 0, MACH_PORT_NULL, &cred);
assert_perror_backtrace (err);
right = ports_get_send_right (cred);
proc_execdata_notify (procserver, right, MACH_MSG_TYPE_COPY_SEND);
mach_port_deallocate (mach_task_self (), right);
}
err = get_privileged_ports (&host_priv, &device_master);
assert_perror_backtrace (err);
err = open_console (device_master);
assert_perror_backtrace (err);
mach_port_deallocate (mach_task_self (), device_master);
proc_register_version (procserver, host_priv, "exec", "", HURD_VERSION);
startup = file_name_lookup (_SERVERS_STARTUP, 0, 0);
if (startup == MACH_PORT_NULL)
{
error (0, errno, "%s", _SERVERS_STARTUP);
err = proc_getmsgport (procserver, HURD_PID_STARTUP, &startup);
assert_perror_backtrace (err);
}
mach_port_deallocate (mach_task_self (), procserver);
err = startup_essential_task (startup, mach_task_self (), MACH_PORT_NULL,
"exec", host_priv);
assert_perror_backtrace (err);
mach_port_deallocate (mach_task_self (), startup);
mach_port_deallocate (mach_task_self (), host_priv);
return 0;
}