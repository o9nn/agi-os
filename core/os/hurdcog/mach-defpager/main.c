#include <mach.h>
#include <hurd.h>
#include <pthread.h>
#include <device/device.h>
#include <device/device_types.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdarg.h>
#include <error.h>
#include <signal.h>
#include <string.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <paths.h>
#include <errno.h>
#include <unistd.h>
#include <hurd.h>
#include <hurd/port.h>
#include <hurd/fd.h>
#include <hurd/paths.h>
#include <hurd/startup.h>
#include <assert-backtrace.h>
#include "default_pager.h"
const char *defpager_server_name = "mach-defpager";
mach_port_t	bootstrap_master_device_port;
mach_port_t	bootstrap_master_host_port;
static void
printf_init (device_t master)
{
mach_port_t cons;
kern_return_t rc;
rc = device_open (master, D_READ|D_WRITE, "console", &cons);
if (rc)
error (2, rc, "cannot open kernel console device");
stdin = mach_open_devstream (cons, "r");
stdout = stderr = mach_open_devstream (cons, "w");
mach_port_deallocate (mach_task_self (), cons);
setlinebuf (stderr);
}
int debug;
static void
nohandler (int sig)
{ }
int
main (int argc, char **argv)
{
const task_t my_task = mach_task_self();
error_t err;
memory_object_t defpager;
err = get_privileged_ports (&bootstrap_master_host_port,
&bootstrap_master_device_port);
if (err)
error (1, err, "cannot get privileged ports");
defpager = MACH_PORT_NULL;
err = vm_set_default_memory_manager (bootstrap_master_host_port, &defpager);
if (err)
error (1, err, "cannot check current default memory manager");
if (MACH_PORT_VALID (defpager))
error (2, 0, "Another default memory manager is already running");
if (!(argc == 2 && !strcmp (argv[1], "-d")))
{
sigset_t set;
signal (SIGUSR1, nohandler);
signal (SIGCHLD, nohandler);
sigemptyset (&set);
sigaddset (&set, SIGUSR1);
sigaddset (&set, SIGCHLD);
sigprocmask (SIG_SETMASK, &set, NULL);
switch (fork ())
{
case -1:
error (1, errno, "cannot become daemon");
case 0:
setsid ();
chdir ("/");
close (0);
close (1);
close (2);
break;
default:
sigemptyset (&set);
sigsuspend (&set);
_exit (0);
}
}
mach_port_t proc = getproc ();
if (proc == MACH_PORT_NULL)
error (3, err, "cannot get a handle to our process");
err = proc_mark_important (proc);
if (err && err != EPERM && err != EMIG_BAD_ID)
error (3, err, "cannot mark us as important");
mach_port_deallocate (mach_task_self (), proc);
mach_port_t startup;
startup = file_name_lookup (_SERVERS_STARTUP, 0, 0);
if (startup == MACH_PORT_NULL)
error (0, errno, "WARNING: Cannot register as essential task\n");
startup_essential_task (startup, mach_task_self (), MACH_PORT_NULL,
program_invocation_short_name,
bootstrap_master_host_port);
mach_port_deallocate (mach_task_self (), startup);
printf_init(bootstrap_master_device_port);
partition_init();
(void) mach_port_insert_right(my_task, default_pager_exception_port,
default_pager_exception_port,
MACH_MSG_TYPE_MAKE_SEND);
if (!debug)
(void) task_set_exception_port(my_task, default_pager_exception_port);
default_pager_initialize (bootstrap_master_host_port);
if (!(argc == 2 && !strcmp (argv[1], "-d")))
kill (getppid (), SIGUSR1);
default_pager();
return -1;
}
void
panic (const char *fmt, ...)
{
va_list ap;
fprintf (stderr, "%s: panic: ", program_invocation_name);
va_start (ap, fmt);
vfprintf (stderr, fmt, ap);
va_end (ap);
assert_backtrace (0);
}