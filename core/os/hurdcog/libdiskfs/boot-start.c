#include "priv.h"
#include <stdio.h>
#include <hurd.h>
#include <hurd/fsys.h>
#include <hurd/exec.h>
#include <hurd/startup.h>
#include <hurd/paths.h>
#include <fcntl.h>
#include <device/device.h>
#include <sys/reboot.h>
#include <string.h>
#include <argz.h>
#include <error.h>
#include "exec_S.h"
#include "exec_startup_S.h"
#include "fsys_S.h"
#include "fsys_reply_U.h"
static struct port_info *bootinfo;
static mach_port_t diskfs_exec_ctl;
extern task_t diskfs_exec_server_task;
extern task_t diskfs_kernel_task;
static task_t parent_task = MACH_PORT_NULL;
static pthread_mutex_t execstartlock;
static pthread_cond_t execstarted;
const char *diskfs_boot_init_program = _HURD_STARTUP;
static void start_execserver (void);
char **diskfs_argv = 0;
static mach_port_t
get_console (void)
{
mach_port_t device_master, console;
error_t err = get_privileged_ports (0, &device_master);
if (err)
return MACH_PORT_NULL;
err = device_open (device_master, D_WRITE | D_READ, "console", &console);
mach_port_deallocate (mach_task_self (), device_master);
if (err)
return MACH_PORT_NULL;
return console;
}
void
_diskfs_boot_privports (void)
{
assert_backtrace (diskfs_boot_filesystem ());
if (_hurd_host_priv == MACH_PORT_NULL)
{
mach_port_t bootstrap;
error_t err = task_get_bootstrap_port (mach_task_self (), &bootstrap);
assert_perror_backtrace (err);
err = fsys_getpriv (bootstrap, &_hurd_host_priv, &_hurd_device_master,
&parent_task);
mach_port_deallocate (mach_task_self (), bootstrap);
assert_perror_backtrace (err);
}
}
void
diskfs_start_bootstrap (void)
{
mach_port_t root_pt, startup_pt, bootpt;
retry_type retry;
char pathbuf[1024];
string_t retry_name;
mach_port_t portarray[INIT_PORT_MAX];
mach_port_t fdarray[3];
task_t newt;
error_t err;
char *exec_argv, *exec_env;
const char *initname;
size_t exec_argvlen, exec_envlen;
struct protid *rootpi;
struct peropen *rootpo;
mach_port_t diskfs_exec;
unsigned int init_lookups = 0;
err = diskfs_make_peropen (diskfs_root_node, O_READ | O_EXEC, 0,
&rootpo);
assert_perror_backtrace (err);
err = diskfs_create_protid (rootpo, 0, &rootpi);
assert_perror_backtrace (err);
root_pt = ports_get_send_right (rootpi);
ports_port_deref (rootpi);
if (diskfs_exec_server_task == MACH_PORT_NULL)
{
assert_backtrace (_hurd_ports);
assert_backtrace (_hurd_ports[INIT_PORT_CRDIR].port != MACH_PORT_NULL);
diskfs_exec = file_name_lookup (_SERVERS_EXEC, 0, 0);
if (diskfs_exec == MACH_PORT_NULL)
error (1, errno, "%s", _SERVERS_EXEC);
else
{
#ifndef NDEBUG
struct port_info *pi = ports_lookup_port (diskfs_port_bucket,
diskfs_exec, 0);
assert_backtrace (!pi);
#endif
}
printf ("\nContinuing on new root filesystem %s:", diskfs_disk_name);
fflush (stdout);
}
else
{
uid_t idlist[] = {0, 0, 0};
file_t execnode;
printf ("Hurd server bootstrap: %s[%s]",
program_invocation_short_name, diskfs_disk_name);
fflush (stdout);
pthread_mutex_init (&execstartlock, NULL);
pthread_cond_init (&execstarted, NULL);
pthread_mutex_lock (&execstartlock);
start_execserver ();
pthread_cond_wait (&execstarted, &execstartlock);
pthread_mutex_unlock (&execstartlock);
assert_backtrace (diskfs_exec_ctl != MACH_PORT_NULL);
err = fsys_getroot (diskfs_exec_ctl, root_pt, MACH_MSG_TYPE_COPY_SEND,
idlist, 3, idlist, 3, 0,
&retry, retry_name, &diskfs_exec);
assert_perror_backtrace (err);
assert_backtrace (retry == FS_RETRY_NORMAL);
assert_backtrace (retry_name[0] == '\0');
assert_backtrace (diskfs_exec != MACH_PORT_NULL);
err = dir_lookup (root_pt, _SERVERS_EXEC, O_NOTRANS, 0,
&retry, retry_name, &execnode);
if (err)
{
mach_print ("cannot set translator on " _SERVERS_EXEC "\n");
error (0, err, "cannot set translator on " _SERVERS_EXEC);
mach_port_deallocate (mach_task_self (), diskfs_exec_ctl);
}
else
{
assert_backtrace (retry == FS_RETRY_NORMAL);
assert_backtrace (retry_name[0] == '\0');
assert_backtrace (execnode != MACH_PORT_NULL);
err = file_set_translator (execnode, 0, FS_TRANS_SET, 0, 0, 0,
diskfs_exec_ctl, MACH_MSG_TYPE_COPY_SEND);
mach_port_deallocate (mach_task_self (), diskfs_exec_ctl);
mach_port_deallocate (mach_task_self (), execnode);
assert_perror_backtrace (err);
}
diskfs_exec_ctl = MACH_PORT_NULL;
}
_hurd_port_set (&_diskfs_exec_portcell, diskfs_exec);
if (_diskfs_boot_command)
{
err = argz_create (_diskfs_boot_command, &exec_argv, &exec_argvlen);
assert_perror_backtrace (err);
}
else
{
initname = diskfs_boot_init_program;
while (*initname == '/')
initname++;
int len = asprintf (&exec_argv, "/%s%c", initname, '\0');
assert_backtrace (len != -1);
exec_argvlen = (size_t) len;
err = argz_add_sep (&exec_argv, &exec_argvlen,
diskfs_boot_command_line, ' ');
assert_perror_backtrace (err);
}
err = task_create (mach_task_self (),
#ifdef KERN_INVALID_LEDGER
NULL, 0,
#endif
0, &newt);
assert_perror_backtrace (err);
if (MACH_PORT_VALID (diskfs_kernel_task))
{
mach_port_t kernel_task_name = MACH_PORT_NULL;
char buf[20];
int len;
do
{
kernel_task_name += 1;
err = mach_port_insert_right (newt, kernel_task_name,
diskfs_kernel_task, MACH_MSG_TYPE_MOVE_SEND);
}
while (err == KERN_NAME_EXISTS);
diskfs_kernel_task = MACH_PORT_NULL;
len = snprintf (buf, sizeof buf, "--kernel-task=%u", kernel_task_name);
assert_backtrace (len > 0);
assert_backtrace ((size_t) len < sizeof buf);
err = argz_insert (&exec_argv, &exec_argvlen,
argz_next (exec_argv, exec_argvlen, exec_argv), buf);
assert_perror_backtrace (err);
}
initname = exec_argv;
while (*initname == '/')
initname++;
lookup_init:
err = dir_lookup (root_pt, (char *) initname, O_READ, 0, &retry, pathbuf,
&startup_pt);
init_lookups++;
if (err)
{
printf ("\nCannot find startup program `%s': %s\n",
initname, strerror (err));
fflush (stdout);
free (exec_argv);
assert_perror_backtrace (err);
}
else if (retry == FS_RETRY_MAGICAL && pathbuf[0] == '/')
{
assert_backtrace (sysconf (_SC_SYMLOOP_MAX) < 0 ||
init_lookups < sysconf (_SC_SYMLOOP_MAX));
initname = strdupa (pathbuf);
goto lookup_init;
}
assert_backtrace (retry == FS_RETRY_NORMAL);
assert_backtrace (pathbuf[0] == '\0');
err = ports_create_port (diskfs_control_class, diskfs_port_bucket,
sizeof (struct port_info), &bootinfo);
assert_perror_backtrace (err);
bootpt = ports_get_send_right (bootinfo);
portarray[INIT_PORT_CRDIR] = root_pt;
portarray[INIT_PORT_CWDIR] = root_pt;
portarray[INIT_PORT_AUTH] = MACH_PORT_NULL;
portarray[INIT_PORT_PROC] = MACH_PORT_NULL;
portarray[INIT_PORT_CTTYID] = MACH_PORT_NULL;
portarray[INIT_PORT_BOOTSTRAP] = bootpt;
fdarray[0] = fdarray[1] = fdarray[2] = get_console ();
err = argz_create (environ, &exec_env, &exec_envlen);
assert_perror_backtrace (err);
if (_diskfs_boot_pause)
{
printf ("pausing for %s...\n", exec_argv);
fflush (stdout);
getc (stdin);
}
printf (" %s", basename (exec_argv));
fflush (stdout);
err = exec_exec (diskfs_exec, startup_pt, MACH_MSG_TYPE_COPY_SEND,
newt, 0, (data_t)exec_argv, exec_argvlen, (data_t)exec_env, exec_envlen,
fdarray, MACH_MSG_TYPE_COPY_SEND, 3,
portarray, MACH_MSG_TYPE_COPY_SEND, INIT_PORT_MAX,
NULL, 0, 0, 0, 0, 0);
if (err)
{
mach_print ("Failed to execute startup\n");
error (1, err, "Executing '%s'", exec_argv);
}
free (exec_argv);
free (exec_env);
mach_port_deallocate (mach_task_self (), root_pt);
mach_port_deallocate (mach_task_self (), startup_pt);
mach_port_deallocate (mach_task_self (), bootpt);
}
kern_return_t
diskfs_S_exec_startup_get_info (struct bootinfo *upt,
vm_address_t *user_entry,
vm_address_t *phdr_data,
vm_size_t *phdr_size,
vm_address_t *base_addr,
vm_size_t *stack_size,
int *flags,
data_t *argvP,
mach_msg_type_number_t *argvlen,
data_t *envpP __attribute__ ((unused)),
mach_msg_type_number_t *envplen,
mach_port_t **dtableP,
mach_msg_type_name_t *dtablepoly,
mach_msg_type_number_t *dtablelen,
mach_port_t **portarrayP,
mach_msg_type_name_t *portarraypoly,
mach_msg_type_number_t *portarraylen,
int **intarrayP,
mach_msg_type_number_t *intarraylen)
{
error_t err;
mach_port_t *portarray, *dtable;
mach_port_t rootport;
struct protid *rootpi;
struct peropen *rootpo;
if (! upt)
return EOPNOTSUPP;
*user_entry = 0;
*phdr_data = *base_addr = 0;
*phdr_size = *stack_size = 0;
*argvlen = *envplen = 0;
*flags = EXEC_STACK_ARGS;
if (*portarraylen < INIT_PORT_MAX)
{
*portarrayP = mmap (0, INIT_PORT_MAX * sizeof (mach_port_t),
PROT_READ|PROT_WRITE, MAP_ANON, 0, 0);
assert_backtrace (*portarrayP != MAP_FAILED);
}
portarray = *portarrayP;
*portarraylen = INIT_PORT_MAX;
if (*dtablelen < 3)
{
*dtableP = mmap (0, 3 * sizeof (mach_port_t), PROT_READ|PROT_WRITE,
MAP_ANON, 0, 0);
assert_backtrace (*dtableP != MAP_FAILED);
}
dtable = *dtableP;
*dtablelen = 3;
dtable[0] = dtable[1] = dtable[2] = get_console ();
*intarrayP = NULL;
*intarraylen = 0;
err = diskfs_make_peropen (diskfs_root_node, O_READ | O_EXEC, 0, &rootpo);
assert_perror_backtrace (err);
err = diskfs_create_protid (rootpo, 0, &rootpi);
assert_perror_backtrace (err);
rootport = ports_get_right (rootpi);
ports_port_deref (rootpi);
portarray[INIT_PORT_CWDIR] = rootport;
portarray[INIT_PORT_CRDIR] = rootport;
portarray[INIT_PORT_AUTH] = MACH_PORT_NULL;
portarray[INIT_PORT_PROC] = MACH_PORT_NULL;
portarray[INIT_PORT_CTTYID] = MACH_PORT_NULL;
portarray[INIT_PORT_BOOTSTRAP] = upt->pi.port_right;
*portarraypoly = MACH_MSG_TYPE_MAKE_SEND;
*dtablepoly = MACH_MSG_TYPE_COPY_SEND;
return 0;
}
error_t
diskfs_execboot_fsys_startup (mach_port_t port, int flags,
mach_port_t ctl,
mach_port_t *real,
mach_msg_type_name_t *realpoly)
{
error_t err;
string_t pathbuf;
enum retry_type retry;
struct port_info *pt;
struct protid *rootpi;
struct peropen *rootpo;
mach_port_t rootport;
if (!(pt = ports_lookup_port (diskfs_port_bucket, port,
diskfs_execboot_class)))
return EOPNOTSUPP;
err = diskfs_make_peropen (diskfs_root_node, flags, 0, &rootpo);
assert_perror_backtrace (err);
err = diskfs_create_protid (rootpo, 0, &rootpi);
assert_perror_backtrace (err);
rootport = ports_get_send_right (rootpi);
ports_port_deref (rootpi);
err = dir_lookup (rootport, _SERVERS_EXEC, flags|O_NOTRANS, 0,
&retry, pathbuf, real);
assert_perror_backtrace (err);
assert_backtrace (retry == FS_RETRY_NORMAL);
assert_backtrace (pathbuf[0] == '\0');
*realpoly = MACH_MSG_TYPE_MOVE_SEND;
mach_port_deallocate (mach_task_self (), rootport);
diskfs_exec_ctl = ctl;
pthread_mutex_lock (&execstartlock);
pthread_cond_signal (&execstarted);
pthread_mutex_unlock (&execstartlock);
ports_port_deref (pt);
return 0;
}
kern_return_t
diskfs_S_fsys_getpriv (struct diskfs_control *init_bootstrap_port,
mach_port_t reply, mach_msg_type_name_t reply_type,
mach_port_t *host_priv, mach_msg_type_name_t *hp_type,
mach_port_t *dev_master, mach_msg_type_name_t *dm_type,
mach_port_t *fstask, mach_msg_type_name_t *task_type)
{
error_t err;
if (!init_bootstrap_port
|| init_bootstrap_port != (struct diskfs_control *) bootinfo)
return EOPNOTSUPP;
err = get_privileged_ports (host_priv, dev_master);
if (!err)
{
*fstask = mach_task_self ();
*hp_type = *dm_type = MACH_MSG_TYPE_MOVE_SEND;
*task_type = MACH_MSG_TYPE_COPY_SEND;
}
return err;
}
kern_return_t
diskfs_S_fsys_init (struct diskfs_control *pt,
mach_port_t reply, mach_msg_type_name_t replytype,
mach_port_t procserver,
mach_port_t authhandle)
{
static int initdone = 0;
mach_port_t host, startup;
error_t err;
mach_port_t root_pt;
mach_port_t bootstrap;
struct protid *rootpi;
struct peropen *rootpo;
if (!pt)
return EOPNOTSUPP;
if (initdone)
return EOPNOTSUPP;
initdone = 1;
fsys_init_reply (reply, replytype, 0);
err = mach_port_mod_refs (mach_task_self (),
procserver, MACH_PORT_RIGHT_SEND, +1);
assert_perror_backtrace (err);
err = mach_port_mod_refs (mach_task_self (),
authhandle, MACH_PORT_RIGHT_SEND, +1);
assert_perror_backtrace (err);
if (diskfs_auth_server_port != MACH_PORT_NULL)
mach_port_deallocate (mach_task_self (), diskfs_auth_server_port);
diskfs_auth_server_port = authhandle;
err = task_get_bootstrap_port (mach_task_self (), &bootstrap);
assert_perror_backtrace (err);
if (diskfs_exec_server_task != MACH_PORT_NULL)
{
process_t execprocess;
err = proc_task2proc (procserver, diskfs_exec_server_task, &execprocess);
assert_perror_backtrace (err);
proc_child (procserver, diskfs_exec_server_task);
proc_mark_exec (execprocess);
HURD_PORT_USE (&_diskfs_exec_portcell,
exec_init (port, authhandle,
execprocess, MACH_MSG_TYPE_COPY_SEND));
mach_port_deallocate (mach_task_self (), execprocess);
mach_port_deallocate (mach_task_self (), diskfs_exec_server_task);
diskfs_exec_server_task = MACH_PORT_NULL;
}
else
assert_backtrace (parent_task != MACH_PORT_NULL);
if (parent_task != MACH_PORT_NULL)
{
err = proc_child (procserver, parent_task);
assert_perror_backtrace (err);
}
if (bootstrap != MACH_PORT_NULL)
{
process_t parent_proc;
assert_backtrace (parent_task != MACH_PORT_NULL);
err = proc_task2proc (procserver, parent_task, &parent_proc);
assert_perror_backtrace (err);
mach_port_deallocate (mach_task_self (), parent_task);
parent_task = MACH_PORT_NULL;
proc_mark_exec (parent_proc);
err = fsys_init (bootstrap, parent_proc, MACH_MSG_TYPE_COPY_SEND,
authhandle);
assert_perror_backtrace (err);
mach_port_deallocate (mach_task_self (), parent_proc);
mach_port_deallocate (mach_task_self (), bootstrap);
}
err = diskfs_make_peropen (diskfs_root_node, O_READ|O_EXEC, 0, &rootpo);
assert_perror_backtrace (err);
err = diskfs_create_protid (rootpo, 0, &rootpi);
assert_perror_backtrace (err);
root_pt = ports_get_send_right (rootpi);
ports_port_deref (rootpi);
mach_port_mod_refs (mach_task_self (), root_pt,
MACH_PORT_RIGHT_SEND, +1);
if (_hurd_ports)
{
_hurd_port_set (&_hurd_ports[INIT_PORT_PROC], procserver);
_hurd_port_set (&_hurd_ports[INIT_PORT_AUTH], authhandle);
_hurd_port_set (&_hurd_ports[INIT_PORT_CRDIR], root_pt);
_hurd_port_set (&_hurd_ports[INIT_PORT_CWDIR], root_pt);
_hurd_proc_init (diskfs_argv, NULL, 0);
}
else
{
mach_port_t *portarray;
unsigned int i;
portarray = mmap (0, INIT_PORT_MAX * sizeof *portarray,
PROT_READ|PROT_WRITE, MAP_ANON, 0, 0);
assert_backtrace (portarray != MAP_FAILED);
if (MACH_PORT_NULL != (mach_port_t) 0)
for (i = 0; i < INIT_PORT_MAX; ++i)
portarray[i] = MACH_PORT_NULL;
portarray[INIT_PORT_PROC] = procserver;
portarray[INIT_PORT_AUTH] = authhandle;
portarray[INIT_PORT_CRDIR] = root_pt;
portarray[INIT_PORT_CWDIR] = root_pt;
_hurd_init (0, diskfs_argv, portarray, INIT_PORT_MAX, NULL, 0);
#ifdef HAVE__HURD_LIBC_PROC_INIT
_hurd_libc_proc_init(diskfs_argv);
#endif
}
err = get_privileged_ports (&host, 0);
if (err)
return err;
proc_register_version (procserver, host, diskfs_server_name, "",
diskfs_server_version);
mach_port_deallocate (mach_task_self (), procserver);
startup = file_name_lookup (_SERVERS_STARTUP, 0, 0);
if (startup == MACH_PORT_NULL)
error (0, errno, "%s", _SERVERS_STARTUP);
else
{
startup_essential_task (startup, mach_task_self (), MACH_PORT_NULL,
diskfs_server_name, host);
mach_port_deallocate (mach_task_self (), startup);
}
mach_port_deallocate (mach_task_self (), host);
_diskfs_init_completed ();
return MIG_NO_REPLY;
}
static void
start_execserver (void)
{
error_t err;
mach_port_t right;
extern task_t diskfs_exec_server_task;
struct port_info *execboot_info;
assert_backtrace (diskfs_exec_server_task != MACH_PORT_NULL);
err = ports_create_port (diskfs_execboot_class, diskfs_port_bucket,
sizeof (struct port_info), &execboot_info);
assert_perror_backtrace (err);
right = ports_get_send_right (execboot_info);
ports_port_deref (execboot_info);
err = task_set_special_port (diskfs_exec_server_task, TASK_BOOTSTRAP_PORT, right);
assert_perror_backtrace (err);
err = mach_port_deallocate (mach_task_self (), right);
assert_perror_backtrace (err);
if (_diskfs_boot_pause)
{
printf ("pausing for exec\n");
fflush (stdout);
getc (stdin);
}
err = task_resume (diskfs_exec_server_task);
assert_perror_backtrace (err);
printf (" exec");
fflush (stdout);
}