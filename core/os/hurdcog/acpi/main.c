#include <stdio.h>
#include <error.h>
#include <fcntl.h>
#include <version.h>
#include <argp.h>
#include <hurd/netfs.h>
#include <hurd/fsys.h>
#include "acpi_S.h"
#include "startup_notify_S.h"
#include "libnetfs/io_S.h"
#include "libnetfs/fs_S.h"
#include "libports/notify_S.h"
#include "libnetfs/fsys_S.h"
#include "libports/interrupt_S.h"
#include "libnetfs/ifsock_S.h"
#include "libmachdev/machdev.h"
#include <device/device.h>
#include <pthread.h>
#include <acpi/acpi_init.h>
#include <acpifs.h>
int netfs_maxsymlinks = 0;
char *netfs_server_name = "acpi";
char *netfs_server_version = HURD_VERSION;
volatile struct mapped_time_value *acpifs_maptime;
struct acpifs *fs;
static mach_port_t acpi_control_port;
int
netfs_demuxer (mach_msg_header_t * inp, mach_msg_header_t * outp)
{
mig_routine_t routine;
if ((routine = netfs_io_server_routine (inp)) ||
(routine = netfs_fs_server_routine (inp)) ||
(routine = ports_notify_server_routine (inp)) ||
(routine = netfs_fsys_server_routine (inp)) ||
(routine = ports_interrupt_server_routine (inp)) ||
(routine = netfs_ifsock_server_routine (inp)) ||
(routine = acpi_server_routine (inp)) ||
(routine = startup_notify_server_routine (inp)))
{
(*routine) (inp, outp);
return TRUE;
}
else
return FALSE;
}
static io_return_t
acpi_device_open (mach_port_t reply_port, mach_msg_type_name_t reply_port_type,
dev_mode_t mode, const char *name, device_t * devp,
mach_msg_type_name_t * devicePoly)
{
io_return_t err = D_SUCCESS;
mach_port_t dev_master, root;
string_t retry_name;
retry_type retry;
uid_t idlist[] = {0, 0, 0};
if (strncmp(name, "acpi", 3))
err = D_NO_SUCH_DEVICE;
if (err)
{
err = get_privileged_ports(NULL, &dev_master);
if (err)
return err;
if (dev_master == MACH_PORT_NULL)
return D_NO_SUCH_DEVICE;
err = device_open (dev_master, mode, name, devp);
mach_port_deallocate (mach_task_self (), dev_master);
if (err)
return err;
*devicePoly = MACH_MSG_TYPE_MOVE_SEND;
return D_SUCCESS;
}
err = fsys_getroot(acpi_control_port, MACH_PORT_NULL, MACH_MSG_TYPE_COPY_SEND,
idlist, 3, idlist, 3, 0,
&retry, retry_name, &root);
if (err)
return err;
*devp = root;
*devicePoly = MACH_MSG_TYPE_COPY_SEND;
return D_SUCCESS;
}
static struct machdev_device_emulation_ops acpi_emulation_ops = {
NULL,
NULL,
NULL,
NULL,
acpi_device_open,
NULL,
NULL,
NULL,
NULL,
NULL,
NULL,
NULL,
NULL,
NULL,
NULL,
NULL,
NULL,
NULL,
};
static mach_port_t
acpifs_startup(mach_port_t bootstrap, int flags)
{
error_t err;
mach_port_t realnode;
struct port_info *newpi;
err = ports_create_port (netfs_control_class, netfs_port_bucket,
sizeof (struct port_info), &newpi);
if (err)
error (11, err, "Translator startup failure: acpifs_startup");
acpi_control_port = ports_get_send_right (newpi);
if (bootstrap != MACH_PORT_NULL)
{
err = fsys_startup (bootstrap, flags, acpi_control_port, MACH_MSG_TYPE_COPY_SEND,
&realnode);
assert_perror_backtrace (err);
}
return realnode;
}
int
main (int argc, char **argv)
{
error_t err;
mach_port_t bootstrap;
mach_port_t next_task;
pthread_t t, mt;
file_t underlying_node = MACH_PORT_NULL;
alloc_file_system (&fs);
argp_parse (netfs_runtime_argp, argc, argv, 0, 0, 0);
next_task = fs->next_task;
if (next_task != MACH_PORT_NULL)
{
machdev_register (&acpi_emulation_ops);
machdev_trivfs_init (argc, argv, next_task, "acpi", NULL , &bootstrap);
machdev_device_init ();
err = pthread_create (&t, NULL, machdev_server, NULL);
if (err)
error (1, err, "creating machdev thread");
pthread_detach (t);
}
else
{
task_get_bootstrap_port (mach_task_self (), &bootstrap);
if (bootstrap == MACH_PORT_NULL)
error (1, 0, "must be started as a translator");
}
netfs_init ();
err = maptime_map (0, 0, &acpifs_maptime);
if (err)
err = maptime_map (1, 0, &acpifs_maptime);
if (err)
error (1, err, "mapping time");
acpi_init ();
if (next_task != MACH_PORT_NULL)
machdev_trivfs_server_startup (bootstrap);
if (next_task == MACH_PORT_NULL)
underlying_node = netfs_startup (bootstrap, O_READ);
err = init_root_node (underlying_node);
if (err)
error (1, err, "creating the root node");
if (next_task != MACH_PORT_NULL)
acpifs_startup (bootstrap, O_READ);
err = init_file_system (fs);
if (err)
error (1, err, "creating the ACPI filesystem");
err = create_fs_tree (fs);
if (err)
error (1, err, "creating the ACPI filesystem tree");
err = fs_set_permissions (fs);
if (err)
error (1, err, "setting permissions");
if (next_task != MACH_PORT_NULL)
{
err = pthread_create (&mt, NULL, machdev_trivfs_server_loop_forever, NULL);
if (err)
error(1, err, "creating machdev_trivfs_server_loop_forever thread");
pthread_detach (mt);
}
netfs_server_loop ();
return 0;
}