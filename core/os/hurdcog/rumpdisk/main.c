#include <stdarg.h>
#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <error.h>
#include <argp.h>
#include <version.h>
#include "libmachdev/machdev.h"
#include "libshouldbeinlibc/wire.h"
#include "block-rump.h"
#include <pthread.h>
#include <mach.h>
#ifdef _RUMP_SATA
#define RUMPNAME "rumpdisk"
#else
#define RUMPNAME "rumpusbdisk"
#endif
mach_port_t bootstrap_resume_task = MACH_PORT_NULL;
static const struct argp_option options[] = {
{"host-priv-port", 'h', "PORT", 0, "Host private port PORT"},
{"device-master-port",'d', "PORT", 0, "Device master port PORT"},
{"next-task", 'N', "TASK", 0, "Next bootstrap task TASK"},
{0}
};
static error_t
parse_opt (int key, char *arg, struct argp_state *state)
{
struct
{
int host_priv;
int dev_master;
int next_task;
} *values = state->hook;
switch (key)
{
case 'h':
values->host_priv = atoi(arg);
break;
case 'd':
values->dev_master = atoi(arg);
break;
case 'N':
values->next_task = atoi(arg);
break;
case ARGP_KEY_INIT:
state->child_inputs[0] = state->input;
values = malloc (sizeof *values);
if (values == 0)
return ENOMEM;
state->hook = values;
memset (values, 0, sizeof *values);
break;
case ARGP_KEY_SUCCESS:
_hurd_host_priv = values->host_priv;
_hurd_device_master = values->dev_master;
bootstrap_resume_task = values->next_task;
break;
default:
return ARGP_ERR_UNKNOWN;
}
return 0;
}
static struct argp_child empty_argp_children[] = {{0}};
static struct argp rumpdisk_argp = {options, parse_opt, 0, 0, empty_argp_children};
static const struct argp *rumpdisk_argp_bootup = &rumpdisk_argp;
static int __thread wired = 0;
static int rumpdisk_demuxer (mach_msg_header_t *inp, mach_msg_header_t *outp)
{
if (!wired)
{
mach_port_t self = mach_thread_self ();
thread_wire (_hurd_host_priv, self, TRUE);
mach_port_deallocate (mach_task_self (), self);
wired = 1;
}
return machdev_demuxer (inp, outp);
}
static void *
rumpdisk_multithread_server(void *arg)
{
pthread_setname_np (pthread_self (), "server");
do
{
ports_manage_port_operations_multithread (machdev_device_bucket,
rumpdisk_demuxer,
1000 * 60 * 2,
1000 * 60 * 10,
0);
} while (1);
return NULL;
}
int
main (int argc, char **argv)
{
mach_port_t bootstrap = MACH_PORT_NULL;
int err;
pthread_t t;
setenv ("RUMP_NCPU", "1", 1);
setenv ("RUMP_VERBOSE", "1", 1);
setenv ("RUMP_HOSTNAME", "HURD0", 1);
setenv ("HOSTNAME", "HURD0", 1);
setenv ("RUMP_PANIC", "1", 1);
err = argp_parse (rumpdisk_argp_bootup, argc, argv, 0, 0, NULL);
if (err)
{
error(1, err, "Missing parameters for bootstrap");
}
rump_register_block ();
machdev_trivfs_init (argc, argv, bootstrap_resume_task, RUMPNAME, "/dev/" RUMPNAME, &bootstrap);
err = wire_task_self ();
if (err)
error (1, err, "cannot lock all memory");
err = thread_wire (_hurd_host_priv, mach_thread_self (), TRUE);
if (err != KERN_SUCCESS)
error (1, err, "cannot get vm_privilege");
machdev_device_init ();
err = pthread_create (&t, NULL, rumpdisk_multithread_server, NULL);
if (err)
return err;
pthread_detach (t);
machdev_trivfs_server_startup (bootstrap);
machdev_trivfs_server_loop (NULL);
return 0;
}