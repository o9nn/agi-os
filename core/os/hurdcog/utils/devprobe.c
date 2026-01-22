#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <argp.h>
#include <hurd.h>
#include <mach.h>
#include <device/device.h>
#include <version.h>
const char *argp_program_version = STANDARD_HURD_VERSION (devprobe);
static const struct argp_option options[] = {
{"silent", 's', 0, 0, "Don't print devices found"},
{"quiet", 0, 0, OPTION_ALIAS},
{"first", 'f', 0, 0, "Stop after the first device found"},
{"master-device", 'M', "FILE", 0, "Get a pseudo master device port"},
{0}
};
static const char *args_doc = "DEVNAME...";
static const char *doc = "Test for the existence of mach device DEVNAME..."
"\vThe exit status is 0 if any devices were found.";
int
main (int argc, char **argv)
{
int print = 1;
int all = 1;
int found_one = 0;
mach_port_t device_master = MACH_PORT_NULL;
error_t parse_opt (int key, char *arg, struct argp_state *state)
{
switch (key)
{
error_t err;
device_t device;
case 's': case 'q':
print = all = 0; break;
case 'f':
all = 0; break;
case 'M':
if (device_master != MACH_PORT_NULL)
mach_port_deallocate (mach_task_self (), device_master);
device_master = file_name_lookup (arg, O_READ | O_WRITE, 0);
if (device_master == MACH_PORT_NULL)
argp_failure (state, 3, errno, "Can't open device master port %s",
arg);
break;
case ARGP_KEY_ARG:
if (device_master == MACH_PORT_NULL)
{
err = get_privileged_ports (0, &device_master);
if (err)
argp_failure (state, 3, err, "Can't get device master port");
}
err = device_open (device_master, D_READ, arg, &device);
if (err == 0)
{
device_close (device);
mach_port_deallocate (mach_task_self (), device);
if (print)
puts (arg);
if (! all)
exit (0);
found_one = 1;
}
else if (err != ED_NO_SUCH_DEVICE)
argp_failure (state, 0, err, "%s", arg);
break;
default:
return ARGP_ERR_UNKNOWN;
}
return 0;
}
const struct argp argp = { options, parse_opt, args_doc, doc };
argp_parse (&argp, argc, argv, 0, 0, 0);
exit (found_one ? 0 : 1);
}