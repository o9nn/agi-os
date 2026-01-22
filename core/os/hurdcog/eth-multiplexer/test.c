#define _GNU_SOURCE
#include <stdio.h>
#include <string.h>
#include <error.h>
#include <hurd.h>
#include <mach.h>
#include <device/device.h>
int
main(int argc , char *argv[])
{
mach_port_t device;
mach_port_t master_device;
error_t err;
err = get_privileged_ports (0, &master_device);
if (err)
error (2, err, "cannot get device master port");
err = device_open (master_device, D_READ | D_WRITE, "eth0", &device);
if (err)
error (1, err, "device_open");
printf ("the device port is %d\n", device);
err = device_open (master_device, D_READ | D_WRITE, "eth0", &device);
if (err)
error (1, err, "device_open");
printf ("the device port is %d\n", device);
return 0;
}