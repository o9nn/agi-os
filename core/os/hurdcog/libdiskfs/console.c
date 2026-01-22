#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <error.h>
#include <signal.h>
#include <assert-backtrace.h>
#include <mach/mach.h>
#include <device/device.h>
#include <hurd.h>
#include "priv.h"
void
diskfs_console_stdio (void)
{
if (getpid () > 0)
{
if (write (2, "", 0) == 0)
dup2 (2, 1);
else
{
int fd = open ("/dev/console", O_RDWR);
if (fd < 0)
{
mach_print ("Failed to open /dev/console\n");
error (0, errno, "Failed to open /dev/console");
}
else
{
dup2 (fd, 0);
dup2 (fd, 1);
dup2 (fd, 2);
if (fd > 2)
close (fd);
}
}
}
else
{
mach_port_t dev, cons;
error_t err;
if (diskfs_boot_filesystem ())
_diskfs_boot_privports ();
err = get_privileged_ports (NULL, &dev);
assert_perror_backtrace (err);
err = device_open (dev, D_READ|D_WRITE, "console", &cons);
mach_port_deallocate (mach_task_self (), dev);
assert_perror_backtrace (err);
stdin = mach_open_devstream (cons, "r");
stdout = stderr = mach_open_devstream (cons, "w");
mach_port_deallocate (mach_task_self (), cons);
setlinebuf (stderr);
}
signal (SIGPIPE, SIG_IGN);
signal (SIGLOST, SIG_IGN);
}