#include <errno.h>
#include <unistd.h>
#include <stdio.h>
#include <sys/mman.h>
#include <sys/fcntl.h>
#include <hurd.h>
#include <mach.h>
#include "cons.h"
void
cons_vcons_destroy (void *port)
{
cons_notify_t notify = (cons_notify_t) port;
vcons_t vcons = (vcons_t) port;
if (notify->cons)
return;
if (vcons->input >= 0)
{
close (vcons->input);
vcons->input = -1;
}
if (vcons->display != MAP_FAILED)
{
munmap (vcons->display, vcons->display_size);
vcons->display = MAP_FAILED;
}
}