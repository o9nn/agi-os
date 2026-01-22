#include <errno.h>
#include <malloc.h>
#include <pthread.h>
#include <hurd.h>
#include <hurd/ports.h>
#include <mach.h>
#include "cons.h"
#include "priv.h"
struct port_bucket *cons_port_bucket;
struct port_class *cons_port_class;
error_t
cons_init (void)
{
error_t err;
cons_t cons;
cons_notify_t dir_notify_port;
mach_port_t dir_notify;
cons_port_bucket = ports_create_bucket ();
if (!cons_port_bucket)
return errno;
cons_port_class = ports_create_class (cons_vcons_destroy, NULL);
if (!cons_port_class)
return errno;
cons = malloc (sizeof (*cons));
if (!cons)
return errno;
pthread_mutex_init (&cons->lock, NULL);
cons->vcons_list = NULL;
cons->vcons_last = NULL;
cons->dir = opendir (cons_file);
cons->slack = _cons_slack;
if (!cons->dir)
{
free (cons);
return errno;
}
cons->dirport = getdport (dirfd (cons->dir));
if (cons->dirport == MACH_PORT_NULL)
{
closedir (cons->dir);
free (cons);
return errno;
}
err = ports_create_port (cons_port_class, cons_port_bucket,
sizeof (*dir_notify_port), &dir_notify_port);
if (err)
{
mach_port_deallocate (mach_task_self (), cons->dirport);
closedir (cons->dir);
free (cons);
return err;
}
dir_notify_port->cons = cons;
dir_notify = ports_get_right (dir_notify_port);
err = dir_notice_changes (cons->dirport, dir_notify,
MACH_MSG_TYPE_MAKE_SEND);
if (err)
{
mach_port_deallocate (mach_task_self (), cons->dirport);
closedir (cons->dir);
free (cons);
return err;
}
return 0;
}