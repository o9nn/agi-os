#include "ports.h"
#include <stddef.h>
#include <errno.h>
#include <stdlib.h>
#include <hurd/ihash.h>
static struct port_class *notify_port_class;
static pthread_once_t init_notify_port_class_once = PTHREAD_ONCE_INIT;
static void
init_notify_port_class (void)
{
notify_port_class = ports_create_class (NULL, NULL);
}
struct port_bucket *
ports_create_bucket (void)
{
struct port_bucket *ret;
error_t err;
ret = malloc (sizeof (struct port_bucket));
if (! ret)
{
errno = ENOMEM;
return NULL;
}
err = mach_port_allocate (mach_task_self (), MACH_PORT_RIGHT_PORT_SET,
&ret->portset);
if (err)
{
errno = err;
free (ret);
return NULL;
}
hurd_ihash_init (&ret->htable, offsetof (struct port_info, hentry));
ret->rpcs = ret->flags = ret->count = 0;
_ports_threadpool_init (&ret->threadpool);
pthread_once (&init_notify_port_class_once, init_notify_port_class);
err = ports_create_port (notify_port_class, ret,
sizeof (struct port_info),
&ret->notify_port);
if (err)
{
hurd_ihash_destroy (&ret->htable);
free (ret);
errno = err;
return NULL;
}
return ret;
}