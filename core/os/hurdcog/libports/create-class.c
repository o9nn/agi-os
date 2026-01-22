#include "ports.h"
#include <stdlib.h>
#include <errno.h>
struct port_class *
ports_create_class (void (*clean_routine)(void *),
void (*dropweak_routine)(void *))
{
struct port_class *cl;
cl = malloc (sizeof (struct port_class));
if (! cl)
{
errno = ENOMEM;
return NULL;
}
cl->clean_routine = clean_routine;
cl->dropweak_routine = dropweak_routine;
cl->flags = 0;
cl->rpcs = 0;
cl->count = 0;
cl->uninhibitable_rpcs = ports_default_uninhibitable_rpcs;
return cl;
}