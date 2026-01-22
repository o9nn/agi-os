#include "fshelp.h"
#include "rlock.h"
#include <stdlib.h>
#include <unistd.h>
error_t
fshelp_rlock_drop_peropen (struct rlock_peropen *po)
{
struct rlock_list *l;
struct rlock_list *t;
for (l = *po->locks; l; l = t)
{
if (l->waiting)
{
l->waiting = 0;
pthread_cond_broadcast (&l->wait);
}
list_unlink (node, l);
pthread_cond_destroy(&l->wait);
t = l->po.next;
free (l);
}
return 0;
}