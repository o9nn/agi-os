#include "fshelp.h"
#include "rlock.h"
#include <fcntl.h>
#include <sys/file.h>
int fshelp_rlock_peropen_status (struct rlock_peropen *po)
{
struct rlock_list *l;
if (! *po->locks)
return LOCK_UN;
for (l = *po->locks; l; l = l->po.next)
if (l->type == F_WRLCK)
return LOCK_EX;
return LOCK_SH;
}
int fshelp_rlock_node_status (struct rlock_box *box)
{
struct rlock_list *l;
if (! box->locks)
return LOCK_UN;
for (l = box->locks; l; l = l->node.next)
if (l->type == F_WRLCK)
return LOCK_EX;
return LOCK_SH;
}