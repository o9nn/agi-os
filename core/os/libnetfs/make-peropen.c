#include "netfs.h"
#include <errno.h>
#include <stdlib.h>
#include <sys/file.h>
struct peropen *
netfs_make_peropen (struct node *np, int flags, struct peropen *context)
{
error_t err;
struct peropen *po = malloc (sizeof (struct peropen));
if (!po)
return NULL;
po->filepointer = 0;
err = fshelp_rlock_po_init (&po->lock_status);
if (err)
{
free (po);
return NULL;
}
refcount_init (&po->refcnt, 1);
po->openstat = flags;
po->np = np;
po->path = NULL;
if (context)
{
if (context->path)
{
po->path = strdup (context->path);
if (! po->path) {
fshelp_rlock_po_fini (&po->lock_status);
free (po);
return NULL;
}
}
po->root_parent = context->root_parent;
if (po->root_parent != MACH_PORT_NULL)
mach_port_mod_refs (mach_task_self (), po->root_parent,
MACH_PORT_RIGHT_SEND, 1);
po->shadow_root = context->shadow_root;
if (po->shadow_root)
netfs_nref (po->shadow_root);
po->shadow_root_parent = context->shadow_root_parent;
if (po->shadow_root_parent != MACH_PORT_NULL)
mach_port_mod_refs (mach_task_self (), po->shadow_root_parent,
MACH_PORT_RIGHT_SEND, 1);
}
netfs_nref (np);
return po;
}