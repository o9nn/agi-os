#include "priv.h"
#include <errno.h>
#include <stdlib.h>
#include <sys/file.h>
#include <hurd/fshelp.h>
error_t
diskfs_make_peropen (struct node *np, int flags, struct peropen *context,
struct peropen **ppo)
{
error_t err;
struct peropen *po = *ppo = malloc (sizeof (struct peropen));
if (! po)
return ENOMEM;
err = fshelp_rlock_po_init (&po->lock_status);
if (err)
{
free (po);
return err;
}
po->filepointer = 0;
refcount_init (&po->refcnt, 1);
po->openstat = flags;
po->np = np;
po->path = NULL;
if (context)
{
if (context->path)
{
po->path = strdup (context->path);
if (! po->path)
{
fshelp_rlock_po_fini (&po->lock_status);
free (po);
return ENOMEM;
}
}
po->root_parent = context->root_parent;
if (po->root_parent != MACH_PORT_NULL)
mach_port_mod_refs (mach_task_self (), po->root_parent,
MACH_PORT_RIGHT_SEND, 1);
po->shadow_root = context->shadow_root;
if (po->shadow_root)
diskfs_nref (po->shadow_root);
po->shadow_root_parent = context->shadow_root_parent;
if (po->shadow_root_parent != MACH_PORT_NULL)
mach_port_mod_refs (mach_task_self (), po->shadow_root_parent,
MACH_PORT_RIGHT_SEND, 1);
}
else
{
po->root_parent = MACH_PORT_NULL;
po->shadow_root_parent = MACH_PORT_NULL;
po->shadow_root = _diskfs_chroot_directory ? diskfs_root_node : 0;
}
diskfs_nref (np);
return 0;
}