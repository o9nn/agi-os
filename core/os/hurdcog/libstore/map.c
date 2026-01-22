#include <hurd.h>
#include <hurd/io.h>
#include "store.h"
error_t
store_map (const struct store *store, vm_prot_t prot,
mach_port_t *memobj)
{
error_t (*map) (const struct store *store, vm_prot_t prot,
mach_port_t *memobj) =
store->class->map;
error_t err = map ? (*map) (store, prot, memobj) : EOPNOTSUPP;
if (err == EOPNOTSUPP && store->source != MACH_PORT_NULL)
{
mach_port_t rd_memobj, wr_memobj;
int ro = (store->flags & STORE_HARD_READONLY);
if ((prot & VM_PROT_WRITE) && ro)
return EACCES;
err = io_map (store->port, &rd_memobj, &wr_memobj);
if (! err)
{
*memobj = rd_memobj;
if (!ro || wr_memobj != MACH_PORT_NULL)
{
if (rd_memobj == wr_memobj)
{
if (rd_memobj != MACH_PORT_NULL)
mach_port_mod_refs (mach_task_self (), rd_memobj,
MACH_PORT_RIGHT_SEND, -1);
}
else
{
if (rd_memobj != MACH_PORT_NULL)
mach_port_deallocate (mach_task_self (), rd_memobj);
if (wr_memobj != MACH_PORT_NULL)
mach_port_deallocate (mach_task_self (), wr_memobj);
err = EOPNOTSUPP;
}
}
}
}
return err;
}