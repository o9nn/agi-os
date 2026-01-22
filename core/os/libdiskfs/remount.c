#include "priv.h"
error_t
diskfs_remount (void)
{
error_t err;
if (! diskfs_check_readonly ())
return EBUSY;
err = ports_inhibit_class_rpcs (diskfs_protid_class);
if (err)
return err;
err = diskfs_reload_global_state ();
if (!err)
err = diskfs_node_iterate (diskfs_node_reload);
ports_resume_class_rpcs (diskfs_protid_class);
return err;
}