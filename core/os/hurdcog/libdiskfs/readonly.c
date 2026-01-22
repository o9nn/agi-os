#include <fcntl.h>
#include <error.h>
#include "priv.h"
int _diskfs_diskdirty;
int diskfs_readonly = 0;
int diskfs_hard_readonly = 0;
int
diskfs_check_readonly (void)
{
error_t err;
if (diskfs_readonly)
return 1;
else
{
if (!_diskfs_diskdirty)
{
err = diskfs_set_hypermetadata (1, 0);
if (err)
{
error (0, 0,
"%s: MEDIA NOT WRITABLE; switching to READ-ONLY",
diskfs_disk_name ?: "-");
diskfs_hard_readonly = diskfs_readonly = 1;
return 1;
}
_diskfs_diskdirty = 1;
}
return 0;
}
}
error_t
diskfs_set_readonly (int readonly)
{
error_t err = 0;
if (diskfs_hard_readonly)
return readonly ? 0 : EROFS;
if (readonly != diskfs_readonly)
{
err = ports_inhibit_class_rpcs (diskfs_protid_class);
if (! err)
{
if (readonly)
{
error_t peropen_writable (void *pi)
{
struct protid *const cred = pi;
return (cred->po->openstat & O_WRITE) ? EBUSY : 0;
}
err = ports_class_iterate (diskfs_protid_class,
peropen_writable);
if (!err && (diskfs_max_user_pager_prot () & VM_PROT_WRITE))
err = EBUSY;
if (!err)
{
diskfs_sync_everything (1);
diskfs_set_hypermetadata (1, 1);
_diskfs_diskdirty = 0;
}
}
if (!err)
{
diskfs_readonly = readonly;
diskfs_readonly_changed (readonly);
}
ports_resume_class_rpcs (diskfs_protid_class);
}
}
return err;
}