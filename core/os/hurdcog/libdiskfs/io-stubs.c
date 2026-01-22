#include "priv.h"
#include "io_S.h"
kern_return_t __attribute__((weak))
diskfs_S_io_readsleep (struct protid *cred)
{
if (!cred)
return EOPNOTSUPP;
return 0;
}
kern_return_t __attribute__((weak))
diskfs_S_io_eofnotify (struct protid *cred)
{
if (!cred)
return EOPNOTSUPP;
return 0;
}
kern_return_t __attribute__((weak))
diskfs_S_io_postnotify (struct protid *cred,
vm_offset_t start __attribute__ ((unused)),
vm_offset_t end __attribute__ ((unused)))
{
return cred ? 0 : EOPNOTSUPP;
}
kern_return_t __attribute__((weak))
diskfs_S_io_readnotify (struct protid *cred)
{
return cred ? 0 : EOPNOTSUPP;
}