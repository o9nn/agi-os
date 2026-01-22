#include <stdio.h>
#include "priv.h"
#include "io_S.h"
kern_return_t
diskfs_S_io_server_version (struct protid *cred,
string_t server_name,
int *major,
int *minor,
int *edit)
{
if (cred)
{
snprintf (server_name, sizeof (string_t), "%s %s",
diskfs_server_name, diskfs_server_version);
return 0;
}
else
return EOPNOTSUPP;
}