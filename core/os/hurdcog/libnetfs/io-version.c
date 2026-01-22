#include <stdio.h>
#include "netfs.h"
#include "io_S.h"
kern_return_t
netfs_S_io_server_version (struct protid *cred,
string_t server_name,
int *major,
int *minor,
int *edit)
{
if (!cred)
return EOPNOTSUPP;
snprintf (server_name, sizeof (string_t), "%s %s",
netfs_server_name, netfs_server_version);
return 0;
}