#include <errno.h>
#include <argz.h>
#include <hurd/fsys.h>
#include <string.h>
#include "netfs.h"
#include "fsys_S.h"
kern_return_t
netfs_S_fsys_get_options (struct netfs_control *port,
mach_port_t reply,
mach_msg_type_name_t reply_type,
data_t *data, mach_msg_type_number_t *data_len)
{
error_t err;
char *argz = 0;
size_t argz_len = 0;
if (!port)
return EOPNOTSUPP;
err = argz_add (&argz, &argz_len, program_invocation_name);
if (! err)
{
#if NOT_YET
pthread_rwlock_rdlock (&netfs_fsys_lock);
#endif
err = netfs_append_args (&argz, &argz_len);
#if NOT_YET
pthread_rwlock_unlock (&netfs_fsys_lock);
#endif
}
if (! err)
err = iohelp_return_malloced_buffer (argz, argz_len, data, data_len);
else
free (argz);
return err;
}