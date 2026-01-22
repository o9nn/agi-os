#include <errno.h>
#include <string.h>
#include <argz.h>
#include "priv.h"
#include "fs_S.h"
kern_return_t
diskfs_S_file_get_fs_options (struct protid *cred, data_t *data,
mach_msg_type_number_t *data_len)
{
error_t err;
char *argz = 0;
size_t argz_len = 0;
if (! cred)
return EOPNOTSUPP;
err = argz_add (&argz, &argz_len, program_invocation_name);
if (err)
return err;
pthread_rwlock_rdlock (&diskfs_fsys_lock);
err = diskfs_append_args (&argz, &argz_len);
pthread_rwlock_unlock (&diskfs_fsys_lock);
if (! err)
err = iohelp_return_malloced_buffer (argz, argz_len, data, data_len);
else
free (argz);
return err;
}