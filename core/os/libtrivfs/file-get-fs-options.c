#include <argz.h>
#include <hurd/fshelp.h>
#include "priv.h"
#include "trivfs_fs_S.h"
kern_return_t
trivfs_S_file_get_fs_options (struct trivfs_protid *cred,
mach_port_t reply,
mach_msg_type_name_t reply_type,
data_t *data, mach_msg_type_number_t *len)
{
error_t err;
char *argz = 0;
size_t argz_len = 0;
if (! cred)
return EOPNOTSUPP;
err = argz_add (&argz, &argz_len, program_invocation_name);
if (err)
return err;
err = trivfs_append_args (cred->po->cntl, &argz, &argz_len);
if (! err)
err = iohelp_return_malloced_buffer (argz, argz_len, data, len);
else
free (argz);
return err;
}