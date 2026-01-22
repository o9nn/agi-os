#include <errno.h>
#include <argz.h>
#include <hurd/fsys.h>
#include <string.h>
#include "priv.h"
#include "fsys_S.h"
struct args
{
const char *data;
mach_msg_type_number_t len;
int do_children;
};
static error_t
helper (void *cookie, const char *name, mach_port_t control)
{
struct args *args = cookie;
error_t err;
(void) name;
err = fsys_set_options (control, args->data, args->len, args->do_children);
if (err == MIG_SERVER_DIED || err == MACH_SEND_INVALID_DEST)
err = 0;
return err;
}
kern_return_t
diskfs_S_fsys_set_options (struct diskfs_control *pt,
mach_port_t reply,
mach_msg_type_name_t replytype,
const_data_t data, mach_msg_type_number_t len,
int do_children)
{
error_t err = 0;
struct args args = { data, len, do_children };
if (!pt)
return EOPNOTSUPP;
if (do_children)
{
pthread_rwlock_wrlock (&diskfs_fsys_lock);
err = fshelp_map_active_translators (helper, &args);
pthread_rwlock_unlock (&diskfs_fsys_lock);
}
if (!err)
{
pthread_rwlock_wrlock (&diskfs_fsys_lock);
err = diskfs_set_options (data, len);
pthread_rwlock_unlock (&diskfs_fsys_lock);
}
return err;
}
kern_return_t
diskfs_S_fsys_get_options (struct diskfs_control *port,
mach_port_t reply,
mach_msg_type_name_t replytype,
data_t *data, mach_msg_type_number_t *data_len)
{
char *argz = 0;
size_t argz_len = 0;
error_t err;
if (!port)
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