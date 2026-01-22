#include <fcntl.h>
#include "priv.h"
#include "fsys_S.h"
#include "fhandle.h"
kern_return_t
diskfs_S_fsys_getfile (struct diskfs_control *pt,
mach_port_t reply, mach_msg_type_name_t reply_type,
const uid_t *uids, mach_msg_type_number_t nuids,
const gid_t *gids, mach_msg_type_number_t ngids,
const_data_t handle, mach_msg_type_number_t handle_len,
mach_port_t *file, mach_msg_type_name_t *file_type)
{
int flags;
error_t err;
struct node *node;
const union diskfs_fhandle *f;
struct protid *new_cred;
struct peropen *new_po;
struct iouser *user;
if (!pt)
return EOPNOTSUPP;
if (handle_len != sizeof *f)
{
return EINVAL;
}
f = (const union diskfs_fhandle *) handle;
err = diskfs_cached_lookup (f->data.cache_id, &node);
if (err)
{
return err;
}
if (node->dn_stat.st_gen != f->data.gen)
{
diskfs_nput (node);
return ESTALE;
}
err = iohelp_create_complex_iouser (&user, uids, nuids, gids, ngids);
if (err)
{
diskfs_nput (node);
return err;
}
flags = 0;
if (! fshelp_access (&node->dn_stat, S_IREAD, user))
flags |= O_READ;
if (! fshelp_access (&node->dn_stat, S_IEXEC, user))
flags |= O_EXEC;
if (! fshelp_access (&node->dn_stat, S_IWRITE, user)
&& ! S_ISDIR (node->dn_stat.st_mode)
&& ! diskfs_check_readonly ())
flags |= O_WRITE;
err = diskfs_make_peropen (node, flags, 0, &new_po);
if (! err)
{
err = diskfs_create_protid (new_po, user, &new_cred);
if (err)
diskfs_release_peropen (new_po);
}
iohelp_free_iouser (user);
diskfs_nput (node);
if (! err)
{
*file = ports_get_right (new_cred);
*file_type = MACH_MSG_TYPE_MAKE_SEND;
}
return err;
}