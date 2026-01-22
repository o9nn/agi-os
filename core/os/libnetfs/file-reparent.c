#include "fs_S.h"
kern_return_t
netfs_S_file_reparent (struct protid *cred, mach_port_t parent,
mach_port_t *new_file, mach_msg_type_name_t *new_file_type)
{
error_t err;
struct node *node;
struct protid *new_cred;
struct iouser *user;
if (! cred)
return EOPNOTSUPP;
err = iohelp_dup_iouser (&user, cred->user);
if (err)
return err;
node = cred->po->np;
pthread_mutex_lock (&node->lock);
new_cred =
netfs_make_protid (netfs_make_peropen (node, cred->po->openstat, cred->po),
user);
pthread_mutex_unlock (&node->lock);
if (new_cred)
{
if (new_cred->po->shadow_root && new_cred->po->shadow_root != node)
{
pthread_mutex_lock (&new_cred->po->shadow_root->lock);
netfs_nput (new_cred->po->shadow_root);
}
if (new_cred->po->shadow_root_parent)
mach_port_deallocate (mach_task_self (), new_cred->po->shadow_root_parent);
new_cred->po->shadow_root = node;
new_cred->po->shadow_root_parent = parent;
*new_file = ports_get_right (new_cred);
*new_file_type = MACH_MSG_TYPE_MAKE_SEND;
ports_port_deref (new_cred);
return 0;
}
else
return errno;
}