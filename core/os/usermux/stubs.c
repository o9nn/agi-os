#include <hurd/netfs.h>
error_t
netfs_attempt_mksymlink (struct iouser *cred, struct node *node, const char *name)
{
return EOPNOTSUPP;
}
error_t
netfs_attempt_mkdev (struct iouser *cred, struct node *node,
mode_t type, dev_t indexes)
{
return EOPNOTSUPP;
}
error_t
netfs_set_translator (struct iouser *cred, struct node *node,
const char *argz, mach_msg_type_number_t argzlen)
{
return EOPNOTSUPP;
}
error_t
netfs_attempt_chflags (struct iouser *cred, struct node *node, int flags)
{
return EOPNOTSUPP;
}
error_t
netfs_attempt_set_size (struct iouser *cred, struct node *node, off_t size)
{
return EOPNOTSUPP;
}
error_t
netfs_attempt_statfs (struct iouser *cred, struct node *node,
struct statfs *st)
{
return EOPNOTSUPP;
}
error_t
netfs_attempt_unlink (struct iouser *user, struct node *dir, const char *name)
{
return EOPNOTSUPP;
}
error_t
netfs_attempt_rename (struct iouser *user, struct node *fromdir,
const char *fromname, struct node *todir,
const char *toname, int excl)
{
return EOPNOTSUPP;
}
error_t
netfs_attempt_mkdir (struct iouser *user, struct node *dir,
const char *name, mode_t mode)
{
return EOPNOTSUPP;
}
error_t
netfs_attempt_rmdir (struct iouser *user,
struct node *dir, const char *name)
{
return EOPNOTSUPP;
}
error_t
netfs_attempt_link (struct iouser *user, struct node *dir,
struct node *file, const char *name, int excl)
{
return EOPNOTSUPP;
}
error_t
netfs_attempt_mkfile (struct iouser *user, struct node *dir,
mode_t mode, struct node **node)
{
return EOPNOTSUPP;
}
error_t
netfs_attempt_read (struct iouser *cred, struct node *node,
off_t offset, size_t *len, void *data)
{
return EOPNOTSUPP;
}
error_t
netfs_attempt_write (struct iouser *cred, struct node *node,
off_t offset, size_t *len, const void *data)
{
return EOPNOTSUPP;
}