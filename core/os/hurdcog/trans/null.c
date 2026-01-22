#include <hurd.h>
#include <hurd/ports.h>
#include <hurd/trivfs.h>
#include <hurd/fsys.h>
#include <version.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <error.h>
#include <string.h>
#include <fcntl.h>
#include <limits.h>
#include <argp.h>
#include <nullauth.h>
#include "libtrivfs/trivfs_fs_S.h"
#include "libtrivfs/trivfs_io_S.h"
const char *argp_program_version = STANDARD_HURD_VERSION (null);
static error_t write_error_code;
static const struct argp_option options[] =
{
{"full",	'f', 0, 0, "Cause writes to fail as if to a full disk"},
{0}
};
static error_t
parse_opt (int opt, char *arg, struct argp_state *state)
{
switch (opt)
{
case 'f':
write_error_code = ENOSPC;
return 0;
}
return ARGP_ERR_UNKNOWN;
}
static const struct argp argp =
{ options, parse_opt, 0, "Endless sink and null source" };
int
main (int argc, char **argv)
{
error_t err;
mach_port_t bootstrap;
struct trivfs_control *fsys;
argp_parse (&argp, argc, argv, 0, 0, 0);
task_get_bootstrap_port (mach_task_self (), &bootstrap);
if (bootstrap == MACH_PORT_NULL)
error(1, 0, "Must be started as a translator");
err = trivfs_startup (bootstrap, 0, 0, 0, 0, 0, &fsys);
mach_port_deallocate (mach_task_self (), bootstrap);
if (err)
error(3, err, "Contacting parent");
err = setnullauth ();
if (err)
error(4, err, "Dropping privileges");
ports_manage_port_operations_multithread (fsys->pi.bucket, trivfs_demuxer,
2 * 60 * 1000, 0, 0);
return 0;
}
int trivfs_fstype = FSTYPE_DEV;
int trivfs_fsid = 0;
int trivfs_support_read = 1;
int trivfs_support_write = 1;
int trivfs_support_exec = 0;
int trivfs_allow_open = O_READ | O_WRITE;
void
trivfs_modify_stat (struct trivfs_protid *cred, struct stat *st)
{
st->st_blksize = vm_page_size * 256;
st->st_size = 0;
st->st_blocks = 0;
st->st_mode &= ~S_IFMT;
st->st_mode |= S_IFCHR;
}
error_t
trivfs_goaway (struct trivfs_control *fsys, int flags)
{
exit (0);
}
kern_return_t
trivfs_S_io_map (struct trivfs_protid *cred,
mach_port_t reply, mach_msg_type_name_t replytype,
memory_object_t *rdobj,
mach_msg_type_name_t *rdtype,
memory_object_t *wrobj,
mach_msg_type_name_t *wrtype)
{
return EOPNOTSUPP;
}
kern_return_t
trivfs_S_io_read(struct trivfs_protid *cred,
mach_port_t reply, mach_msg_type_name_t replytype,
data_t *data,
mach_msg_type_number_t *datalen,
off_t offs,
vm_size_t amt)
{
if (!cred)
return EOPNOTSUPP;
else if (!(cred->po->openmodes & O_READ))
return EBADF;
else
{
*datalen = 0;
return 0;
}
}
kern_return_t
trivfs_S_io_readable (struct trivfs_protid *cred,
mach_port_t reply, mach_msg_type_name_t replytype,
vm_size_t *amount)
{
if (!cred)
return EOPNOTSUPP;
else if (!(cred->po->openmodes & O_READ))
return EINVAL;
else
*amount = 0;
return 0;
}
kern_return_t
trivfs_S_io_seek (struct trivfs_protid *cred,
mach_port_t reply, mach_msg_type_name_t replytype,
off_t offset, int whence, off_t *new_offset)
{
if (!cred)
return EOPNOTSUPP;
*new_offset = 0;
return 0;
}
kern_return_t
trivfs_S_io_select (struct trivfs_protid *cred,
mach_port_t reply, mach_msg_type_name_t replytype,
int *type)
{
if (!cred)
return EOPNOTSUPP;
*type &= ~SELECT_URG;
return 0;
}
kern_return_t
trivfs_S_io_select_timeout (struct trivfs_protid *cred,
mach_port_t reply, mach_msg_type_name_t replytype,
struct timespec ts,
int *type)
{
return trivfs_S_io_select (cred, reply, replytype, type);
}
kern_return_t
trivfs_S_io_write (struct trivfs_protid *cred,
mach_port_t reply, mach_msg_type_name_t replytype,
const_data_t data, mach_msg_type_number_t datalen,
off_t offs, vm_size_t *amt)
{
if (!cred)
return EOPNOTSUPP;
else if (!(cred->po->openmodes & O_WRITE))
return EBADF;
*amt = datalen;
return write_error_code;
}
kern_return_t
trivfs_S_file_set_size (struct trivfs_protid *cred,
mach_port_t reply, mach_msg_type_name_t replytype,
loff_t size)
{
if (size < 0)
return EINVAL;
return 0;
}
kern_return_t
trivfs_S_io_get_openmodes (struct trivfs_protid *cred,
mach_port_t reply, mach_msg_type_name_t replytype,
int *bits)
{
if (!cred)
return EOPNOTSUPP;
else
{
*bits = cred->po->openmodes;
return 0;
}
}
kern_return_t
trivfs_S_io_set_all_openmodes(struct trivfs_protid *cred,
mach_port_t reply,
mach_msg_type_name_t replytype,
int mode)
{
if (!cred)
return EOPNOTSUPP;
else
return 0;
}
kern_return_t
trivfs_S_io_set_some_openmodes (struct trivfs_protid *cred,
mach_port_t reply,
mach_msg_type_name_t replytype,
int bits)
{
if (!cred)
return EOPNOTSUPP;
else
return 0;
}
kern_return_t
trivfs_S_io_clear_some_openmodes (struct trivfs_protid *cred,
mach_port_t reply,
mach_msg_type_name_t replytype,
int bits)
{
if (!cred)
return EOPNOTSUPP;
else
return 0;
}
kern_return_t
trivfs_S_io_get_owner (struct trivfs_protid *cred,
mach_port_t reply,
mach_msg_type_name_t replytype,
pid_t *owner)
{
if (!cred)
return EOPNOTSUPP;
*owner = 0;
return 0;
}
kern_return_t
trivfs_S_io_mod_owner (struct trivfs_protid *cred,
mach_port_t reply, mach_msg_type_name_t replytype,
pid_t owner)
{
if (!cred)
return EOPNOTSUPP;
else
return EINVAL;
}