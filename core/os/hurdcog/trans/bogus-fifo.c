#include <hurd.h>
#include <stdio.h>
#include <error.h>
#include <sys/socket.h>
#include <hurd/paths.h>
#include <hurd/socket.h>
#include <hurd/fsys.h>
#include "fsys_S.h"
extern int fsys_server (mach_msg_header_t *, mach_msg_header_t *);
static socket_t fifo;
void
main (int argc, char **argv)
{
error_t err;
char pflocal_name[1024];
mach_port_t pflocal;
mach_port_t bootstrap, fsys, realnode;
if (argc != 1)
{
fprintf(stderr, "Usage: %s", program_invocation_name);
exit(-1);
}
task_get_bootstrap_port (mach_task_self (), &bootstrap);
if (bootstrap == MACH_PORT_NULL)
error(3, 0, "Must be started as a translator");
mach_port_allocate (mach_task_self (), MACH_PORT_RIGHT_RECEIVE, &fsys);
err = fsys_startup (bootstrap, fsys, MACH_MSG_TYPE_MAKE_SEND, &realnode);
mach_port_deallocate (mach_task_self (), bootstrap);
if (err)
error(1, err, "starting translator");
sprintf (pflocal_name, "%s/%d", _SERVERS_SOCKET, PF_LOCAL);
pflocal = file_name_lookup (pflocal_name, 0, 0);
if (pflocal == MACH_PORT_NULL)
error (2, errno, "%s", pflocal_name);
err = socket_create (pflocal, SOCK_STREAM, 0, &fifo);
if (err)
error (3, err, "%s: socket_create", pflocal_name);
err = socket_connect2 (fifo, fifo);
if (err)
error (3, err, "%s: socket_connect2", pflocal_name);
for (;;)
mach_msg_server_timeout (fsys_server, 0, fsys, 0, 0);
}
error_t
S_fsys_getroot (mach_port_t fsys, mach_port_t parent,
const id_t *uids, unsigned num_uids, const id_t *gids, unsigned num_gids,
int flags,
retry_type *do_retry, char *retry_name,
mach_port_t *result, mach_msg_type_name_t *result_type)
{
*do_retry = FS_RETRY_NORMAL;
*retry_name = '\0';
*result = fifo;
*result_type = MACH_MSG_TYPE_COPY_SEND;
return 0;
}
error_t
S_fsys_startup (mach_port_t bootstrap, mach_port_t fsys,
mach_port_t *real, mach_msg_type_name_t *real_type)
{
return EOPNOTSUPP;
}
error_t
S_fsys_goaway (mach_port_t fsys, int flags)
{
exit (0);
}
error_t
S_fsys_syncfs (mach_port_t fsys, int wait, int recurse)
{
return 0;
}
error_t
S_fsys_set_options (mach_port_t fsys,
const char *data, mach_msg_type_number_t data_len, int recurse)
{
return EOPNOTSUPP;
}
error_t
S_fsys_getfile (mach_port_t fsys,
const uid_t *uids, unsigned num_uids, const gid_t *gids, unsigned num_gids,
const char *handle, unsigned handle_len,
mach_port_t *port, mach_msg_type_name_t *port_type)
{
return EOPNOTSUPP;
}
error_t
S_fsys_getpriv (mach_port_t fsys,
mach_port_t *hostpriv, mach_port_t *devmaster, task_t *fstask)
{
return EOPNOTSUPP;
}
error_t
S_fsys_init (mach_port_t fsys,
mach_port_t reply, mach_msg_type_name_t reply_type,
mach_port_t proc, auth_t auth)
{
return EOPNOTSUPP;
}
error_t
S_fsys_forward (mach_port_t server, mach_port_t requestor,
const char *argz, mach_msg_type_number_t argz_len)
{
return EOPNOTSUPP;
}