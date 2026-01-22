#include "netfs.h"
#include "execserver.h"
#include "fs_S.h"
#include <sys/stat.h>
#include <fcntl.h>
#include <hurd/exec.h>
#include <hurd/paths.h>
#include <string.h>
#include <idvec.h>
kern_return_t
netfs_S_file_exec (struct protid *cred,
task_t task,
int flags,
const_data_t argv,
mach_msg_type_number_t argvlen,
const_data_t envp,
mach_msg_type_number_t envplen,
const mach_port_t *fds,
mach_msg_type_number_t fdslen,
const mach_port_t *portarray,
mach_msg_type_number_t portarraylen,
const int *intarray,
mach_msg_type_number_t intarraylen,
const mach_port_t *deallocnames,
mach_msg_type_number_t deallocnameslen,
const mach_port_t *destroynames,
mach_msg_type_number_t destroynameslen)
{
return netfs_S_file_exec_paths (cred,
task,
flags,
"",
"",
argv, argvlen,
envp, envplen,
fds, fdslen,
portarray, portarraylen,
intarray, intarraylen,
deallocnames, deallocnameslen,
destroynames, destroynameslen);
}
kern_return_t
netfs_S_file_exec_paths (struct protid *cred,
task_t task,
int flags,
const_string_t path,
const_string_t abspath,
const char *argv,
mach_msg_type_number_t argvlen,
const char *envp,
mach_msg_type_number_t envplen,
const mach_port_t *fds,
mach_msg_type_number_t fdslen,
const mach_port_t *portarray,
mach_msg_type_number_t portarraylen,
const int *intarray,
mach_msg_type_number_t intarraylen,
const mach_port_t *deallocnames,
mach_msg_type_number_t deallocnameslen,
const mach_port_t *destroynames,
mach_msg_type_number_t destroynameslen)
{
struct node *np;
error_t err;
uid_t uid;
gid_t gid;
mode_t mode;
int suid, sgid;
mach_port_t right, cred_right;
if (!cred)
return EOPNOTSUPP;
if (_netfs_exec == MACH_PORT_NULL)
_netfs_exec = file_name_lookup (_SERVERS_EXEC, 0, 0);
if (_netfs_exec == MACH_PORT_NULL)
return EOPNOTSUPP;
if ((cred->po->openstat & O_EXEC) == 0)
return EBADF;
np = cred->po->np;
pthread_mutex_lock (&np->lock);
mode = np->nn_stat.st_mode;
uid = np->nn_stat.st_uid;
gid = np->nn_stat.st_gid;
err = netfs_validate_stat (np, cred->user);
pthread_mutex_unlock (&np->lock);
if (err)
return err;
if (!((mode & (S_IXUSR|S_IXGRP|S_IXOTH))
|| ((mode & S_IUSEUNK) && (mode & (S_IEXEC << S_IUNKSHIFT)))))
return EACCES;
if ((mode & S_IFMT) == S_IFDIR)
return EACCES;
suid = mode & S_ISUID;
sgid = mode & S_ISGID;
if (suid || sgid)
{
int secure = 0;
error_t get_file_ids (struct idvec *uidsvec, struct idvec *gidsvec)
{
error_t err;
err = idvec_merge (uidsvec, cred->user->uids);
if (! err)
err = idvec_merge (gidsvec, cred->user->gids);
return err;
}
err =
fshelp_exec_reauth (suid, uid, sgid, gid,
netfs_auth_server_port, get_file_ids,
(mach_port_t *) portarray, portarraylen,
(mach_port_t *) fds, fdslen, &secure);
if (secure)
flags |= EXEC_SECURE | EXEC_NEWTASK;
}
#if 0
if (diskfs_access (np, S_IREAD, cred))
flags |= EXEC_NEWTASK;
#endif
if (! err)
{
struct iouser *user;
struct protid *newpi;
err = iohelp_dup_iouser (&user, cred->user);
if (! err)
{
newpi = netfs_make_protid (netfs_make_peropen (np, O_READ, cred->po),
user);
if (newpi)
{
right = ports_get_send_right (newpi);
cred_right = ports_get_send_right (cred);
#ifdef HAVE_EXEC_EXEC_PATHS
err = exec_exec_paths (_netfs_exec,
right, MACH_MSG_TYPE_COPY_SEND,
task, flags, path, abspath,
argv, argvlen, envp, envplen,
fds, MACH_MSG_TYPE_COPY_SEND, fdslen,
portarray, MACH_MSG_TYPE_COPY_SEND,
portarraylen,
intarray, intarraylen,
deallocnames, deallocnameslen,
destroynames, destroynameslen);
if (err == MIG_BAD_ID)
#endif
err = exec_exec (_netfs_exec,
right, MACH_MSG_TYPE_COPY_SEND,
task, flags, argv, argvlen, envp, envplen,
fds, MACH_MSG_TYPE_COPY_SEND, fdslen,
portarray, MACH_MSG_TYPE_COPY_SEND,
portarraylen,
intarray, intarraylen,
deallocnames, deallocnameslen,
destroynames, destroynameslen);
mach_port_deallocate (mach_task_self (), right);
mach_port_deallocate (mach_task_self (), cred_right);
ports_port_deref (newpi);
}
else
{
err = errno;
iohelp_free_iouser (user);
}
}
}
if (! err)
{
mach_msg_type_number_t i;
mach_port_deallocate (mach_task_self (), task);
for (i = 0; i < fdslen; i++)
mach_port_deallocate (mach_task_self (), fds[i]);
for (i = 0; i < portarraylen; i++)
mach_port_deallocate (mach_task_self (), portarray[i]);
}
return err;
}