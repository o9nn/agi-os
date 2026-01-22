#include "priv.h"
#include "fs_S.h"
#include <sys/stat.h>
#include <fcntl.h>
#include <hurd/exec.h>
#include <hurd/paths.h>
#include <string.h>
#include <idvec.h>
kern_return_t
diskfs_S_file_exec (struct protid *cred,
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
return diskfs_S_file_exec_paths (cred,
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
diskfs_S_file_exec_paths (struct protid *cred,
task_t task,
int flags,
const_string_t path,
const_string_t abspath,
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
struct node *np;
uid_t uid;
gid_t gid;
mode_t mode;
int suid, sgid;
struct protid *newpi;
struct peropen *newpo;
error_t err = 0;
mach_port_t execserver;
int cached_exec;
struct hurd_userlink ulink;
mach_port_t right, cred_right;
#define RETURN(code) do { err = (code); goto out; } while (0)
if (!cred)
return EOPNOTSUPP;
execserver = _hurd_port_get (&_diskfs_exec_portcell, &ulink);
cached_exec = (execserver != MACH_PORT_NULL);
if (execserver == MACH_PORT_NULL)
{
execserver = file_name_lookup (_SERVERS_EXEC, 0, 0);
if (execserver == MACH_PORT_NULL)
return EOPNOTSUPP;
else
{
_hurd_port_set (&_diskfs_exec_portcell, execserver);
execserver = _hurd_port_get (&_diskfs_exec_portcell, &ulink);
}
}
np = cred->po->np;
pthread_mutex_lock (&np->lock);
mode = np->dn_stat.st_mode;
uid = np->dn_stat.st_uid;
gid = np->dn_stat.st_gid;
pthread_mutex_unlock (&np->lock);
if (_diskfs_noexec)
RETURN (EACCES);
if ((cred->po->openstat & O_EXEC) == 0)
RETURN (EBADF);
if (!((mode & (S_IXUSR|S_IXGRP|S_IXOTH))
|| ((mode & S_IUSEUNK) && (mode & (S_IEXEC << S_IUNKSHIFT)))))
RETURN (EACCES);
if ((mode & S_IFMT) == S_IFDIR)
RETURN (EACCES);
suid = mode & S_ISUID;
sgid = mode & S_ISGID;
if (!_diskfs_nosuid && (suid || sgid))
{
int secure = 0;
error_t get_file_ids (struct idvec *uids, struct idvec *gids)
{
error_t err = idvec_merge (uids, cred->user->uids);
if (! err)
err = idvec_merge (gids, cred->user->gids);
return err;
}
err =
fshelp_exec_reauth (suid, uid, sgid, gid,
diskfs_auth_server_port, get_file_ids,
(mach_port_t *) portarray, portarraylen,
(mach_port_t *) fds, fdslen, &secure);
if (secure)
flags |= EXEC_SECURE | EXEC_NEWTASK;
}
#if 0
if (fshelp_access (&np->dn_stat, S_IREAD, cred->user))
flags |= EXEC_NEWTASK;
#endif
if (! err)
{
err = diskfs_make_peropen (np, O_READ|O_EXEC, cred->po, &newpo);
if (! err)
{
err = diskfs_create_protid (newpo, cred->user, &newpi);
if (err)
diskfs_release_peropen (newpo);
}
}
if (! err)
{
do
{
right = ports_get_send_right (newpi);
cred_right = ports_get_send_right (cred);
#ifdef HAVE_EXEC_EXEC_PATHS
err = exec_exec_paths (execserver,
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
err = exec_exec (execserver,
right, MACH_MSG_TYPE_COPY_SEND,
task, flags, argv, argvlen, envp, envplen,
fds, MACH_MSG_TYPE_COPY_SEND, fdslen,
portarray, MACH_MSG_TYPE_COPY_SEND, portarraylen,
intarray, intarraylen,
deallocnames, deallocnameslen,
destroynames, destroynameslen);
mach_port_deallocate (mach_task_self (), right);
mach_port_deallocate (mach_task_self (), cred_right);
if (err == MACH_SEND_INVALID_DEST)
{
if (cached_exec)
{
cached_exec = 0;
_hurd_port_free (&_diskfs_exec_portcell, &ulink, execserver);
execserver = file_name_lookup (_SERVERS_EXEC, 0, 0);
if (execserver == MACH_PORT_NULL)
err = EOPNOTSUPP;
else
{
_hurd_port_set (&_diskfs_exec_portcell, execserver);
execserver = _hurd_port_get (&_diskfs_exec_portcell,
&ulink);
}
}
else
err = EOPNOTSUPP;
}
} while (err == MACH_SEND_INVALID_DEST);
ports_port_deref (newpi);
}
if (! err)
{
unsigned int i;
mach_port_deallocate (mach_task_self (), task);
for (i = 0; i < fdslen; i++)
mach_port_deallocate (mach_task_self (), fds[i]);
for (i = 0; i < portarraylen; i++)
mach_port_deallocate (mach_task_self (), portarray[i]);
}
out:
_hurd_port_free (&_diskfs_exec_portcell, &ulink, execserver);
return err;
}