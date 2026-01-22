#include <fcntl.h>
#include <assert-backtrace.h>
#include <string.h>
#include <stdio.h>
#include <hurd/paths.h>
#include "netfs.h"
#include "fs_S.h"
#include "callbacks.h"
#include "misc.h"
kern_return_t
netfs_S_dir_lookup (struct protid *dircred,
const_string_t filename,
int flags,
mode_t mode,
retry_type *do_retry,
string_t retry_name,
mach_port_t *retry_port,
mach_msg_type_name_t *retry_port_type)
{
int create;
int excl;
int mustbedir = 0;
int lastcomp = 0;
int newnode = 0;
int nsymlinks = 0;
struct node *dnp, *np;
char *nextname;
char *relpath;
error_t err;
struct protid *newpi = NULL;
struct iouser *user;
if (!dircred)
return EOPNOTSUPP;
create = (flags & O_CREAT);
excl = (flags & O_EXCL);
while (*filename == '/')
filename++;
relpath = strdup (filename);
if (! relpath)
return ENOMEM;
const char *filename_start = filename;
*retry_port_type = MACH_MSG_TYPE_MAKE_SEND;
*do_retry = FS_RETRY_NORMAL;
*retry_name = '\0';
if (*filename == '\0')
{
dnp = 0;
np = dircred->po->np;
pthread_mutex_lock (&np->lock);
netfs_nref (np);
goto gotit;
}
dnp = dircred->po->np;
pthread_mutex_lock (&dnp->lock);
netfs_nref (dnp);
do
{
assert_backtrace (!lastcomp);
nextname = index (filename, '/');
if (nextname)
{
*nextname++ = '\0';
while (*nextname == '/')
nextname++;
if (*nextname == '\0')
{
nextname = 0;
lastcomp = 1;
mustbedir = 1;
create = 0;
}
else
lastcomp = 0;
}
else
lastcomp = 1;
np = 0;
retry_lookup:
if ((dnp == netfs_root_node || dnp == dircred->po->shadow_root)
&& filename[0] == '.' && filename[1] == '.' && filename[2] == '\0')
if (dnp == dircred->po->shadow_root)
{
if (dircred->po->shadow_root_parent == MACH_PORT_NULL)
{
err = 0;
np = dnp;
netfs_nref (np);
}
else
{
*do_retry = FS_RETRY_REAUTH;
*retry_port = dircred->po->shadow_root_parent;
*retry_port_type = MACH_MSG_TYPE_COPY_SEND;
if (lastcomp && mustbedir)
strcpy (retry_name, "/");
else if (!lastcomp)
strcpy (retry_name, nextname);
err = 0;
pthread_mutex_unlock (&dnp->lock);
goto out;
}
}
else if (dircred->po->root_parent != MACH_PORT_NULL)
{
*do_retry = FS_RETRY_REAUTH;
*retry_port = dircred->po->root_parent;
*retry_port_type = MACH_MSG_TYPE_COPY_SEND;
if (lastcomp && mustbedir)
strcpy (retry_name, "/");
else if (!lastcomp)
strcpy (retry_name, nextname);
err = 0;
pthread_mutex_unlock (&dnp->lock);
goto out;
}
else
{
err = 0;
np = dnp;
netfs_nref (np);
}
else
err = netfs_attempt_lookup (dircred->user, dnp, filename, &np);
if (lastcomp && create && excl && !err)
err = EEXIST;
if (lastcomp && create && err == ENOENT)
{
mode &= ~(S_IFMT | S_ISPARE | S_ISVTX);
mode |= S_IFREG;
pthread_mutex_lock (&dnp->lock);
err = netfs_attempt_create_file (dircred->user, dnp,
filename, mode, &np);
if (err == EEXIST && !excl)
{
pthread_mutex_lock (&dnp->lock);
goto retry_lookup;
}
newnode = 1;
}
if (err)
goto out;
err = netfs_validate_stat (np, dircred->user);
if (err)
goto out;
if ((((flags & O_NOTRANS) == 0) || !lastcomp || mustbedir)
&& ((np->nn_translated & S_IPTRANS)
|| S_ISFIFO (np->nn_translated)
|| S_ISCHR (np->nn_translated)
|| S_ISBLK (np->nn_translated)
|| fshelp_translated (&np->transbox)))
{
mach_port_t dirport;
err = iohelp_create_empty_iouser (&user);
if (! err)
{
newpi = netfs_make_protid (netfs_make_peropen (dnp, 0,
dircred->po),
user);
if (! newpi)
{
err = errno;
iohelp_free_iouser (user);
}
}
if (! err)
{
struct fshelp_stat_cookie2 cookie = {
.statp = &np->nn_stat,
.modep = &np->nn_translated,
.next = dircred->po,
};
dirport = ports_get_send_right (newpi);
err = fshelp_fetch_root (&np->transbox,
&cookie,
dirport,
dircred->user,
lastcomp ? flags : 0,
((np->nn_translated & S_IPTRANS)
? _netfs_translator_callback1
: fshelp_short_circuited_callback1),
_netfs_translator_callback2,
do_retry, retry_name, retry_port);
mach_port_deallocate (mach_task_self (), dirport);
}
if (err != ENOENT)
{
*retry_port_type = MACH_MSG_TYPE_MOVE_SEND;
if (!err)
{
char *end = strchr (retry_name, '\0');
if (mustbedir)
*end++ = '/';
else if (!lastcomp) {
if (end != retry_name)
*end++ = '/';
strcpy (end, nextname);
}
}
{
char *translator_path = strdupa (relpath);
char *end;
char *complete_path;
struct port_info *notify_port;
if (nextname != NULL)
{
end = nextname;
while (*end != 0)
end--;
translator_path[end - filename_start] = '\0';
}
end = &translator_path[strlen (translator_path) - 1];
while (*end == '/' && end >= translator_path)
*end = '\0', end--;
if (dircred->po->path == NULL
|| !strcmp (dircred->po->path,"."))
complete_path = translator_path;
else
asprintf (&complete_path, "%s/%s", dircred->po->path,
translator_path);
notify_port = newpi->pi.bucket->notify_port;
err = fshelp_set_active_translator (notify_port,
complete_path,
&np->transbox);
if (complete_path != translator_path)
free(complete_path);
if (err)
{
ports_port_deref (newpi);
goto out;
}
}
ports_port_deref (newpi);
goto out;
}
ports_port_deref (newpi);
err = 0;
}
if (S_ISLNK (np->nn_translated)
&& (!lastcomp
|| mustbedir
|| !(flags & (O_NOLINK|O_NOTRANS))))
{
size_t nextnamelen, newnamelen, linklen;
char *linkbuf;
if (nsymlinks++ > netfs_maxsymlinks)
{
err = ELOOP;
goto out;
}
linklen = np->nn_stat.st_size;
nextnamelen = nextname ? strlen (nextname) + 1 : 0;
newnamelen = nextnamelen + linklen + 1 + 1;
linkbuf = alloca (newnamelen);
err = netfs_attempt_readlink (dircred->user, np, linkbuf);
if (err)
goto out;
if (nextname)
{
linkbuf[linklen] = '/';
memcpy (linkbuf + linklen + 1, nextname,
nextnamelen - 1);
}
if (mustbedir)
{
linkbuf[nextnamelen + linklen] = '/';
linkbuf[nextnamelen + linklen + 1] = '\0';
}
else
linkbuf[nextnamelen + linklen] = '\0';
if (linkbuf[0] == '/')
{
*do_retry = FS_RETRY_MAGICAL;
*retry_port = MACH_PORT_NULL;
strcpy (retry_name, linkbuf);
goto out;
}
filename = linkbuf;
mustbedir = 0;
if (lastcomp)
{
lastcomp = 0;
create = 0;
}
netfs_nput (np);
pthread_mutex_lock (&dnp->lock);
np = 0;
}
else
{
filename = nextname;
netfs_nrele (dnp);
if (lastcomp)
dnp = 0;
else
{
dnp = np;
np = 0;
}
}
}
while (filename && *filename);
gotit:
if (mustbedir || (flags & O_DIRECTORY))
{
err = netfs_validate_stat (np, dircred->user);
if (err)
goto out;
if (!S_ISDIR (np->nn_stat.st_mode))
{
err = ENOTDIR;
goto out;
}
}
err = netfs_check_open_permissions (dircred->user, np,
flags, newnode);
if (err)
goto out;
flags &= ~OPENONLY_STATE_MODES;
err = iohelp_dup_iouser (&user, dircred->user);
if (err)
goto out;
newpi = netfs_make_protid (netfs_make_peropen (np, flags, dircred->po),
user);
if (! newpi)
{
iohelp_free_iouser (user);
err = errno;
goto out;
}
mach_port_t rendezvous = MACH_PORT_NULL;
struct flock64 lock =
{
l_start: 0,
l_len: 0,
l_whence: SEEK_SET
};
if (flags & O_EXLOCK)
{
lock.l_type = F_WRLCK;
err = fshelp_rlock_tweak (&np->userlock, &np->lock,
&newpi->po->lock_status, flags, 0, 0,
F_SETLK64, &lock, rendezvous);
}
else if (flags & O_SHLOCK)
{
lock.l_type = F_RDLCK;
err = fshelp_rlock_tweak (&np->userlock, &np->lock,
&newpi->po->lock_status, flags, 0, 0,
F_SETLK64, &lock, rendezvous);
}
if (! err)
{
free (newpi->po->path);
if (dircred->po->path == NULL || !strcmp (dircred->po->path,"."))
{
newpi->po->path = relpath;
relpath = NULL;
}
else
{
newpi->po->path = NULL;
asprintf (&newpi->po->path, "%s/%s", dircred->po->path, relpath);
}
if (! newpi->po->path)
err = errno;
*retry_port = ports_get_right (newpi);
ports_port_deref (newpi);
}
out:
if (np)
netfs_nput (np);
if (dnp)
netfs_nrele (dnp);
free (relpath);
return err;
}