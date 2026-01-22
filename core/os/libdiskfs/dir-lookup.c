#include <stdio.h>
#include <fcntl.h>
#include <string.h>
#include <sys/file.h>
#include <hurd/fshelp.h>
#include <hurd/fsys.h>
#include <hurd/paths.h>
#include "priv.h"
#include "fs_S.h"
kern_return_t
diskfs_S_dir_lookup (struct protid *dircred,
const_string_t filename,
int flags,
mode_t mode,
retry_type *do_retry,
string_t retry_name,
mach_port_t *retry_port,
mach_msg_type_name_t *retry_port_type)
{
struct node *dnp;
struct node *np;
int nsymlinks = 0;
char *nextname;
char *relpath;
int nextnamelen;
error_t err = 0;
char *pathbuf = 0;
int newnamelen;
int create, excl;
int lastcomp = 0;
int newnode = 0;
struct dirstat *ds = 0;
int mustbedir = 0;
mach_msg_type_name_t amt;
int type;
struct protid *newpi = 0;
struct peropen *newpo = 0;
int orig_flags = flags;
if (!dircred)
return EOPNOTSUPP;
flags &= O_HURD;
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
diskfs_nref (np);
goto gotit;
}
dnp = dircred->po->np;
pthread_mutex_lock (&dnp->lock);
diskfs_nref (dnp);
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
if (lastcomp && create)
{
if (!ds)
ds = alloca (diskfs_dirstat_size);
err = diskfs_lookup (dnp, filename, CREATE, &np, ds, dircred);
}
else
err = diskfs_lookup (dnp, filename, LOOKUP, &np, 0, dircred);
if (lastcomp && create && excl && (!err || err == EAGAIN))
err = EEXIST;
if (err == EAGAIN)
{
if (dnp == dircred->po->shadow_root)
{
if (dircred->po->shadow_root_parent == MACH_PORT_NULL)
{
err = 0;
np = dnp;
diskfs_nref (np);
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
goto out;
}
else
{
err = 0;
np = dnp;
diskfs_nref (np);
}
}
if (lastcomp && create)
{
if (err == ENOENT)
{
mode &= ~(S_IFMT | S_ISPARE | S_ISVTX | S_ITRANS);
mode |= S_IFREG;
err = diskfs_create_node (dnp, filename, mode, &np, dircred, ds);
if (diskfs_synchronous)
{
diskfs_file_update (dnp, 1);
diskfs_file_update (np, 1);
}
newnode = 1;
}
else
diskfs_drop_dirstat (dnp, ds);
}
if (err)
goto out;
if ((((flags & O_NOTRANS) == 0) || !lastcomp || mustbedir)
&& ((np->dn_stat.st_mode & S_IPTRANS)
|| S_ISFIFO (np->dn_stat.st_mode)
|| S_ISCHR (np->dn_stat.st_mode)
|| S_ISBLK (np->dn_stat.st_mode)
|| fshelp_translated (&np->transbox)))
{
mach_port_t dirport;
struct iouser *user;
err = iohelp_create_empty_iouser (&user);
if (! err)
{
err = diskfs_make_peropen (dnp, 0, dircred->po, &newpo);
if (! err)
{
err = diskfs_create_protid (newpo, user, &newpi);
if (! err)
newpo = 0;
}
iohelp_free_iouser (user);
}
if (err)
goto out;
dirport = ports_get_send_right (newpi);
if (np != dnp)
pthread_mutex_unlock (&dnp->lock);
struct fshelp_stat_cookie2 cookie = {
.statp = &np->dn_stat,
.modep = &np->dn_stat.st_mode,
.next = dircred->po,
};
err = fshelp_fetch_root (&np->transbox,
&cookie,
dirport,
dircred->user,
lastcomp ? flags : 0,
((np->dn_stat.st_mode & S_IPTRANS)
? _diskfs_translator_callback1
: fshelp_short_circuited_callback1),
_diskfs_translator_callback2,
do_retry, retry_name, retry_port);
mach_port_deallocate (mach_task_self (), dirport);
if (err != ENOENT)
{
*retry_port_type = MACH_MSG_TYPE_MOVE_SEND;
if (!err)
{
char *end = strchr (retry_name, '\0');
char *translator_path = strdupa (relpath);
char *complete_path;
struct port_info *notify_port;
if (mustbedir)
*end++ = '/';
else if (!lastcomp) {
if (end != retry_name)
*end++ = '/';
strcpy (end, nextname);
}
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
goto out;
}
goto out;
}
ports_port_deref (newpi);
newpi = NULL;
err = 0;
if (np != dnp)
{
if (!strcmp (filename, ".."))
pthread_mutex_lock (&dnp->lock);
else
{
if (pthread_mutex_trylock (&dnp->lock))
{
pthread_mutex_unlock (&np->lock);
pthread_mutex_lock (&dnp->lock);
pthread_mutex_lock (&np->lock);
}
}
}
}
if (S_ISLNK (np->dn_stat.st_mode)
&& (!lastcomp
|| mustbedir
|| !(flags & (O_NOLINK|O_NOTRANS))))
{
if (nsymlinks++ > diskfs_maxsymlinks)
{
err = ELOOP;
goto out;
}
nextnamelen = nextname ? strlen (nextname) + 1 : 0;
newnamelen = nextnamelen + np->dn_stat.st_size + 1 + 1;
pathbuf = alloca (newnamelen);
if (diskfs_read_symlink_hook)
err = (*diskfs_read_symlink_hook)(np, pathbuf);
if (!diskfs_read_symlink_hook || err == EINVAL)
{
err = diskfs_node_rdwr (np, pathbuf,
0, np->dn_stat.st_size, 0,
dircred, &amt);
if (!err)
assert_backtrace (amt == np->dn_stat.st_size);
}
if (err)
goto out;
if (np->dn_stat.st_size == 0)
filename = nextname;
else
{
if (nextname)
{
pathbuf[np->dn_stat.st_size] = '/';
memcpy (pathbuf + np->dn_stat.st_size + 1,
nextname, nextnamelen - 1);
}
if (mustbedir)
{
pathbuf[nextnamelen + np->dn_stat.st_size] = '/';
pathbuf[nextnamelen + np->dn_stat.st_size + 1] = '\0';
}
else
pathbuf[nextnamelen + np->dn_stat.st_size] = '\0';
if (pathbuf[0] == '/')
{
*do_retry = FS_RETRY_MAGICAL;
*retry_port = MACH_PORT_NULL;
strcpy (retry_name, pathbuf);
goto out;
}
filename = pathbuf;
mustbedir = 0;
}
if (lastcomp)
lastcomp = 0;
diskfs_nput (np);
np = 0;
if (filename == 0)
{
np = dnp;
dnp = 0;
break;
}
}
else
{
filename = nextname;
if (np == dnp)
diskfs_nrele (dnp);
else
diskfs_nput (dnp);
if (!lastcomp)
{
dnp = np;
np = 0;
}
else
dnp = 0;
}
}
while (filename && *filename);
gotit:
type = np->dn_stat.st_mode & S_IFMT;
if ((mustbedir || orig_flags & O_DIRECTORY) && type != S_IFDIR)
{
err = ENOTDIR;
goto out;
}
if (!newnode)
{
if ((type == S_IFSOCK || type == S_IFBLK || type == S_IFCHR ||
type == S_IFIFO)
&& (flags & (O_READ|O_WRITE|O_EXEC)))
err = EACCES;
if (!err && type == S_IFLNK && (flags & (O_WRITE|O_EXEC)))
err = ELOOP;
if (!err && (flags & O_READ))
err = fshelp_access (&np->dn_stat, S_IREAD, dircred->user);
if (!err && (flags & O_EXEC))
err = fshelp_access (&np->dn_stat, S_IEXEC, dircred->user);
if (!err && (flags & O_WRITE))
{
if (type == S_IFDIR)
err = EISDIR;
else if (diskfs_check_readonly ())
err = EROFS;
else
err = fshelp_access (&np->dn_stat, S_IWRITE, dircred->user);
}
if (err)
goto out;
}
if ((flags & O_NOATIME)
&& (fshelp_isowner (&np->dn_stat, dircred->user) == EPERM))
flags &= ~O_NOATIME;
err = diskfs_make_peropen (np, (flags &~OPENONLY_STATE_MODES),
dircred->po, &newpo);
if (! err)
err = diskfs_create_protid (newpo, dircred->user, &newpi);
if (! err)
{
newpo = 0;
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
newpi = 0;
}
out:
if (np)
{
if (dnp == np)
diskfs_nrele (np);
else
diskfs_nput (np);
}
if (dnp)
diskfs_nput (dnp);
if (newpi)
ports_port_deref (newpi);
if (newpo)
diskfs_release_peropen (newpo);
free (relpath);
return err;
}