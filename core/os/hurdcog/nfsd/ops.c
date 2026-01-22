#include <hurd/io.h>
#include <hurd/fs.h>
#include <fcntl.h>
#include <hurd/paths.h>
#include <hurd.h>
#include <dirent.h>
#include <string.h>
#include <sys/mman.h>
#include "nfsd.h"
#include "../nfs/mount.h"
#include <rpc/xdr.h>
#include <rpc/pmap_prot.h>
static error_t
op_null (struct cache_handle *c,
int *p,
int **reply,
int version)
{
return 0;
}
static error_t
op_getattr (struct cache_handle *c,
int *p,
int **reply,
int version)
{
struct stat st;
error_t err;
err = io_stat (c->port, &st);
if (!err)
*reply = encode_fattr (*reply, &st, version);
return err;
}
static error_t
complete_setattr (mach_port_t port,
int *p)
{
uid_t uid, gid;
off_t size;
struct timespec atime, mtime;
struct stat st;
error_t err;
err = io_stat (port, &st);
if (err)
return err;
uid = ntohl (*p);
p++;
gid = ntohl (*p);
p++;
if (uid == -1)
uid = st.st_uid;
if (gid == -1)
gid = st.st_gid;
if (uid != st.st_uid || gid != st.st_gid)
err = file_chown (port, uid, gid);
if (err)
return err;
size = ntohl (*p);
p++;
if (size != -1 && size != st.st_size)
err = file_set_size (port, size);
if (err)
return err;
atime.tv_sec = ntohl (*p);
p++;
atime.tv_nsec = ntohl (*p) * 1000;
p++;
mtime.tv_sec = ntohl (*p);
p++;
mtime.tv_nsec = ntohl (*p) * 1000;
p++;
if (atime.tv_sec != -1 && atime.tv_nsec == -1)
atime.tv_nsec = 0;
if (mtime.tv_sec != -1 && mtime.tv_nsec == -1)
mtime.tv_nsec = 0;
if (atime.tv_nsec == -1)
atime.tv_sec = st.st_atim.tv_sec;
if (atime.tv_nsec == -1)
atime.tv_nsec = st.st_atim.tv_nsec;
if (mtime.tv_sec == -1)
mtime.tv_sec = st.st_mtim.tv_sec;
if (mtime.tv_nsec == -1)
mtime.tv_nsec = st.st_mtim.tv_nsec;
if (atime.tv_sec != st.st_atim.tv_sec
|| atime.tv_nsec != st.st_atim.tv_nsec
|| mtime.tv_sec != st.st_mtim.tv_sec
|| mtime.tv_nsec != st.st_mtim.tv_nsec)
{
#ifdef HAVE_FILE_UTIMENS
err = file_utimens (port, atime, mtime);
if (err == MIG_BAD_ID || err == EOPNOTSUPP)
#endif
{
time_value_t atim, mtim;
TIMESPEC_TO_TIME_VALUE (&atim, &atime);
TIMESPEC_TO_TIME_VALUE (&mtim, &mtime);
err = file_utimes (port, atim, mtim);
}
}
return err;
}
static error_t
op_setattr (struct cache_handle *c,
int *p,
int **reply,
int version)
{
error_t err = 0;
mode_t mode;
struct stat st;
mode = ntohl (*p);
p++;
if (mode != -1)
err = file_chmod (c->port, mode);
if (!err)
err = complete_setattr (c->port, p);
if (!err)
err = io_stat (c->port, &st);
if (err)
return err;
*reply = encode_fattr (*reply, &st, version);
return 0;
}
static error_t
op_lookup (struct cache_handle *c,
int *p,
int **reply,
int version)
{
error_t err;
char *name;
retry_type do_retry;
char retry_name [1024];
mach_port_t newport;
struct cache_handle *newc;
struct stat st;
decode_name (p, &name);
err = dir_lookup (c->port, name, O_NOTRANS, 0, &do_retry, retry_name,
&newport);
free (name);
if (!err
&& (do_retry != FS_RETRY_NORMAL
|| retry_name[0] != '\0'))
err = EACCES;
if (!err)
err = io_stat (newport, &st);
if (err)
return err;
newc = create_cached_handle (c->handle.fs, c, newport);
if (!newc)
return ESTALE;
*reply = encode_fhandle (*reply, newc->handle.array);
*reply = encode_fattr (*reply, &st, version);
return 0;
}
static error_t
op_readlink (struct cache_handle *c,
int *p,
int **reply,
int version)
{
char buf[2048], *transp = buf;
mach_msg_type_number_t len = sizeof (buf);
error_t err;
err = file_get_translator (c->port, &transp, &len);
if (err)
{
if (transp != buf)
munmap (transp, len);
return err;
}
if (len < sizeof (_HURD_SYMLINK)
|| memcmp (transp, _HURD_SYMLINK, sizeof (_HURD_SYMLINK)))
return EINVAL;
transp += sizeof (_HURD_SYMLINK);
*reply = encode_string (*reply, transp);
if (transp != buf)
munmap (transp, len);
return 0;
}
static size_t
count_read_buffersize (int *p, int version)
{
p++;
return ntohl (*p);
}
static error_t
op_read (struct cache_handle *c,
int *p,
int **reply,
int version)
{
off_t offset;
size_t count;
char buf[2048], *bp = buf;
mach_msg_type_number_t buflen = sizeof (buf);
struct stat st;
error_t err;
offset = ntohl (*p);
p++;
count = ntohl (*p);
p++;
err = io_read (c->port, &bp, &buflen, offset, count);
if (err)
{
if (bp != buf)
munmap (bp, buflen);
return err;
}
err = io_stat (c->port, &st);
if (err)
return err;
*reply = encode_fattr (*reply, &st, version);
*reply = encode_data (*reply, bp, buflen);
if (bp != buf)
munmap (bp, buflen);
return 0;
}
static error_t
op_write (struct cache_handle *c,
int *p,
int **reply,
int version)
{
off_t offset;
size_t count;
error_t err;
vm_size_t amt;
char *bp;
struct stat st;
p++;
offset = ntohl (*p);
p++;
p++;
count = ntohl (*p);
p++;
bp = (char *) *reply;
while (count)
{
err = io_write (c->port, bp, count, offset, &amt);
if (err)
return err;
if (amt == 0)
return EIO;
count -= amt;
bp += amt;
offset += amt;
}
file_sync (c->port, 1, 0);
err = io_stat (c->port, &st);
if (err)
return err;
*reply = encode_fattr (*reply, &st, version);
return 0;
}
static error_t
op_create (struct cache_handle *c,
int *p,
int **reply,
int version)
{
error_t err;
char *name;
retry_type do_retry;
char retry_name [1024];
mach_port_t newport;
struct cache_handle *newc;
struct stat st;
mode_t mode;
int statchanged = 0;
off_t size;
p = decode_name (p, &name);
mode = ntohl (*p);
p++;
err = dir_lookup (c->port, name, O_NOTRANS | O_CREAT | O_TRUNC, mode,
&do_retry, retry_name, &newport);
if (!err
&& (do_retry != FS_RETRY_NORMAL
|| retry_name[0] != '\0'))
err = EACCES;
if (err)
return err;
if (!err)
err = io_stat (newport, &st);
if (err)
goto errout;
p++, p++;
size = ntohl (*p);
p++;
if (size != -1 && size != st.st_size)
{
err = file_set_size (newport, size);
statchanged = 1;
}
if (err)
goto errout;
if (statchanged)
err = io_stat (newport, &st);
if (err)
{
errout:
dir_unlink (c->port, name);
free (name);
return err;
}
free (name);
newc = create_cached_handle (c->handle.fs, c, newport);
if (!newc)
return ESTALE;
*reply = encode_fhandle (*reply, newc->handle.array);
*reply = encode_fattr (*reply, &st, version);
return 0;
}
static error_t
op_remove (struct cache_handle *c,
int *p,
int **reply,
int version)
{
error_t err;
char *name;
decode_name (p, &name);
err = dir_unlink (c->port, name);
free (name);
return err;
}
static error_t
op_rename (struct cache_handle *fromc,
int *p,
int **reply,
int version)
{
struct cache_handle *toc;
char *fromname, *toname;
error_t err = 0;
p = decode_name (p, &fromname);
p = lookup_cache_handle (p, &toc, fromc->ids);
decode_name (p, &toname);
if (!toc)
err = ESTALE;
if (!err)
err = dir_rename (fromc->port, fromname, toc->port, toname, 0);
free (fromname);
free (toname);
return err;
}
static error_t
op_link (struct cache_handle *filec,
int *p,
int **reply,
int version)
{
struct cache_handle *dirc;
char *name;
error_t err = 0;
p = lookup_cache_handle (p, &dirc, filec->ids);
decode_name (p, &name);
if (!dirc)
err = ESTALE;
if (!err)
err = dir_link (dirc->port, filec->port, name, 1);
free (name);
return err;
}
static error_t
op_symlink (struct cache_handle *c,
int *p,
int **reply,
int version)
{
char *name, *target;
error_t err;
mode_t mode;
file_t newport = MACH_PORT_NULL;
size_t len;
char *buf;
p = decode_name (p, &name);
p = decode_name (p, &target);
mode = ntohl (*p);
p++;
if (mode == -1)
mode = 0777;
len = strlen (target) + 1;
buf = alloca (sizeof (_HURD_SYMLINK) + len);
memcpy (buf, _HURD_SYMLINK, sizeof (_HURD_SYMLINK));
memcpy (buf + sizeof (_HURD_SYMLINK), target, len);
err = dir_mkfile (c->port, O_WRITE, mode, &newport);
if (!err)
err = file_set_translator (newport,
FS_TRANS_EXCL|FS_TRANS_SET,
FS_TRANS_EXCL|FS_TRANS_SET, 0,
buf, sizeof (_HURD_SYMLINK) + len,
MACH_PORT_NULL, MACH_MSG_TYPE_COPY_SEND);
if (!err)
err = dir_link (c->port, newport, name, 1);
free (name);
free (target);
if (newport != MACH_PORT_NULL)
mach_port_deallocate (mach_task_self (), newport);
return err;
}
static error_t
op_mkdir (struct cache_handle *c,
int *p,
int **reply,
int version)
{
char *name;
mode_t mode;
retry_type do_retry;
char retry_name [1024];
mach_port_t newport;
struct stat st;
struct cache_handle *newc;
error_t err;
p = decode_name (p, &name);
mode = ntohl (*p);
p++;
err = dir_mkdir (c->port, name, mode);
if (err)
{
free (name);
return err;
}
err = dir_lookup (c->port, name, O_NOTRANS, 0, &do_retry,
retry_name, &newport);
free (name);
if (!err
&& (do_retry != FS_RETRY_NORMAL
|| retry_name[0] != '\0'))
err = EACCES;
if (err)
return err;
if (!err)
err = io_stat (newport, &st);
if (err)
return err;
newc = create_cached_handle (c->handle.fs, c, newport);
if (!newc)
return ESTALE;
*reply = encode_fhandle (*reply, newc->handle.array);
*reply = encode_fattr (*reply, &st, version);
return 0;
}
static error_t
op_rmdir (struct cache_handle *c,
int *p,
int **reply,
int version)
{
char *name;
error_t err;
decode_name (p, &name);
err = dir_rmdir (c->port, name);
free (name);
return err;
}
static error_t
op_readdir (struct cache_handle *c,
int *p,
int **reply,
int version)
{
int cookie;
unsigned count;
error_t err;
char *buf;
struct dirent *dp;
mach_msg_type_number_t bufsize;
int nentries;
int i;
int *replystart;
int *r;
cookie = ntohl (*p);
p++;
count = ntohl (*p);
p++;
buf = (char *) 0;
bufsize = 0;
err = dir_readdir (c->port, &buf, &bufsize, cookie, -1, count, &nentries);
if (err)
{
if (buf)
munmap (buf, bufsize);
return err;
}
r = *reply;
if (nentries == 0)
{
*(r++) = htonl (0);
*(r++) = htonl (1);
}
else
{
for (i = 0, dp = (struct dirent *) buf, replystart = *reply;
((char *)dp < buf + bufsize
&& i < nentries
&& (char *)reply < (char *)replystart + count);
i++, dp = (struct dirent *) ((char *)dp + dp->d_reclen))
{
*(r++) = htonl (1);
*(r++) = htonl (dp->d_ino);
r = encode_string (r, dp->d_name);
*(r++) = htonl (i + cookie + 1);
}
*(r++) = htonl (0);
*(r++) = htonl (0);
}
*reply = r;
if (buf)
munmap (buf, bufsize);
return 0;
}
static size_t
count_readdir_buffersize (int *p, int version)
{
p++;
return ntohl (*p);
}
static error_t
op_statfs (struct cache_handle *c,
int *p,
int **reply,
int version)
{
struct statfs st;
error_t err;
err = file_statfs (c->port, &st);
if (!err)
*reply = encode_statfs (*reply, &st);
return err;
}
static error_t
op_mnt (struct cache_handle *c,
int *p,
int **reply,
int version)
{
file_t root;
struct cache_handle *newc;
char *name;
decode_name (p, &name);
root = file_name_lookup (name, 0, 0);
if (!root)
{
free (name);
return errno;
}
newc = create_cached_handle (enter_filesystem (name, root), c, root);
free (name);
if (!newc)
return ESTALE;
*reply = encode_fhandle (*reply, newc->handle.array);
return 0;
}
static error_t
op_getport (struct cache_handle *c,
int *p,
int **reply,
int version)
{
int prog, vers, prot;
prog = ntohl (*p);
p++;
vers = ntohl (*p);
p++;
prot = ntohl (*p);
p++;
if (prot != IPPROTO_UDP)
*(*reply)++ = htonl (0);
else if ((prog == MOUNTPROG && vers == MOUNTVERS)
|| (prog == NFS_PROGRAM && vers == NFS_VERSION))
*(*reply)++ = htonl (NFS_PORT);
else if (prog == PMAPPROG && vers == PMAPVERS)
*(*reply)++ = htonl (PMAPPORT);
else
*(*reply)++ = 0;
return 0;
}
struct proctable nfs2table =
{
NFS2PROC_NULL,
NFS2PROC_STATFS,
{
{ op_null, 0, 0, 0},
{ op_getattr, 0, 1, 1},
{ op_setattr, 0, 1, 1},
{ 0, 0, 0, 0 },
{ op_lookup, 0, 1, 1},
{ op_readlink, 0, 1, 1},
{ op_read, count_read_buffersize, 1, 1},
{ 0, 0, 0, 0 },
{ op_write, 0, 1, 1},
{ op_create, 0, 1, 1},
{ op_remove, 0, 1, 1},
{ op_rename, 0, 1, 1},
{ op_link, 0, 1, 1},
{ op_symlink, 0, 1, 1},
{ op_mkdir, 0, 1, 1},
{ op_rmdir, 0, 1, 1},
{ op_readdir, count_readdir_buffersize, 1, 1},
{ op_statfs, 0, 1, 1},
}
};
struct proctable mounttable =
{
MOUNTPROC_NULL,
MOUNTPROC_EXPORT,
{
{ op_null, 0, 0, 0},
{ op_mnt, 0, 0, 1},
{ 0, 0, 0, 0},
{ op_null, 0, 0, 0},
{ op_null, 0, 0, 0},
{ 0, 0, 0, 0},
}
};
struct proctable pmaptable =
{
PMAPPROC_NULL,
PMAPPROC_CALLIT,
{
{ op_null, 0, 0, 0},
{ 0, 0, 0, 0},
{ 0, 0, 0, 0},
{ op_getport, 0, 0, 0},
{ 0, 0, 0, 0},
{ 0, 0, 0, 0},
}
};