#include "nfs.h"
#include <string.h>
#include <netinet/in.h>
#include <stdbool.h>
#include <stdio.h>
#include <sys/sysmacros.h>
mode_t
nfs_mode_to_hurd_mode (int type, int mode)
{
int hurdmode;
switch (type)
{
case NFDIR:
hurdmode = S_IFDIR;
break;
case NFCHR:
hurdmode = S_IFCHR;
break;
case NFBLK:
hurdmode = S_IFBLK;
break;
case NFREG:
hurdmode = S_IFREG;
break;
case NFLNK:
hurdmode = S_IFLNK;
break;
case NFSOCK:
hurdmode = S_IFSOCK;
break;
default:
if (protocol_version == 2)
switch (type)
{
case NF2NON:
case NF2BAD:
default:
hurdmode = S_IFREG;
break;
case NF2FIFO:
hurdmode = S_IFIFO;
break;
}
else
switch (type)
{
case NF3FIFO:
hurdmode = S_IFIFO;
break;
default:
hurdmode = S_IFREG;
break;
}
break;
}
hurdmode |= mode & ~NFSMODE_FMT;
return hurdmode;
}
int
hurd_mode_to_nfs_mode (mode_t mode)
{
return mode & 07777;
}
int
hurd_mode_to_nfs_type (mode_t mode)
{
switch (mode & S_IFMT)
{
case S_IFDIR:
return NFDIR;
case S_IFCHR:
default:
return NFCHR;
case S_IFBLK:
return NFBLK;
case S_IFREG:
return NFREG;
case S_IFLNK:
return NFLNK;
case S_IFSOCK:
return NFSOCK;
case S_IFIFO:
return protocol_version == 2 ? NF2FIFO : NF3FIFO;
}
}
int *
xdr_encode_fhandle (int *p, const struct fhandle *fhandle)
{
if (protocol_version == 2)
{
memcpy (p, fhandle->data, NFS2_FHSIZE);
return p + INTSIZE (NFS2_FHSIZE);
}
else
return xdr_encode_data (p, fhandle->data, fhandle->size);
}
int *
xdr_encode_data (int *p, const char *data, size_t len)
{
int nints = INTSIZE (len);
p[nints] = 0;
*(p++) = htonl (len);
memcpy (p, data, len);
return p + nints;
}
int *
xdr_encode_64bit (int *p, uint64_t n)
{
*(p++) = htonl ((n & 0xffffffff00000000ULL) >> 32);
*(p++) = htonl (n & 0xffffffff);
return p;
}
int *
xdr_encode_string (int *p, const char *string)
{
return xdr_encode_data (p, string, strlen (string));
}
static inline bool
nfs_sattr3_size_needed(mode_t mode, size_t sz)
{
if (sz == 0)
{
switch (hurd_mode_to_nfs_type (mode))
{
case NFSOCK:
case NF3FIFO:
return false;
default:
break;
}
}
return true;
}
int *
xdr_encode_sattr_mode (int *p, mode_t mode)
{
if (protocol_version == 2)
{
*(p++) = htonl (hurd_mode_to_nfs_mode (mode));
*(p++) = -1;
*(p++) = -1;
*(p++) = -1;
*(p++) = -1;
*(p++) = -1;
*(p++) = -1;
*(p++) = -1;
}
else
{
*(p++) = htonl (1);
*(p++) = htonl (hurd_mode_to_nfs_mode (mode));
*(p++) = 0;
*(p++) = 0;
*(p++) = 0;
*(p++) = DONT_CHANGE;
*(p++) = DONT_CHANGE;
}
return p;
}
int *
xdr_encode_sattr_ids (int *p, u_int uid, u_int gid)
{
if (protocol_version == 2)
{
*(p++) = -1;
*(p++) = htonl (uid);
*(p++) = htonl (gid);
*(p++) = -1;
*(p++) = -1;
*(p++) = -1;
*(p++) = -1;
*(p++) = -1;
}
else
{
*(p++) = 0;
*(p++) = htonl (1);
*(p++) = htonl (uid);
*(p++) = htonl (1);
*(p++) = htonl (gid);
*(p++) = 0;
*(p++) = DONT_CHANGE;
*(p++) = DONT_CHANGE;
}
return p;
}
int *
xdr_encode_sattr_size (int *p, off_t size)
{
if (protocol_version == 2)
{
*(p++) = -1;
*(p++) = -1;
*(p++) = -1;
*(p++) = htonl (size);
*(p++) = -1;
*(p++) = -1;
*(p++) = -1;
*(p++) = -1;
}
else
{
*(p++) = 0;
*(p++) = 0;
*(p++) = 0;
*(p++) = htonl (1);
p = xdr_encode_64bit (p, size);
*(p++) = DONT_CHANGE;
*(p++) = DONT_CHANGE;
}
return p;
}
int *
xdr_encode_sattr_times (int *p, const struct timespec *atime, const struct timespec *mtime)
{
if (protocol_version == 2)
{
*(p++) = -1;
*(p++) = -1;
*(p++) = -1;
*(p++) = -1;
if (atime)
{
*(p++) = htonl (atime->tv_sec);
*(p++) = htonl (atime->tv_nsec / 1000);
}
else
{
*(p++) = -1;
*(p++) = -1;
}
if (mtime)
{
*(p++) = htonl (mtime->tv_sec);
*(p++) = htonl (mtime->tv_nsec / 1000);
}
else
{
*(p++) = -1;
*(p++) = -1;
}
}
else
{
*(p++) = 0;
*(p++) = 0;
*(p++) = 0;
*(p++) = 0;
if (atime)
{
*(p++) = htonl (SET_TO_CLIENT_TIME);
*(p++) = htonl (atime->tv_sec);
*(p++) = htonl (atime->tv_nsec);
}
else
*(p++) = DONT_CHANGE;
if (mtime)
{
*(p++) = htonl (SET_TO_CLIENT_TIME);
*(p++) = htonl (mtime->tv_sec);
*(p++) = htonl (mtime->tv_nsec);
}
else
*(p++) = DONT_CHANGE;
}
return p;
}
int *
xdr_encode_create_state (int *p,
mode_t mode,
uid_t owner)
{
if (protocol_version == 2)
{
*(p++) = htonl (hurd_mode_to_nfs_mode (mode));
*(p++) = htonl (owner);
*(p++) = -1;
*(p++) = 0;
*(p++) = -1;
*(p++) = -1;
*(p++) = -1;
*(p++) = -1;
}
else
{
*(p++) = htonl (1);
*(p++) = htonl (hurd_mode_to_nfs_mode (mode));
*(p++) = htonl (1);
*(p++) = htonl (owner);
*(p++) = 0;
*(p++) = htonl (1);
p = xdr_encode_64bit (p, 0);
*(p++) = htonl (SET_TO_SERVER_TIME);
*(p++) = htonl (SET_TO_SERVER_TIME);
}
return p;
}
int *
xdr_encode_sattr_stat (int *p,
const struct stat *st)
{
if (protocol_version == 2)
{
*(p++) = htonl (hurd_mode_to_nfs_mode (st->st_mode));
*(p++) = htonl (st->st_uid);
*(p++) = htonl (st->st_gid);
*(p++) = htonl (st->st_size);
*(p++) = htonl (st->st_atim.tv_sec);
*(p++) = htonl (st->st_atim.tv_nsec / 1000);
*(p++) = htonl (st->st_mtim.tv_sec);
*(p++) = htonl (st->st_mtim.tv_nsec / 1000);
}
else
{
bool needs_size = nfs_sattr3_size_needed (st->st_mode, st->st_size);
*(p++) = htonl (1);
*(p++) = htonl (hurd_mode_to_nfs_mode (st->st_mode));
*(p++) = htonl (1);
*(p++) = htonl (st->st_uid);
*(p++) = htonl (1);
*(p++) = htonl (st->st_gid);
*(p++) = htonl (needs_size);
if (needs_size)
p = xdr_encode_64bit (p, st->st_size);
*(p++) = htonl (SET_TO_CLIENT_TIME);
*(p++) = htonl (st->st_atim.tv_sec);
*(p++) = htonl (st->st_atim.tv_nsec);
*(p++) = htonl (SET_TO_CLIENT_TIME);
*(p++) = htonl (st->st_mtim.tv_sec);
*(p++) = htonl (st->st_mtim.tv_nsec);
}
return p;
}
int *
xdr_decode_64bit (int *p, uint64_t *n)
{
uint64_t high, low;
high = ntohl (*p);
p++;
low = ntohl (*p);
p++;
*n = (((uint64_t)(high & 0xffffffff)) << 32) | (low & 0xffffffff);
return p;
}
int *
xdr_decode_fhandle (int *p, struct node **npp)
{
struct fhandle handle;
if (protocol_version == 2)
handle.size = NFS2_FHSIZE;
else
{
handle.size = ntohl (*p);
p++;
}
memcpy (&handle.data, p, handle.size);
lookup_fhandle (&handle, npp);
return p + handle.size / sizeof (int);
}
int *
xdr_decode_fattr (int *p, struct stat *st)
{
int type, mode;
type = ntohl (*p);
p++;
mode = ntohl (*p);
p++;
st->st_mode = nfs_mode_to_hurd_mode (type, mode);
st->st_nlink = ntohl (*p);
p++;
st->st_uid = ntohl (*p);
p++;
st->st_gid = ntohl (*p);
p++;
if (protocol_version == 2)
{
st->st_size = ntohl (*p);
p++;
st->st_blksize = ntohl (*p);
p++;
st->st_rdev = ntohl (*p);
p++;
st->st_blocks = ntohl (*p);
p++;
st->st_fsid = ntohl (*p);
p++;
st->st_ino = ntohl (*p);
p++;
}
else
{
uint64_t size;
int major, minor;
p = xdr_decode_64bit (p, &size);
st->st_size = size;
p = xdr_decode_64bit (p, &size);
st->st_blocks = size / 512;
st->st_blksize = read_size < write_size ? read_size : write_size;
major = ntohl (*p);
p++;
minor = ntohl (*p);
p++;
st->st_rdev = gnu_dev_makedev (major, minor);
p = xdr_decode_64bit(p, &st->st_fsid);
p = xdr_decode_64bit(p, &st->st_ino);
}
st->st_atim.tv_sec = ntohl (*p);
p++;
st->st_atim.tv_nsec = ntohl (*p);
p++;
st->st_mtim.tv_sec = ntohl (*p);
p++;
st->st_mtim.tv_nsec = ntohl (*p);
p++;
st->st_ctim.tv_sec = ntohl (*p);
p++;
st->st_ctim.tv_nsec = ntohl (*p);
p++;
if (protocol_version < 3)
{
st->st_atim.tv_nsec *= 1000;
st->st_mtim.tv_nsec *= 1000;
st->st_ctim.tv_nsec *= 1000;
}
return p;
}
int *
xdr_decode_string (int *p, char *buf)
{
int len;
len = ntohl (*p);
p++;
memcpy (buf, p, len);
buf[len] = '\0';
return p + INTSIZE (len);
}
int *
nfs_initialize_rpc (int rpc_proc, struct iouser *cred,
size_t len, void **bufp, struct node *np,
uid_t second_gid)
{
uid_t uid;
uid_t gid;
error_t err;
if (cred == (struct iouser *) -1)
{
uid = gid = 0;
second_gid = -1;
}
else if (cred
&& (cred->uids->num || cred->gids->num))
{
if (idvec_contains (cred->uids, 0))
{
err = netfs_validate_stat (np, 0);
uid = 0;
gid = err ? -2 : 0;
if (err)
printf ("NFS warning, internal stat failure\n");
}
else
{
if (cred->uids->num == 0)
uid = -2;
else if (cred->uids->num == 1)
uid = cred->uids->ids[0];
else
{
err = netfs_validate_stat (np, 0);
if (err)
{
uid = cred->uids->ids[0];
printf ("NFS warning, internal stat failure\n");
}
else
{
if (idvec_contains (cred->uids, np->nn_stat.st_uid))
uid = np->nn_stat.st_uid;
else
uid = cred->uids->ids[0];
}
}
if (cred->gids->num == 0)
{
gid = -2;
second_gid = -1;
}
else if (cred->gids->num == 1)
{
gid = cred->gids->ids[0];
second_gid = -1;
}
else
{
err = netfs_validate_stat (np, 0);
if (err)
{
gid = cred->gids->ids[0];
printf ("NFS warning, internal stat failure\n");
}
else
{
if (idvec_contains (cred->gids, np->nn_stat.st_gid))
gid = np->nn_stat.st_gid;
else
gid = cred->gids->ids[0];
}
if (second_gid != -1
&& !idvec_contains (cred->gids, second_gid))
second_gid = -1;
}
}
}
else
uid = gid = second_gid = -1;
return initialize_rpc (nfs_program, nfs_version, rpc_proc, len, bufp,
uid, gid, second_gid);
}
error_t
nfs_error_trans (int error)
{
switch (error)
{
case NFS_OK:
return 0;
case NFSERR_PERM:
return EPERM;
case NFSERR_NOENT:
return ENOENT;
case NFSERR_IO:
return EIO;
case NFSERR_NXIO:
return ENXIO;
case NFSERR_ACCES:
return EACCES;
case NFSERR_EXIST:
return EEXIST;
case NFSERR_NODEV:
return ENODEV;
case NFSERR_NOTDIR:
return ENOTDIR;
case NFSERR_ISDIR:
return EISDIR;
case NFSERR_FBIG:
return E2BIG;
case NFSERR_NOSPC:
return ENOSPC;
case NFSERR_ROFS:
return EROFS;
case NFSERR_NAMETOOLONG:
return ENAMETOOLONG;
case NFSERR_NOTEMPTY:
return ENOTEMPTY;
case NFSERR_DQUOT:
return EDQUOT;
case NFSERR_STALE:
return ESTALE;
case NFSERR_WFLUSH:
return EINVAL;
default:
if (protocol_version == 2)
return EINVAL;
else
switch (error)
{
case NFSERR_XDEV:
return EXDEV;
case NFSERR_INVAL:
case NFSERR_REMOTE:
default:
return EINVAL;
case NFSERR_MLINK:
return EMLINK;
case NFSERR_NOTSUPP:
case NFSERR_BADTYPE:
return EOPNOTSUPP;
case NFSERR_SERVERFAULT:
return EIO;
case NFSERR_BADHANDLE:
case NFSERR_NOT_SYNC:
case NFSERR_BAD_COOKIE:
case NFSERR_TOOSMALL:
case NFSERR_JUKEBOX:
return EGRATUITOUS;
}
}
}