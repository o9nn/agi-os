#include <sys/stat.h>
#include <sys/statfs.h>
#include <string.h>
#include "nfsd.h"
static int
hurd_mode_to_nfs_mode (mode_t m)
{
return m & 0177777;
}
static int
hurd_mode_to_nfs_type (mode_t m, int version)
{
switch (m & S_IFMT)
{
case S_IFDIR:
return NFDIR;
case S_IFCHR:
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
return (version == 2 ? NF2FIFO : NF3FIFO);
default:
return (version == 2 ? NF2NON : NFREG);
}
}
int *
encode_fattr (int *p, struct stat *st, int version)
{
*(p++) = htonl (hurd_mode_to_nfs_type (st->st_mode, version));
*(p++) = htonl (hurd_mode_to_nfs_mode (st->st_mode));
*(p++) = htonl (st->st_nlink);
*(p++) = htonl (st->st_uid);
*(p++) = htonl (st->st_gid);
*(p++) = htonl (st->st_size);
*(p++) = htonl (st->st_blksize);
*(p++) = htonl (st->st_rdev);
*(p++) = htonl (st->st_blocks);
*(p++) = htonl (st->st_fsid);
*(p++) = htonl (st->st_ino);
*(p++) = htonl (st->st_atim.tv_sec);
*(p++) = htonl (st->st_atim.tv_nsec / 1000);
*(p++) = htonl (st->st_mtim.tv_sec);
*(p++) = htonl (st->st_mtim.tv_nsec / 1000);
*(p++) = htonl (st->st_ctim.tv_sec);
*(p++) = htonl (st->st_ctim.tv_nsec / 1000);
return p;
}
int *
decode_name (int *p, char **name)
{
int len;
len = ntohl (*p);
p++;
*name = malloc (len + 1);
memcpy (*name, p, len);
(*name)[len] = '\0';
return p + INTSIZE (len);
}
int *
encode_fhandle (int *p, char *handle)
{
memcpy (p, handle, NFS2_FHSIZE);
return p + INTSIZE (NFS2_FHSIZE);
}
int *
encode_string (int *p, char *string)
{
return encode_data (p, string, strlen (string));
}
int *
encode_data (int *p, char *data, size_t len)
{
int nints = INTSIZE (len);
p[nints] = 0;
*(p++) = htonl (len);
memcpy (p, data, len);
return p + nints;
}
int *
encode_statfs (int *p, struct statfs *st)
{
*(p++) = st->f_bsize;
*(p++) = st->f_bsize;
*(p++) = st->f_blocks;
*(p++) = st->f_bfree;
*(p++) = st->f_bavail;
return p;
}
int
nfs_error_trans (error_t err, int version)
{
switch (err)
{
case 0:
return NFS_OK;
case EPERM:
return NFSERR_PERM;
case ENOENT:
return NFSERR_NOENT;
case EIO:
return NFSERR_IO;
case ENXIO:
return NFSERR_NXIO;
case EACCES:
return NFSERR_ACCES;
case EEXIST:
return NFSERR_EXIST;
case ENODEV:
return NFSERR_NODEV;
case ENOTDIR:
return NFSERR_NOTDIR;
case EISDIR:
return NFSERR_ISDIR;
case E2BIG:
return NFSERR_FBIG;
case ENOSPC:
return NFSERR_NOSPC;
case EROFS:
return NFSERR_ROFS;
case ENAMETOOLONG:
return NFSERR_NAMETOOLONG;
case ENOTEMPTY:
return NFSERR_NOTEMPTY;
case EDQUOT:
return NFSERR_DQUOT;
case ESTALE:
return NFSERR_STALE;
default:
if (version == 2)
return NFSERR_IO;
else switch (err)
{
case EXDEV:
return NFSERR_XDEV;
case EINVAL:
return NFSERR_INVAL;
case EOPNOTSUPP:
return NFSERR_NOTSUPP;
default:
return NFSERR_IO;
}
}
}