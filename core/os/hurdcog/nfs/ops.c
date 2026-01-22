#include "nfs.h"
#include <hurd/netfs.h>
#include <netinet/in.h>
#include <string.h>
#include <fcntl.h>
#include <stdio.h>
#include <stddef.h>
#include <dirent.h>
#include <unistd.h>
#include <maptime.h>
#include <sys/sysmacros.h>
static error_t
nfs_lookup_rpc (struct iouser *cred, struct node *np,
const char *name, struct node **newnp);
int *
register_fresh_stat (struct node *np, int *p)
{
int *ret;
ret = xdr_decode_fattr (p, &np->nn_stat);
np->nn->stat_updated = mapped_time->seconds;
switch (np->nn->dtrans)
{
case NOT_POSSIBLE:
case POSSIBLE:
break;
case SYMLINK:
np->nn_stat.st_size = strlen (np->nn->transarg.name);
np->nn_stat.st_mode = ((np->nn_stat.st_mode & ~S_IFMT) | S_IFLNK);
break;
case CHRDEV:
np->nn_stat.st_rdev = np->nn->transarg.indexes;
np->nn_stat.st_mode = ((np->nn_stat.st_mode & ~S_IFMT) | S_IFCHR);
break;
case BLKDEV:
np->nn_stat.st_rdev = np->nn->transarg.indexes;
np->nn_stat.st_mode = ((np->nn_stat.st_mode & ~S_IFMT) | S_IFBLK);
break;
case FIFO:
np->nn_stat.st_mode = ((np->nn_stat.st_mode & ~S_IFMT) | S_IFIFO);
break;
case SOCK:
np->nn_stat.st_mode = ((np->nn_stat.st_mode & ~S_IFMT) | S_IFSOCK);
break;
}
np->nn_stat.st_fsid = getpid ();
np->nn_stat.st_fstype = FSTYPE_NFS;
np->nn_stat.st_gen = 0;
np->nn_stat.st_author = np->nn_stat.st_uid;
np->nn_stat.st_flags = 0;
np->nn_translated = np->nn_stat.st_mode & S_IFMT;
return ret;
}
int *
process_returned_stat (struct node *np, int *p, int mod)
{
if (protocol_version == 2)
return register_fresh_stat (np, p);
else
{
int attrs_exist;
attrs_exist = ntohl (*p);
p++;
if (attrs_exist)
p = register_fresh_stat (np, p);
else if (mod)
np->nn->stat_updated = 0;
return p;
}
}
int *
process_wcc_stat (struct node *np, int *p, int mod)
{
if (protocol_version == 2)
return register_fresh_stat (np, p);
else
{
int attrs_exist;
attrs_exist = ntohl (*p);
p++;
if (attrs_exist)
{
p += 2;
p += 2;
p += 2;
}
return process_returned_stat (np, p, mod);
}
}
static int *
skip_returned_stat (int *p)
{
struct stat st;
if (protocol_version == 2)
return xdr_decode_fattr (p, &st);
int attrs_exist = ntohl (*p);
p++;
return (attrs_exist ? xdr_decode_fattr (p, &st) : p);
}
static error_t
process_create_reply (struct iouser *cred,
struct node *np,
const char* name,
struct node **newnp,
int *p)
{
assert_backtrace (protocol_version == 3);
error_t err = nfs_error_trans (ntohl (*p));
p++;
if (!err)
{
int handle_follows = ntohl (*p);
p++;
if (handle_follows)
{
p = (*newnp != NULL
? recache_handle (p, *newnp)
: xdr_decode_fhandle (p, newnp));
p = process_returned_stat (*newnp, p, 1);
}
else
p = skip_returned_stat (p);
if (*newnp)
pthread_mutex_unlock (&(*newnp)->lock);
pthread_mutex_lock (&np->lock);
p = process_wcc_stat (np, p, 1);
if (!handle_follows)
{
err = nfs_lookup_rpc (cred, np, name, newnp);
}
else
pthread_mutex_unlock (&np->lock);
}
else
{
if (*newnp)
pthread_mutex_unlock (&(*newnp)->lock);
pthread_mutex_lock (&np->lock);
p = process_wcc_stat (np, p, 1);
pthread_mutex_unlock (&np->lock);
if (*newnp)
pthread_mutex_lock (&(*newnp)->lock);
}
return err;
}
error_t
netfs_validate_stat (struct node *np, struct iouser *cred)
{
int *p;
void *rpcbuf;
error_t err;
if (mapped_time->seconds - np->nn->stat_updated < stat_timeout)
return 0;
p = nfs_initialize_rpc (NFSPROC_GETATTR (protocol_version),
(struct iouser *) -1, 0, &rpcbuf, np, -1);
if (! p)
return errno;
p = xdr_encode_fhandle (p, &np->nn->handle);
err = conduct_rpc (&rpcbuf, &p);
if (!err)
{
err = nfs_error_trans (ntohl (*p));
p++;
}
if (!err)
register_fresh_stat (np, p);
free (rpcbuf);
return err;
}
static error_t
nfs_setattr_rpc (struct iouser *cred, struct node *np, gid_t gid,
int *(sattr_encoder) (int *))
{
int *p;
void *rpcbuf;
error_t err;
p = nfs_initialize_rpc (NFSPROC_SETATTR (protocol_version),
cred, 0, &rpcbuf, np, gid);
if (! p)
return errno;
p = xdr_encode_fhandle (p, &np->nn->handle);
p = (sattr_encoder) (p);
if (protocol_version == 3)
*(p++) = 0;
err = conduct_rpc (&rpcbuf, &p);
if (!err)
{
err = nfs_error_trans (ntohl (*p));
p++;
if (!err || protocol_version == 3)
p = process_wcc_stat (np, p, !err);
}
free (rpcbuf);
return err;
}
error_t
netfs_attempt_chown (struct iouser *cred, struct node *np,
uid_t uid, gid_t gid)
{
int *_chown_sattr_encoder (int *p)
{
return xdr_encode_sattr_ids (p, uid, gid);
}
return nfs_setattr_rpc (cred, np, gid, _chown_sattr_encoder);
}
error_t
netfs_attempt_chauthor (struct iouser *cred, struct node *rp,
uid_t author)
{
return EOPNOTSUPP;
}
error_t
netfs_attempt_chmod (struct iouser *cred, struct node *np,
mode_t mode)
{
if ((mode & S_IFMT) != 0)
{
error_t err = netfs_validate_stat (np, cred);
if (err)
return err;
if ((mode & S_IFMT) != (np->nn_stat.st_mode & S_IFMT))
{
char *f = 0;
if (np->nn->dtrans == NOT_POSSIBLE)
return EOPNOTSUPP;
if (np->nn->dtrans == SYMLINK)
f = np->nn->transarg.name;
switch (mode & S_IFMT)
{
default:
return EOPNOTSUPP;
case S_IFIFO:
np->nn->dtrans = FIFO;
np->nn->stat_updated = 0;
break;
case S_IFSOCK:
np->nn->dtrans = SOCK;
np->nn->stat_updated = 0;
}
free (f);
return 0;
}
}
int *_chmod_sattr_encoder (int *p)
{
return xdr_encode_sattr_mode (p, mode);
}
return nfs_setattr_rpc (cred, np, -1, _chmod_sattr_encoder);
}
error_t
netfs_attempt_chflags (struct iouser *cred, struct node *np,
int flags)
{
return EOPNOTSUPP;
}
error_t
netfs_attempt_utimes (struct iouser *cred, struct node *np,
struct timespec *atime, struct timespec *mtime)
{
if (!atime && !mtime)
return 0;
int *_utimes_sattr_encoder (int *p)
{
return xdr_encode_sattr_times (p, atime, mtime);
}
return nfs_setattr_rpc (cred, np, -1, _utimes_sattr_encoder);
}
error_t
netfs_attempt_set_size (struct iouser *cred, struct node *np,
off_t size)
{
error_t err;
int *_size_sattr_encoder (int *p)
{
return xdr_encode_sattr_size (p, size);
}
err = nfs_setattr_rpc (cred, np, -1, _size_sattr_encoder);
if (err == EACCES)
{
int error = netfs_validate_stat (np, cred);
if (!error && np->nn_stat.st_size == size)
err = 0;
}
return err;
}
static error_t
netfs_attempt_statfs_v2 (struct iouser *cred, struct node *np,
struct statfs *st)
{
int *p;
void *rpcbuf;
error_t err;
p = nfs_initialize_rpc (NFS2PROC_STATFS, cred, 0, &rpcbuf, np, -1);
if (! p)
return errno;
p = xdr_encode_fhandle (p, &np->nn->handle);
err = conduct_rpc (&rpcbuf, &p);
if (!err)
{
err = nfs_error_trans (ntohl (*p));
p++;
}
if (!err)
{
p++;
st->f_bsize = ntohl (*p);
p++;
st->f_blocks = ntohl (*p);
p++;
st->f_bfree = ntohl (*p);
p++;
st->f_bavail = ntohl (*p);
p++;
st->f_type = FSTYPE_NFS;
st->f_files = 0;
st->f_ffree = 0;
st->f_fsid = getpid ();
st->f_namelen = 0;
}
free (rpcbuf);
return err;
}
static error_t
netfs_attempt_statfs_v3 (struct iouser *cred, struct node *np,
struct statfs *st)
{
int *p;
void *rpcbuf;
error_t err;
p = nfs_initialize_rpc (NFS3PROC_FSSTAT, cred, 0, &rpcbuf, np, -1);
if (! p)
return errno;
p = xdr_encode_fhandle (p, &np->nn->handle);
err = conduct_rpc (&rpcbuf, &p);
if (!err)
{
err = nfs_error_trans (ntohl (*p++));
p = process_returned_stat (np, p, 1);
if (!err)
{
st->f_bsize = 1;
p = xdr_decode_64bit(p, &st->f_blocks);
p = xdr_decode_64bit(p, &st->f_bfree);
p = xdr_decode_64bit(p, &st->f_bavail);
p = xdr_decode_64bit(p, &st->f_files);
p = xdr_decode_64bit(p, &st->f_ffree);
st->f_type = FSTYPE_NFS;
st->f_fsid = getpid ();
st->f_namelen = 0;
}
}
free (rpcbuf);
return err;
}
error_t
netfs_attempt_statfs (struct iouser *cred, struct node *np,
struct statfs *st)
{
return (protocol_version == 2
? netfs_attempt_statfs_v2 (cred, np, st)
: netfs_attempt_statfs_v3 (cred, np, st));
}
error_t
netfs_attempt_sync (struct iouser *cred, struct node *np, int wait)
{
return 0;
}
error_t
netfs_attempt_syncfs (struct iouser *cred, int wait)
{
return 0;
}
error_t
netfs_attempt_read (struct iouser *cred, struct node *np,
off_t offset, size_t *len, void *data)
{
int *p;
void *rpcbuf;
size_t trans_len;
error_t err;
size_t amt, thisamt;
int eof;
for (amt = *len; amt;)
{
thisamt = amt;
if (thisamt > read_size)
thisamt = read_size;
p = nfs_initialize_rpc (NFSPROC_READ (protocol_version),
cred, 0, &rpcbuf, np, -1);
if (! p)
return errno;
p = xdr_encode_fhandle (p, &np->nn->handle);
if (protocol_version == 2)
{
*(p++) = htonl (offset);
*(p++) = htonl (thisamt);
*(p++) = 0;
}
else
{
p = xdr_encode_64bit (p, offset);
*(p++) = htonl (thisamt);
}
err = conduct_rpc (&rpcbuf, &p);
if (!err)
{
err = nfs_error_trans (ntohl (*p));
p++;
if (!err || protocol_version == 3)
p = process_returned_stat (np, p, !err);
if (err)
{
free (rpcbuf);
return err;
}
trans_len = ntohl (*p);
p++;
if (trans_len > thisamt)
trans_len = thisamt;
if (protocol_version == 3)
{
size_t opaque_data_len;
eof = ntohl (*p);
p++;
opaque_data_len = ntohl (*p++);
if (opaque_data_len < trans_len)
trans_len = opaque_data_len;
}
else
eof = (trans_len < thisamt);
memcpy (data, p, trans_len);
free (rpcbuf);
data += trans_len;
offset += trans_len;
amt -= trans_len;
if (eof)
{
*len -= amt;
return 0;
}
}
}
return 0;
}
error_t
netfs_attempt_write (struct iouser *cred, struct node *np,
off_t offset, size_t *len, const void *data)
{
int *p;
void *rpcbuf;
error_t err;
size_t amt, thisamt;
size_t count;
for (amt = *len; amt;)
{
thisamt = amt;
if (thisamt > write_size)
thisamt = write_size;
p = nfs_initialize_rpc (NFSPROC_WRITE (protocol_version),
cred, thisamt, &rpcbuf, np, -1);
if (! p)
return errno;
p = xdr_encode_fhandle (p, &np->nn->handle);
if (protocol_version == 2)
{
*(p++) = 0;
*(p++) = htonl (offset);
*(p++) = 0;
}
else
{
p = xdr_encode_64bit(p, offset);
*(p++) = htonl (thisamt);
*(p++) = htonl (FILE_SYNC);
}
p = xdr_encode_data (p, data, thisamt);
err = conduct_rpc (&rpcbuf, &p);
if (!err)
{
err = nfs_error_trans (ntohl (*p));
p++;
if (!err || protocol_version == 3)
p = process_wcc_stat (np, p, !err);
if (!err)
{
if (protocol_version == 3)
{
count = ntohl (*p);
p++;
p++;
p += NFS3_WRITEVERFSIZE / sizeof (int);
}
else
count = thisamt;
amt -= count;
data += count;
offset += count;
}
}
free (rpcbuf);
if (err == EINTR && amt != *len)
{
*len -= amt;
return 0;
}
if (err)
{
*len = 0;
return err;
}
}
return 0;
}
error_t
verify_nonexistent (struct iouser *cred, struct node *dir,
const char *name)
{
int *p;
void *rpcbuf;
error_t err;
assert_backtrace (protocol_version == 2);
p = nfs_initialize_rpc (NFSPROC_LOOKUP (protocol_version),
cred, 0, &rpcbuf, dir, -1);
if (! p)
return errno;
p = xdr_encode_fhandle (p, &dir->nn->handle);
p = xdr_encode_string (p, name);
err = conduct_rpc (&rpcbuf, &p);
if (!err)
{
err = nfs_error_trans (ntohl (*p));
p++;
}
free (rpcbuf);
if (!err)
return EEXIST;
else
return 0;
}
error_t
netfs_attempt_lookup (struct iouser *cred, struct node *np,
const char *name, struct node **newnp)
{
*newnp = check_lookup_cache (np, name);
if (*newnp)
{
if (*newnp == (struct node *) -1)
{
*newnp = 0;
return ENOENT;
}
else
return 0;
}
return nfs_lookup_rpc (cred, np, name, newnp);
}
static error_t
nfs_lookup_rpc (struct iouser *cred, struct node *np,
const char *name, struct node **newnp)
{
int *p;
void *rpcbuf;
error_t err;
char dirhandle[NFS3_FHSIZE];
size_t dirlen;
p = nfs_initialize_rpc (NFSPROC_LOOKUP (protocol_version),
cred, 0, &rpcbuf, np, -1);
if (! p)
{
pthread_mutex_unlock (&np->lock);
return errno;
}
p = xdr_encode_fhandle (p, &np->nn->handle);
p = xdr_encode_string (p, name);
dirlen = np->nn->handle.size;
memcpy (dirhandle, np->nn->handle.data, dirlen);
pthread_mutex_unlock (&np->lock);
err = conduct_rpc (&rpcbuf, &p);
if (!err)
{
err = nfs_error_trans (ntohl (*p));
p++;
if (!err)
{
if (*newnp != NULL)
{
pthread_mutex_lock (&(*newnp)->lock);
p = recache_handle (p, *newnp);
}
else
p = xdr_decode_fhandle (p, newnp);
p = process_returned_stat (*newnp, p, 1);
}
if (protocol_version == 3)
{
if (*newnp)
pthread_mutex_unlock (&(*newnp)->lock);
pthread_mutex_lock (&np->lock);
p = process_returned_stat (np, p, 0);
pthread_mutex_unlock (&np->lock);
if (*newnp)
pthread_mutex_lock (&(*newnp)->lock);
}
}
if (!err || err == ENOENT)
{
enter_lookup_cache (dirhandle, dirlen, *newnp, name);
}
free (rpcbuf);
return err;
}
error_t
netfs_attempt_mkdir (struct iouser *cred, struct node *np,
const char *name, mode_t mode)
{
int *p;
void *rpcbuf;
error_t err;
uid_t owner;
struct node *newnp;
if (cred->uids->num)
owner = cred->uids->ids[0];
else
{
err = netfs_validate_stat (np, cred);
owner = err ? 0 : np->nn_stat.st_uid;
mode &= ~S_ISUID;
}
purge_lookup_cache (np, name, strlen (name));
p = nfs_initialize_rpc (NFSPROC_MKDIR (protocol_version),
cred, 0, &rpcbuf, np, -1);
if (! p)
return errno;
p = xdr_encode_fhandle (p, &np->nn->handle);
p = xdr_encode_string (p, name);
p = xdr_encode_create_state (p, mode, owner);
pthread_mutex_unlock (&np->lock);
err = conduct_rpc (&rpcbuf, &p);
if (!err)
{
if (protocol_version == 2)
{
err = nfs_error_trans (ntohl (*p));
p++;
if (!err)
{
p = xdr_decode_fhandle (p, &newnp);
p = process_returned_stat (newnp, p, 1);
}
}
else
{
newnp = NULL;
err = process_create_reply (cred, np, name, &newnp, p);
}
}
if (!err)
{
if (!netfs_validate_stat (newnp, (struct iouser *) -1)
&& newnp->nn_stat.st_uid != owner)
netfs_attempt_chown ((struct iouser *) -1, newnp, owner,
newnp->nn_stat.st_gid);
netfs_nput (newnp);
}
pthread_mutex_lock (&np->lock);
free (rpcbuf);
return err;
}
error_t
netfs_attempt_rmdir (struct iouser *cred, struct node *np,
const char *name)
{
int *p;
void *rpcbuf;
error_t err;
purge_lookup_cache (np, name, strlen (name));
p = nfs_initialize_rpc (NFSPROC_RMDIR (protocol_version),
cred, 0, &rpcbuf, np, -1);
if (! p)
return errno;
p = xdr_encode_fhandle (p, &np->nn->handle);
p = xdr_encode_string (p, name);
err = conduct_rpc (&rpcbuf, &p);
if (!err)
{
err = nfs_error_trans (ntohl (*p));
p++;
if (protocol_version == 3)
p = process_wcc_stat (np, p, !err);
}
free (rpcbuf);
return err;
}
error_t
netfs_attempt_link (struct iouser *cred, struct node *dir,
struct node *np, const char *name, int excl)
{
int *p;
void *rpcbuf;
error_t err = 0;
if (!excl)
{
pthread_mutex_lock (&dir->lock);
netfs_attempt_unlink (cred, dir, name);
pthread_mutex_unlock (&dir->lock);
}
switch (np->nn->dtrans)
{
case POSSIBLE:
case NOT_POSSIBLE:
pthread_mutex_lock (&dir->lock);
p = nfs_initialize_rpc (NFSPROC_LINK (protocol_version),
cred, 0, &rpcbuf, dir, -1);
if (! p)
{
pthread_mutex_unlock (&dir->lock);
return errno;
}
pthread_mutex_unlock (&dir->lock);
pthread_mutex_lock (&np->lock);
p = xdr_encode_fhandle (p, &np->nn->handle);
pthread_mutex_unlock (&np->lock);
pthread_mutex_lock (&dir->lock);
purge_lookup_cache (dir, name, strlen (name));
p = xdr_encode_fhandle (p, &dir->nn->handle);
p = xdr_encode_string (p, name);
err = conduct_rpc (&rpcbuf, &p);
if (!err)
{
err = nfs_error_trans (ntohl (*p));
p++;
}
pthread_mutex_unlock (&dir->lock);
free (rpcbuf);
break;
case SYMLINK:
pthread_mutex_lock (&dir->lock);
p = nfs_initialize_rpc (NFSPROC_SYMLINK (protocol_version),
cred, 0, &rpcbuf, dir, -1);
if (! p)
{
pthread_mutex_unlock (&dir->lock);
return errno;
}
p = xdr_encode_fhandle (p, &dir->nn->handle);
pthread_mutex_unlock (&dir->lock);
p = xdr_encode_string (p, name);
pthread_mutex_lock (&np->lock);
err = netfs_validate_stat (np, cred);
if (err)
{
pthread_mutex_unlock (&np->lock);
free (rpcbuf);
return err;
}
if (protocol_version == 2)
{
p = xdr_encode_string (p, np->nn->transarg.name);
p = xdr_encode_sattr_stat (p, &np->nn_stat);
}
else
{
p = xdr_encode_sattr_stat (p, &np->nn_stat);
p = xdr_encode_string (p, np->nn->transarg.name);
}
pthread_mutex_unlock (&np->lock);
purge_lookup_cache (dir, name, strlen (name));
err = conduct_rpc (&rpcbuf, &p);
if (!err)
{
err = nfs_error_trans (ntohl (*p));
p++;
if (protocol_version == 2 && !err)
{
free (rpcbuf);
pthread_mutex_lock (&dir->lock);
p = nfs_initialize_rpc (NFSPROC_LOOKUP (protocol_version),
cred, 0, &rpcbuf, dir, -1);
if (! p)
{
pthread_mutex_unlock (&dir->lock);
return errno;
}
p = xdr_encode_fhandle (p, &dir->nn->handle);
p = xdr_encode_string (p, name);
pthread_mutex_unlock (&dir->lock);
err = conduct_rpc (&rpcbuf, &p);
if (!err)
{
err = nfs_error_trans (ntohl (*p));
p++;
}
if (!err)
{
pthread_mutex_lock (&np->lock);
p = recache_handle (p, np);
p = process_returned_stat (np, p, 1);
pthread_mutex_unlock (&np->lock);
}
if (err)
err = EGRATUITOUS;
}
else if (protocol_version == 3)
{
p--;
pthread_mutex_lock (&np->lock);
err = process_create_reply (cred, dir, name, &np, p);
pthread_mutex_unlock (&np->lock);
}
}
free (rpcbuf);
break;
case CHRDEV:
case BLKDEV:
case FIFO:
case SOCK:
if (protocol_version == 2)
{
pthread_mutex_lock (&dir->lock);
err = verify_nonexistent (cred, dir, name);
if (err)
return err;
p = nfs_initialize_rpc (NFSPROC_CREATE (protocol_version),
cred, 0, &rpcbuf, dir, -1);
if (! p)
{
pthread_mutex_unlock (&dir->lock);
return errno;
}
p = xdr_encode_fhandle (p, &dir->nn->handle);
p = xdr_encode_string (p, name);
pthread_mutex_unlock (&dir->lock);
pthread_mutex_lock (&np->lock);
err = netfs_validate_stat (np, cred);
if (err)
{
pthread_mutex_unlock (&np->lock);
free (rpcbuf);
return err;
}
p = xdr_encode_sattr_stat (p, &np->nn_stat);
pthread_mutex_unlock (&np->lock);
pthread_mutex_lock (&dir->lock);
purge_lookup_cache (dir, name, strlen (name));
pthread_mutex_unlock (&dir->lock);
err = conduct_rpc (&rpcbuf, &p);
if (!err)
{
err = nfs_error_trans (ntohl (*p));
p++;
}
if (!err)
{
pthread_mutex_lock (&np->lock);
p = recache_handle (p, np);
register_fresh_stat (np, p);
pthread_mutex_unlock (&np->lock);
}
free (rpcbuf);
}
else
{
pthread_mutex_lock (&dir->lock);
p = nfs_initialize_rpc (NFS3PROC_MKNOD, cred, 0, &rpcbuf, dir, -1);
if (! p)
{
pthread_mutex_unlock (&dir->lock);
return errno;
}
p = xdr_encode_fhandle (p, &dir->nn->handle);
p = xdr_encode_string (p, name);
pthread_mutex_unlock (&dir->lock);
pthread_mutex_lock (&np->lock);
err = netfs_validate_stat (np, cred);
if (err)
{
pthread_mutex_unlock (&np->lock);
free (rpcbuf);
return err;
}
*(p++) = htonl (hurd_mode_to_nfs_type (np->nn_stat.st_mode));
p = xdr_encode_sattr_stat (p, &np->nn_stat);
if (np->nn->dtrans == BLKDEV || np->nn->dtrans == CHRDEV)
{
*(p++) = htonl (gnu_dev_major (np->nn_stat.st_rdev));
*(p++) = htonl (gnu_dev_minor (np->nn_stat.st_rdev));
}
pthread_mutex_unlock (&np->lock);
purge_lookup_cache (dir, name, strlen (name));
err = conduct_rpc (&rpcbuf, &p);
if (!err)
{
pthread_mutex_lock (&np->lock);
err = process_create_reply (cred, dir, name, &np, p);
pthread_mutex_unlock (&np->lock);
}
free (rpcbuf);
}
break;
}
if (err)
return err;
pthread_mutex_lock (&np->lock);
if (np->nn->dtrans == SYMLINK)
free (np->nn->transarg.name);
np->nn->dtrans = NOT_POSSIBLE;
if (np->nn->dead_dir)
{
struct node *dir = np->nn->dead_dir;
char *name = np->nn->dead_name;
np->nn->dead_dir = 0;
np->nn->dead_name = 0;
pthread_mutex_unlock (&np->lock);
pthread_mutex_lock (&dir->lock);
netfs_attempt_unlink ((struct iouser *)-1, dir, name);
pthread_mutex_unlock (&dir->lock);
}
else
pthread_mutex_unlock (&np->lock);
return 0;
}
error_t
netfs_attempt_mkfile (struct iouser *cred, struct node *dir,
mode_t mode, struct node **newnp)
{
error_t err;
char *name;
static int n = 0;
name = malloc (50);
if (! name)
{
pthread_mutex_unlock (&dir->lock);
return ENOMEM;
}
do
{
sprintf (name, ".nfstmpgnu.%d", n++);
err = netfs_attempt_create_file (cred, dir, name, mode, newnp);
if (err == EEXIST)
pthread_mutex_lock (&dir->lock);
}
while (err == EEXIST);
if (err)
{
free (name);
return err;
}
assert_backtrace (!(*newnp)->nn->dead_dir);
assert_backtrace (!(*newnp)->nn->dead_name);
netfs_nref (dir);
(*newnp)->nn->dead_dir = dir;
(*newnp)->nn->dead_name = name;
if ((*newnp)->nn->dtrans == NOT_POSSIBLE)
(*newnp)->nn->dtrans = POSSIBLE;
return 0;
}
error_t
netfs_attempt_create_file (struct iouser *cred, struct node *np,
const char *name, mode_t mode, struct node **newnp)
{
int *p;
void *rpcbuf;
error_t err;
uid_t owner;
if (cred->uids->num)
owner = cred->uids->ids[0];
else
{
err = netfs_validate_stat (np, cred);
owner = err ? 0 : np->nn_stat.st_uid;
mode &= ~S_ISUID;
}
if (protocol_version == 2)
{
err = verify_nonexistent (cred, np, name);
if (err)
{
pthread_mutex_unlock (&np->lock);
return err;
}
}
purge_lookup_cache (np, name, strlen (name));
p = nfs_initialize_rpc (NFSPROC_CREATE (protocol_version),
cred, 0, &rpcbuf, np, -1);
if (! p) {
pthread_mutex_unlock (&np->lock);
return errno;
}
p = xdr_encode_fhandle (p, &np->nn->handle);
p = xdr_encode_string (p, name);
if (protocol_version == 3)
{
int verf = *(int *)rpcbuf;
*(p++) = ntohl (EXCLUSIVE);
*(p++) = ntohl (verf);
p++;
}
else
p = xdr_encode_create_state (p, mode, owner);
err = conduct_rpc (&rpcbuf, &p);
*newnp = 0;
pthread_mutex_unlock (&np->lock);
if (!err)
{
if (protocol_version == 2)
{
err = nfs_error_trans (ntohl (*p));
p++;
if (!err)
{
p = xdr_decode_fhandle (p, newnp);
p = process_returned_stat (*newnp, p, 1);
}
}
else
err = process_create_reply (cred, np, name, newnp, p);
if (protocol_version == 3 && !err)
{
int *_cs_sattr_encoder (int * sp)
{
return xdr_encode_create_state (sp, mode, owner);
}
err = nfs_setattr_rpc(cred, *newnp, -1, _cs_sattr_encoder);
}
}
free (rpcbuf);
return err;
}
error_t
netfs_attempt_unlink (struct iouser *cred, struct node *dir,
const char *name)
{
int *p;
void *rpcbuf;
error_t err;
struct node *np;
err = netfs_attempt_lookup (cred, dir, name, &np);
if (err)
{
pthread_mutex_lock (&dir->lock);
return err;
}
pthread_mutex_unlock (&np->lock);
pthread_mutex_lock (&dir->lock);
purge_lookup_cache_node (np);
struct references result;
refcounts_references (&np->refcounts, &result);
if (result.hard > 1)
{
char *newname = 0;
int n = 0;
pthread_mutex_unlock (&dir->lock);
newname = malloc (50);
if (! newname)
{
pthread_mutex_lock (&dir->lock);
netfs_nrele (np);
return ENOMEM;
}
do
{
sprintf (newname, ".nfs%txgnu.%d", (ptrdiff_t) np, n++);
err = netfs_attempt_link (cred, dir, np, newname, 1);
}
while (err == EEXIST);
if (err)
{
free (newname);
pthread_mutex_lock (&dir->lock);
netfs_nrele (np);
return err;
}
pthread_mutex_lock (&np->lock);
if (np->nn->dead_dir)
netfs_nrele (np->nn->dead_dir);
netfs_nref (dir);
np->nn->dead_dir = dir;
if (np->nn->dead_name)
free (np->nn->dead_name);
np->nn->dead_name = newname;
if (np->nn->dtrans == NOT_POSSIBLE)
np->nn->dtrans = POSSIBLE;
netfs_nput (np);
pthread_mutex_lock (&dir->lock);
}
else
netfs_nrele (np);
p = nfs_initialize_rpc (NFSPROC_REMOVE (protocol_version),
cred, 0, &rpcbuf, dir, -1);
if (! p)
return errno;
p = xdr_encode_fhandle (p, &dir->nn->handle);
p = xdr_encode_string (p, name);
err = conduct_rpc (&rpcbuf, &p);
if (!err)
{
err = nfs_error_trans (ntohl (*p));
p++;
if (protocol_version == 3)
p = process_wcc_stat (dir, p, !err);
}
free (rpcbuf);
return err;
}
error_t
netfs_attempt_rename (struct iouser *cred, struct node *fromdir,
const char *fromname, struct node *todir, const char *toname,
int excl)
{
int *p;
void *rpcbuf;
error_t err;
if (excl)
{
struct node *np;
pthread_mutex_lock (&fromdir->lock);
err = netfs_attempt_lookup (cred, fromdir, fromname, &np);
if (err)
return err;
pthread_mutex_unlock(&np->lock);
err = netfs_attempt_link (cred, todir, np, toname, 1);
netfs_nput (np);
if (err)
return err;
pthread_mutex_lock (&fromdir->lock);
err = netfs_attempt_unlink (cred, fromdir, fromname);
pthread_mutex_unlock (&fromdir->lock);
if (err)
{
pthread_mutex_lock (&todir->lock);
netfs_attempt_unlink (cred, todir, toname);
pthread_mutex_unlock (&todir->lock);
return err;
}
return 0;
}
pthread_mutex_lock (&fromdir->lock);
purge_lookup_cache (fromdir, fromname, strlen (fromname));
p = nfs_initialize_rpc (NFSPROC_RENAME (protocol_version),
cred, 0, &rpcbuf, fromdir, -1);
if (! p)
{
pthread_mutex_unlock (&fromdir->lock);
return errno;
}
p = xdr_encode_fhandle (p, &fromdir->nn->handle);
p = xdr_encode_string (p, fromname);
pthread_mutex_unlock (&fromdir->lock);
pthread_mutex_lock (&todir->lock);
purge_lookup_cache (todir, toname, strlen (toname));
p = xdr_encode_fhandle (p, &todir->nn->handle);
p = xdr_encode_string (p, toname);
pthread_mutex_unlock (&todir->lock);
err = conduct_rpc (&rpcbuf, &p);
if (!err)
{
err = nfs_error_trans (ntohl (*p));
p++;
if (protocol_version == 3)
{
pthread_mutex_lock (&fromdir->lock);
p = process_wcc_stat (fromdir, p, !err);
pthread_mutex_unlock (&fromdir->lock);
pthread_mutex_lock (&todir->lock);
p = process_wcc_stat (todir, p, !err);
pthread_mutex_unlock (&todir->lock);
}
}
free (rpcbuf);
return err;
}
error_t
netfs_attempt_readlink (struct iouser *cred, struct node *np,
char *buf)
{
int *p;
void *rpcbuf;
error_t err;
if (np->nn->dtrans == SYMLINK)
{
strcpy (buf, np->nn->transarg.name);
return 0;
}
p = nfs_initialize_rpc (NFSPROC_READLINK (protocol_version),
cred, 0, &rpcbuf, np, -1);
if (! p)
return errno;
p = xdr_encode_fhandle (p, &np->nn->handle);
err = conduct_rpc (&rpcbuf, &p);
if (!err)
{
err = nfs_error_trans (ntohl (*p));
p++;
if (protocol_version == 3)
p = process_returned_stat (np, p, 0);
if (!err)
p = xdr_decode_string (p, buf);
}
free (rpcbuf);
return err;
}
error_t
netfs_check_open_permissions (struct iouser *cred, struct node *np,
int flags, int newnode)
{
int modes;
if (newnode || (flags & (O_READ|O_WRITE|O_EXEC)) == 0)
return 0;
netfs_report_access (cred, np, &modes);
if ((flags & (O_READ|O_WRITE|O_EXEC)) == (flags & modes))
return 0;
else
return EACCES;
}
error_t
netfs_report_access (struct iouser *cred,
struct node *np,
int *types)
{
error_t err;
err = netfs_validate_stat (np, cred);
if (err)
return err;
if (protocol_version == 2)
{
*types = 0;
if (fshelp_access (&np->nn_stat, S_IREAD, cred) == 0)
*types |= O_READ;
if (fshelp_access (&np->nn_stat, S_IWRITE, cred) == 0)
*types |= O_WRITE;
if (fshelp_access (&np->nn_stat, S_IEXEC, cred) == 0)
*types |= O_EXEC;
return 0;
}
else
{
int *p;
void *rpcbuf;
error_t err;
int ret;
int write_check, execute_check;
if (S_ISDIR (np->nn_stat.st_mode))
{
write_check = ACCESS3_MODIFY | ACCESS3_DELETE | ACCESS3_EXTEND;
execute_check = ACCESS3_LOOKUP;
}
else
{
write_check = ACCESS3_MODIFY;
execute_check = ACCESS3_EXECUTE;
}
p = nfs_initialize_rpc (NFS3PROC_ACCESS, cred, 0, &rpcbuf, np, -1);
if (! p)
return errno;
p = xdr_encode_fhandle (p, &np->nn->handle);
*(p++) = htonl (ACCESS3_READ | write_check | execute_check);
err = conduct_rpc (&rpcbuf, &p);
if (!err)
{
err = nfs_error_trans (ntohl (*p));
p++;
p = process_returned_stat (np, p, 0);
if (!err)
{
ret = ntohl (*p);
p++;
*types = ((ret & ACCESS3_READ ? O_READ : 0)
| (ret & write_check ? O_WRITE : 0)
| (ret & execute_check ? O_EXEC : 0));
}
}
return err;
}
}
#if 0
error_t
netfs_check_open_permissions (struct iouser *cred, struct node *np,
int flags, int newnode)
{
char byte;
error_t err;
size_t len;
if ((flags & O_READ) == 0
&& (flags & O_WRITE) == 0
&& (flags & O_EXEC) == 0)
return 0;
err = netfs_validate_stat (np, cred);
if (err)
return err;
switch (np->nn_stat.st_mode & S_IFMT)
{
default:
return 0;
case S_IFREG:
len = 1;
err = netfs_attempt_read (cred, np, 0, &len, &byte);
if (err)
{
if ((flags & O_READ) || (flags & O_EXEC))
return err;
else
return 0;
}
if (len != 1)
return 0;
if (flags & O_WRITE)
{
err = netfs_attempt_write (cred, np, 0, &len, &byte);
return err;
}
return 0;
case S_IFDIR:
if (flags & O_READ)
{
void *rpcbuf;
int *p;
p = nfs_initialize_rpc (NFSPROC_READDIR, cred, 0, &rpcbuf, np, -1);
p = xdr_encode_fhandle (p, &np->nn->handle);
*(p++) = 0;
*(p++) = htonl (50);
err = conduct_rpc (&rpcbuf, &p);
if (!err)
{
err = nfs_error_trans (ntohl (*p));
p++;
}
free (rpcbuf);
if (err)
return err;
}
return 0;
}
}
void
netfs_report_access (struct iouser *cred,
struct node *np,
int *types)
{
char byte;
error_t err;
size_t len;
*types = 0;
len = 1;
err = netfs_attempt_read (cred, np, 0, &len, &byte);
if (err)
return;
assert_backtrace (len == 1 || len == 0);
*types |= O_READ | O_EXEC;
if (len == 1)
{
err = netfs_attempt_write (cred, np, 0, &len, &byte);
if (!err)
*types |= O_WRITE;
}
else
{
byte = 0;
err = netfs_attempt_write (cred, np, 0, &len, &byte);
if (!err)
*types |= O_WRITE;
netfs_attempt_set_size (cred, np, 0);
}
}
#endif
static error_t
fetch_directory (struct iouser *cred, struct node *dir,
void **bufp, size_t *bufsizep, int *totalentries)
{
void *buf;
int *p;
void *rpcbuf;
struct dirent *entry;
void *bp;
int bufmalloced;
int eof;
error_t err;
int isnext;
char cookieverf[NFS3_COOKIEVERFSIZE];
char cookie[NFS_MAXCOOKIESIZE];
const unsigned int cookie_size =
(protocol_version == 2 ? NFS2_COOKIESIZE : NFS3_COOKIESIZE);
bufmalloced = read_size;
buf = malloc (bufmalloced);
if (! buf)
return ENOMEM;
bp = buf;
memset (cookie, 0, cookie_size);
if (protocol_version == 3)
memset (cookieverf, 0, sizeof (cookieverf));
eof = 0;
*totalentries = 0;
while (!eof)
{
p = nfs_initialize_rpc (NFSPROC_READDIR (protocol_version),
cred, 0, &rpcbuf, dir, -1);
if (! p)
{
free (buf);
return errno;
}
p = xdr_encode_fhandle (p, &dir->nn->handle);
memcpy (p, cookie, cookie_size);
p += INTSIZE (cookie_size);
if (protocol_version == 3)
{
memcpy (p, cookieverf, sizeof (cookieverf));
p += INTSIZE (sizeof (cookieverf));
}
*(p++) = ntohl (read_size);
err = conduct_rpc (&rpcbuf, &p);
if (err)
{
free (rpcbuf);
free (buf);
return err;
}
err = nfs_error_trans (ntohl (*p));
p++;
if (protocol_version == 3)
p = process_returned_stat (dir, p, 1);
if (err)
{
free (rpcbuf);
free (buf);
return err;
}
if (protocol_version == 3)
{
memcpy (cookieverf, p, sizeof (cookieverf));
p += INTSIZE (sizeof (cookieverf));
}
isnext = ntohl (*p);
p++;
while (isnext)
{
ino_t fileno;
int namlen;
int reclen;
if (protocol_version == 2)
{
fileno = ntohl (*p);
p++;
}
else
p = xdr_decode_64bit(p, &fileno);
namlen = ntohl (*p);
p++;
reclen = sizeof (struct dirent) + namlen;
reclen = (reclen + 3) & ~3;
if (bp + reclen > buf + bufmalloced)
{
char *newbuf;
newbuf = realloc (buf, bufmalloced *= 2);
assert_backtrace (newbuf);
if (newbuf != buf)
bp = newbuf + (bp - buf);
buf = newbuf;
}
entry = (struct dirent *) bp;
entry->d_fileno = fileno;
entry->d_reclen = reclen;
entry->d_type = DT_UNKNOWN;
entry->d_namlen = namlen;
memcpy (entry->d_name, p, namlen);
entry->d_name[namlen] = '\0';
p += INTSIZE (namlen);
bp = bp + entry->d_reclen;
++*totalentries;
memcpy (cookie, p, cookie_size);
p += INTSIZE (cookie_size);
isnext = ntohl (*p);
p++;
}
eof = ntohl (*p);
p++;
free (rpcbuf);
}
*bufp = buf;
*bufsizep = bufmalloced;
return 0;
}
error_t
netfs_get_dirents (struct iouser *cred, struct node *np,
int entry, int nentries, char **data,
mach_msg_type_number_t *datacnt,
vm_size_t bufsiz, int *amt)
{
void *buf = NULL;
size_t our_bufsiz, allocsize;
void *bp;
char *userdp;
error_t err;
int totalentries;
int thisentry;
err = fetch_directory (cred, np, &buf, &our_bufsiz, &totalentries);
if (err)
return err;
if (!bufsiz || bufsiz > our_bufsiz)
allocsize = round_page (our_bufsiz);
else
allocsize = round_page (bufsiz);
if (allocsize > *datacnt)
*data = mmap (0, allocsize, PROT_READ|PROT_WRITE, MAP_ANON, 0, 0);
bp = buf;
for (thisentry = 0; thisentry < entry;)
{
struct dirent *entry = (struct dirent *) bp;
bp += entry->d_reclen;
thisentry++;
}
{
int entries_copied;
for (entries_copied = 0, userdp = *data;
(nentries == -1 || entries_copied < nentries)
&& (!bufsiz || userdp - *data < bufsiz)
&& thisentry < totalentries;)
{
struct dirent *entry = (struct dirent *) bp;
memcpy (userdp, bp, entry->d_reclen);
bp += entry->d_reclen;
userdp += entry->d_reclen;
entries_copied++;
thisentry++;
}
*amt = entries_copied;
}
free (buf);
if (allocsize > *datacnt
&& round_page (userdp - *data) < round_page (allocsize))
munmap ((caddr_t) round_page (userdp),
round_page (allocsize) - round_page (userdp - *data));
*datacnt = userdp - *data;
return 0;
}
error_t
netfs_attempt_mksymlink (struct iouser *cred,
struct node *np,
const char *arg)
{
if (np->nn->dtrans == NOT_POSSIBLE)
return EOPNOTSUPP;
if (np->nn->dtrans == SYMLINK)
free (np->nn->transarg.name);
np->nn->transarg.name = malloc (strlen (arg) + 1);
strcpy (np->nn->transarg.name, arg);
np->nn->dtrans = SYMLINK;
np->nn->stat_updated = 0;
return 0;
}
error_t
netfs_attempt_mkdev (struct iouser *cred,
struct node *np,
mode_t type,
dev_t indexes)
{
if (np->nn->dtrans == NOT_POSSIBLE)
return EOPNOTSUPP;
if (np->nn->dtrans == SYMLINK)
free (np->nn->transarg.name);
np->nn->transarg.indexes = indexes;
if (type == S_IFBLK)
np->nn->dtrans = BLKDEV;
else
np->nn->dtrans = CHRDEV;
np->nn->stat_updated = 0;
return 0;
}