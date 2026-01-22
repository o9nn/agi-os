#include <string.h>
#include <stdio.h>
#include <hurd/paths.h>
#include "netfs.h"
#include "fs_S.h"
#include <sys/mman.h>
#include <sys/sysmacros.h>
kern_return_t
netfs_S_file_get_translator (struct protid *user,
data_t *trans,
mach_msg_type_number_t *translen)
{
struct node *np;
error_t err;
if (!user)
return EOPNOTSUPP;
np = user->po->np;
pthread_mutex_lock (&np->lock);
err = netfs_validate_stat (np, user->user);
if (err)
{
pthread_mutex_unlock (&np->lock);
return err;
}
if (np->nn_translated & S_IPTRANS)
{
char *string = NULL;
mach_msg_type_number_t len = 0;
err = netfs_get_translator (np, &string, &len);
if (!err)
{
if (len > *translen)
*trans = mmap (0, len, PROT_READ|PROT_WRITE, MAP_ANON, 0, 0);
memcpy (*trans, string, len);
*translen = len;
free (string);
}
}
else if (S_ISLNK (np->nn_stat.st_mode))
{
unsigned int len = sizeof _HURD_SYMLINK + np->nn_stat.st_size + 1;
if (len > *translen)
*trans = mmap (0, len, PROT_READ|PROT_WRITE, MAP_ANON, 0, 0);
memcpy (*trans, _HURD_SYMLINK, sizeof _HURD_SYMLINK);
err = netfs_attempt_readlink (user->user, np,
*trans + sizeof _HURD_SYMLINK);
if (!err)
{
(*trans)[sizeof _HURD_SYMLINK + np->nn_stat.st_size] = '\0';
*translen = len;
}
else
if (len > *translen)
munmap (*trans, len);
}
else if (S_ISCHR (np->nn_stat.st_mode) || S_ISBLK (np->nn_stat.st_mode))
{
char *buf;
int buflen;
buflen = asprintf (&buf, "%s%c%d%c%d",
(S_ISCHR (np->nn_stat.st_mode)
? _HURD_CHRDEV
: _HURD_BLKDEV),
'\0', gnu_dev_major (np->nn_stat.st_rdev),
'\0', gnu_dev_minor (np->nn_stat.st_rdev));
if (buflen < 0)
err = ENOMEM;
else
{
buflen++;
if (buflen > *translen)
*trans = mmap (0, buflen, PROT_READ|PROT_WRITE, MAP_ANON, 0, 0);
memcpy (*trans, buf, buflen);
free (buf);
*translen = buflen;
err = 0;
}
}
else if (S_ISFIFO (np->nn_stat.st_mode))
{
unsigned int len;
len = sizeof _HURD_FIFO;
if (len > *translen)
*trans = mmap (0, len, PROT_READ|PROT_WRITE, MAP_ANON, 0, 0);
memcpy (*trans, _HURD_FIFO, sizeof _HURD_FIFO);
*translen = len;
err = 0;
}
else if (S_ISSOCK (np->nn_stat.st_mode))
{
unsigned int len;
len = sizeof _HURD_IFSOCK;
if (len > *translen)
*trans = mmap (0, len, PROT_READ|PROT_WRITE, MAP_ANON, 0, 0);
memcpy (*trans, _HURD_IFSOCK, sizeof _HURD_IFSOCK);
*translen = len;
err = 0;
}
else
err = EINVAL;
pthread_mutex_unlock (&np->lock);
return err;
}