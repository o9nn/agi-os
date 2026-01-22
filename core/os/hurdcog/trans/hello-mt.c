#define _GNU_SOURCE 1
#include <hurd/trivfs.h>
#include <stdio.h>
#include <stdlib.h>
#include <argp.h>
#include <argz.h>
#include <error.h>
#include <string.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <pthread.h>
#include <version.h>
#include "libtrivfs/trivfs_io_S.h"
const char *argp_program_version = STANDARD_HURD_VERSION (hello-mt);
static const char hello[] = "Hello, world!\n";
static char *contents = (char *) hello;
static size_t contents_len = sizeof hello - 1;
static pthread_rwlock_t contents_lock;
int trivfs_fstype = FSTYPE_MISC;
int trivfs_fsid = 0;
int trivfs_allow_open = O_READ;
int trivfs_support_read = 1;
int trivfs_support_write = 0;
int trivfs_support_exec = 0;
struct open
{
pthread_mutex_t lock;
off_t offs;
};
void
trivfs_modify_stat (struct trivfs_protid *cred, struct stat *st)
{
st->st_mode &= ~(S_IFMT | ALLPERMS);
st->st_mode |= (S_IFREG | S_IRUSR | S_IRGRP | S_IROTH);
st->st_size = contents_len;
}
error_t
trivfs_goaway (struct trivfs_control *cntl, int flags)
{
exit (0);
}
static error_t
open_hook (struct trivfs_peropen *peropen)
{
struct open *op = malloc (sizeof (struct open));
if (op == NULL)
return ENOMEM;
op->offs = 0;
pthread_mutex_init (&op->lock, NULL);
peropen->hook = op;
return 0;
}
static void
close_hook (struct trivfs_peropen *peropen)
{
struct open *op = peropen->hook;
pthread_mutex_destroy (&op->lock);
free (op);
}
kern_return_t
trivfs_S_io_read (struct trivfs_protid *cred,
mach_port_t reply, mach_msg_type_name_t reply_type,
data_t *data, mach_msg_type_number_t *data_len,
off_t offs, vm_size_t amount)
{
struct open *op;
if (! cred)
return EOPNOTSUPP;
else if (! (cred->po->openmodes & O_READ))
return EBADF;
op = cred->po->hook;
pthread_mutex_lock (&op->lock);
if (offs == -1)
offs = op->offs;
pthread_rwlock_rdlock (&contents_lock);
if (offs > contents_len)
offs = contents_len;
if (offs + amount > contents_len)
amount = contents_len - offs;
if (amount > 0)
{
if (*data_len < amount)
*data = mmap (0, amount, PROT_READ|PROT_WRITE, MAP_ANON, 0, 0);
if (*data == MAP_FAILED)
{
pthread_mutex_unlock (&op->lock);
pthread_rwlock_unlock (&contents_lock);
return ENOMEM;
}
memcpy ((char *) *data, contents + offs, amount);
op->offs += amount;
}
pthread_mutex_unlock (&op->lock);
pthread_rwlock_unlock (&contents_lock);
*data_len = amount;
return 0;
}
kern_return_t
trivfs_S_io_seek (struct trivfs_protid *cred,
mach_port_t reply, mach_msg_type_name_t reply_type,
off_t offs, int whence, off_t *new_offs)
{
struct open *op;
error_t err = 0;
if (! cred)
return EOPNOTSUPP;
op = cred->po->hook;
pthread_mutex_lock (&op->lock);
switch (whence)
{
case SEEK_CUR:
offs += op->offs;
goto check;
case SEEK_END:
offs += contents_len;
case SEEK_SET:
check:
if (offs >= 0)
{
*new_offs = op->offs = offs;
break;
}
default:
err = EINVAL;
}
pthread_mutex_unlock (&op->lock);
return err;
}
error_t (*trivfs_peropen_create_hook)(struct trivfs_peropen *) = open_hook;
void (*trivfs_peropen_destroy_hook) (struct trivfs_peropen *) = close_hook;
static const struct argp_option options[] =
{
{"contents",	'c', "STRING",	0, "Specify the contents of the virtual file"},
{0}
};
static error_t
parse_opt (int opt, char *arg, struct argp_state *state)
{
switch (opt)
{
default:
return ARGP_ERR_UNKNOWN;
case ARGP_KEY_INIT:
case ARGP_KEY_SUCCESS:
case ARGP_KEY_ERROR:
break;
case 'c':
{
char *new = strdup (arg);
if (new == NULL)
return ENOMEM;
pthread_rwlock_wrlock (&contents_lock);
if (contents != hello)
free (contents);
contents = new;
contents_len = strlen (new);
pthread_rwlock_unlock (&contents_lock);
break;
}
}
return 0;
}
error_t
trivfs_append_args (struct trivfs_control *fsys,
char **argz, size_t *argz_len)
{
error_t err;
char *opt;
size_t opt_len;
FILE *s;
char *c;
s = open_memstream (&opt, &opt_len);
fprintf (s, "--contents='");
pthread_rwlock_rdlock (&contents_lock);
for (c = contents; *c; c++)
switch (*c)
{
case 0x27:
fprintf (s, "'\"'\"'");
break;
default:
fprintf (s, "%c", *c);
}
pthread_rwlock_unlock (&contents_lock);
fprintf (s, "'");
fclose (s);
err = argz_add (argz, argz_len, opt);
free (opt);
return err;
}
static struct argp hello_argp =
{ options, parse_opt, 0,
"A multi-threaded translator providing a warm greeting." };
struct argp *trivfs_runtime_argp = &hello_argp;
int
main (int argc, char **argv)
{
error_t err;
mach_port_t bootstrap;
struct trivfs_control *fsys;
pthread_rwlock_init (&contents_lock, NULL);
argp_parse (&hello_argp, argc, argv, 0, 0, 0);
task_get_bootstrap_port (mach_task_self (), &bootstrap);
if (bootstrap == MACH_PORT_NULL)
error (1, 0, "Must be started as a translator");
err = trivfs_startup (bootstrap, 0, 0, 0, 0, 0, &fsys);
mach_port_deallocate (mach_task_self (), bootstrap);
if (err)
error (3, err, "trivfs_startup");
ports_manage_port_operations_multithread (fsys->pi.bucket, trivfs_demuxer,
10 * 1000,
10 * 60 * 1000,
0);
return 0;
}