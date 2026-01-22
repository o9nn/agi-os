#include <stdio.h>
#include <error.h>
#include <assert-backtrace.h>
#include <fcntl.h>
#include <argp.h>
#include <argz.h>
#include <sys/sysmacros.h>
#include <stdbool.h>
#include <hurd.h>
#include <hurd/ports.h>
#include <hurd/trivfs.h>
#include <version.h>
#include "open.h"
#include "dev.h"
#include "libtrivfs/trivfs_fsys_S.h"
static struct argp_option options[] =
{
{"readonly", 'r', 0, 0,"Disallow writing"},
{"writable", 'w', 0, 0,"Allow writing"},
{"no-cache", 'c', 0, 0,"Never cache data--user io does direct device io"},
{"no-file-io", 'F', 0, 0,"Never perform io via plain file io RPCs"},
{"no-fileio", 0, 0, OPTION_ALIAS | OPTION_HIDDEN},
{"enforced", 'e', 0, 0,"Never reveal underlying devices, even to root"},
{"debug", 'd', "PATH", 0,
"Launch a standalone translator, for debug purposes"},
{"rdev", 'n', "ID", 0,
"The stat rdev number for this node; may be either a"
" single integer, or of the form MAJOR,MINOR"},
{0}
};
static const char doc[] = "Translator for devices and other stores";
const char *argp_program_version = STANDARD_HURD_VERSION (storeio);
static bool debug=false;
static char *debug_fname=NULL;
struct storeio_argp_params
{
struct store_argp_params store_params;
struct dev *dev;
};
static error_t
parse_opt (int key, char *arg, struct argp_state *state)
{
struct storeio_argp_params *params = state->input;
switch (key)
{
case 'r': params->dev->readonly = 1; break;
case 'w': params->dev->readonly = 0; break;
case 'c': params->dev->inhibit_cache = 1; break;
case 'e': params->dev->enforced = 1; break;
case 'F': params->dev->no_fileio = 1; break;
case 'n':
{
char *start = arg, *end;
dev_t rdev;
rdev = strtoul (start, &end, 0);
if (*end == ',')
{
start = end + 1;
rdev = gnu_dev_makedev (rdev, strtoul (start, &end, 0));
}
if (end == start || *end != '\0')
{
argp_error (state, "%s: Invalid argument to --rdev", arg);
return EINVAL;
}
params->dev->rdev = rdev;
}
break;
case 'd':
{
debug=true;
char *new = strdup (arg);
if (new == NULL)
return ENOMEM;
debug_fname = new;
}
break;
case ARGP_KEY_INIT:
memset (&params->store_params, 0, sizeof params->store_params);
params->store_params.default_type = "device";
params->store_params.store_optional = 1;
state->child_inputs[0] = &params->store_params;
break;
case ARGP_KEY_SUCCESS:
params->dev->store_name = params->store_params.result;
break;
default:
return ARGP_ERR_UNKNOWN;
}
return 0;
}
static const struct argp_child argp_kids[] = { { &store_argp }, {0} };
static const struct argp argp = { options, parse_opt, 0, doc, argp_kids };
struct trivfs_control *storeio_fsys;
int
main (int argc, char *argv[])
{
error_t err;
mach_port_t bootstrap;
struct dev device;
struct storeio_argp_params params;
memset (&device, 0, sizeof device);
pthread_mutex_init (&device.lock, NULL);
params.dev = &device;
argp_parse (&argp, argc, argv, 0, 0, &params);
if (debug)
{
if (!debug_fname)
error (3, EINVAL, "missing translated node");
err = trivfs_startup_debug (debug_fname, 0, 0, 0, 0, &storeio_fsys);
if (err)
error (3, err, "trivfs_startup_debug failed");
}
else
{
task_get_bootstrap_port (mach_task_self (), &bootstrap);
if (bootstrap == MACH_PORT_NULL)
error (2, 0, "Must be started as a translator");
err = trivfs_startup (bootstrap, 0, 0, 0, 0, 0, &storeio_fsys);
if (err)
error (3, err, "trivfs_startup");
}
storeio_fsys->hook = &device;
ports_manage_port_operations_multithread (storeio_fsys->pi.bucket,
trivfs_demuxer,
30*1000, 5*60*1000, 0);
return 0;
}
error_t
trivfs_append_args (struct trivfs_control *trivfs_control,
char **argz, size_t *argz_len)
{
struct dev *const dev = trivfs_control->hook;
error_t err = 0;
if (dev->rdev != (dev_t) 0)
{
char buf[40];
snprintf (buf, sizeof buf, "--rdev=%d,%d",
gnu_dev_major (dev->rdev), gnu_dev_minor (dev->rdev));
err = argz_add (argz, argz_len, buf);
}
if (!err && dev->inhibit_cache)
err = argz_add (argz, argz_len, "--no-cache");
if (!err && dev->enforced)
err = argz_add (argz, argz_len, "--enforced");
if (!err && dev->no_fileio)
err = argz_add (argz, argz_len, "--no-file-io");
if (! err)
err = argz_add (argz, argz_len,
dev->readonly ? "--readonly" : "--writable");
if (! err)
err = store_parsed_append_args (dev->store_name, argz, argz_len);
return err;
}
static error_t
getroot_hook (struct trivfs_control *cntl,
mach_port_t reply_port,
mach_msg_type_name_t reply_port_type,
mach_port_t dotdot,
const uid_t *uids, mach_msg_type_number_t nuids, const uid_t *gids, mach_msg_type_number_t ngids,
int flags,
retry_type *do_retry, char *retry_name,
mach_port_t *node, mach_msg_type_name_t *node_type)
{
struct dev *const dev = cntl->hook;
return (dev_is_readonly (dev) && (flags & O_WRITE)) ? EROFS : EAGAIN;
}
static error_t
check_open_hook (struct trivfs_control *trivfs_control,
struct iouser *user,
int flags)
{
struct dev *const dev = trivfs_control->hook;
error_t err = 0;
if (!err && dev_is_readonly (dev) && (flags & O_WRITE))
return EROFS;
pthread_mutex_lock (&dev->lock);
if (dev->store == NULL)
{
err = dev_open (dev);
if (err && (flags & (O_READ|O_WRITE)) == 0)
err = 0;
}
pthread_mutex_unlock (&dev->lock);
return err;
}
static error_t
open_hook (struct trivfs_peropen *peropen)
{
error_t err = 0;
struct dev *const dev = peropen->cntl->hook;
if (dev->store)
{
pthread_mutex_lock (&dev->lock);
if (dev->nperopens++ == 0)
err = store_clear_flags (dev->store, STORE_INACTIVE);
pthread_mutex_unlock (&dev->lock);
if (!err)
err = open_create (dev, (struct open **)&peropen->hook);
}
return err;
}
static void
close_hook (struct trivfs_peropen *peropen)
{
struct dev *const dev = peropen->cntl->hook;
if (peropen->hook)
{
pthread_mutex_lock (&dev->lock);
if (--dev->nperopens == 0)
store_set_flags (dev->store, STORE_INACTIVE);
pthread_mutex_unlock (&dev->lock);
open_free (peropen->hook);
}
}
int trivfs_fstype = FSTYPE_DEV;
int trivfs_fsid = 0;
int trivfs_support_read = 1;
int trivfs_support_write = 1;
int trivfs_support_exec = 0;
int trivfs_allow_open = O_READ | O_WRITE;
void
trivfs_modify_stat (struct trivfs_protid *cred, struct stat *st)
{
struct dev *const dev = cred->po->cntl->hook;
struct open *open = cred->po->hook;
st->st_mode &= ~S_IFMT;
if (open)
{
struct store *store = open->dev->store;
store_offset_t size = store->size;
if (store->block_size > 1)
st->st_blksize = store->block_size;
st->st_size = size;
st->st_mode |= ((dev->inhibit_cache || store->block_size == 1)
? S_IFCHR : S_IFBLK);
}
else
{
st->st_blksize = 0;
st->st_size = 0;
st->st_mode |= dev->inhibit_cache ? S_IFCHR : S_IFBLK;
}
st->st_rdev = dev->rdev;
if (dev_is_readonly (dev))
st->st_mode &= ~(S_IWUSR | S_IWGRP | S_IWOTH);
}
error_t
trivfs_goaway (struct trivfs_control *fsys, int flags)
{
struct dev *const device = fsys->hook;
error_t err;
int force = (flags & FSYS_GOAWAY_FORCE);
int nosync = (flags & FSYS_GOAWAY_NOSYNC);
struct port_class *root_port_class = fsys->protid_class;
pthread_mutex_lock (&device->lock);
if (device->store == NULL)
exit (0);
err = ports_inhibit_class_rpcs (root_port_class);
if (err == EINTR || (err && !force))
{
pthread_mutex_unlock (&device->lock);
return err;
}
if (force && nosync)
exit (0);
if (!force && ports_count_class (root_port_class) > 0)
goto busy;
if (! nosync)
dev_sync (device, 1);
if (dev_stop_paging (device, nosync) || force)
{
if (! nosync)
dev_close (device);
exit (0);
}
busy:
ports_enable_class (root_port_class);
ports_resume_class_rpcs (root_port_class);
pthread_mutex_unlock (&device->lock);
return EBUSY;
}
error_t (*trivfs_getroot_hook) (struct trivfs_control *cntl,
mach_port_t reply_port,
mach_msg_type_name_t reply_port_type,
mach_port_t dotdot,
const uid_t *uids, mach_msg_type_number_t nuids, const uid_t *gids, mach_msg_type_number_t ngids,
int flags,
retry_type *do_retry, char *retry_name,
mach_port_t *node, mach_msg_type_name_t *node_type)
= getroot_hook;
error_t (*trivfs_check_open_hook)(struct trivfs_control *trivfs_control,
struct iouser *user,
int flags)
= check_open_hook;
error_t (*trivfs_peropen_create_hook)(struct trivfs_peropen *) = open_hook;
void (*trivfs_peropen_destroy_hook) (struct trivfs_peropen *) = close_hook;
kern_return_t
trivfs_S_fsys_syncfs (struct trivfs_control *cntl,
mach_port_t reply, mach_msg_type_name_t replytype,
int wait, int dochildren)
{
struct dev *dev = cntl->hook;
if (dev)
return dev_sync (dev, wait);
else
return 0;
}