#include <stdio.h>
#include <errno.h>
#include <argp.h>
#include <unistd.h>
#include <error.h>
#include <string.h>
#include <fcntl.h>
#include <assert-backtrace.h>
#include <pthread.h>
#include <hurd.h>
#include <argz.h>
#include <hurd/fshelp.h>
#include <hurd/ports.h>
#include <hurd/trivfs.h>
#include <hurd/fsys.h>
#include <hurd/pipe.h>
#include <hurd/paths.h>
#include <version.h>
#include "libtrivfs/trivfs_fs_S.h"
#include "libtrivfs/trivfs_fsys_S.h"
#include "libtrivfs/trivfs_io_S.h"
#define DEFAULT_SERVER _SERVERS "fifo";
const char *argp_program_version = STANDARD_HURD_VERSION (new-fifo);
struct port_bucket *port_bucket;
struct port_class *fifo_port_class, *server_port_class, *fsys_port_class;
static const struct argp_option options[] =
{
{"multiple-readers", 'r', 0,     0, "Allow multiple simultaneous readers"},
{"noblock",          'n', 0,     0, "Don't block on open"},
{"dgram",            'd', 0,     0, "Reflect write record boundaries"},
{"server",	       's', 0,     0, "Operate in server mode"},
{"standalone",       'S', 0,     0, "Don't attempt to use a fifo server"},
{"use-server",       'U', "NAME",0, "Attempt use server NAME"},
{0,0}
};
struct fifo_trans
{
int server;
int wait_for_reader;
int wait_for_writer;
int one_reader;
char *use_server;
struct fifo_trans *parent;
struct pipe_class *fifo_pipe_class;
struct pipe *active_fifo;
pthread_mutex_t active_fifo_lock;
pthread_cond_t active_fifo_changed;
};
static void
fifo_trans_create (struct fifo_trans *from, struct fifo_trans **trans)
{
struct fifo_trans *new = malloc (sizeof (struct fifo_trans));
new->server = 0;
pthread_mutex_init (&new->active_fifo_lock, NULL);
pthread_cond_init (&new->active_fifo_changed, NULL);
new->parent = from;
if (from)
{
new->wait_for_reader = from->wait_for_reader;
new->wait_for_writer = from->wait_for_writer;
new->one_reader = from->one_reader;
new->use_server = from->use_server;
new->fifo_pipe_class = from->fifo_pipe_class;
}
else
{
new->wait_for_reader = 1;
new->wait_for_writer = 1;
new->one_reader = 1;
new->use_server = DEFAULT_SERVER;
new->fifo_pipe_class = stream_pipe_class;
}
*trans = new;
}
static void
fifo_trans_free (struct fifo_trans *trans)
{
free (trans);
}
static error_t
fifo_trans_start (struct fifo_trans *trans, mach_port_t requestor)
{
struct trivfs_control *control;
struct port_class *class =
(trans->server ? server_port_class : fifo_port_class);
error_t
err = trivfs_startup (requestor, 0,
fsys_port_class, port_bucket, class, port_bucket,
&control);
if (!err)
control->hook = trans;
return err;
}
static error_t
fifo_trans_parse_args (struct fifo_trans *trans, int argc, char **argv,
int print_errs)
{
error_t parse_opt (int key, char *arg, struct argp_state *state)
{
switch (key)
{
case 'r': trans->one_reader = 0; break;
case 'n': trans->wait_for_reader = trans->wait_for_writer = 0; break;
case 'd': trans->fifo_pipe_class = seqpack_pipe_class;
case 's': trans->server = 1; break;
case 'U': trans->use_server = arg; break;
case 'S': trans->use_server = 0; break;
default:
return ARGP_ERR_UNKNOWN;
}
return 0;
}
struct argp argp = {options, parse_opt, 0, "A translator for fifos." };
return argp_parse (&argp, argc, argv, print_errs ? 0 : ARGP_SILENT, 0, 0);
}
int
main (int argc, char **argv)
{
error_t err;
mach_port_t bootstrap;
struct fifo_trans *trans;
void clean_fsys (void *vfsys)
{
struct trivfs_control *fsys = vfsys;
if (fsys->hook)
fifo_trans_free (fsys->hook);
trivfs_clean_cntl (fsys);
}
fifo_trans_create (0, &trans);
if (fifo_trans_parse_args (trans, argc, argv, 1) != 0)
exit (1);
task_get_bootstrap_port (mach_task_self (), &bootstrap);
if (bootstrap == MACH_PORT_NULL)
error(1, 0, "must be started as a translator");
if (!trans->server && trans->use_server)
{
err = fshelp_delegate_translation (trans->use_server, bootstrap, argv);
if (!err)
exit (0);
}
err = trivfs_add_port_bucket (&port_bucket);
if (err)
error (1, 0, "error creating port bucket");
err = trivfs_add_control_port_class (&fsys_port_class);
if (err)
error (1, 0, "error creating control port class");
err = trivfs_add_protid_port_class (&fifo_port_class);
if (err)
error (1, 0, "error creating protid port class");
err = trivfs_add_protid_port_class (&server_port_class);
if (err)
error (1, 0, "error creating protid port class");
fifo_trans_start (trans, bootstrap);
do
{
ports_enable_class (fifo_port_class);
ports_manage_port_operations_multithread (port_bucket,
trivfs_demuxer,
30*1000, 5*60*1000, 0);
}
while (ports_count_class (fifo_port_class) > 0);
return 0;
}
static error_t
fifo_trans_open (struct fifo_trans *trans, int flags, void **hook)
{
error_t err = 0;
if (flags & (O_READ | O_WRITE))
{
pthread_mutex_lock (&trans->active_fifo_lock);
#define WAIT(condition, noblock_err)					      \
while (!err && !(condition))						      \
{									      \
if (flags & O_NONBLOCK)						      \
{								      \
err = noblock_err;						      \
break;							      \
}								      \
else if (pthread_hurd_cond_wait_np (&trans->active_fifo_changed,	      \
&trans->active_fifo_lock))	      \
err = EINTR;							      \
}
if (flags & O_READ)
{
if (trans->one_reader)
WAIT (!trans->active_fifo || !trans->active_fifo->readers,
EWOULDBLOCK);
if (!err && trans->active_fifo == NULL)
err = pipe_create (trans->fifo_pipe_class, &trans->active_fifo);
if (!err)
{
pipe_add_reader (trans->active_fifo);
pthread_cond_broadcast (&trans->active_fifo_changed);
if (trans->wait_for_writer)
{
WAIT (trans->active_fifo->writers, 0);
if (err)
{
pipe_remove_reader (trans->active_fifo);
trans->active_fifo = NULL;
pthread_cond_broadcast (&trans->active_fifo_changed);
}
}
else
trans->active_fifo->flags &= ~PIPE_BROKEN;
}
}
if (!err && (flags & O_WRITE))
{
if (trans->wait_for_reader)
WAIT (trans->active_fifo && trans->active_fifo->readers, 0);
if (!err && trans->active_fifo == NULL)
{
err = pipe_create (trans->fifo_pipe_class, &trans->active_fifo);
if (!err)
trans->active_fifo->flags &= ~PIPE_BROKEN;
}
if (!err)
{
pipe_add_writer (trans->active_fifo);
pthread_cond_broadcast (&trans->active_fifo_changed);
}
}
*hook = trans->active_fifo;
}
pthread_mutex_unlock (&trans->active_fifo_lock);
return err;
}
static void
fifo_trans_close (struct fifo_trans *trans, struct trivfs_peropen *po)
{
int was_active, going_away = 0;
int flags = po->openmodes;
struct pipe *pipe = po->hook;
if (!pipe)
return;
pthread_mutex_lock (&trans->active_fifo_lock);
was_active = (trans->active_fifo == pipe);
if (was_active)
going_away = ((flags & O_READ) && pipe->readers == 1);
else
pthread_mutex_unlock (&trans->active_fifo_lock);
if (flags & O_READ)
pipe_remove_reader (pipe);
if (flags & O_WRITE)
pipe_remove_writer (pipe);
if (was_active)
{
if (going_away)
trans->active_fifo = NULL;
pthread_cond_broadcast (&trans->active_fifo_changed);
pthread_mutex_unlock (&trans->active_fifo_lock);
}
}
static error_t
open_hook (struct trivfs_peropen *po)
{
struct fifo_trans *trans = po->cntl->hook;
if (! trans->server)
return fifo_trans_open (trans, po->openmodes, &po->hook);
else if (po->openmodes & (O_READ|O_WRITE|O_APPEND))
return EPERM;
else
return 0;
}
static void
close_hook (struct trivfs_peropen *po)
{
struct fifo_trans *trans = po->cntl->hook;
if (! trans->server)
fifo_trans_close (trans, po);
}
int trivfs_fstype = FSTYPE_MISC;
int trivfs_fsid = 0;
int trivfs_support_read = 1;
int trivfs_support_write = 1;
int trivfs_support_exec = 0;
int trivfs_allow_open = O_READ | O_WRITE;
error_t (*trivfs_peropen_create_hook) (struct trivfs_peropen *) = open_hook;
void (*trivfs_peropen_destroy_hook) (struct trivfs_peropen *) = close_hook;
void
trivfs_modify_stat (struct trivfs_protid *cred, struct stat *st)
{
struct fifo_trans *trans = cred->po->cntl->hook;
if (! trans->server)
{
struct pipe *pipe = cred->po->hook;
st->st_mode &= ~S_IFMT;
st->st_mode |= S_IFIFO;
if (pipe)
{
pthread_mutex_lock (&pipe->lock);
st->st_size = pipe_readable (pipe, 1);
st->st_blocks = st->st_size >> 9;
pthread_mutex_unlock (&pipe->lock);
}
else
st->st_size = st->st_blocks = 0;
st->st_blksize = vm_page_size * 16;
}
}
error_t
trivfs_goaway (struct trivfs_control *fsys, int flags)
{
error_t err;
int num_opens;
int force = flags & FSYS_GOAWAY_FORCE;
int unlink = flags & FSYS_GOAWAY_UNLINK;
struct fifo_trans *trans = fsys->hook;
err = ports_inhibit_port_rpcs (fsys);
if (err == EINTR || (err && !force))
return err;
num_opens = ports_count_class (fsys->protid_class);
if (num_opens > 0 && !force && !unlink)
{
ports_enable_class (fsys->protid_class);
ports_resume_port_rpcs (fsys);
return EBUSY;
}
mach_port_deallocate (mach_task_self (), fsys->underlying);
fsys->underlying = MACH_PORT_NULL;
ports_destroy_right (fsys);
if (force)
{
error_t maybe_trash_protid (void *vcred)
{
struct trivfs_protid *cred = vcred;
if (cred->po->cntl == fsys)
{
ports_destroy_right (cred);
ports_interrupt_rpcs (cred);
}
return 0;
}
ports_bucket_iterate (((struct port_info *)fsys)->bucket,
maybe_trash_protid);
}
if (! trans->parent)
exit (0);
ports_enable_class (fsys->protid_class);
ports_resume_port_rpcs (fsys);
return 0;
}
kern_return_t
trivfs_S_io_map (struct trivfs_protid *cred,
mach_port_t reply, mach_msg_type_name_t replytype,
memory_object_t *rdobj,
mach_msg_type_name_t *rdtype,
memory_object_t *wrobj,
mach_msg_type_name_t *wrtype)
{
return EOPNOTSUPP;
}
kern_return_t
trivfs_S_io_read (struct trivfs_protid *cred,
mach_port_t reply, mach_msg_type_name_t reply_type,
data_t *data, mach_msg_type_name_t *data_len,
off_t offs, vm_size_t amount)
{
error_t err;
if (!cred)
err = EOPNOTSUPP;
else if (!(cred->po->openmodes & O_READ))
err = EBADF;
else
{
struct pipe *pipe = cred->po->hook;
size_t data_size = *data_len;
assert_backtrace (pipe);
pthread_mutex_lock (&pipe->lock);
err = pipe_read (pipe, cred->po->openmodes & O_NONBLOCK, NULL,
data, &data_size, amount);
pthread_mutex_unlock (&pipe->lock);
*data_len = data_size;
}
return err;
}
kern_return_t
trivfs_S_io_readable (struct trivfs_protid *cred,
mach_port_t reply, mach_msg_type_name_t reply_type,
vm_size_t *amount)
{
error_t err;
if (!cred)
err = EOPNOTSUPP;
else if (!(cred->po->openmodes & O_READ))
err = EBADF;
else
{
struct pipe *pipe = cred->po->hook;
assert_backtrace (pipe);
pthread_mutex_lock (&pipe->lock);
*amount = pipe_readable (pipe, 1);
pthread_mutex_unlock (&pipe->lock);
err = 0;
}
return err;
}
kern_return_t
trivfs_S_io_seek (struct trivfs_protid *cred,
mach_port_t reply, mach_msg_type_name_t reply_type,
off_t offset, int whence, off_t *new_offset)
{
if (!cred)
return EOPNOTSUPP;
return ESPIPE;
}
static error_t
io_select_common (struct trivfs_protid *cred,
mach_port_t reply, mach_msg_type_name_t reply_type,
struct timespec *tsp, int *select_type)
{
struct pipe *pipe;
error_t err = 0;
int ready = 0;
if (!cred)
return EOPNOTSUPP;
pipe = cred->po->hook;
if (*select_type & SELECT_READ)
{
if (cred->po->openmodes & O_READ)
{
pthread_mutex_lock (&pipe->lock);
err = pipe_wait_readable (pipe, 1, 1);
if (err == EWOULDBLOCK)
err = 0;
else
ready |= SELECT_READ;
pthread_mutex_unlock (&pipe->lock);
}
else
{
ready |= SELECT_READ;
}
if (err)
*select_type &= ~SELECT_WRITE;
}
if (*select_type & SELECT_WRITE)
{
if (cred->po->openmodes & O_WRITE)
{
pthread_mutex_lock (&pipe->lock);
err = pipe_wait_writable (pipe, 1);
if (err == EWOULDBLOCK)
err = 0;
else
ready |= SELECT_WRITE;
pthread_mutex_unlock (&pipe->lock);
}
else
{
ready |= SELECT_WRITE;
}
}
if (ready)
*select_type = ready;
else
{
ports_interrupt_self_on_port_death (cred, reply);
err = pipe_pair_select (pipe, pipe, tsp, select_type, 1);
}
return err;
}
kern_return_t
trivfs_S_io_select (struct trivfs_protid *cred,
mach_port_t reply, mach_msg_type_name_t reply_type,
int *select_type)
{
return io_select_common (cred, reply, reply_type, NULL, select_type);
}
kern_return_t
trivfs_S_io_select_timeout (struct trivfs_protid *cred,
mach_port_t reply, mach_msg_type_name_t reply_type,
struct timespec ts,
int *select_type)
{
return io_select_common (cred, reply, reply_type, &ts, select_type);
}
kern_return_t
trivfs_S_io_write (struct trivfs_protid *cred,
mach_port_t reply, mach_msg_type_name_t reply_type,
const_data_t data, mach_msg_type_name_t data_len,
off_t offs, vm_size_t *amount)
{
error_t err;
if (!cred)
err = EOPNOTSUPP;
else if (!(cred->po->openmodes & O_WRITE))
err = EBADF;
else
{
struct pipe *pipe = cred->po->hook;
pthread_mutex_lock (&pipe->lock);
err = pipe_write (pipe, cred->po->openmodes & O_NONBLOCK, NULL,
data, data_len, amount);
pthread_mutex_unlock (&pipe->lock);
}
return err;
}
kern_return_t
trivfs_S_file_set_size (struct trivfs_protid *cred,
mach_port_t reply, mach_msg_type_name_t reply_type,
off_t size)
{
return size == 0 ? 0 : EINVAL;
}
kern_return_t
trivfs_S_io_get_openmodes (struct trivfs_protid *cred,
mach_port_t reply, mach_msg_type_name_t reply_type,
int *bits)
{
if (!cred)
return EOPNOTSUPP;
else
{
*bits = cred->po->openmodes;
return 0;
}
}
kern_return_t
trivfs_S_io_set_all_openmodes(struct trivfs_protid *cred,
mach_port_t reply,
mach_msg_type_name_t reply_type,
int mode)
{
if (!cred)
return EOPNOTSUPP;
else
return 0;
}
kern_return_t
trivfs_S_io_set_some_openmodes (struct trivfs_protid *cred,
mach_port_t reply,
mach_msg_type_name_t reply_type,
int bits)
{
if (!cred)
return EOPNOTSUPP;
else
return 0;
}
kern_return_t
trivfs_S_io_clear_some_openmodes (struct trivfs_protid *cred,
mach_port_t reply,
mach_msg_type_name_t reply_type,
int bits)
{
if (!cred)
return EOPNOTSUPP;
else
return 0;
}
kern_return_t
trivfs_S_io_get_owner (struct trivfs_protid *cred,
mach_port_t reply,
mach_msg_type_name_t reply_type,
pid_t *owner)
{
if (!cred)
return EOPNOTSUPP;
*owner = 0;
return 0;
}
kern_return_t
trivfs_S_io_mod_owner (struct trivfs_protid *cred,
mach_port_t reply, mach_msg_type_name_t reply_type,
pid_t owner)
{
if (!cred)
return EOPNOTSUPP;
else
return EINVAL;
}
kern_return_t
trivfs_S_fsys_forward (mach_port_t server,
mach_port_t reply,
mach_msg_type_name_t replytype,
mach_port_t requestor,
const_data_t argz,
mach_msg_type_number_t argz_len)
{
error_t err;
struct fifo_trans *server_trans, *trans;
int argc = argz_count (argz, argz_len);
char **argv = alloca (sizeof (char *) * (argc + 1));
struct trivfs_protid *cred =
ports_lookup_port (port_bucket, server, server_port_class);
if (!cred)
return EOPNOTSUPP;
server_trans = cred->po->cntl->hook;
assert_backtrace (server_trans->server);
argz_extract (argz, argz_len, argv);
fifo_trans_create (server_trans, &trans);
err = fifo_trans_parse_args (trans, argc, argv, 0);
if (!err)
fifo_trans_start (trans, requestor);
ports_port_deref (cred);
return err;
}