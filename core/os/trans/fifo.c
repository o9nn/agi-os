#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <unistd.h>
#include <error.h>
#include <string.h>
#include <fcntl.h>
#include <argp.h>
#include <pthread.h>
#include <hurd.h>
#include <hurd/ports.h>
#include <hurd/trivfs.h>
#include <hurd/fsys.h>
#include <hurd/pipe.h>
#include <version.h>
#include "libtrivfs/trivfs_fs_S.h"
#include "libtrivfs/trivfs_io_S.h"
int wait_for_reader = 1, wait_for_writer = 1;
int one_reader = 1;
struct pipe_class *fifo_pipe_class;
struct pipe *active_fifo = NULL;
pthread_mutex_t active_fifo_lock;
pthread_cond_t active_fifo_changed;
const char *argp_program_version = STANDARD_HURD_VERSION (fifo);
static struct argp_option options[] =
{
{ "multiple-readers", 'm', 0, 0, "Allow multiple simultaneous readers" },
{ "noblock", 'n', 0, 0, "Don't block on open" },
{ "dgram", 'd', 0, 0, "Reads reflect write record boundaries" },
{ 0 }
};
static error_t
parse_opt (int key, char *arg, struct argp_state *state)
{
switch (key)
{
case 'm': one_reader = 0; break;
case 'n': wait_for_reader = wait_for_writer = 0; break;
case 'd': fifo_pipe_class = seqpack_pipe_class; break;
default: return ARGP_ERR_UNKNOWN;
}
return 0;
}
static const struct argp argp = {
options, parse_opt, 0, "Translator for fifos."
};
int
main (int argc, char **argv)
{
error_t err;
mach_port_t bootstrap;
struct trivfs_control *fsys;
fifo_pipe_class = stream_pipe_class;
argp_parse (&argp, argc, argv, 0, 0, 0);
task_get_bootstrap_port (mach_task_self (), &bootstrap);
if (bootstrap == MACH_PORT_NULL)
error (1, 0, "must be started as a translator");
err = trivfs_startup (bootstrap, 0, 0, 0, 0, 0, &fsys);
mach_port_deallocate (mach_task_self (), bootstrap);
if (err)
error (3, err, "Contacting parent");
do
{
ports_enable_class (fsys->protid_class);
ports_manage_port_operations_multithread (fsys->pi.bucket,
trivfs_demuxer,
30*1000, 5*60*1000, 0);
}
while (ports_count_class (fsys->protid_class) > 0);
return 0;
}
static error_t
open_hook (struct trivfs_peropen *po)
{
error_t err = 0;
int flags = po->openmodes;
if (flags & (O_READ | O_WRITE))
{
pthread_mutex_lock (&active_fifo_lock);
#define WAIT(condition, noblock_err) \
while (!err && !(condition)) \
{ \
if (flags & O_NONBLOCK) \
{ \
err = noblock_err; \
break; \
} \
else if (pthread_hurd_cond_wait_np (&active_fifo_changed, \
&active_fifo_lock)) \
err = EINTR; \
}
if (flags & O_READ)
{
if (one_reader)
WAIT (!active_fifo || !active_fifo->readers, EWOULDBLOCK);
if (!err && active_fifo == NULL)
{
err = pipe_create (fifo_pipe_class, &active_fifo);
if (! err)
active_fifo->flags &= ~PIPE_BROKEN;
}
if (!err)
{
pipe_add_reader (active_fifo);
pthread_cond_broadcast (&active_fifo_changed);
if (wait_for_writer && (!(flags & O_WRITE)))
{
WAIT (active_fifo->writers, 0);
if (err)
{
pipe_remove_reader (active_fifo);
active_fifo = NULL;
pthread_cond_broadcast (&active_fifo_changed);
}
}
}
}
if (!err && (flags & O_WRITE))
{
if (wait_for_reader)
WAIT (active_fifo && active_fifo->readers, ENXIO);
if (!err && active_fifo == NULL)
{
err = pipe_create (fifo_pipe_class, &active_fifo);
if (!err)
active_fifo->flags &= ~PIPE_BROKEN;
}
if (!err)
{
pipe_add_writer (active_fifo);
pthread_cond_broadcast (&active_fifo_changed);
}
}
po->hook = active_fifo;
pthread_mutex_unlock (&active_fifo_lock);
}
return err;
}
static void
close_hook (struct trivfs_peropen *po)
{
int was_active, detach = 0;
int flags = po->openmodes;
struct pipe *pipe = po->hook;
if (!pipe)
return;
pthread_mutex_lock (&active_fifo_lock);
was_active = (active_fifo == pipe);
if (was_active)
detach = ((flags & O_READ) && pipe->readers == 1);
else
pthread_mutex_unlock (&active_fifo_lock);
if (flags & O_READ)
pipe_remove_reader (pipe);
if (flags & O_WRITE)
pipe_remove_writer (pipe);
if (was_active)
{
if (detach)
active_fifo = NULL;
pthread_cond_broadcast (&active_fifo_changed);
pthread_mutex_unlock (&active_fifo_lock);
}
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
error_t
trivfs_goaway (struct trivfs_control *cntl, int flags)
{
error_t err;
int force = (flags & FSYS_GOAWAY_FORCE);
struct port_bucket *bucket = ((struct port_info *)cntl)->bucket;
err = ports_inhibit_bucket_rpcs (bucket);
if (err == EINTR || (err && !force))
return err;
if (ports_count_class (cntl->protid_class) > 0 && !force)
{
ports_enable_class (cntl->protid_class);
ports_resume_bucket_rpcs (bucket);
return EBUSY;
}
exit (0);
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
size_t data_size = *data_len;
if (!cred)
err = EOPNOTSUPP;
else if (!(cred->po->openmodes & O_READ))
err = EBADF;
else
{
struct pipe *pipe = cred->po->hook;
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
else
{
int flags = cred->po->openmodes;
struct pipe *pipe = cred->po->hook;
if (!(flags & O_WRITE))
err = EBADF;
else
{
pthread_mutex_lock (&pipe->lock);
err = pipe_write (pipe, flags & O_NONBLOCK, NULL,
data, data_len, amount);
pthread_mutex_unlock (&pipe->lock);
}
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