#include <argp.h>
#include <argz.h>
#include <assert-backtrace.h>
#include <error.h>
#include <fcntl.h>
#include <gcrypt.h>
#include <hurd/paths.h>
#include <hurd/startup.h>
#include <hurd/trivfs.h>
#include <mach/gnumach.h>
#include <mach/vm_cache_statistics.h>
#include <mach/vm_param.h>
#include <mach/vm_statistics.h>
#include <mach_debug/mach_debug_types.h>
#include <maptime.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>
#include <signal.h>
#include <version.h>
#include "mach_debug_U.h"
char *trivfs_server_name = "random";
char *__trivfs_server_name = "random";
static gcry_md_hd_t pool;
enum gcry_md_algos hash_algo = GCRY_MD_SHAKE128;
static pthread_mutex_t pool_lock = PTHREAD_MUTEX_INITIALIZER;
volatile struct mapped_time_value *mtime;
static void
pool_initialize (void)
{
error_t err;
gcry_error_t cerr;
if (! gcry_check_version ("1.8.0"))
error (1, 0, "libgcrypt version mismatch\n");
cerr = gcry_control (GCRYCTL_INITIALIZATION_FINISHED, 0);
if (cerr)
error (1, 0, "Finalizing gcrypt failed: %s",
gcry_strerror (cerr));
cerr = gcry_md_open (&pool, hash_algo, GCRY_MD_FLAG_SECURE);
if (cerr)
error (1, 0, "Initializing hash failed: %s",
gcry_strerror (cerr));
err = maptime_map (1, NULL, &mtime);
if (err)
err = maptime_map (0, NULL, &mtime);
if (err)
error (1, err, "Failed to map time device");
}
static void
pool_add_entropy (const void *buffer, size_t length)
{
pthread_mutex_lock (&pool_lock);
gcry_md_write (pool, buffer, length);
pthread_mutex_unlock (&pool_lock);
}
static error_t
pool_randomize (void *buffer, size_t length)
{
gcry_error_t cerr;
pthread_mutex_lock (&pool_lock);
gcry_md_write (pool, (void *) mtime, sizeof *mtime);
cerr = gcry_md_extract (pool, hash_algo, buffer, length);
pthread_mutex_unlock (&pool_lock);
return cerr ? EIO : 0;
}
static char *seed_file;
size_t seed_size = 600;
static error_t
update_random_seed_file (void)
{
error_t err;
int fd;
void *map;
if (seed_file == NULL)
return 0;
fd = open (seed_file, O_RDWR|O_CREAT, 0600);
if (fd < 0)
return errno;
if (ftruncate (fd, seed_size))
{
err = errno;
goto out;
}
map = mmap (NULL, seed_size, PROT_READ|PROT_WRITE, MAP_SHARED, fd, 0);
if (map == MAP_FAILED)
{
err = errno;
goto out;
}
err = pool_randomize (map, seed_size);
munmap (map, seed_size);
out:
close (fd);
return err;
}
static error_t
read_random_seed_file (void)
{
error_t err = 0;
int fd;
struct stat s;
void *map;
if (seed_file == NULL)
return 0;
fd = open (seed_file, O_RDWR);
if (fd < 0)
return errno;
if (fstat (fd, &s))
{
err = errno;
goto out;
}
map = mmap (NULL, s.st_size, PROT_READ|PROT_WRITE, MAP_SHARED, fd, 0);
if (map == MAP_FAILED)
{
err = errno;
goto out;
}
pool_add_entropy (map, s.st_size);
pool_randomize (map, s.st_size);
munmap (map, s.st_size);
out:
close (fd);
return err;
}
static void
gather_slab_info (void)
{
error_t err;
cache_info_array_t cache_info;
mach_msg_type_number_t cache_info_count;
cache_info = NULL;
cache_info_count = 0;
err = host_slab_info (mach_host_self(), &cache_info, &cache_info_count);
if (err)
return;
pool_add_entropy (cache_info, cache_info_count * sizeof *cache_info);
vm_deallocate (mach_task_self (),
(vm_address_t) cache_info,
cache_info_count * sizeof *cache_info);
}
static void
gather_vm_statistics (void)
{
error_t err;
struct vm_statistics vmstats;
err = vm_statistics (mach_task_self (), &vmstats);
if (err)
return;
pool_add_entropy (&vmstats, sizeof vmstats);
}
static void
gather_vm_cache_statistics (void)
{
error_t err;
struct vm_cache_statistics cache_stats;
err = vm_cache_statistics (mach_task_self (), &cache_stats);
if (err)
return;
pool_add_entropy (&cache_stats, sizeof cache_stats);
}
static void *
gather_thread (void *args)
{
pthread_setname_np (pthread_self (), "gather");
while (1)
{
gather_slab_info ();
gather_vm_statistics ();
gather_vm_cache_statistics ();
usleep (
(useconds_t) (1000000. * (1.
+ (float) random () / (float) RAND_MAX)));
}
assert_backtrace (! "reached");
}
error_t
start_gather_thread (void)
{
error_t err;
pthread_t thread;
err = pthread_create (&thread, NULL, gather_thread, NULL);
if (err)
return err;
err = pthread_detach (thread);
return err;
}
const char *argp_program_version = STANDARD_HURD_VERSION (random);
struct trivfs_control *fsys;
int trivfs_fstype = FSTYPE_MISC;
int trivfs_fsid = 0;
int trivfs_allow_open = O_READ | O_WRITE;
int trivfs_support_read = 1;
int trivfs_support_write = 1;
int trivfs_support_exec = 0;
void
trivfs_modify_stat (struct trivfs_protid *cred, struct stat *st)
{
st->st_mode &= ~((unsigned) S_IFMT);
st->st_mode |= (S_IFCHR);
st->st_size = 0;
}
error_t
trivfs_goaway (struct trivfs_control *cntl, int flags)
{
error_t err;
err = update_random_seed_file ();
if (err)
error (0, err, "Warning: Failed to save random seed to %s", seed_file);
exit (0);
}
kern_return_t
trivfs_S_io_read (struct trivfs_protid *cred,
mach_port_t reply, mach_msg_type_name_t reply_type,
data_t *data, mach_msg_type_number_t *data_len,
off_t offs, vm_size_t amount)
{
error_t err;
void *buf = NULL;
size_t length = 0;
if (! cred)
return EOPNOTSUPP;
else if (! (cred->po->openmodes & O_READ))
return EBADF;
if (amount > 0)
{
if (*data_len < amount)
{
*data = mmap (0, amount, PROT_READ|PROT_WRITE, MAP_ANON, 0, 0);
if (*data == MAP_FAILED)
{
err = errno;
goto errout;
}
buf = *data, length = amount;
*data_len = amount;
}
err = pool_randomize (*data, amount);
if (err)
goto errout;
}
*data_len = amount;
trivfs_set_atime (fsys);
return 0;
errout:
if (buf)
munmap (buf, length);
return err;
}
kern_return_t
trivfs_S_io_write (struct trivfs_protid *cred,
mach_port_t reply,
mach_msg_type_name_t replytype,
const_data_t data,
mach_msg_type_number_t datalen,
off_t offset,
vm_size_t *amount)
{
if (! cred)
return EOPNOTSUPP;
else if (! (cred->po->openmodes & O_WRITE))
return EBADF;
pool_add_entropy (data, datalen);
*amount = datalen;
trivfs_set_mtime (fsys);
return 0;
}
kern_return_t
trivfs_S_io_readable (struct trivfs_protid *cred,
mach_port_t reply, mach_msg_type_name_t replytype,
mach_msg_type_number_t *amount)
{
if (! cred)
return EOPNOTSUPP;
else if (! (cred->po->openmodes & O_READ))
return EBADF;
*amount = PAGE_SIZE;
return 0;
}
error_t
trivfs_S_io_select (struct trivfs_protid *cred,
mach_port_t reply,
mach_msg_type_name_t reply_type,
int *type)
{
if (!cred)
return EOPNOTSUPP;
if (*type & ~(SELECT_READ | SELECT_WRITE))
return EINVAL;
return 0;
}
kern_return_t
trivfs_S_io_seek (struct trivfs_protid *cred,
mach_port_t reply, mach_msg_type_name_t reply_type,
loff_t offs, int whence, loff_t *new_offs)
{
if (! cred)
return EOPNOTSUPP;
return ESPIPE;
}
kern_return_t
trivfs_S_file_set_size (struct trivfs_protid *cred,
mach_port_t reply, mach_msg_type_name_t reply_type,
loff_t size)
{
if (!cred)
return EOPNOTSUPP;
return size == 0 ? 0 : EINVAL;
}
kern_return_t
trivfs_S_io_set_all_openmodes(struct trivfs_protid *cred,
mach_port_t reply,
mach_msg_type_name_t reply_type,
int mode)
{
if (!cred)
return EOPNOTSUPP;
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
return EINVAL;
}
kern_return_t
trivfs_S_io_map(struct trivfs_protid *cred,
mach_port_t reply, mach_msg_type_name_t reply_type,
mach_port_t *rdobj,
mach_msg_type_name_t *rdtype,
mach_port_t *wrobj,
mach_msg_type_name_t *wrtype)
{
if (!cred)
return EOPNOTSUPP;
return EINVAL;
}
int
random_demuxer (mach_msg_header_t *inp,
mach_msg_header_t *outp)
{
extern int startup_notify_server (mach_msg_header_t *, mach_msg_header_t *);
return (trivfs_demuxer (inp, outp)
|| startup_notify_server (inp, outp));
}
static const struct argp_option options[] =
{
{"fast", 'f', 0, 0, "(ignored)"},
{"secure", 's', 0, 0, "(ignored)"},
{"seed-file", 'S', "FILE", 0, "Use FILE to remember the seed"},
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
case 'f':
case 's':
break;
case 'S':
seed_file = strdup (arg);
break;
}
return 0;
}
error_t
trivfs_append_args (struct trivfs_control *fsys,
char **argz, size_t *argz_len)
{
error_t err = 0;
char *opt;
if (seed_file)
{
if (asprintf (&opt, "--seed-file=%s", seed_file) < 0)
err = ENOMEM;
else
{
err = argz_add (argz, argz_len, opt);
free (opt);
}
}
return err;
}
static struct argp random_argp =
{ options, parse_opt, 0,
"A translator providing random output." };
struct argp *trivfs_runtime_argp = &random_argp;
struct port_class *shutdown_notify_class;
error_t
S_startup_dosync (mach_port_t handle)
{
error_t err;
struct port_info *inpi = ports_lookup_port (fsys->pi.bucket, handle,
shutdown_notify_class);
if (!inpi)
return EOPNOTSUPP;
err = update_random_seed_file ();
if (err)
error (0, err, "Warning: Failed to save random seed to %s", seed_file);
return 0;
}
void
sigterm_handler (int signo)
{
error_t err;
err = update_random_seed_file ();
if (err)
error (0, err, "Warning: Failed to save random seed to %s", seed_file);
signal (SIGTERM, SIG_DFL);
raise (SIGTERM);
}
static error_t
arrange_shutdown_notification (void)
{
error_t err;
mach_port_t initport, notify;
struct port_info *pi;
shutdown_notify_class = ports_create_class (0, 0);
if (signal (SIGTERM, sigterm_handler) == SIG_ERR)
return errno;
err = ports_create_port (shutdown_notify_class, fsys->pi.bucket,
sizeof (struct port_info), &pi);
if (err)
return err;
initport = file_name_lookup (_SERVERS_STARTUP, 0, 0);
if (! MACH_PORT_VALID (initport))
return errno;
notify = ports_get_send_right (pi);
ports_port_deref (pi);
err = startup_request_notification (initport, notify,
MACH_MSG_TYPE_MAKE_SEND,
program_invocation_short_name);
mach_port_deallocate (mach_task_self (), notify);
mach_port_deallocate (mach_task_self (), initport);
return err;
}
int
main (int argc, char **argv)
{
error_t err;
unsigned int seed;
mach_port_t bootstrap;
argp_parse (&random_argp, argc, argv, 0, 0, 0);
pool_initialize ();
err = read_random_seed_file ();
if (err)
error (0, err, "Warning: Failed to read random seed file %s", seed_file);
pool_randomize (&seed, sizeof seed);
srandom (seed);
task_get_bootstrap_port (mach_task_self (), &bootstrap);
if (bootstrap == MACH_PORT_NULL)
error (1, 0, "Must be started as a translator");
err = trivfs_startup (bootstrap, 0, 0, 0, 0, 0, &fsys);
mach_port_deallocate (mach_task_self (), bootstrap);
if (err)
error (3, err, "trivfs_startup");
err = arrange_shutdown_notification ();
if (err)
error (0, err, "Warning: Cannot request shutdown notification");
err = start_gather_thread ();
if (err)
error (1, err, "Starting gather thread failed");
ports_manage_port_operations_multithread (fsys->pi.bucket, random_demuxer,
10 * 1000,
10 * 60 * 1000,
0);
return 0;
}