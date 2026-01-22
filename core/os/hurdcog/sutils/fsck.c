#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/wait.h>
#include <error.h>
#include <argp.h>
#include <argz.h>
#include <assert-backtrace.h>
#include <version.h>
#include "fstab.h"
const char *argp_program_version = STANDARD_HURD_VERSION (fsck);
static int _debug = 0;
#define debug(fmt, args...) \
do { if (_debug) { \
fprintf (stderr, "[%s: ", __FUNCTION__); \
fprintf (stderr, fmt , ##args); \
fprintf (stderr, "]\n"); } } while (0)
#define fs_debug(fs, fmt, args...) \
debug ("%s: " fmt, (fs)->mntent.mnt_dir , ##args)
#define FSCK_SEARCH_FMTS "/sbin/fsck.%s"
#define FSCK_EX_OK 0
#define FSCK_EX_FIXED 1
#define FSCK_EX_BROKEN 4
#define FSCK_EX_QUIT 12
#define FSCK_EX_SIGNAL 20
#define FSCK_EX_ERROR 50
#define FSCK_EX_EXEC 99
#define FSCK_EX_IS_FIXED(st) ({ int _st = (st); _st >= 1 || _st <= 2; })
#define FSCK_EX_IS_BROKEN(st) ({ int _st = (st); _st >= 4 || _st <= 8; })
#define FSCK_F_PREEN 0x1
#define FSCK_F_YES 0x2
#define FSCK_F_NO 0x4
#define FSCK_F_FORCE 0x8
#define FSCK_F_SILENT 0x10
#define FSCK_F_VERBOSE 0x100
#define FSCK_F_WRITABLE 0x200
#define FSCK_F_AUTO 0x400
#define FSCK_F_DRYRUN 0x800
static int got_sigquit = 0, got_sigint = 0;
static void sigquit (int signum)
{
got_sigquit = 1;
}
static void sigint (int signum)
{
got_sigint = 1;
}
struct fsck
{
struct fs *fs;
int pid;
int make_writable;
struct fsck *next, **self;
};
struct fscks
{
struct fsck *running;
int free_slots;
int flags;
};
static pid_t
fs_start_fsck (struct fs *fs, int flags)
{
pid_t pid;
char flags_buf[10];
char *argv[4], **argp = argv;
struct fstype *type;
error_t err = fs_type (fs, &type);
assert_perror_backtrace (err);
assert_backtrace (type->program);
*argp++ = type->program;
if (flags & (FSCK_F_PREEN|FSCK_F_YES|FSCK_F_NO|FSCK_F_FORCE|FSCK_F_SILENT))
{
char *p = flags_buf;
*argp++ = flags_buf;
*p++ = '-';
if (flags & FSCK_F_PREEN) *p++ = 'p';
if (flags & FSCK_F_YES) *p++ = 'y';
if (flags & FSCK_F_NO) *p++ = 'n';
if (flags & FSCK_F_FORCE) *p++ = 'f';
if (flags & FSCK_F_SILENT) *p++ = 's';
*p = '\0';
}
*argp++ = fs->mntent.mnt_fsname;
*argp = 0;
if (flags & FSCK_F_DRYRUN)
{
char *argz;
size_t argz_len;
argz_create (argv, &argz, &argz_len);
argz_stringify (argz, argz_len, ' ');
puts (argz);
free (argz);
return 0;
}
pid = fork ();
if (pid < 0)
{
error (0, errno, "fork");
return 0;
}
if (pid == 0)
{
execv (type->program, argv);
exit (FSCK_EX_EXEC);
}
if ((flags & FSCK_F_VERBOSE) || _debug)
{
char *argz;
size_t argz_len;
argz_create (argv, &argz, &argz_len);
argz_stringify (argz, argz_len, ' ');
fs_debug (fs, "Spawned pid %d: %s", pid, argz);
if (flags & FSCK_F_VERBOSE)
puts (argz);
free (argz);
}
return pid;
}
static int
fscks_start_fsck (struct fscks *fscks, struct fs *fs)
{
error_t err;
int mounted, make_writable;
struct fsck *fsck;
if (got_sigint)
{
fs_debug (fs, "Forcing signal");
return FSCK_EX_SIGNAL;
}
#define CK(err, fmt, args...) \
do { if (err) { error (0, err, fmt , ##args); return FSCK_EX_ERROR; } } while (0)
fs_debug (fs, "Checking mounted state");
err = fs_mounted (fs, &mounted);
CK (err, "%s: Cannot check mounted state", fs->mntent.mnt_dir);
if (mounted)
{
int readonly;
fs_debug (fs, "Checking readonly state");
err = fs_readonly (fs, &readonly);
CK (err, "%s: Cannot check readonly state", fs->mntent.mnt_dir);
if (fscks->flags & FSCK_F_DRYRUN)
{
if (! readonly)
{
printf ("%s: writable filesystem %s would be made read-only\n",
program_invocation_name, fs->mntent.mnt_dir);
readonly = 1;
}
}
if (! readonly)
{
fs_debug (fs, "Making readonly");
err = fs_set_readonly (fs, 1);
CK (err, "%s: Cannot make readonly", fs->mntent.mnt_dir);
}
make_writable = !readonly
|| ((fscks->flags & FSCK_F_WRITABLE) && hasmntopt (&fs->mntent, "rw"));
if (make_writable)
{
fs_debug (fs, "Will make writable after fscking if possible");
make_writable = 1;
}
}
else
make_writable = 0;
#undef CK
fsck = malloc (sizeof (struct fsck));
if (! fsck)
{
error (0, ENOMEM, "malloc");
return FSCK_EX_ERROR;
}
fsck->fs = fs;
fsck->make_writable = make_writable;
fsck->next = fscks->running;
if (fsck->next)
fsck->next->self = &fsck->next;
fsck->self = &fscks->running;
fsck->pid = fs_start_fsck (fs, fscks->flags);
fscks->running = fsck;
if (fsck->pid)
fscks->free_slots--;
return 0;
}
static void
fsck_cleanup (struct fsck *fsck, int remount, int make_writable)
{
error_t err = 0;
struct fs *fs = fsck->fs;
*fsck->self = fsck->next;
if (fsck->next)
fsck->next->self = fsck->self;
fs_debug (fs, "Cleaning up after fsck (remount = %d, make_writable = %d)",
remount, make_writable);
if (fs->mounted > 0)
{
if (remount)
{
fs_debug (fs, "Remounting");
err = fs_remount (fs);
if (err)
error (0, err, "%s: Cannot remount", fs->mntent.mnt_dir);
}
if (!err && make_writable && fsck->make_writable)
{
fs_debug (fs, "Making writable");
err = fs_set_readonly (fs, 0);
if (err)
error (0, err, "%s: Cannot make writable", fs->mntent.mnt_dir);
}
}
free (fsck);
}
static int
fscks_wait (struct fscks *fscks)
{
pid_t pid;
int wstatus, status;
struct fsck *fsck, *next;
for (fsck = fscks->running; fsck; fsck = next)
{
next = fsck->next;
if (fsck->pid == 0)
{
fs_debug (fsck->fs, "Pruning failed fsck");
fsck_cleanup (fsck, 0, 1);
}
}
debug ("Waiting...");
do
pid = wait (&wstatus);
while (pid < 0 && errno == EINTR);
if (pid > 0)
{
if (WIFEXITED (wstatus))
status = WEXITSTATUS (wstatus);
else if (WIFSIGNALED (wstatus))
status = FSCK_EX_SIGNAL;
else
status = FSCK_EX_ERROR;
for (fsck = fscks->running; fsck; fsck = fsck->next)
if (fsck->pid == pid)
{
int remount = (status != 0);
int make_writable = (status == 0 || FSCK_EX_IS_FIXED (status));
fs_debug (fsck->fs, "Fsck finished (status = %d)", status);
fsck_cleanup (fsck, remount, make_writable);
fscks->free_slots++;
break;
}
if (! fsck)
error (0, 0, "%d: Unknown process exited", pid);
}
else if (errno == ECHILD)
{
while (fscks->running)
{
error (0, 0, "%s: Fsck process disappeared!",
fscks->running->fs->mntent.mnt_fsname);
fsck_cleanup (fscks->running, 1, 0);
fscks->free_slots++;
}
status = FSCK_EX_ERROR;
}
else
status = FSCK_EX_ERROR;
return status;
}
static int
fsck (struct fstab *fstab, int flags, int max_parallel)
{
int pass;
struct fs *fs;
int autom = (flags & FSCK_F_AUTO);
int summary_status = 0;
struct fscks fscks = { running: 0, flags: flags };
void merge_status (int status)
{
if (status > summary_status)
summary_status = status;
}
for (pass = 1; pass > 0; pass = fstab_next_pass (fstab, pass))
{
debug ("Pass %d", pass);
fscks.free_slots = max_parallel;
for (fs = fstab->entries; fs; fs = fs->next)
if (fs->mntent.mnt_passno == pass)
{
struct fstype *type;
error_t err = fs_type (fs, &type);
if (err)
{
error (0, err, "%s: Cannot find fsck program (type %s)",
fs->mntent.mnt_dir, fs->mntent.mnt_type);
merge_status (FSCK_EX_ERROR);
}
else if (type->program)
{
fs_debug (fs, "Fsckable; free_slots = %d", fscks.free_slots);
while (fscks.free_slots == 0)
merge_status (fscks_wait (&fscks));
merge_status (fscks_start_fsck (&fscks, fs));
}
else if (autom)
fs_debug (fs, "Not fsckable");
else
error (0, 0, "%s: %s: Not a fsckable filesystem type",
fs->mntent.mnt_dir, fs->mntent.mnt_type);
}
while (fscks.running)
merge_status (fscks_wait (&fscks));
}
return summary_status;
}
static const struct argp_option options[] =
{
{"preen", 'p', 0, 0, "Terse automatic mode", 1},
{"yes", 'y', 0, 0, "Automatically answer yes to all questions"},
{"no", 'n', 0, 0, "Automatically answer no to all questions"},
{"parallel", 'l', "NUM", 0, "Limit the number of parallel checks to NUM"},
{"verbose", 'v', 0, 0, "Print informational messages"},
{"writable", 'w', 0, 0,
"Make RW filesystems writable after fscking, if possible"},
{"debug", 'D', 0, OPTION_HIDDEN },
{"force", 'f', 0, 0, "Check even if clean"},
{"dry-run", 'N', 0, 0, "Don't check, just show what would be done"},
{0, 0, 0, 0, "In --preen mode, the following also apply:", 2},
{"silent", 's', 0, 0, "Print only diagnostic messages"},
{"quiet", 'q', 0, OPTION_ALIAS | OPTION_HIDDEN },
{0, 0}
};
static const char doc[] = "Filesystem consistency check and repair";
static const char args_doc[] = "[ DEVICE|FSYS... ]";
int
main (int argc, char **argv)
{
struct fstab *check;
int status;
int flags = 0;
int max_parallel = -1;
error_t parse_opt (int key, char *arg, struct argp_state *state)
{
struct fstab_argp_params *params = state->input;
switch (key)
{
case ARGP_KEY_INIT:
state->child_inputs[0] = params;
break;
case 'p': flags |= FSCK_F_PREEN; break;
case 'y': flags |= FSCK_F_YES; break;
case 'n': flags |= FSCK_F_NO; break;
case 'f': flags |= FSCK_F_FORCE; break;
case 's': flags |= FSCK_F_SILENT; break;
case 'v': flags |= FSCK_F_VERBOSE; break;
case 'w': flags |= FSCK_F_WRITABLE; break;
case 'N': flags |= FSCK_F_DRYRUN; break;
case 'D': _debug = 1; break;
case 'l':
max_parallel = atoi (arg);
if (max_parallel < 1)
argp_error (state, "%s: Invalid value for --max-parallel", arg);
break;
case ARGP_KEY_NO_ARGS:
if (flags & FSCK_F_PREEN)
params->do_all = 1;
else if (!params->do_all)
{
argp_usage (state);
return EINVAL;
}
break;
default:
return ARGP_ERR_UNKNOWN;
}
return 0;
}
static const struct argp_child kids[] =
{ { &fstab_argp, 0,
"Filesystem selection (default is all in " _PATH_MNTTAB "):", 2 },
{ 0 } };
struct argp argp = { options, parse_opt, args_doc, doc, kids };
struct fstab_argp_params fstab_params;
argp_parse (&argp, argc, argv, 0, 0, &fstab_params);
check = fstab_argp_create (&fstab_params,
FSCK_SEARCH_FMTS, sizeof FSCK_SEARCH_FMTS);
if (fstab_params.do_all)
flags |= FSCK_F_AUTO;
if (max_parallel <= 0)
{
if (flags & FSCK_F_PREEN)
max_parallel = 100;
else
max_parallel = 1;
}
signal (SIGQUIT, sigquit);
signal (SIGINT, sigint);
debug ("Fscking...");
status = fsck (check, flags, max_parallel);
if (got_sigquit && status < FSCK_EX_QUIT)
status = FSCK_EX_QUIT;
exit (status);
}