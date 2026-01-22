#include <hurd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <paths.h>
#include <ctype.h>
#include <utmp.h>
#include <pwd.h>
#include <grp.h>
#include <netdb.h>
#include <time.h>
#include <assert-backtrace.h>
#include <version.h>
#include <sys/mman.h>
#include <signal.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <sys/fcntl.h>
#include <argp.h>
#include <argz.h>
#include <envz.h>
#include <idvec.h>
#include <error.h>
#include <timefmt.h>
#include <hurd/lookup.h>
#include <ugids.h>
const char *argp_program_version = STANDARD_HURD_VERSION (login);
extern error_t
exec_reauth (auth_t auth, int secure, int must_reauth,
mach_port_t *ports, unsigned num_ports,
mach_port_t *fds, unsigned num_fds);
extern error_t
get_nonsugid_ids (struct idvec *uids, struct idvec *gids);
char *default_args[] = {
"SHELL=/bin/bash",
"BACKUP_SHELLS=/bin/bash:" _PATH_BSHELL,
"HOME=/etc/login",
"USER=login",
"UMASK=022",
"NAME=Not logged in",
"HUSHLOGIN=.hushlogin",
"MOTD=/etc/motd",
"PATH=/bin",
"NOBODY=login",
"NOAUTH_TIMEOUT=300",
0
};
char *default_env[] = {
"PATH=/bin",
0
};
char *copied_args[] = {
"USER", "SHELL", "HOME", "NAME", "VIA", "VIA_ADDR", "PATH", 0
};
static struct argp_option options[] =
{
{"arg0",	'0', "ARG",   0, "Make ARG the shell's argv[0]"},
{"envvar",	'e', "ENTRY", 0, "Add ENTRY to the environment"},
{"envvar-default", 'E', "ENTRY", 0, "Use ENTRY as a default environment variable"},
{"no-args",	'x', 0,	      0, "Don't put login args into the environment"},
{"arg",	'a', "ARG",   0, "Add login parameter ARG"},
{"arg-default", 'A', "ARG", 0, "Use ARG as a default login parameter"},
{"no-environment-args", 'X', 0, 0, "Don't add the parent environment as default login params"},
{"no-login",  'L', 0,       0, "Don't modify the shells argv[0] to look"
" like a login shell"},
{"preserve-environment", 'p', 0, 0, "Inherit the parent's environment"},
{"via",	'h', "HOST",  0, "This login is from HOST"},
{"no-passwd", 'f', 0,       0, "Don't ask for passwords"},
{"paranoid",  'P', 0,       0, "Don't admit that a user doesn't exist"},
{"save",      's', 0,       0, "Keep the old available ids, and save the old"
" effective ids as available ids"},
{"shell-from-args", 'S', 0, 0, "Use the first shell arg as the shell to invoke"},
{"retry",     'R', "ARG",   OPTION_ARG_OPTIONAL,
"Re-exec login with no users after non-fatal errors; if ARG is supplied,"
"add it to the list of args passed to login when retrying"},
{0, 0}
};
static struct argp_child child_argps[] =
{
{ &ugids_argp, 0, "Adding individual user/group ids:" },
{ 0 }
};
static char *args_doc = "[USER [ARG...]]";
static char *doc =
"Exec a program with uids and/or the environment changed appropriately.\v"
"To give args to the shell without specifying a user, use - for USER.\n"
"Current login parameters include HOME, SHELL, USER, NAME, and ROOT.";
static void
cat (mach_port_t node, char *str)
{
error_t err;
if (node == MACH_PORT_NULL)
err = errno;
else
for (;;)
{
char buf[1024], *data = buf;
mach_msg_type_number_t data_len = sizeof (buf);
err = io_read (node, &data, &data_len, -1, 16384);
if (err || data_len == 0)
break;
else
{
write (0, data, data_len);
if (data != buf)
munmap (data, data_len);
}
}
if (err)
error (0, errno, "%s", str);
}
static void
add_utmp_entry (char *args, unsigned args_len, int inherit_host)
{
struct utmp utmp;
struct timeval current_time;
char const *host = 0;
long addr = 0;
memset (&utmp, 0, sizeof(utmp));
gettimeofday (&current_time, NULL);
utmp.ut_tv.tv_sec = current_time.tv_sec;
utmp.ut_tv.tv_usec = current_time.tv_usec;
strncpy (utmp.ut_name, envz_get (args, args_len, "USER") ?: "",
sizeof (utmp.ut_name));
if (! inherit_host)
{
char *via_addr = envz_get (args, args_len, "VIA_ADDR");
host = envz_get (args, args_len, "VIA");
if (host && strlen (host) > sizeof (utmp.ut_host))
host = via_addr ?: host;
if (via_addr)
addr = inet_addr (via_addr);
}
if (!host || !addr)
{
int tty_fd = 0;
char *tty = 0;
while (!tty && tty_fd < 3)
tty = ttyname (tty_fd++);
if (tty)
{
struct utmp *old_utmp;
strncpy (utmp.ut_line, basename (tty), sizeof (utmp.ut_line));
setutent ();
old_utmp = getutline (&utmp);
endutent ();
if (old_utmp)
{
if (! host)
host = old_utmp->ut_host;
if (! addr)
addr = old_utmp->ut_addr;
}
}
}
strncpy (utmp.ut_host, host ?: "", sizeof (utmp.ut_host));
utmp.ut_addr = addr;
login (&utmp);
}
static error_t
add_canonical_host (char **args, size_t *args_len, char *host)
{
struct hostent *he = gethostbyname (host);
if (he)
{
char *addr = 0;
switch (he->h_addrtype)
{
case AF_INET:
addr = strdup (inet_ntoa (*(struct in_addr *)he->h_addr));
break;
}
if (addr && strcmp (he->h_name, addr) == 0)
he = gethostbyaddr (he->h_addr, he->h_length, he->h_addrtype);
if (he)
host = he->h_name;
if (addr)
{
envz_add (args, args_len, "VIA_ADDR", addr);
free (addr);
}
}
return envz_add (args, args_len, "VIA", host);
}
static void
add_entry (char **env, size_t *env_len, char *entry)
{
char *name = strsep (&entry, "=");
error_t err = envz_add (env, env_len, name, entry);
if (err)
error (8, err, "Adding %s", entry);
}
static error_t
check_owned (process_t proc_server, pid_t pid, int *owned)
{
int flags = PI_FETCH_TASKINFO;
char *waits = 0;
mach_msg_type_number_t num_waits = 0;
struct procinfo _pi, *pi = &_pi;
mach_msg_type_number_t pi_size = sizeof _pi / sizeof (*(procinfo_t)0);
error_t err =
proc_getprocinfo (proc_server, pid, &flags, (procinfo_t *)&pi, &pi_size,
&waits, &num_waits);
if (! err)
{
*owned = !(pi->state & PI_NOTOWNED);
if (pi != &_pi)
munmap (pi, pi_size * sizeof (*(procinfo_t)0));
}
return err;
}
static void
kill_login (process_t proc_server, pid_t pid, int sig)
{
error_t err;
mach_msg_type_number_t num_pids;
pid_t self = getpid ();
do
{
pid_t _pids[num_pids = 20], *pids = _pids;
err = proc_getloginpids (proc_server, pid, &pids, &num_pids);
if (! err)
{
size_t i;
for (i = 0; i < num_pids; i++)
if (pids[i] != self)
kill (pids[i], sig);
if (pids != _pids)
munmap (pids, num_pids);
}
}
while (!err && num_pids > 0);
}
static void
check_login (process_t proc_server, int lid)
{
int owned;
error_t err = check_owned (proc_server, lid, &owned);
if (err == ESRCH)
exit (42);
else
assert_perror_backtrace (err);
if (owned)
exit (0);
}
static void
dog (time_t timeout, pid_t pid, char **argv)
{
if (fork () == 0)
{
char buf[25];
char *name = basename (argv[0]);
time_t left = timeout;
struct timeval tv = { 0, 0 };
process_t proc_server = getproc ();
while (left)
{
time_t interval = left < 5 ? left : 5;
tv.tv_sec = left;
fmt_named_interval (&tv, 0, buf, sizeof buf);
asprintf (&argv[0], "(watchdog for %s %d: %s remaining)",
name, pid, buf);
argv[1] = 0;
sleep (interval);
left -= interval;
check_login (proc_server, pid);
}
check_login (proc_server, pid);
tv.tv_sec = timeout;
fmt_named_interval (&tv, 0, buf, sizeof buf);
putc ('\n', stderr);
error (0, 0, "Timed out after %s.", buf);
kill_login (proc_server, pid, SIGHUP);
sleep (5);
kill_login (proc_server, pid, SIGKILL);
exit (0);
}
}
int
main(int argc, char *argv[])
{
int i;
io_t node;
char *arg;
char *path;
error_t err = 0;
char *args = 0;
size_t args_len = 0;
char *args_defs = 0;
size_t args_defs_len = 0;
char *env = 0;
size_t env_len = 0;
char *env_defs = 0;
size_t env_defs_len = 0;
char *parent_env = 0;
size_t parent_env_len = 0;
int no_environ = 0;
int no_args = 0;
int inherit_environ = 0;
int no_passwd = 0;
int no_login = 0;
int paranoid = 0;
int retry = 0;
char *retry_args = 0;
size_t retry_args_len = 0;
char *shell = 0;
char *sh_arg0 = 0;
char *sh_args = 0;
size_t sh_args_len = 0;
int shell_arg = 0;
struct ugids ugids = UGIDS_INIT;
struct ugids_argp_params ugids_argp_params = { &ugids, 0, 0, 0, -1, 0 };
struct idvec parent_uids = IDVEC_INIT;
struct idvec parent_gids = IDVEC_INIT;
mach_port_t exec;
mach_port_t root;
mach_port_t ports[INIT_PORT_MAX];
int ints[INIT_INT_MAX];
mach_port_t fds[3];
mach_port_t auth;
mach_port_t proc_server = getproc ();
pid_t pid = getpid (), sid;
mach_port_t *please_dealloc, *pdp;
struct hurd_userlink ulink_ports[INIT_PORT_MAX];
mach_port_t port;
error_t use_child_init_port (int which, error_t (*operate)(mach_port_t))
{
return (*operate)(ports[which]);
}
mach_port_t get_child_fd_port (int fd)
{
return fd < 0 || fd > 2 ? __hurd_fail (EBADF) : fds[fd];
}
mach_port_t child_lookup (char *name, char *path, int flags)
{
mach_port_t port = MACH_PORT_NULL;
errno =
hurd_file_name_path_lookup (use_child_init_port, get_child_fd_port, 0,
name, path, flags, 0, &port, 0);
return port;
}
void fail (int code, error_t err, char *fmt, const char *str)
{
int retry_argc;
char **retry_argv;
char *via = envz_get (args, args_len, "VIA");
if (fmt)
error (retry ? 0 : code, err, fmt, str);
else if (! retry)
exit (code);
if (via)
envz_add (&retry_args, &retry_args_len, "--via", via);
argz_insert (&retry_args, &retry_args_len, retry_args, argv[0]);
retry_argc = argz_count (retry_args, retry_args_len);
retry_argv = alloca ((retry_argc + 1) * sizeof (char *));
argz_extract (retry_args, retry_args_len, retry_argv);
main (retry_argc, retry_argv);
exit (code);
}
error_t parse_opt (int key, char *arg, struct argp_state *state)
{
switch (key)
{
case 'p': inherit_environ = 1; break;
case 'x': no_args = 1; break;
case 'X': no_environ = 1; break;
case 'e': add_entry (&env, &env_len, arg); break;
case 'E': add_entry (&env_defs, &env_defs_len, arg); break;
case 'a': add_entry (&args, &args_len, arg); break;
case 'A': add_entry (&args_defs, &args_defs_len, arg); break;
case '0': sh_arg0 = arg; break;
case 'L': no_login = 1; break;
case 'f': no_passwd = 1; break;
case 'P': paranoid = 1; break;
case 'S': shell_arg = 1; break;
case 'R':
retry = 1;
if (arg)
{
err = argz_add (&retry_args, &retry_args_len, arg);
if (err)
error (10, err, "Adding retry arg %s", arg);
}
break;
case 'h':
add_canonical_host (&args, &args_len, arg);
retry = 1;
break;
case 's':
idvec_merge (&ugids.avail_uids, &parent_uids);
idvec_merge (&ugids.avail_gids, &parent_gids);
break;
case ARGP_KEY_ARG:
if (state->arg_num > 0)
{
err = argz_create (state->argv + state->next - 1,
&sh_args, &sh_args_len);
state->next = state->argc;
if (err)
error (9, err, "Adding %s", arg);
break;
}
if (strcmp (arg, "-") == 0)
break;
if (isdigit (*arg))
err = ugids_set_posix_user (&ugids, atoi (arg));
else
{
struct passwd *pw = getpwnam (arg);
if (pw)
err = ugids_set_posix_user (&ugids, pw->pw_uid);
else if (paranoid)
idvec_add (&ugids.eff_uids, -1);
else
fail (10, 0, "%s: Unknown user", arg);
}
if (err)
fail (11, err, "%s: Can't set user!", arg);
break;
case ARGP_KEY_INIT:
state->child_inputs[0] = &ugids_argp_params;
break;
default:
return ARGP_ERR_UNKNOWN;
}
return 0;
}
struct argp argp = { options, parse_opt, args_doc, doc, child_argps };
node = file_name_lookup (_PATH_NOLOGIN, O_RDONLY, 0);
if (node != MACH_PORT_NULL)
{
cat (node, _PATH_NOLOGIN);
exit (40);
}
err = argz_create (default_args, &args_defs, &args_defs_len);
if (! err)
err = argz_create (default_env, &env_defs, &env_defs_len);
if (! err)
{
size_t path_len = confstr (_CS_PATH, 0, 0);
if (path_len > 0)
{
char path[path_len];
path_len = confstr (_CS_PATH, path, path_len);
if (path_len > 0)
err = envz_add (&env_defs, &env_defs_len, "PATH", path);
}
}
if (err)
error (23, err, "adding defaults");
err = argz_create (environ, &parent_env, &parent_env_len);
get_nonsugid_ids (&parent_uids, &parent_gids);
argp_parse (&argp, argc, argv, ARGP_IN_ORDER, 0, 0);
err = ugids_verify_make_auth (&ugids,
no_passwd ? &parent_uids : 0,
no_passwd ? &parent_gids : 0,
0, 0, 0, 0, &auth);
if (err == EACCES)
fail (5, 0, "Invalid password", 0);
else if (err)
error (5, err, "Authentication failure");
{
struct passwd *pw;
char *passwd = 0;
size_t passwd_len = 0;
if (ugids.eff_uids.num > 0)
pw = getpwuid (ugids.eff_uids.ids[0]);
else if (ugids.avail_uids.num > 0)
pw = getpwuid (ugids.avail_uids.ids[0]);
else
pw = getpwnam (envz_get (args, args_len, "NOBODY")
?: envz_get (args_defs, args_defs_len, "NOBODY")
?: "login");
if (pw)
{
envz_add (&passwd, &passwd_len, "HOME", pw->pw_dir);
envz_add (&passwd, &passwd_len, "SHELL", pw->pw_shell);
envz_add (&passwd, &passwd_len, "NAME", pw->pw_gecos);
envz_add (&passwd, &passwd_len, "USER", pw->pw_name);
}
err = envz_merge (&args, &args_len, passwd, passwd_len, 0);
if (! err && ! no_environ)
err = envz_merge (&args, &args_len, parent_env, parent_env_len, 0);
if (! err)
err = envz_merge (&args, &args_len, args_defs, args_defs_len, 0);
if (err)
error (24, err, "merging parameters");
free (passwd);
}
err = proc_getsid (proc_server, pid, &sid);
assert_perror_backtrace (err);
if (!no_login
&& (parent_uids.num != 0
|| ugids.eff_uids.num + ugids.avail_uids.num > 0))
{
char *user = envz_get (args, args_len, "USER");
if (user && *user)
setlogin (user);
proc_make_login_coll (proc_server);
if (ugids.eff_uids.num + ugids.avail_uids.num == 0)
{
char *to = envz_get (args, args_len, "NOAUTH_TIMEOUT");
time_t timeout = to ? atoi (to) : 0;
if (timeout)
dog (timeout, pid, argv);
}
}
if (ugids.eff_uids.num > 0)
proc_setowner (proc_server, ugids.eff_uids.ids[0], 0);
else
proc_setowner (proc_server, 0, 1);
please_dealloc = alloca ((3 + INIT_PORT_MAX + 1) * sizeof(mach_port_t));
pdp = please_dealloc;
memset (ints, 0, sizeof (*ints) * INIT_INT_MAX);
arg = envz_get (args, args_len, "UMASK");
ints[INIT_UMASK] = arg && *arg ? strtoul (arg, 0, 8) : umask (0);
for (i = 0; i < 3; i++)
{
fds[i] = getdport (i);
*pdp++ = fds[i];
}
for (i = 0; i < INIT_PORT_MAX; i++)
{
ports[i] = MACH_PORT_NULL;
port = _hurd_port_get (&_hurd_ports[i], &ulink_ports[i]);
if (port != MACH_PORT_NULL)
*pdp++ = port;
}
ports[INIT_PORT_PROC] = getproc ();
ports[INIT_PORT_CTTYID] = getcttyid ();
ports[INIT_PORT_CRDIR] = getcrdir ();
ports[INIT_PORT_CWDIR] = getcwdir ();
err = exec_reauth (auth, 0, 1, ports, INIT_PORT_MAX, fds, 3);
if (err)
error (40, err, "Port reauth failure");
root = ports[INIT_PORT_CRDIR];
if (shell_arg && sh_args && *sh_args)
{
shell = strdup (sh_args);
argz_delete (&sh_args, &sh_args_len, sh_args);
}
else
{
arg = envz_get (args, args_len, "SHELL");
if (arg && *arg)
shell = strdup (arg);
else
shell = 0;
}
path = envz_get (args, args_len, "PATH");
exec = shell ? child_lookup (shell, path, O_EXEC) : MACH_PORT_NULL;
if (exec == MACH_PORT_NULL)
{
char *backup = 0;
char *backups = envz_get (args, args_len, "BACKUP_SHELLS");
err = errno;
if (backups && *backups)
{
backups = strdupa (backups);
while (exec == MACH_PORT_NULL && backups)
{
backup = strsep (&backups, ":, ");
if (*backup && (!shell || strcmp (shell, backup) != 0))
exec = child_lookup (backup, path, O_EXEC);
}
}
if (exec == MACH_PORT_NULL)
fail (1, err, "%s", shell);
else
error (0, err, "%s", shell);
shell = strdup (backup);
error (0, 0, "Using SHELL=%s", shell);
envz_add (&args, &args_len, "SHELL", shell);
err = 0;
}
arg = envz_get (args, args_len, "HOME");
if (arg && *arg)
{
mach_port_t cwd = child_lookup (arg, 0, O_RDONLY);
if (cwd == MACH_PORT_NULL)
{
error (0, errno, "%s", arg);
error (0, 0, "Using HOME=/");
envz_add (&args, &args_len, "HOME", "/");
}
else
{
mach_port_deallocate (mach_task_self (), ports[INIT_PORT_CWDIR]);
ports[INIT_PORT_CWDIR] = cwd;
}
}
arg = envz_get (args, args_len, "ROOT");
if (arg && *arg)
{
root = child_lookup (arg, 0, O_RDONLY);
if (root == MACH_PORT_NULL)
fail (40, errno, "%s", arg);
}
if (! no_args)
{
char **name;
char *user = envz_get (args, args_len, "USER");
for (name = copied_args; *name && !err; name++)
if (! envz_get (env, env_len, *name))
{
char *val = envz_get (args, args_len, *name);
if (val && *val)
err = envz_add (&env, &env_len, *name, val);
}
if (user)
err = envz_add (&env, &env_len, "LOGNAME", user);
}
if (! err && inherit_environ)
err = envz_merge (&env, &env_len, parent_env, parent_env_len, 0);
if (! err)
err = envz_merge (&env, &env_len, env_defs, env_defs_len, 0);
if (err)
error (24, err, "Can't build environment");
if (! sh_arg0)
{
char *shell_base = rindex (shell, '/');
if (shell_base)
shell_base++;
else
shell_base = shell;
if (no_login)
sh_arg0 = shell_base;
else if (ugids.eff_uids.num + ugids.avail_uids.num == 0)
err = (asprintf (&sh_arg0, "-login prompt (%s)", shell_base) == -1
? ENOMEM : 0);
else
err = asprintf (&sh_arg0, "-%s", shell_base) == -1 ? ENOMEM : 0;
}
if (! err)
err = argz_insert (&sh_args, &sh_args_len, sh_args, sh_arg0);
if (err)
error (21, err, "Error building shell args");
arg = envz_get (args, args_len, "MOTD");
if (arg && *arg)
{
char *hush = envz_get (args, args_len, "HUSHLOGIN");
mach_port_t hush_node =
(hush && *hush) ? child_lookup (hush, 0, O_RDONLY) : MACH_PORT_NULL;
if (hush_node == MACH_PORT_NULL)
{
mach_port_t motd_node = child_lookup (arg, 0, O_RDONLY);
if (motd_node != MACH_PORT_NULL)
{
cat (motd_node, arg);
mach_port_deallocate (mach_task_self (), motd_node);
}
}
else
mach_port_deallocate (mach_task_self (), hush_node);
}
if (ports[INIT_PORT_CRDIR] != root)
{
mach_port_deallocate (mach_task_self (), ports[INIT_PORT_CRDIR]);
ports[INIT_PORT_CRDIR] = root;
}
envz_strip (&env, &env_len);
if (pid == sid)
add_utmp_entry (args, args_len, !idvec_contains (&parent_uids, 0));
if ((ugids.eff_uids.num | ugids.eff_gids.num) && !no_login)
{
char *tty = ttyname (0);
if (tty)
{
err = chown (tty,
ugids.eff_uids.num ? ugids.eff_uids.ids[0] : -1,
ugids.eff_gids.num ? ugids.eff_gids.ids[0] : -1);
if (err)
error (0, errno, "chown: %s", tty);
}
}
#ifdef HAVE_FILE_EXEC_PATHS
err = file_exec_paths (exec, mach_task_self (), EXEC_DEFAULTS, shell, shell,
sh_args, sh_args_len, env, env_len,
fds, MACH_MSG_TYPE_COPY_SEND, 3,
ports, MACH_MSG_TYPE_COPY_SEND, INIT_PORT_MAX,
ints, INIT_INT_MAX,
please_dealloc, pdp-please_dealloc, 0, 0);
if (err == MIG_BAD_ID)
#endif
err = file_exec (exec, mach_task_self (), EXEC_DEFAULTS,
sh_args, sh_args_len, env, env_len,
fds, MACH_MSG_TYPE_COPY_SEND, 3,
ports, MACH_MSG_TYPE_COPY_SEND, INIT_PORT_MAX,
ints, INIT_INT_MAX,
please_dealloc, pdp-please_dealloc, 0, 0);
if (err)
error(5, err, "%s", shell);
return 0;
}