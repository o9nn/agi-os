#include "priv.h"
#include <hurd/sigpreempt.h>
#include <unistd.h>
#include <envz.h>
#include <sys/param.h>
void
check_hashbang (struct execdata *e,
file_t file,
task_t oldtask,
int flags,
const char *file_name_exec,
char *argv, mach_msg_type_number_t argvlen, boolean_t argv_copy,
char *envp, mach_msg_type_number_t envplen, boolean_t envp_copy,
mach_port_t *dtable, mach_msg_type_number_t dtablesize, boolean_t dtable_copy,
mach_port_t *portarray, mach_msg_type_number_t nports, boolean_t portarray_copy,
int *intarray, mach_msg_type_number_t nints, boolean_t intarray_copy,
const mach_port_t *deallocnames, mach_msg_type_number_t ndeallocnames,
const mach_port_t *destroynames, mach_msg_type_number_t ndestroynames)
{
char *p;
char *interp, *arg;
size_t interp_len, arg_len;
file_t interp_file;
char *new_argv;
size_t new_argvlen;
mach_port_t *new_dtable = NULL;
mach_msg_type_number_t new_dtablesize;
sigset_t arg_env_sigset;
sigemptyset (&arg_env_sigset);
sigaddset (&arg_env_sigset, SIGSEGV);
sigaddset (&arg_env_sigset, SIGBUS);
file_t user_fd (int fd)
{
if (fd >= 0 && fd < dtablesize)
{
const file_t dport = dtable[fd];
if (dport != MACH_PORT_NULL)
{
mach_port_mod_refs (mach_task_self (), dport,
MACH_PORT_RIGHT_SEND, +1);
return dport;
}
}
errno = EBADF;
return MACH_PORT_NULL;
}
file_t user_crdir, user_cwdir;
error_t user_port (int which, error_t (*operate) (mach_port_t))
{
error_t reauthenticate (file_t unauth, file_t *result)
{
error_t err;
mach_port_t ref;
error_t uauth (auth_t auth)
{
return auth_user_authenticate (auth,
ref, MACH_MSG_TYPE_MAKE_SEND,
result);
}
if (*result != MACH_PORT_NULL)
return 0;
ref = mach_reply_port ();
err = io_reauthenticate (unauth, ref, MACH_MSG_TYPE_MAKE_SEND);
if (!err)
err = user_port (INIT_PORT_AUTH, &uauth);
mach_port_destroy (mach_task_self (), ref);
return err;
}
mach_port_t port = ((which < nports &&
portarray[which] != MACH_PORT_NULL)
? portarray[which] :
(flags & EXEC_DEFAULTS && which < std_nports)
? std_ports[which]
: MACH_PORT_NULL);
switch (which)
{
case INIT_PORT_CRDIR:
if ((which < std_nports && flags & EXEC_SECURE) ||
(which < std_nports && port == std_ports[which]))
return (reauthenticate (std_ports[which], &user_crdir) ?:
(*operate) (user_crdir));
break;
case INIT_PORT_CWDIR:
if ((flags & EXEC_SECURE) ||
(which < std_nports && port == std_ports[which]))
return (reauthenticate (port, &user_cwdir) ?:
(*operate) (user_cwdir));
break;
}
return (*operate) (port);
}
inline error_t lookup (const char *name, int flags, mach_port_t *result)
{
return hurd_file_name_lookup (&user_port, &user_fd, 0,
name, flags, 0, result);
}
const char *page;
char interp_buf[vm_page_size - 2 + 1];
e->error = 0;
page = map (e, 0, 2);
if (!page)
{
if (!e->error)
e->error = ENOEXEC;
return;
}
if (page[0] != '#' || page[1] != '!')
{
e->error = ENOEXEC;
return;
}
p = memccpy (interp_buf, page + 2, '\n',
MIN (map_fsize (e) - 2, sizeof interp_buf));
if (p == NULL)
{
interp_len = sizeof interp_buf;
interp_buf[interp_len - 1] = '\0';
}
else
{
interp_len = p - interp_buf;
*--p = '\0';
}
finish (e, 0);
interp = interp_buf + strspn (interp_buf, " \t");
p = strpbrk (interp, " \t");
if (p)
{
*p++ = '\0';
arg = p + strspn (p, " \t");
arg_len = interp_len - 1 - (arg - interp_buf);
interp_len = p - interp;
if (arg_len == 0)
arg = NULL;
else
{
size_t i = arg_len - 1;
while (arg[i] == ' ' || arg[i] == '\t')
arg[i--] = '\0';
arg_len = i + 2;
}
}
else
{
arg = NULL;
arg_len = 0;
interp_len -= interp - interp_buf;
}
user_crdir = user_cwdir = MACH_PORT_NULL;
pthread_rwlock_rdlock (&std_lock);
e->error = lookup (interp, O_EXEC, &interp_file);
if (! e->error)
{
char * volatile file_name_to_free = NULL;
jmp_buf args_faulted;
void fault_handler (int signo)
{ longjmp (args_faulted, 1); }
error_t setup_args (struct hurd_signal_preemptor *preemptor)
{
size_t namelen;
const char * volatile file_name = NULL;
if (setjmp (args_faulted))
file_name = NULL;
else if (! (flags & EXEC_SECURE))
{
if (file_name_exec && file_name_exec[0] != '\0')
file_name = file_name_exec;
else
{
error_t error;
file_t name_file;
mach_port_t fileid, filefsid;
ino_t fileno;
char *name;
error_t search_path (struct hurd_signal_preemptor *preemptor)
{
error_t err;
char *path = envz_get (envp, envplen, "PATH"), *pfxed_name;
if (! path)
{
const size_t len = confstr (_CS_PATH, NULL, 0);
path = alloca (len);
confstr (_CS_PATH, path, len);
}
err = hurd_file_name_path_lookup (user_port, user_fd, 0,
name, path, O_EXEC, 0,
&name_file, &pfxed_name);
if (!err && pfxed_name)
{
name = pfxed_name;
file_name_to_free = pfxed_name;
}
return err;
}
error = io_identity (file, &fileid, &filefsid, &fileno);
if (error)
goto out;
mach_port_deallocate (mach_task_self (), filefsid);
if (memchr (argv, '\0', argvlen) == NULL)
{
name = alloca (argvlen + 1);
memcpy (name, argv, argvlen);
name[argvlen] = '\0';
}
else
name = argv;
if (strchr (name, '/') != NULL)
error = lookup (name, 0, &name_file);
else if ((error = hurd_catch_signal
(arg_env_sigset,
(vm_address_t) envp, (vm_address_t) envp + envplen,
&search_path, SIG_ERR)))
name_file = MACH_PORT_NULL;
if (!error && name_file != MACH_PORT_NULL)
{
mach_port_t id, fsid;
ino_t ino;
error = io_identity (name_file, &id, &fsid, &ino);
mach_port_deallocate (mach_task_self (), name_file);
if (!error)
{
mach_port_deallocate (mach_task_self (), fsid);
mach_port_deallocate (mach_task_self (), id);
if (id != fileid)
error = 1;
}
}
mach_port_deallocate (mach_task_self (), fileid);
if (!error)
file_name = name;
}
}
if (file_name == NULL)
{
int fd;
out:
for (fd = 0; fd < dtablesize; ++fd)
if (dtable[fd] == MACH_PORT_NULL)
break;
if (fd == dtablesize)
{
new_dtable = alloca ((dtablesize + 1) * sizeof (file_t));
memcpy (new_dtable, dtable, dtablesize * sizeof (file_t));
new_dtablesize = dtablesize + 1;
new_dtable[fd] = file;
}
else
dtable[fd] = file;
mach_port_mod_refs (mach_task_self (), file,
MACH_PORT_RIGHT_SEND, +1);
char *fd_file_name = alloca (100);
sprintf (fd_file_name, "/dev/fd/%d", fd);
file_name = fd_file_name;
}
namelen = strlen (file_name) + 1;
new_argvlen
= (argvlen - strlen (argv) - 1)
+ interp_len + arg_len + namelen;
new_argv = mmap (0, new_argvlen, PROT_READ|PROT_WRITE,
MAP_ANON, 0, 0);
if (new_argv == (caddr_t) -1)
{
e->error = errno;
goto end_setup_args;
}
else
e->error = 0;
if (! setjmp (args_faulted))
{
char *other_args;
p = new_argv;
memcpy (p, interp, interp_len);
p += interp_len;
if (arg)
{
memcpy (p, arg, arg_len);
p += arg_len;
}
memcpy (p, file_name, namelen);
p += namelen;
other_args = argv + strlen (argv) + 1;
if (other_args - argv < argvlen)
memcpy (p, other_args, argvlen - (other_args - argv));
}
else
{
char *n = stpncpy (new_argv,
"**fault in exec server reading argv[0]**",
argvlen);
memcpy (memcpy (n, arg, arg_len) + arg_len, file_name, namelen);
}
end_setup_args:
if (file_name_to_free)
free (file_name_to_free);
return e->error;
}
hurd_catch_signal (arg_env_sigset,
(vm_address_t) argv, (vm_address_t) argv + argvlen,
&setup_args, &fault_handler);
}
pthread_rwlock_unlock (&std_lock);
if (user_crdir != MACH_PORT_NULL)
mach_port_deallocate (mach_task_self (), user_crdir);
if (user_cwdir != MACH_PORT_NULL)
mach_port_deallocate (mach_task_self (), user_cwdir);
if (e->error)
return;
#ifdef HAVE_FILE_EXEC_PATHS
e->error = file_exec_paths (interp_file,
oldtask, flags, interp, interp,
new_argv, new_argvlen, envp, envplen,
new_dtable ?: dtable,
MACH_MSG_TYPE_COPY_SEND,
new_dtable ? new_dtablesize : dtablesize,
portarray, MACH_MSG_TYPE_COPY_SEND, nports,
intarray, nints,
deallocnames, ndeallocnames,
destroynames, ndestroynames);
if (e->error == MIG_BAD_ID)
#endif
e->error = file_exec (interp_file,
oldtask, flags,
new_argv, new_argvlen, envp, envplen,
new_dtable ?: dtable, MACH_MSG_TYPE_COPY_SEND,
new_dtable ? new_dtablesize : dtablesize,
portarray, MACH_MSG_TYPE_COPY_SEND, nports,
intarray, nints,
deallocnames, ndeallocnames,
destroynames, ndestroynames);
mach_port_deallocate (mach_task_self (), interp_file);
munmap (new_argv, new_argvlen);
if (! e->error)
{
unsigned i;
mach_port_deallocate (mach_task_self (), file);
task_resume (oldtask);
mach_port_deallocate (mach_task_self (), oldtask);
if (! argv_copy)
munmap (argv, argvlen);
if (! envp_copy)
munmap (envp, envplen);
for (i = 0; i < dtablesize; ++i)
if (MACH_PORT_VALID (dtable[i]))
mach_port_deallocate (mach_task_self (), dtable[i]);
if (! dtable_copy)
munmap (dtable, dtablesize * sizeof *dtable);
for (i = 0; i < nports; ++i)
mach_port_deallocate (mach_task_self (), portarray[i]);
if (! portarray_copy)
munmap (portarray, nports * sizeof *portarray);
if (! intarray_copy)
munmap (intarray, nints * sizeof *intarray);
}
}