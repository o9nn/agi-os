#include <mach.h>
#include <hurd.h>
#include <sys/types.h>
#include <hurd/hurd_types.h>
#include <stdlib.h>
#include <errno.h>
#include <mach/notify.h>
#include <string.h>
#include <stdio.h>
#include <hurd/exec.h>
#include <unistd.h>
#include <assert-backtrace.h>
#include <version.h>
#include <sys/mman.h>
#include "proc.h"
#include "process_S.h"
static mach_port_t *std_port_array;
static int *std_int_array;
static int n_std_ports, n_std_ints;
static struct utsname uname_info;
struct server_version
{
char *name;
char *version;
} *server_versions;
int nserver_versions, server_versions_nalloc;
struct execdata_notify
{
mach_port_t notify_port;
struct execdata_notify *next;
} *execdata_notifys;
kern_return_t
S_proc_getprivports (struct proc *p,
mach_port_t *hostpriv,
mach_port_t *devpriv)
{
if (!p)
return EOPNOTSUPP;
if (! check_uid (p, 0))
return EPERM;
*hostpriv = _hurd_host_priv;
*devpriv = _hurd_device_master;
return 0;
}
kern_return_t
S_proc_setexecdata (struct proc *p,
const mach_port_t *ports,
mach_msg_type_number_t nports,
const int *ints,
mach_msg_type_number_t nints)
{
int i;
struct execdata_notify *n;
mach_port_t *std_port_array_new;
int *std_int_array_new;
if (!p)
return EOPNOTSUPP;
if (!check_uid (p, 0))
return EPERM;
std_port_array_new = malloc (sizeof (mach_port_t) * nports);
if (! std_port_array_new)
return ENOMEM;
std_int_array_new = malloc (sizeof (int) * nints);
if (! std_int_array_new)
{
free (std_port_array_new);
return ENOMEM;
}
if (std_port_array)
{
for (i = 0; i < n_std_ports; i++)
mach_port_deallocate (mach_task_self (), std_port_array[i]);
free (std_port_array);
}
free (std_int_array);
std_port_array = std_port_array_new;
n_std_ports = nports;
memcpy (std_port_array, ports, sizeof (mach_port_t) * nports);
std_int_array = std_int_array_new;
n_std_ints = nints;
memcpy (std_int_array, ints, sizeof (int) * nints);
for (n = execdata_notifys; n; n = n->next)
exec_setexecdata (n->notify_port, std_port_array, MACH_MSG_TYPE_COPY_SEND,
n_std_ports, std_int_array, n_std_ints);
return 0;
}
kern_return_t
S_proc_getexecdata (struct proc *p,
mach_port_t **ports,
mach_msg_type_name_t *portspoly,
mach_msg_type_number_t *nports,
int **ints,
mach_msg_type_number_t *nints)
{
int i;
int ports_allocated = 0;
if (!std_port_array)
return ENOENT;
if (*nports < n_std_ports)
{
*ports = mmap (0, round_page (n_std_ports * sizeof (mach_port_t)),
PROT_READ|PROT_WRITE, MAP_ANON, 0, 0);
if (*ports == MAP_FAILED)
return ENOMEM;
ports_allocated = 1;
}
memcpy (*ports, std_port_array, n_std_ports * sizeof (mach_port_t));
*nports = n_std_ports;
if (*nints < n_std_ints)
{
*ints = mmap (0, round_page (n_std_ints * sizeof (int)),
PROT_READ|PROT_WRITE, MAP_ANON, 0, 0);
if (*ints == MAP_FAILED)
{
if (ports_allocated)
munmap (*ports, round_page (n_std_ports * sizeof (mach_port_t)));
return ENOMEM;
}
}
memcpy (*ints, std_int_array, n_std_ints * sizeof (int));
*nints = n_std_ints;
for (i = 0; i < n_std_ports; i++)
mach_port_mod_refs (mach_task_self (), std_port_array[i], MACH_PORT_RIGHT_SEND, 1);
*portspoly = MACH_MSG_TYPE_MOVE_SEND;
return 0;
}
kern_return_t
S_proc_execdata_notify (struct proc *p,
mach_port_t notify)
{
struct execdata_notify *n;
if (!p)
return EOPNOTSUPP;
n = malloc (sizeof (struct execdata_notify));
if (! n)
return ENOMEM;
n->notify_port = notify;
n->next = execdata_notifys;
execdata_notifys = n;
ports_request_dead_name_notification (p, notify, NULL);
if (std_port_array)
exec_setexecdata (n->notify_port, std_port_array, MACH_MSG_TYPE_COPY_SEND,
n_std_ports, std_int_array, n_std_ints);
return 0;
}
void
check_dead_execdata_notify (mach_port_t port)
{
struct execdata_notify *en, **prevp;
for (en = execdata_notifys, prevp = &execdata_notifys; en; en = *prevp)
{
if (en->notify_port == port)
{
mach_port_deallocate (mach_task_self (), port);
*prevp = en->next;
free (en);
}
else
prevp = &en->next;
}
}
char *kernel_name, *kernel_version;
static void
rebuild_uname (void)
{
unsigned int i, j;
char *p, *end;
inline void initstr (char *string)
{
p = string;
end = p + _UTSNAME_LENGTH;
}
inline void addstr (const char *name, const char *version)
{
size_t len;
if (name)
{
len = strlen (name);
if (p + len + 1 < end)
memcpy (p, name, len);
p += len;
if (p < end)
*p++ = '-';
}
len = strlen (version);
if (p + len + 1 < end)
memcpy (p, version, len);
p += len;
if (p < end)
*p++ = '/';
}
struct version
{
const char *version;
unsigned int count;
} versions[nserver_versions];
int compare_versions (const void *a, const void *b)
{
return (((const struct version *) b)->count -
((const struct version *) a)->count);
}
unsigned int nversions = 0;
for (i = 0; i < nserver_versions; ++i)
{
for (j = 0; j < nversions; ++j)
if (! strcmp (versions[j].version, server_versions[i].version))
{
++versions[j].count;
break;
}
if (j == nversions)
{
versions[nversions].version = server_versions[i].version;
versions[nversions].count = 1;
++nversions;
}
}
qsort (versions, nversions, sizeof (struct version), compare_versions);
strcpy (uname_info.release, versions[0].version);
initstr (uname_info.version);
addstr (kernel_name, kernel_version);
if (versions[0].count > 1)
addstr ("Hurd", versions[0].version);
if (versions[0].count != nserver_versions)
for (i = 0; i < nserver_versions; i++)
if (versions[0].count == 1
|| strcmp (server_versions[i].version, versions[0].version))
addstr (server_versions[i].name, server_versions[i].version);
if (p > end)
#ifdef notyet
syslog (LOG_EMERG,
"_UTSNAME_LENGTH %u too short; inform bug-glibc@prep.ai.mit.edu\n",
p - end)
#endif
;
else
p[-1] = '\0';
end[-1] = '\0';
}
void
initialize_version_info (void)
{
extern const char *const mach_cpu_types[];
#ifdef __i386__
extern const char *const mach_cpu_subtypes[][32];
#endif
kernel_version_t kv;
char *p;
struct host_basic_info info;
mach_msg_type_number_t n = sizeof info;
error_t err;
strcpy (uname_info.sysname, "GNU");
err = host_info (mach_host_self (), HOST_BASIC_INFO,
(host_info_t) &info, &n);
assert_backtrace (! err);
snprintf (uname_info.machine, sizeof uname_info.machine,
"%s"
#ifdef __i386__
"-%s"
#endif
, mach_cpu_types[info.cpu_type]
#ifdef __i386__
, mach_cpu_subtypes[info.cpu_type][info.cpu_subtype]
#endif
);
server_versions = malloc (sizeof (struct server_version) * 10);
assert_backtrace (server_versions);
server_versions_nalloc = 10;
err = host_get_kernel_version (mach_host_self (), kv);
#ifdef __i386__
if (err == MIG_BAD_ID)
{
err = host_kernel_version (mach_host_self (), kv);
}
#endif
assert_backtrace (! err);
kv[sizeof (kv) - 1] = '\0';
p = index (kv, ':');
if (p)
*p = '\0';
p = index (kv, ' ');
if (p)
*p = '\0';
kernel_name = strdup (p ? kv : "mach");
assert_backtrace (kernel_name);
kernel_version = strdup (p ? p + 1 : kv);
assert_backtrace (kernel_version);
server_versions[0].name = strdup ("proc");
assert_backtrace (server_versions[0].name);
server_versions[0].version = strdup (HURD_VERSION);
assert_backtrace (server_versions[0].version);
nserver_versions = 1;
rebuild_uname ();
uname_info.nodename[0] = '\0';
}
kern_return_t
S_proc_uname (pstruct_t process,
struct utsname *uname)
{
*uname = uname_info;
return 0;
}
kern_return_t
S_proc_register_version (pstruct_t server,
mach_port_t credential,
const_string_t name,
const_string_t release,
const_string_t version)
{
error_t err = 0;
int i;
if (credential != _hurd_host_priv)
return EPERM;
for (i = 0; i < nserver_versions; i++)
if (!strcmp (name, server_versions[i].name))
{
free (server_versions[i].version);
server_versions[i].version = malloc (strlen (version) + 1);
if (! server_versions[i].version)
{
err = ENOMEM;
goto out;
}
strcpy (server_versions[i].version, version);
break;
}
if (i == nserver_versions)
{
if (nserver_versions == server_versions_nalloc)
{
void *new = realloc (server_versions,
sizeof (struct server_version) *
server_versions_nalloc * 2);
if (! new)
{
err = ENOMEM;
goto out;
}
server_versions_nalloc *= 2;
server_versions = new;
}
server_versions[nserver_versions].name = malloc (strlen (name) + 1);
if (! server_versions[nserver_versions].name)
{
err = ENOMEM;
goto out;
}
server_versions[nserver_versions].version = malloc (strlen (version)
+ 1);
if (! server_versions[nserver_versions].version)
{
free (server_versions[nserver_versions].name);
err = ENOMEM;
goto out;
}
strcpy (server_versions[nserver_versions].name, name);
strcpy (server_versions[nserver_versions].version, version);
nserver_versions++;
}
rebuild_uname ();
out:
if (!err)
mach_port_deallocate (mach_task_self (), credential);
return err;
}