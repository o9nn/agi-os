#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <hurd/process.h>
#include <hurd/resource.h>
#include <mach/vm_param.h>
#include <ps.h>
#include "procfs.h"
#include "procfs_dir.h"
#include "process.h"
#include "main.h"
static char state_char (struct proc_stat *ps)
{
int i;
for (i = 0; (1U << i) & (PSTAT_STATE_P_STATES | PSTAT_STATE_T_STATES); i++)
if (proc_stat_state (ps) & (1U << i))
return proc_stat_state_tags[i];
return '?';
}
static const char *state_string (struct proc_stat *ps)
{
static const char *const state_strings[] = {
"T (stopped)",
"Z (zombie)",
"R (running)",
"H (halted)",
"D (disk sleep)",
"S (sleeping)",
"I (idle)",
NULL
};
int i;
for (i = 0; state_strings[i]; i++)
if (proc_stat_state (ps) & (1U << i))
return state_strings[i];
return "? (unknown)";
}
static long long int timeval_jiffies (time_value_t tv)
{
double secs = tv.seconds * 1000000. + tv.microseconds;
return secs * opt_clk_tck / 1000000.;
}
static const char *args_filename (const char *name)
{
char *sp = strrchr (name, '/');
return sp != NULL && *(sp + 1) != '\0' ? sp + 1 : name;
}
static int args_filename_length (const char *name)
{
return strchrnul (name, ' ') - name;
}
static ssize_t
process_file_gc_exe (struct proc_stat *ps, char **contents)
{
if (proc_stat_exe_len (ps) == 0)
{
*contents = "-";
return 1;
}
*contents = proc_stat_exe(ps);
return proc_stat_exe_len(ps);
}
static ssize_t
process_file_gc_cmdline (struct proc_stat *ps, char **contents)
{
*contents = proc_stat_args(ps);
return proc_stat_args_len(ps);
}
static ssize_t
process_file_gc_environ (struct proc_stat *ps, char **contents)
{
*contents = proc_stat_env(ps);
return proc_stat_env_len(ps);
}
static ssize_t
process_file_gc_maps (struct proc_stat *ps, char **contents)
{
error_t err;
FILE *s;
size_t contents_len;
vm_offset_t addr = 0;
vm_size_t size;
vm_prot_t prot, max_prot;
mach_port_t obj;
vm_offset_t offs;
vm_inherit_t inh;
int shared;
struct mem_obj
{
mach_port_t port;
struct mem_obj *next;
};
struct mem_obj *objects = NULL;
s = open_memstream (contents, &contents_len);
if (s == NULL)
{
*contents = NULL;
return 0;
}
while (1)
{
err =
vm_region (ps->task, &addr, &size, &prot, &max_prot, &inh,
&shared, &obj, &offs);
if (err)
break;
fprintf (s, "%0*zx-%0*zx %c%c%c%c %0*zx %s %d ",
(int) (2 * sizeof s), addr,
(int) (2 * sizeof s), addr + size,
prot & VM_PROT_READ? 'r': '-',
prot & VM_PROT_WRITE? 'w': '-',
prot & VM_PROT_EXECUTE? 'x': '-',
shared? 's': 'p',
(int) (2 * sizeof s), offs,
"00:00",
0);
if (MACH_PORT_VALID (obj))
{
struct mem_obj *o = malloc (sizeof *o);
if (o)
{
o->port = obj;
o->next = objects;
objects = o;
}
else
mach_port_deallocate (mach_task_self (), obj);
fprintf (s, "[mem_obj=%u]\n", obj);
}
else
fprintf (s, "\n");
addr += size;
}
while (objects)
{
struct mem_obj *o = objects;
mach_port_deallocate (mach_task_self (), o->port);
objects = o->next;
free (o);
}
if (err != KERN_NO_SPACE)
fprintf (s, "%s\n", strerror (err));
fclose (s);
return contents_len;
}
static ssize_t
process_file_gc_stat (struct proc_stat *ps, char **contents)
{
struct procinfo *pi = proc_stat_proc_info (ps);
task_basic_info_t tbi = proc_stat_task_basic_info (ps);
thread_basic_info_t thbi = proc_stat_thread_basic_info (ps);
thread_sched_info_t thsi = proc_stat_thread_sched_info (ps);
const char *fn = args_filename (proc_stat_args (ps));
vm_address_t start_code = 1;
vm_address_t end_code = 1;
process_t p;
error_t err = proc_pid2proc (ps->context->server, ps->pid, &p);
unsigned last_processor;
#ifdef HAVE_STRUCT_THREAD_SCHED_INFO_LAST_PROCESSOR
last_processor = thsi->last_processor;
#else
last_processor = 0;
#endif
if (! err)
{
boolean_t essential = 0;
proc_is_important (p, &essential);
if (essential)
start_code = end_code = 0;
else
proc_get_code (p, &start_code, &end_code);
mach_port_deallocate (mach_task_self (), p);
}
return asprintf (contents,
"%d (%.*s) %c "
"%d %d %d "
"%d %d "
"%u "
"%lu %lu %lu %lu "
"%lu %lu %ld %ld "
"%d %d "
"%d %ld "
"%llu "
"%lu %ld %lu "
"%zu %zu %lu %lu %lu "
"%lu %lu %lu %lu "
"%lu "
"%lu %lu "
"%d "
"%d "
"%u %u "
"%llu "
"\n",
proc_stat_pid (ps), args_filename_length (fn), fn, state_char (ps),
pi->ppid, pi->pgrp, pi->session,
0, 0,
0,
0L, 0L, 0L, 0L,
(long unsigned) timeval_jiffies (thbi->user_time),
(long unsigned) timeval_jiffies (thbi->system_time),
0L, 0L,
MACH_PRIORITY_TO_NICE(thbi->base_priority) + 20,
MACH_PRIORITY_TO_NICE(thbi->base_priority),
pi->nthreads, 0L,
timeval_jiffies (tbi->creation_time),
(long unsigned) tbi->virtual_size,
(long unsigned) tbi->resident_size / PAGE_SIZE, 0L,
start_code,
end_code,
0L, 0L, 0L,
0L, 0L, 0L, 0L,
(long unsigned) proc_stat_thread_rpc (ps),
0L, 0L,
0,
last_processor,
0, 0,
0LL);
}
static ssize_t
process_file_gc_statm (struct proc_stat *ps, char **contents)
{
task_basic_info_t tbi = proc_stat_task_basic_info (ps);
return asprintf (contents,
"%lu %lu 0 0 0 0 0\n",
tbi->virtual_size / sysconf(_SC_PAGE_SIZE),
tbi->resident_size / sysconf(_SC_PAGE_SIZE));
}
static ssize_t
process_file_gc_status (struct proc_stat *ps, char **contents)
{
task_basic_info_t tbi = proc_stat_task_basic_info (ps);
const char *fn = args_filename (proc_stat_args (ps));
return asprintf (contents,
"Name:\t%.*s\n"
"State:\t%s\n"
"Tgid:\t%u\n"
"Pid:\t%u\n"
"PPid:\t%u\n"
"Uid:\t%u\t%u\t%u\t%u\n"
"VmSize:\t%8zu kB\n"
"VmPeak:\t%8zu kB\n"
"VmRSS:\t%8zu kB\n"
"VmHWM:\t%8zu kB\n"
"Threads:\t%u\n",
args_filename_length (fn), fn,
state_string (ps),
proc_stat_pid (ps),
proc_stat_pid (ps),
proc_stat_proc_info (ps)->ppid,
proc_stat_owner_uid (ps),
proc_stat_owner_uid (ps),
proc_stat_owner_uid (ps),
proc_stat_owner_uid (ps),
tbi->virtual_size / 1024,
tbi->virtual_size / 1024,
tbi->resident_size / 1024,
tbi->resident_size / 1024,
proc_stat_num_threads (ps));
}
struct process_file_desc
{
ps_flags_t needs;
ssize_t (*get_contents) (struct proc_stat *ps, char **contents);
int no_cleanup;
mode_t mode;
};
struct process_file_node
{
const struct process_file_desc *desc;
struct proc_stat *ps;
};
static error_t
process_file_get_contents (void *hook, char **contents, ssize_t *contents_len)
{
struct process_file_node *file = hook;
error_t err;
err = proc_stat_set_flags (file->ps, file->desc->needs);
if (err)
return EIO;
if ((proc_stat_flags (file->ps) & file->desc->needs) != file->desc->needs)
return EIO;
*contents_len = file->desc->get_contents (file->ps, contents);
return 0;
}
static void
process_file_cleanup_contents (void *hook, char *contents, ssize_t len)
{
struct process_file_node *file = hook;
if (! file->desc->no_cleanup)
free (contents);
}
static struct node *
process_file_make_node (void *dir_hook, const void *entry_hook)
{
static const struct procfs_node_ops ops = {
.get_contents = process_file_get_contents,
.cleanup_contents = process_file_cleanup_contents,
.cleanup = free,
};
struct process_file_node *f;
struct node *np;
f = malloc (sizeof *f);
if (! f)
return NULL;
f->desc = entry_hook;
f->ps = dir_hook;
np = procfs_make_node (&ops, f);
if (! np)
return NULL;
procfs_node_chown (np, proc_stat_owner_uid (f->ps));
if (f->desc->mode)
procfs_node_chmod (np, f->desc->mode);
return np;
}
static struct node *
process_file_symlink_make_node (void *dir_hook, const void *entry_hook)
{
struct node *np = process_file_make_node (dir_hook, entry_hook);
if (np) procfs_node_chtype (np, S_IFLNK);
return np;
}
static struct node *
process_stat_make_node (void *dir_hook, const void *entry_hook)
{
struct node *np = process_file_make_node (dir_hook, entry_hook);
if (np) procfs_node_chmod (np, opt_stat_mode);
return np;
}
static struct procfs_dir_entry entries[] = {
{
.name = "exe",
.hook = & (struct process_file_desc) {
.get_contents = process_file_gc_exe,
.needs = PSTAT_EXE,
.no_cleanup = 1,
},
.ops = {
.make_node = process_file_symlink_make_node,
},
},
{
.name = "cmdline",
.hook = & (struct process_file_desc) {
.get_contents = process_file_gc_cmdline,
.needs = PSTAT_ARGS,
.no_cleanup = 1,
},
},
{
.name = "environ",
.hook = & (struct process_file_desc) {
.get_contents = process_file_gc_environ,
.needs = PSTAT_ENV,
.no_cleanup = 1,
.mode = 0400,
},
},
{
.name = "maps",
.hook = & (struct process_file_desc) {
.get_contents = process_file_gc_maps,
.needs = PSTAT_TASK,
.mode = 0400,
},
},
{
.name = "stat",
.hook = & (struct process_file_desc) {
.get_contents = process_file_gc_stat,
.needs = PSTAT_PID | PSTAT_ARGS | PSTAT_STATE | PSTAT_PROC_INFO
| PSTAT_TASK | PSTAT_TASK_BASIC | PSTAT_THREAD_BASIC
| PSTAT_THREAD_SCHED | PSTAT_THREAD_WAIT,
},
.ops = {
.make_node = process_stat_make_node,
}
},
{
.name = "statm",
.hook = & (struct process_file_desc) {
.get_contents = process_file_gc_statm,
.needs = PSTAT_TASK_BASIC,
},
},
{
.name = "status",
.hook = & (struct process_file_desc) {
.get_contents = process_file_gc_status,
.needs = PSTAT_PID | PSTAT_ARGS | PSTAT_STATE | PSTAT_PROC_INFO
| PSTAT_TASK_BASIC | PSTAT_OWNER_UID | PSTAT_NUM_THREADS,
},
},
{}
};
error_t
process_lookup_pid (struct ps_context *pc, pid_t pid, struct node **np)
{
static const struct procfs_dir_ops dir_ops = {
.entries = entries,
.cleanup = (void (*)(void *)) _proc_stat_free,
.entry_ops = {
.make_node = process_file_make_node,
},
};
struct proc_stat *ps;
int owner;
error_t err;
err = _proc_stat_create (pid, pc, &ps);
if (err == ESRCH)
return ENOENT;
if (err)
return EIO;
err = proc_stat_set_flags (ps, PSTAT_OWNER_UID);
if (err || ! (proc_stat_flags (ps) & PSTAT_OWNER_UID))
{
_proc_stat_free (ps);
return EIO;
}
*np = procfs_dir_make_node (&dir_ops, ps);
if (! *np)
return ENOMEM;
owner = proc_stat_owner_uid (ps);
procfs_node_chown (*np, owner >= 0 ? owner : opt_anon_owner);
return 0;
}