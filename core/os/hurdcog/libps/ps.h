#ifndef __PS_H__
#define __PS_H__
#include <hurd/hurd_types.h>
#include <hurd/ihash.h>
#include <mach/mach.h>
#include <pwd.h>
#include <errno.h>
enum ps_user_passwd_state
{ PS_USER_PASSWD_OK, PS_USER_PASSWD_PENDING, PS_USER_PASSWD_ERROR };
struct ps_user
{
uid_t uid;
enum ps_user_passwd_state passwd_state;
struct passwd passwd;
char *storage;
};
#define ps_user_uid(u) ((u)->uid)
error_t ps_user_create (uid_t uid, struct ps_user **u);
error_t ps_user_uname_create (char *uname, struct ps_user **u);
error_t ps_user_passwd_create (struct passwd *pw, struct ps_user **u);
void ps_user_free (struct ps_user *u);
struct passwd *ps_user_passwd (struct ps_user *u);
char *ps_user_name (struct ps_user *u);
enum ps_tty_name_state
{ PS_TTY_NAME_OK, PS_TTY_NAME_PENDING, PS_TTY_NAME_ERROR };
struct ps_tty {
file_t port;
const char *name;
enum ps_tty_name_state name_state;
const char *short_name;
int short_name_alloced : 1;
};
#define ps_tty_port(tty) ((tty)->port)
error_t ps_tty_create (file_t port, struct ps_tty **tty);
void ps_tty_free (struct ps_tty *tty);
const char *ps_tty_name (struct ps_tty *tty);
const char *ps_tty_short_name (struct ps_tty *tty);
struct proc_stat;
struct ps_context
{
process_t server;
struct hurd_ihash procs;
struct hurd_ihash ttys;
struct hurd_ihash ttys_by_cttyid;
struct hurd_ihash users;
struct ps_user_hooks *user_hooks;
};
#define ps_context_server(pc) ((pc)->server)
error_t ps_context_create (process_t server, struct ps_context **pc);
void ps_context_free (struct ps_context *pc);
error_t ps_context_find_proc_stat (struct ps_context *pc, pid_t pid,
struct proc_stat **ps);
error_t ps_context_find_tty (struct ps_context *pc, mach_port_t tty_port,
struct ps_tty **tty);
error_t ps_context_find_tty_by_cttyid (struct ps_context *pc,
mach_port_t cttyid_port,
struct ps_tty **tty);
error_t ps_context_find_user (struct ps_context *pc, uid_t uid,
struct ps_user **u);
typedef unsigned ps_flags_t;
typedef unsigned ps_state_t;
struct proc_stat
{
struct ps_context *context;
pid_t pid;
ps_flags_t flags;
ps_flags_t failed;
ps_flags_t inapp;
struct proc_stat *thread_origin;
unsigned thread_index;
process_t process;
task_t task;
mach_port_t msgport;
struct procinfo *proc_info;
unsigned proc_info_size;
unsigned num_threads;
task_basic_info_t task_basic_info;
thread_basic_info_t thread_basic_info;
thread_sched_info_t thread_sched_info;
char *thread_wait;
mach_msg_id_t thread_rpc;
char *thread_waits;
size_t thread_waits_len;
int suspend_count;
ps_state_t state;
struct ps_user *owner;
int owner_uid;
char *args;
size_t args_len;
task_events_info_t task_events_info;
task_events_info_data_t task_events_info_buf;
size_t task_events_info_size;
unsigned proc_info_vm_alloced : 1;
unsigned thread_waits_vm_alloced : 1;
unsigned args_vm_alloced : 1;
unsigned env_vm_alloced : 1;
unsigned exe_vm_alloced : 1;
mach_port_t cttyid;
mach_port_t cwdir;
mach_port_t auth;
unsigned umask;
struct ps_tty *tty;
void *hook;
char *env;
size_t env_len;
unsigned num_ports;
char *exe;
size_t exe_len;
};
#define PSTAT_PID	       0x00001
#define PSTAT_THREAD	       0x00002
#define PSTAT_PROCESS	       0x00004
#define PSTAT_TASK	       0x00008
#define PSTAT_MSGPORT	       0x00010
#define PSTAT_PROC_INFO	       0x00020
#define PSTAT_TASK_BASIC       0x00040
#define PSTAT_TASK_EVENTS      0x00080
#define PSTAT_NUM_THREADS      0x00100
#define PSTAT_THREAD_BASIC     0x00200
#define PSTAT_THREAD_SCHED     0x00400
#define PSTAT_THREAD_WAIT      0x00800
#define PSTAT_THREAD_WAITS     0x01000
#define PSTAT_ARGS	       0x02000
#define PSTAT_ENV	     0x2000000
#define PSTAT_STATE	       0x04000
#define PSTAT_SUSPEND_COUNT    0x08000
#define PSTAT_CTTYID	       0x10000
#define PSTAT_CWDIR	       0x20000
#define PSTAT_AUTH	       0x40000
#define PSTAT_TTY	       0x80000
#define PSTAT_OWNER	      0x100000
#define PSTAT_OWNER_UID	      0x200000
#define PSTAT_UMASK	      0x400000
#define PSTAT_HOOK	      0x800000
#define PSTAT_NUM_PORTS      0x4000000
#define PSTAT_TIMES          0x8000000
#define PSTAT_EXE           0x10000000
#define PSTAT_NO_MSGPORT     0x1000000
#define PSTAT_USER_BASE      0x20000000
#define PSTAT_USER_MASK      ~(PSTAT_USER_BASE - 1)
#define PSTAT_STATE_P_STOP	0x00001
#define PSTAT_STATE_P_ZOMBIE	0x00002
#define PSTAT_STATE_P_STATES	(PSTAT_STATE_P_STOP | PSTAT_STATE_P_ZOMBIE)
#define PSTAT_STATE_P_FG	0x00400
#define PSTAT_STATE_P_SESSLDR	0x00800
#define PSTAT_STATE_P_LOGINLDR	0x01000
#define PSTAT_STATE_P_FORKED	0x02000
#define PSTAT_STATE_P_NOMSG	0x04000
#define PSTAT_STATE_P_NOPARENT	0x08000
#define PSTAT_STATE_P_ORPHAN	0x10000
#define PSTAT_STATE_P_TRACE     0x20000
#define PSTAT_STATE_P_WAIT	0x40000
#define PSTAT_STATE_P_GETMSG	0x80000
#define PSTAT_STATE_P_ATTRS  (PSTAT_STATE_P_FG | PSTAT_STATE_P_SESSLDR \
| PSTAT_STATE_P_LOGINLDR | PSTAT_STATE_P_FORKED \
| PSTAT_STATE_P_NOMSG | PSTAT_STATE_P_NOPARENT \
| PSTAT_STATE_P_ORPHAN | PSTAT_STATE_P_TRACE \
| PSTAT_STATE_P_WAIT | PSTAT_STATE_P_GETMSG)
#define PSTAT_STATE_T_RUN	0x00004
#define PSTAT_STATE_T_HALT	0x00008
#define PSTAT_STATE_T_WAIT	0x00010
#define PSTAT_STATE_T_SLEEP	0x00020
#define PSTAT_STATE_T_IDLE	0x00040
#define PSTAT_STATE_T_STATES	(PSTAT_STATE_T_RUN | PSTAT_STATE_T_HALT \
| PSTAT_STATE_T_WAIT | PSTAT_STATE_T_SLEEP \
| PSTAT_STATE_T_IDLE)
#define PSTAT_STATE_T_NICE	0x00080
#define PSTAT_STATE_T_NASTY     0x00100
#define PSTAT_STATE_T_UNCLEAN	0x00200
#define PSTAT_STATE_T_ATTRS	(PSTAT_STATE_T_UNCLEAN \
| PSTAT_STATE_T_NICE | PSTAT_STATE_T_NASTY)
extern char *proc_stat_state_tags;
#define proc_stat_flags(ps) ((ps)->flags)
#define proc_stat_thread_origin(ps) ((ps)->thread_origin)
#define proc_stat_thread_index(ps) ((ps)->thread_index)
#define proc_stat_pid(ps) ((ps)->pid)
#define proc_stat_process(ps) ((ps)->process)
#define proc_stat_task(ps) ((ps)->task)
#define proc_stat_msgport(ps) ((ps)->msgport)
#define proc_stat_proc_info(ps) ((ps)->proc_info)
#define proc_stat_num_threads(ps) ((ps)->num_threads)
#define proc_stat_task_basic_info(ps) ((ps)->task_basic_info)
#define proc_stat_thread_basic_info(ps) ((ps)->thread_basic_info)
#define proc_stat_thread_sched_info(ps) ((ps)->thread_sched_info)
#define proc_stat_thread_rpc(ps) ((ps)->thread_rpc)
#define proc_stat_thread_wait(ps) ((ps)->thread_rpc)
#define proc_stat_suspend_count(ps) ((ps)->suspend_count)
#define proc_stat_args(ps) ((ps)->args)
#define proc_stat_args_len(ps) ((ps)->args_len)
#define proc_stat_env(ps) ((ps)->env)
#define proc_stat_env_len(ps) ((ps)->env_len)
#define proc_stat_state(ps) ((ps)->state)
#define proc_stat_cttyid(ps) ((ps)->cttyid)
#define proc_stat_cwdir(ps) ((ps)->cwdir)
#define proc_stat_owner(ps) ((ps)->owner)
#define proc_stat_owner_uid(ps) ((ps)->owner_uid)
#define proc_stat_auth(ps) ((ps)->auth)
#define proc_stat_umask(ps) ((ps)->umask)
#define proc_stat_tty(ps) ((ps)->tty)
#define proc_stat_task_events_info(ps) ((ps)->task_events_info)
#define proc_stat_num_ports(ps) ((ps)->num_ports)
#define proc_stat_exe(ps) ((ps)->exe)
#define proc_stat_exe_len(ps) ((ps)->exe_len)
#define proc_stat_has(ps, needs) (((ps)->flags & needs) == needs)
#define proc_stat_is_thread(ps) ((ps)->pid < 0)
error_t _proc_stat_create (pid_t pid, struct ps_context *context,
struct proc_stat **ps);
void _proc_stat_free (struct proc_stat *ps);
error_t proc_stat_set_flags (struct proc_stat *ps, ps_flags_t flags);
error_t proc_stat_thread_create (struct proc_stat *ps, unsigned n,
struct proc_stat **thread_ps);
struct ps_user_hooks
{
ps_flags_t (*dependencies) (ps_flags_t flags);
ps_flags_t (*fetch) (struct proc_stat *ps, ps_flags_t need, ps_flags_t have);
void (*cleanup) (struct proc_stat *ps);
};
struct ps_getter
{
char *name;
ps_flags_t needs;
void *fn;
};
#define ps_getter_name(g) ((g)->name)
#define ps_getter_needs(g) ((g)->needs)
#define ps_getter_function(g) ((g)->fn)
struct ps_filter
{
char *name;
ps_flags_t needs;
int (*fn) (struct proc_stat *ps);
};
#define ps_filter_name(f) ((f)->name)
#define ps_filter_needs(f) ((f)->needs)
#define ps_filter_predicate(f) ((f)->fn)
extern const struct ps_filter ps_own_filter;
extern const struct ps_filter ps_not_leader_filter;
extern const struct ps_filter ps_ctty_filter;
extern const struct ps_filter ps_unorphaned_filter;
extern const struct ps_filter ps_parent_filter;
extern const struct ps_filter ps_alive_filter;
struct ps_stream
{
FILE *stream;
int pos;
int spaces;
};
error_t ps_stream_create (FILE *dest, struct ps_stream **stream);
void ps_stream_free (struct ps_stream *stream);
error_t ps_stream_write (struct ps_stream *stream,
const char *string, ssize_t max_len);
error_t ps_stream_space (struct ps_stream *stream, ssize_t num);
error_t ps_stream_pad (struct ps_stream *stream,
ssize_t sofar, ssize_t width);
error_t ps_stream_newline (struct ps_stream *stream);
error_t ps_stream_write_field (struct ps_stream *stream,
const char *buf, int width);
error_t ps_stream_write_trunc_field (struct ps_stream *stream,
const char *buf, int width);
error_t ps_stream_write_int_field (struct ps_stream *stream,
int value, int width);
struct ps_fmt_field;
struct ps_fmt_spec
{
const char *name;
const char *title;
int width;
int precision;
int flags;
const struct ps_getter *getter;
error_t (*output_fn)(struct proc_stat *ps, struct ps_fmt_field *field,
struct ps_stream *stream);
int (*cmp_fn)(struct proc_stat *ps1, struct proc_stat *ps2,
const struct ps_getter *getter);
int (*nominal_fn)(struct proc_stat *ps, const struct ps_getter *getter);
};
#define ps_fmt_spec_name(spec) ((spec)->name)
#define ps_fmt_spec_title(spec) ((spec)->title)
#define ps_fmt_spec_width(spec) ((spec)->width)
#define ps_fmt_spec_output_fn(spec) ((spec)->output_fn)
#define ps_fmt_spec_compare_fn(spec) ((spec)->cmp_fn)
#define ps_fmt_spec_nominal_fn(spec) ((spec)->nominal_fn)
#define ps_fmt_spec_getter(spec) ((spec)->getter)
#define ps_fmt_spec_is_end(spec) ((spec)->name == NULL)
struct ps_fmt_specs
{
const struct ps_fmt_spec *specs;
struct ps_fmt_specs *parent;
struct ps_fmt_spec_block *expansions;
};
extern struct ps_fmt_specs ps_std_fmt_specs;
const struct ps_fmt_spec *ps_fmt_specs_find (struct ps_fmt_specs *specs,
const char *name);
#define PS_FMT_FIELD_AT_MOD		0x1
#define PS_FMT_FIELD_COLON_MOD		0x2
#define PS_FMT_FIELD_KEEP		0x4
#define PS_FMT_FIELD_UPCASE_TITLE	0x8
struct ps_fmt_field
{
const struct ps_fmt_spec *spec;
const char *pfx;
unsigned pfx_len;
int width;
int precision;
int flags;
const char *title;
};
#define ps_fmt_field_fmt_spec(field) ((field)->spec)
#define ps_fmt_field_prefix(field) ((field)->pfx)
#define ps_fmt_field_prefix_length(field) ((field)->pfx_len)
#define ps_fmt_field_width(field) ((field)->width)
#define ps_fmt_field_title(field) ((field)->title)
struct ps_fmt
{
struct ps_fmt_field *fields;
unsigned num_fields;
ps_flags_t needs;
char *src;
size_t src_len;
char *inapp;
char *error;
};
#define ps_fmt_fields(fmt) ((fmt)->fields)
#define ps_fmt_num_fields(fmt) ((fmt)->num_fields)
#define ps_fmt_needs(fmt) ((fmt)->needs)
#define ps_fmt_inval (fmt) ((fmt)->inval)
error_t ps_fmt_create (char *src, int posix, struct ps_fmt_specs *fmt_specs,
struct ps_fmt **fmt);
void ps_fmt_creation_error (char *src, int posix,
struct ps_fmt_specs *fmt_specs,
char **error);
void ps_fmt_free (struct ps_fmt *fmt);
error_t ps_fmt_clone (struct ps_fmt *fmt, struct ps_fmt **copy);
error_t ps_fmt_write_titles (struct ps_fmt *fmt, struct ps_stream *stream);
error_t ps_fmt_write_proc_stat (struct ps_fmt *fmt, struct proc_stat *ps,
struct ps_stream *stream);
void ps_fmt_squash (struct ps_fmt *fmt, int (*fn)(struct ps_fmt_field *field));
void ps_fmt_squash_flags (struct ps_fmt *fmt, ps_flags_t flags);
void ps_fmt_set_output_width (struct ps_fmt *fmt, int width);
struct proc_stat_list
{
struct proc_stat **proc_stats;
unsigned num_procs;
unsigned alloced;
struct ps_context *context;
};
#define proc_stat_list_num_procs(pp) ((pp)->num_procs)
#define proc_stat_list_context(pp) ((pp)->context)
error_t proc_stat_list_create (struct ps_context *context,
struct proc_stat_list **pp);
void proc_stat_list_free (struct proc_stat_list *pp);
error_t proc_stat_list_clone (struct proc_stat_list *pp,
struct proc_stat_list **copy);
struct proc_stat *proc_stat_list_pid_proc_stat (struct proc_stat_list *pp,
pid_t pid);
error_t proc_stat_list_add_pids (struct proc_stat_list *pp,
pid_t *pids, unsigned num_procs,
struct proc_stat ***proc_stats);
error_t proc_stat_list_add_pid (struct proc_stat_list *pp, pid_t pid,
struct proc_stat **ps);
error_t proc_stat_list_merge (struct proc_stat_list *pp,
struct proc_stat_list *mergee);
error_t proc_stat_list_add_all (struct proc_stat_list *pp,
struct proc_stat ***proc_stats,
size_t *num_procs);
error_t proc_stat_list_add_login_coll (struct proc_stat_list *pp,
pid_t login_id,
struct proc_stat ***proc_stats,
size_t *num_procs);
error_t proc_stat_list_add_session (struct proc_stat_list *pp,
pid_t session_id,
struct proc_stat ***proc_stats,
size_t *num_procs);
error_t proc_stat_list_add_pgrp (struct proc_stat_list *pp, pid_t pgrp,
struct proc_stat ***proc_stats,
size_t *num_procs);
error_t proc_stat_list_set_flags (struct proc_stat_list *pp, ps_flags_t flags);
error_t proc_stat_list_filter1 (struct proc_stat_list *pp,
int (*predicate)(struct proc_stat *ps),
ps_flags_t flags,
int invert);
error_t proc_stat_list_filter (struct proc_stat_list *pp,
const struct ps_filter *filter, int invert);
typedef int (*proc_stat_cmp_fun)(struct proc_stat *ps1,
struct proc_stat *ps2,
const struct ps_getter *getter);
error_t proc_stat_list_sort1 (struct proc_stat_list *pp,
const struct ps_getter *getter,
proc_stat_cmp_fun cmp_fn,
int reverse);
error_t proc_stat_list_sort (struct proc_stat_list *pp,
const struct ps_fmt_spec *key, int reverse);
error_t proc_stat_list_fmt (struct proc_stat_list *pp, struct ps_fmt *fmt,
struct ps_stream *stream);
error_t proc_stat_list_find_bogus_flags (struct proc_stat_list *pp,
ps_flags_t *flags);
error_t proc_stat_list_add_threads (struct proc_stat_list *pp);
error_t proc_stat_list_remove_threads (struct proc_stat_list *pp);
int proc_stat_list_for_each (struct proc_stat_list *pp,
int (*fn)(struct proc_stat *ps));
int proc_stat_list_spec_nominal (struct proc_stat_list *pp,
const struct ps_fmt_spec *spec);
mach_port_t ps_get_host (void);
error_t ps_host_basic_info (host_basic_info_t *host_info);
error_t ps_host_sched_info (host_sched_info_t *host_info);
error_t ps_host_load_info (host_load_info_t *host_info);
error_t ps_emit_string (struct proc_stat *ps, struct ps_fmt_field *field,
struct ps_stream *stream);
int ps_nominal_string (struct proc_stat *ps, const struct ps_getter *getter);
error_t
ps_emit_user_name (struct proc_stat *ps, struct ps_fmt_field *field,
struct ps_stream *stream);
error_t
ps_emit_past_time (struct proc_stat *ps, struct ps_fmt_field *field,
struct ps_stream *stream);
error_t
ps_emit_minutes (struct proc_stat *ps, struct ps_fmt_field *field,
struct ps_stream *stream);
int
ps_cmp_times (struct proc_stat *ps1, struct proc_stat *ps2,
const struct ps_getter *getter);
int
ps_cmp_strings (struct proc_stat *ps1, struct proc_stat *ps2,
const struct ps_getter *getter);
int
ps_cmp_unames (struct proc_stat *ps1, struct proc_stat *ps2,
const struct ps_getter *getter);
#endif