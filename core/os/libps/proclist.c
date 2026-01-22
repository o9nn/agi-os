#include <hurd.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert-backtrace.h>
#include <string.h>
#include "ps.h"
#include "common.h"
error_t
proc_stat_list_create (struct ps_context *context, struct proc_stat_list **pp)
{
*pp = NEW (struct proc_stat_list);
if (*pp == NULL)
return ENOMEM;
(*pp)->proc_stats = 0;
(*pp)->num_procs = 0;
(*pp)->alloced = 0;
(*pp)->context = context;
return 0;
}
void
proc_stat_list_free (struct proc_stat_list *pp)
{
proc_stat_list_remove_threads (pp);
FREE (pp->proc_stats);
FREE (pp);
}
error_t
proc_stat_list_clone (struct proc_stat_list *pp, struct proc_stat_list **copy)
{
struct proc_stat_list *new = NEW (struct proc_stat_list);
struct proc_stat **procs = NEWVEC (struct proc_stat *, pp->num_procs);
if (!new || !procs)
{
free (new);
free (procs);
return ENOMEM;
}
memcpy (procs, pp->proc_stats, sizeof *procs * pp->num_procs);
new->proc_stats = procs;
new->num_procs = pp->num_procs;
new->alloced = pp->num_procs;
new->context = pp->context;
*copy = new;
return 0;
}
static error_t
proc_stat_list_grow (struct proc_stat_list *pp, int amount)
{
amount += pp->num_procs;
if (amount > pp->alloced)
{
struct proc_stat **new_procs =
GROWVEC (pp->proc_stats, struct proc_stat *, amount);
if (new_procs == NULL)
return ENOMEM;
pp->alloced = amount;
pp->proc_stats = new_procs;
}
return 0;
}
error_t
proc_stat_list_add_pids (struct proc_stat_list *pp,
pid_t *pids, unsigned num_procs,
struct proc_stat ***proc_stats)
{
error_t err = proc_stat_list_grow (pp, num_procs);
if (err)
return err;
else
{
unsigned i;
struct proc_stat **end = pp->proc_stats + pp->num_procs;
if (proc_stats)
*proc_stats = NEWVEC (struct proc_stat *, num_procs);
for (i = 0; i < num_procs; i++)
{
int pid = *pids++;
struct proc_stat *ps = proc_stat_list_pid_proc_stat (pp, pid);
if (ps == NULL)
{
err = ps_context_find_proc_stat (pp->context, pid, end);
if (err)
{
if (proc_stats)
free (*proc_stats);
return err;
}
else
ps = *end++;
}
if (proc_stats)
(*proc_stats)[i] = ps;
}
pp->num_procs = end - pp->proc_stats;
return 0;
}
}
error_t
proc_stat_list_add_pid (struct proc_stat_list *pp, pid_t pid, struct proc_stat **ps)
{
struct proc_stat *_ps = proc_stat_list_pid_proc_stat (pp, pid);
if (_ps == NULL)
{
error_t err;
if (pp->num_procs == pp->alloced)
{
err = proc_stat_list_grow (pp, 32);
if (err)
return err;
}
err = ps_context_find_proc_stat (pp->context, pid, &_ps);
if (err)
return err;
pp->proc_stats[pp->num_procs++] = _ps;
}
if (ps)
*ps = _ps;
return 0;
}
struct proc_stat *
proc_stat_list_pid_proc_stat (struct proc_stat_list *pp, pid_t pid)
{
unsigned nprocs = pp->num_procs;
struct proc_stat **procs = pp->proc_stats;
while (nprocs-- > 0)
if (proc_stat_pid (*procs) == pid)
return *procs;
else
procs++;
return NULL;
}
error_t
proc_stat_list_merge (struct proc_stat_list *pp, struct proc_stat_list *mergee)
{
if (pp->context != mergee->context)
return EINVAL;
else
{
error_t err = proc_stat_list_grow (pp, mergee->num_procs);
if (err)
return err;
else
{
int mnprocs = mergee->num_procs;
struct proc_stat **mprocs = mergee->proc_stats;
int nprocs = pp->num_procs;
struct proc_stat **procs = pp->proc_stats;
while (mnprocs-- > 0)
if (proc_stat_list_pid_proc_stat(pp, proc_stat_pid (mprocs[mnprocs]))
== NULL)
{
procs[nprocs++] = mprocs[mnprocs];
mprocs[mnprocs] = NULL;
}
proc_stat_list_free (mergee);
return 0;
}
}
}
#define STATICPIDS 200
typedef kern_return_t fetch_fn_pids_t (process_t proc, pid_t **pids,
mach_msg_type_number_t *num_pids);
static error_t
proc_stat_list_add_fn_pids (struct proc_stat_list *pp,
fetch_fn_pids_t fetch_fn,
struct proc_stat ***proc_stats, size_t *num_procs)
{
error_t err;
pid_t pid_array[STATICPIDS], *pids = pid_array;
mach_msg_type_number_t num_pids = STATICPIDS;
err = (*fetch_fn)(ps_context_server (pp->context), &pids, &num_pids);
if (err)
return err;
err = proc_stat_list_add_pids (pp, pids, num_pids, proc_stats);
if (!err && num_procs)
*num_procs = num_pids;
if (pids != pid_array)
VMFREE (pids, sizeof (pid_t) * num_pids);
return err;
}
typedef kern_return_t fetch_id_fn_pids_t (process_t proc, pid_t id,
pid_t **pids,
mach_msg_type_number_t *num_pids);
static error_t
proc_stat_list_add_id_fn_pids (struct proc_stat_list *pp, unsigned id,
fetch_id_fn_pids_t fetch_fn,
struct proc_stat ***proc_stats,
size_t *num_procs)
{
error_t id_fetch_fn (process_t proc, pid_t **pids,
mach_msg_type_number_t *num_pids)
{
return (*fetch_fn)(proc, id, pids, num_pids);
}
return proc_stat_list_add_fn_pids (pp, id_fetch_fn, proc_stats, num_procs);
}
error_t
proc_stat_list_add_all (struct proc_stat_list *pp,
struct proc_stat ***proc_stats, size_t *num_procs)
{
return
proc_stat_list_add_fn_pids (pp, proc_getallpids, proc_stats, num_procs);
}
error_t
proc_stat_list_add_login_coll (struct proc_stat_list *pp, pid_t login_id,
struct proc_stat ***proc_stats,
size_t *num_procs)
{
return
proc_stat_list_add_id_fn_pids (pp, login_id, proc_getloginpids,
proc_stats, num_procs);
}
error_t
proc_stat_list_add_session (struct proc_stat_list *pp, pid_t session_id,
struct proc_stat ***proc_stats,
size_t *num_procs)
{
return
proc_stat_list_add_id_fn_pids (pp, session_id, proc_getsessionpids,
proc_stats, num_procs);
}
error_t
proc_stat_list_add_pgrp (struct proc_stat_list *pp, pid_t pgrp,
struct proc_stat ***proc_stats, size_t *num_procs)
{
return
proc_stat_list_add_id_fn_pids (pp, pgrp, proc_getpgrppids,
proc_stats, num_procs);
}
error_t
proc_stat_list_set_flags (struct proc_stat_list *pp, ps_flags_t flags)
{
unsigned nprocs = pp->num_procs;
struct proc_stat **procs = pp->proc_stats;
while (nprocs-- > 0)
{
struct proc_stat *ps = *procs++;
if (!proc_stat_has (ps, flags))
{
error_t err = proc_stat_set_flags (ps, flags);
if (err)
return err;
}
}
return 0;
}
error_t
proc_stat_list_filter1(struct proc_stat_list *pp,
int (*predicate)(struct proc_stat *ps), ps_flags_t flags,
int invert)
{
unsigned which = 0;
unsigned num_procs = pp->num_procs;
struct proc_stat **procs = pp->proc_stats;
struct proc_stat **kept = procs;
error_t err = proc_stat_list_set_flags (pp, flags);
if (err)
return err;
invert = !!invert;
while (which < num_procs)
{
struct proc_stat *ps = procs[which++];
if (!proc_stat_has(ps, flags) || !!predicate (ps) != invert)
*kept++ = ps;
}
pp->num_procs = kept - procs;
return 0;
}
error_t
proc_stat_list_filter (struct proc_stat_list *pp,
const struct ps_filter *filter, int invert)
{
return
proc_stat_list_filter1(pp,
ps_filter_predicate (filter),
ps_filter_needs (filter),
invert);
}
error_t
proc_stat_list_sort1 (struct proc_stat_list *pp,
const struct ps_getter *getter,
proc_stat_cmp_fun cmp_fn,
int reverse)
{
int needs = ps_getter_needs (getter);
struct proc_stat **procs = pp->proc_stats;
error_t err = proc_stat_list_set_flags (pp, needs);
int lessp (const void *p1, const void *p2)
{
struct proc_stat *ps1 = *(struct proc_stat **)p1;
struct proc_stat *ps2 = *(struct proc_stat **)p2;
int is_th_1 = proc_stat_is_thread (ps1);
int is_th_2 = proc_stat_is_thread (ps2);
if (!is_th_1 || !is_th_2
|| proc_stat_thread_origin(ps1) != proc_stat_thread_origin (ps2))
{
if (is_th_1)
ps1 = proc_stat_thread_origin (ps1);
if (is_th_2)
ps2 = proc_stat_thread_origin (ps2);
}
if (ps1 == ps2 || !proc_stat_has(ps1, needs) || !proc_stat_has (ps2, needs))
return p1 - p2;
else if (reverse)
return cmp_fn (ps2, ps1, getter);
else
return cmp_fn (ps1, ps2, getter);
}
if (err)
return err;
qsort((void *)procs, (size_t)pp->num_procs, sizeof (struct proc_stat *), lessp);
return 0;
}
error_t
proc_stat_list_sort (struct proc_stat_list *pp,
const struct ps_fmt_spec *key, int reverse)
{
proc_stat_cmp_fun cmp_fn = ps_fmt_spec_compare_fn (key);
if (cmp_fn == NULL)
return EINVAL;
else
return
proc_stat_list_sort1 (pp, ps_fmt_spec_getter (key), cmp_fn, reverse);
}
error_t
proc_stat_list_fmt (struct proc_stat_list *pp, struct ps_fmt *fmt,
struct ps_stream *stream)
{
unsigned nprocs = pp->num_procs;
struct proc_stat **procs = pp->proc_stats;
error_t err = proc_stat_list_set_flags(pp, ps_fmt_needs (fmt));
while (!err && nprocs-- > 0)
{
err = ps_fmt_write_proc_stat (fmt, *procs++, stream);
if (! err)
ps_stream_newline (stream);
}
return err;
}
error_t
proc_stat_list_find_bogus_flags (struct proc_stat_list *pp, ps_flags_t *flags)
{
unsigned nprocs = pp->num_procs;
struct proc_stat **procs = pp->proc_stats;
error_t err = proc_stat_list_set_flags (pp, *flags);
if (err)
return err;
while (nprocs-- > 0 && *flags != 0)
*flags &= ~proc_stat_flags (*procs++);
return 0;
}
error_t
proc_stat_list_add_threads (struct proc_stat_list *pp)
{
error_t err = proc_stat_list_set_flags (pp, PSTAT_NUM_THREADS);
if (err)
return err;
else
{
int new_entries = 0;
int nprocs = pp->num_procs;
struct proc_stat **procs = pp->proc_stats;
while (nprocs-- > 0)
{
struct proc_stat *ps = *procs++;
if (proc_stat_has (ps, PSTAT_NUM_THREADS))
new_entries += proc_stat_num_threads (ps);
}
err = proc_stat_list_grow (pp, new_entries);
if (err)
return err;
else
{
struct proc_stat **end = pp->proc_stats + pp->num_procs + new_entries;
nprocs = pp->num_procs;
procs = pp->proc_stats + nprocs;
while (nprocs-- > 0)
{
struct proc_stat *ps = *--procs;
if (proc_stat_has (ps, PSTAT_NUM_THREADS))
{
int nthreads = proc_stat_num_threads (ps);
while (nthreads-- > 0)
proc_stat_thread_create (ps, nthreads, --end);
}
*--end = ps;
}
pp->num_procs += new_entries;
}
}
return 0;
}
error_t
proc_stat_list_remove_threads (struct proc_stat_list *pp)
{
int is_thread (struct proc_stat *ps)
{
return proc_stat_is_thread (ps);
}
return proc_stat_list_filter1(pp, is_thread, 0, FALSE);
}
int
proc_stat_list_for_each (struct proc_stat_list *pp, int (*fn)(struct proc_stat *ps))
{
unsigned nprocs = pp->num_procs;
struct proc_stat **procs = pp->proc_stats;
while (nprocs-- > 0)
{
int val = (*fn)(*procs++);
if (val)
return val;
}
return 0;
}
int
proc_stat_list_spec_nominal (struct proc_stat_list *pp,
const struct ps_fmt_spec *spec)
{
int (*nominal_fn)(struct proc_stat *ps, const struct ps_getter *getter) =
spec->nominal_fn;
if (nominal_fn == NULL)
return FALSE;
else
{
const struct ps_getter *getter = ps_fmt_spec_getter (spec);
ps_flags_t needs = ps_getter_needs (getter);
int interesting (struct proc_stat *ps)
{
return proc_stat_has (ps, needs) && !(*nominal_fn)(ps, getter);
}
proc_stat_list_set_flags (pp, needs);
return !proc_stat_list_for_each (pp, interesting);
}
}