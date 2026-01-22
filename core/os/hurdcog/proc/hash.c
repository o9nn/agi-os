#include <mach.h>
#include <stddef.h>
#include <sys/types.h>
#include <hurd/hurd_types.h>
#include <string.h>
#include <stdlib.h>
#include <sys/resource.h>
#include "proc.h"
#include <hurd/ihash.h>
static struct hurd_ihash pghash
= HURD_IHASH_INITIALIZER (offsetof (struct pgrp, pg_hashloc));
static struct hurd_ihash pidhash
= HURD_IHASH_INITIALIZER (offsetof (struct proc, p_pidhashloc));
static struct hurd_ihash taskhash
= HURD_IHASH_INITIALIZER (offsetof (struct proc, p_taskhashloc));
static struct hurd_ihash sidhash
= HURD_IHASH_INITIALIZER (offsetof (struct session, s_hashloc));
struct proc *
pid_find (pid_t pid)
{
struct proc *p;
p = hurd_ihash_find (&pidhash, pid);
return (!p || p->p_dead) ? 0 : p;
}
struct proc *
pid_find_allow_zombie (pid_t pid)
{
return hurd_ihash_find (&pidhash, pid);
}
struct proc *
task_find (task_t task)
{
struct proc *p;
p = hurd_ihash_find (&taskhash, task) ? : add_tasks (task);
return (!p || p->p_dead) ? 0 : p;
}
struct proc *
task_find_nocreate (task_t task)
{
struct proc *p;
p = hurd_ihash_find (&taskhash, task);
return (!p || p->p_dead) ? 0 : p;
}
struct pgrp *
pgrp_find (pid_t pgid)
{
return hurd_ihash_find (&pghash, pgid);
}
struct session *
session_find (pid_t sid)
{
return hurd_ihash_find (&sidhash, sid);
}
void
add_proc_to_hash (struct proc *p)
{
hurd_ihash_add (&pidhash, p->p_pid, p);
hurd_ihash_add (&taskhash, p->p_task, p);
}
void
add_pgrp_to_hash (struct pgrp *pg)
{
hurd_ihash_add (&pghash, pg->pg_pgid, pg);
}
void
add_session_to_hash (struct session *s)
{
hurd_ihash_add (&sidhash, s->s_sid, s);
}
void
remove_pgrp_from_hash (struct pgrp *pg)
{
hurd_ihash_locp_remove (&pghash, pg->pg_hashloc);
}
void
remove_proc_from_hash (struct proc *p)
{
hurd_ihash_locp_remove (&pidhash, p->p_pidhashloc);
hurd_ihash_locp_remove (&taskhash, p->p_taskhashloc);
}
void
remove_session_from_hash (struct session *s)
{
hurd_ihash_locp_remove (&sidhash, s->s_hashloc);
}
void
prociterate (void (*fun) (struct proc *, void *), void *arg)
{
HURD_IHASH_ITERATE (&pidhash, value)
{
struct proc *p = value;
if (!p->p_dead)
(*fun)(p, arg);
}
}
int
pidfree (pid_t pid)
{
return (!pid_find_allow_zombie (pid)
&& !pgrp_find (pid) && !session_find (pid));
}