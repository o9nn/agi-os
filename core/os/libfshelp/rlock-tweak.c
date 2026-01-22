#include "fshelp.h"
#include "rlock.h"
#include <assert.h>
#include <stdlib.h>
#include <errno.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/types.h>
#include <hurd.h>
#include <hurd/process.h>
static inline long overlap (loff_t start, loff_t len, struct rlock_list *l)
{
return ((len == 0 && l->len == 0)
|| (len == 0 && l->start + l->len > start)
|| (l->len == 0 && start + len > l->start)
|| (l->start + l->len > start && start + len > l->start));
}
error_t
fshelp_rlock_tweak (struct rlock_box *box, pthread_mutex_t *mutex,
struct rlock_peropen *po, int open_mode,
loff_t obj_size, loff_t cur_pointer, int cmd,
struct flock64 *lock, mach_port_t rendezvous)
{
inline struct rlock_list *
gen_lock (loff_t start, loff_t len, int type)
{
struct rlock_list *l = malloc (sizeof (struct rlock_list));
if (! l)
return NULL;
rlock_list_init (po, l);
l->start = start;
l->len = len;
l->type = type;
list_link (po, po->locks, l);
list_link (node, &box->locks, l);
return l;
}
inline void
rele_lock (struct rlock_list *l, int wake_waiters)
{
list_unlink (po, l);
list_unlink (node, l);
if (wake_waiters && l->waiting)
pthread_cond_broadcast (&l->wait);
pthread_cond_destroy(&l->wait);
free (l);
}
error_t
unlock_region (loff_t start, loff_t len)
{
struct rlock_list *l;
for (l = *po->locks; l; l = l->po.next)
{
if (l->len != 0 && l->start + l->len <= start)
{
continue;
}
else if (len != 0 && start + len <= l->start)
{
return 0;
}
else if (start <= l->start
&& (len == 0
|| (l->len != 0
&& l->start + l->len <= start + len)))
{
rele_lock (l, 1);
continue;
}
else if (start <= l->start
&& (l->len == 0
|| (l->start < start + len)))
{
assert (len != 0);
assert (l->len == 0 || start + len < l->start + l->len);
if (l->len != 0)
l->len -= start + len - l->start;
l->start = start + len;
if (l->waiting)
{
l->waiting = 0;
pthread_cond_broadcast (&l->wait);
}
}
else if (l->start < start
&& ((start < l->start + l->len
&& (len == 0 || l->start + l->len <= start + len))
|| (len == 0 && l->len == 0)))
{
assert (len == 0
|| (l->len != 0 && l->start + l->len <= start + len));
l->len = start - l->start;
if (l->waiting)
{
l->waiting = 0;
pthread_cond_broadcast (&l->wait);
}
continue;
}
else if (l->start < start
&& (l->len == 0
|| (len != 0
&& start + len < l->start + l->len)))
{
struct rlock_list *upper_half;
assert (len != 0);
upper_half = gen_lock (start + len,
l->len
? l->start + l->len - (start + len)
: 0,
l->type);
if (! upper_half)
return ENOMEM;
l->len = start - l->start;
return 0;
}
else if (start < l->start
&& len != 0
&& start + len <= l->start)
{
return 0;
}
else
assert (! "Impossible!");
}
return 0;
}
inline struct rlock_list *
find_conflict (loff_t start, loff_t len, int type)
{
struct rlock_list *l;
for (l = box->locks; l; l = l->node.next)
{
if (po->locks == l->po_id)
continue;
if ((l->type == F_WRLCK || type == F_WRLCK)
&& overlap (start, len, l))
return l;
}
return NULL;
}
inline error_t
merge_in (loff_t start, loff_t len, int type)
{
struct rlock_list *l;
for (l = *po->locks; l; l = l->po.next)
{
if (l->start <= start
&& (l->len == 0
|| (len != 0
&& start + len <= l->start + l->len)))
{
struct rlock_list *head = NULL;
struct rlock_list *tail = NULL;
if (type == l->type || type == F_RDLCK)
return 0;
assert (type == F_WRLCK && l->type == F_RDLCK);
if (l->start < start)
{
head = gen_lock (l->start, start - l->start, F_RDLCK);
if (! head)
return ENOMEM;
}
if ((l->len == 0 && len != 0)
|| start + len < l->start + l->len)
{
tail = gen_lock (start + len,
l->len
? l->start + l->len - (start + len)
: 0,
F_RDLCK);
if (! tail)
{
if (head)
rele_lock (head, 0);
return ENOMEM;
}
}
if (head)
{
loff_t shift = start - l->start;
if (l->len != 0)
l->len -= shift;
l->start += shift;
}
if (tail)
l->len = tail->start - l->start;
if (! tail)
{
start = l->start;
len = l->len;
rele_lock (l, 1);
continue;
}
else
{
l->type = F_WRLCK;
return 0;
}
}
else if (start <= l->start
&& (len == 0
|| (l->len != 0
&& l->start + l->len <= start + len)))
{
struct rlock_list *head;
if (type == l->type || type == F_WRLCK)
{
rele_lock (l, 1);
continue;
}
assert (type == F_RDLCK && l->type == F_WRLCK);
if (start < l->start)
{
head = gen_lock (start, l->start - start, F_RDLCK);
if (! head)
return ENOMEM;
}
else
head = NULL;
if (l->len != 0
&& (len == 0 || l->start + l->len < start + len))
{
if (len != 0)
len = start + len - (l->start + l->len);
start = l->start + l->len;
continue;
}
else
{
assert (head);
return 0;
}
}
else if (l->start < start && start <= l->start + l->len
&& (len == 0 || start + len > l->start + l->len))
{
assert (l->len != 0);
if (type == l->type)
{
if (len != 0)
len += start - l->start;
start = l->start;
rele_lock (l, 1);
continue;
}
else if (start == l->start + l->len)
{
continue;
}
else if (type == F_WRLCK)
{
assert (l->type == F_RDLCK);
l->len -= l->start + l->len - start;
continue;
}
else
{
loff_t common = l->start + l->len - start;
assert (type == F_RDLCK);
assert (l->type == F_WRLCK);
start += common;
if (len != 0)
len -= common;
continue;
}
}
else if (start < l->start
&& (l->len == 0
|| l->start <= start + len))
{
assert (len != 0);
assert (l->len == 0 || start + len < l->start + l->len);
if (type == l->type)
{
if (l->len)
l->len += l->start - start;
l->start = start;
return 0;
}
else if (l->start == start + len)
{
continue;
}
else if (type == F_WRLCK)
{
struct rlock_list *e;
loff_t common = start + len - l->start;
assert (l->type == F_RDLCK);
e = gen_lock (start, len, F_WRLCK);
if (! e)
return ENOMEM;
if (l->len)
l->len -= common;
l->start += common;
return 0;
}
else
{
struct rlock_list *e;
assert (l->type == F_WRLCK);
assert (type == F_RDLCK);
e = gen_lock (start, l->start - start, F_RDLCK);
if (! e)
return ENOMEM;
return 0;
}
}
else if (start < l->start
&& len != 0
&& start + len <= l->start)
{
break;
}
else
{
assert (start >= l->start + l->len);
assert (l->len != 0);
continue;
}
}
return (gen_lock (start, len, type) ? 0 : ENOMEM);
}
struct rlock_list *e;
loff_t start;
loff_t len;
if (rendezvous != MACH_PORT_NULL)
return EOPNOTSUPP;
if (cmd != F_GETLK64
&& cmd != F_SETLK64
&& cmd != F_SETLKW64)
return EOPNOTSUPP;
if (lock->l_type != F_UNLCK
&& lock->l_type != F_RDLCK
&& lock->l_type != F_WRLCK)
return EINVAL;
if (lock->l_type == F_UNLCK)
{
if (cmd == F_SETLKW64)
cmd = F_SETLK64;
else if (cmd == F_GETLK64)
return EINVAL;
}
if ((cmd == F_SETLK64 || cmd == F_SETLKW64 )
&& lock->l_type == F_WRLCK && !(open_mode & O_WRITE))
return EBADF;
switch (lock->l_whence)
{
case SEEK_SET:
start = lock->l_start;
break;
case SEEK_CUR:
start = cur_pointer + lock->l_start;
break;
case SEEK_END:
start = obj_size + lock->l_start;
break;
default:
return EINVAL;
}
if (start < 0)
return EINVAL;
len = lock->l_len;
if (len < 0)
return EINVAL;
if (cmd == F_SETLK64 && lock->l_type == F_UNLCK)
return unlock_region (start, len);
retry:
e = find_conflict (start, len, lock->l_type);
if (cmd == F_GETLK64)
{
if (e)
{
lock->l_type = e->type;
lock->l_start = e->start;
lock->l_whence = SEEK_SET;
lock->l_len = e->len;
lock->l_pid = -1;
return 0;
}
else
{
lock->l_type = F_UNLCK;
return 0;
}
}
else
{
assert (cmd == F_SETLK64 || cmd == F_SETLKW64);
if (! e)
return merge_in (start, len, lock->l_type);
else
{
if (cmd == F_SETLKW64)
{
e->waiting = 1;
if (pthread_hurd_cond_wait_np (&e->wait, mutex))
return EINTR;
goto retry;
}
else
return EAGAIN;
}
}
}