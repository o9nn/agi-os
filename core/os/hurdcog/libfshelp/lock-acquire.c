#include <assert-backtrace.h>
#include <sys/file.h>
#include "fshelp.h"
#define EWOULDBLOCK EAGAIN
#ifndef __LOCK_ATOMIC
#define __LOCK_ATOMIC 16
#endif
error_t
fshelp_acquire_lock (struct lock_box *box, int *user, pthread_mutex_t *mut,
int flags)
{
int atomic = 0;
if (!(flags & (LOCK_UN | LOCK_EX | LOCK_SH)))
return 0;
if ((flags & LOCK_UN)
&& (flags & (LOCK_SH | LOCK_EX)))
return EINVAL;
if (flags & __LOCK_ATOMIC)
{
atomic = 1;
flags &= ~__LOCK_ATOMIC;
}
if (flags & LOCK_EX)
flags &= ~LOCK_SH;
if (flags & LOCK_UN)
{
if (*user & LOCK_UN)
return 0;
assert_backtrace (*user == box->type ||
(*user == LOCK_SH && box->type == (LOCK_SH | LOCK_EX)));
assert_backtrace (*user == LOCK_SH || *user == LOCK_EX ||
*user == (LOCK_SH | LOCK_EX));
if (*user == LOCK_SH)
{
if (!--box->shcount)
box->type = LOCK_UN;
}
else if (*user == LOCK_EX)
box->type = LOCK_UN;
if (box->type == LOCK_UN && box->waiting)
{
box->waiting = 0;
pthread_cond_broadcast (&box->wait);
}
if (box->type == (LOCK_SH | LOCK_EX) && box->shcount == 1 && box->waiting)
{
box->waiting = 0;
pthread_cond_broadcast (&box->wait);
}
*user = LOCK_UN;
}
else
{
if (atomic && *user == (flags & (LOCK_SH | LOCK_EX)))
return 0;
if (atomic && *user == LOCK_EX && flags & LOCK_SH)
{
*user = LOCK_SH;
box->type = LOCK_SH;
box->shcount = 1;
if (box->waiting)
{
box->waiting = 0;
pthread_cond_broadcast (&box->wait);
}
return 0;
}
if (*user == LOCK_SH && atomic && box->type == (LOCK_SH | LOCK_EX))
return EDEADLK;
if (*user == LOCK_EX && !atomic)
{
*user = LOCK_UN;
box->type = LOCK_UN;
if (box->waiting)
{
box->waiting = 0;
pthread_cond_broadcast (&box->wait);
}
}
if (*user == LOCK_SH && !atomic)
{
*user = LOCK_UN;
if (!--box->shcount)
{
box->type = LOCK_UN;
if (box->waiting)
{
box->waiting = 0;
pthread_cond_broadcast (&box->wait);
}
}
if (box->type == (LOCK_SH | LOCK_EX) && box->shcount == 1 &&
box->waiting)
{
box->waiting = 0;
pthread_cond_broadcast (&box->wait);
}
}
while (box->type & LOCK_EX)
{
if (flags & LOCK_NB)
return EWOULDBLOCK;
box->waiting = 1;
if (pthread_hurd_cond_wait_np (&box->wait, mut))
return EINTR;
}
assert_backtrace ((flags & LOCK_SH) || (flags & LOCK_EX));
if (flags & LOCK_SH)
{
assert_backtrace (!(box->type & LOCK_EX));
*user = LOCK_SH;
box->type = LOCK_SH;
box->shcount++;
}
else if (flags & LOCK_EX)
{
while ((*user == LOCK_SH && box->shcount > 1) ||
(*user == LOCK_UN && box->type != LOCK_UN))
{
if (flags & LOCK_NB)
return EWOULDBLOCK;
else
{
if (*user == LOCK_SH && atomic)
box->type = LOCK_SH | LOCK_EX;
box->waiting = 1;
if (pthread_hurd_cond_wait_np (&box->wait, mut))
return EINTR;
}
}
if (*user == LOCK_SH)
{
assert_backtrace (box->shcount == 1);
box->shcount = 0;
}
box->type = LOCK_EX;
*user = LOCK_EX;
}
}
return 0;
}