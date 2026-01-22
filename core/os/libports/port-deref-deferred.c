#include <assert-backtrace.h>
#include <pthread.h>
#include "ports.h"
#define COLOR_BLACK	0
#define COLOR_WHITE	1
#define COLOR_INVALID	~0U
static inline int
valid_color (unsigned int c)
{
return c == COLOR_BLACK || c == COLOR_WHITE;
}
static inline unsigned int
flip_color (unsigned int c)
{
assert_backtrace (valid_color (c));
return ! c;
}
void
_ports_threadpool_init (struct ports_threadpool *pool)
{
pthread_spin_init (&pool->lock, PTHREAD_PROCESS_PRIVATE);
pool->color = COLOR_BLACK;
pool->old_threads = 0;
pool->old_objects = NULL;
pool->young_threads = 0;
pool->young_objects = NULL;
}
static inline void
flip_generations (struct ports_threadpool *pool)
{
assert_backtrace (pool->old_threads == 0);
assert_backtrace (pool->old_objects == NULL);
pool->old_threads = pool->young_threads;
pool->old_objects = pool->young_objects;
pool->young_threads = 0;
pool->young_objects = NULL;
pool->color = flip_color (pool->color);
}
void
_ports_thread_online (struct ports_threadpool *pool,
struct ports_thread *thread)
{
pthread_spin_lock (&pool->lock);
thread->color = flip_color (pool->color);
pool->young_threads += 1;
pthread_spin_unlock (&pool->lock);
}
struct pi_list
{
struct pi_list *next;
struct port_info *pi;
};
void
_ports_thread_quiescent (struct ports_threadpool *pool,
struct ports_thread *thread)
{
struct pi_list *free_list = NULL, *p;
assert_backtrace (valid_color (thread->color));
pthread_spin_lock (&pool->lock);
if (thread->color == pool->color)
{
pool->old_threads -= 1;
pool->young_threads += 1;
thread->color = flip_color (thread->color);
if (pool->old_threads == 0)
{
free_list = pool->old_objects;
pool->old_objects = NULL;
flip_generations (pool);
}
}
pthread_spin_unlock (&pool->lock);
for (p = free_list; p;)
{
struct pi_list *old = p;
p = p->next;
ports_port_deref (old->pi);
free (old);
}
}
void
_ports_thread_offline (struct ports_threadpool *pool,
struct ports_thread *thread)
{
assert_backtrace (valid_color (thread->color));
retry:
pthread_spin_lock (&pool->lock);
if (thread->color == pool->color)
{
pthread_spin_unlock (&pool->lock);
_ports_thread_quiescent (pool, thread);
goto retry;
}
thread->color = COLOR_INVALID;
pool->young_threads -= 1;
pthread_spin_unlock (&pool->lock);
}
void
_ports_port_deref_deferred (struct port_info *pi)
{
struct ports_threadpool *pool = &pi->bucket->threadpool;
struct pi_list *pl = malloc (sizeof *pl);
if (pl == NULL)
return;
pl->pi = pi;
pthread_spin_lock (&pool->lock);
pl->next = pool->young_objects;
pool->young_objects = pl;
if (pool->old_threads == 0)
{
assert_backtrace (pool->old_objects == NULL);
flip_generations (pool);
}
pthread_spin_unlock (&pool->lock);
}