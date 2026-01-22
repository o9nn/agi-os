#ifndef	_HURD_PORTS_DEREF_DEFERRED_
#define	_HURD_PORTS_DEREF_DEFERRED_
#include <pthread.h>
struct pi_list;
struct ports_threadpool
{
pthread_spinlock_t lock;
unsigned int color;
size_t old_threads;
struct pi_list *old_objects;
size_t young_threads;
struct pi_list *young_objects;
};
struct ports_thread
{
unsigned int color;
};
void _ports_threadpool_init (struct ports_threadpool *);
void _ports_thread_online (struct ports_threadpool *, struct ports_thread *);
void _ports_thread_quiescent (struct ports_threadpool *, struct ports_thread *);
void _ports_thread_offline (struct ports_threadpool *, struct ports_thread *);
struct port_info;
void _ports_port_deref_deferred (struct port_info *);
#endif