#ifndef _GLIBCXX_GCC_GTHR_POSIX_H
#define _GLIBCXX_GCC_GTHR_POSIX_H
#define __GTHREADS 1
#define __GTHREADS_CXX0X 1
#include <pthread.h>
#if ((defined(_LIBOBJC) || defined(_LIBOBJC_WEAK)) \
|| !defined(_GTHREAD_USE_MUTEX_TIMEDLOCK))
# include <unistd.h>
# if defined(_POSIX_TIMEOUTS) && _POSIX_TIMEOUTS >= 0
#  define _GTHREAD_USE_MUTEX_TIMEDLOCK 1
# else
#  define _GTHREAD_USE_MUTEX_TIMEDLOCK 0
# endif
#endif
typedef pthread_t __gthread_t;
typedef pthread_key_t __gthread_key_t;
typedef pthread_once_t __gthread_once_t;
typedef pthread_mutex_t __gthread_mutex_t;
typedef pthread_mutex_t __gthread_recursive_mutex_t;
typedef pthread_cond_t __gthread_cond_t;
typedef struct timespec __gthread_time_t;
#define __GTHREAD_HAS_COND	1
#define __GTHREAD_MUTEX_INIT PTHREAD_MUTEX_INITIALIZER
#define __GTHREAD_MUTEX_INIT_FUNCTION __gthread_mutex_init_function
#define __GTHREAD_ONCE_INIT PTHREAD_ONCE_INIT
#if defined(PTHREAD_RECURSIVE_MUTEX_INITIALIZER)
#define __GTHREAD_RECURSIVE_MUTEX_INIT PTHREAD_RECURSIVE_MUTEX_INITIALIZER
#elif defined(PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP)
#define __GTHREAD_RECURSIVE_MUTEX_INIT PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP
#else
#define __GTHREAD_RECURSIVE_MUTEX_INIT_FUNCTION __gthread_recursive_mutex_init_function
#endif
#define __GTHREAD_COND_INIT PTHREAD_COND_INITIALIZER
#define __GTHREAD_TIME_INIT {0,0}
#ifdef _GTHREAD_USE_MUTEX_INIT_FUNC
# undef __GTHREAD_MUTEX_INIT
#endif
#ifdef _GTHREAD_USE_RECURSIVE_MUTEX_INIT_FUNC
# undef __GTHREAD_RECURSIVE_MUTEX_INIT
# undef __GTHREAD_RECURSIVE_MUTEX_INIT_FUNCTION
# define __GTHREAD_RECURSIVE_MUTEX_INIT_FUNCTION __gthread_recursive_mutex_init_function
#endif
#ifdef _GTHREAD_USE_COND_INIT_FUNC
# undef __GTHREAD_COND_INIT
# define __GTHREAD_COND_INIT_FUNCTION __gthread_cond_init_function
#endif
#if __GXX_WEAK__ && _GLIBCXX_GTHREAD_USE_WEAK
# ifndef __gthrw_pragma
#  define __gthrw_pragma(pragma)
# endif
# define __gthrw2(name,name2,type) \
static __typeof(type) name \
__attribute__ ((__weakref__(#name2), __copy__ (type))); \
__gthrw_pragma(weak type)
# define __gthrw_(name) __gthrw_ ## name
#else
# define __gthrw2(name,name2,type)
# define __gthrw_(name) name
#endif
#define __gthrw(name) __gthrw2(__gthrw_ ## name,name,name)
__gthrw(pthread_once)
__gthrw(pthread_getspecific)
__gthrw(pthread_setspecific)
__gthrw(pthread_create)
__gthrw(pthread_join)
__gthrw(pthread_equal)
__gthrw(pthread_self)
__gthrw(pthread_detach)
#ifndef __BIONIC__
__gthrw(pthread_cancel)
#endif
__gthrw(sched_yield)
__gthrw(pthread_mutex_lock)
__gthrw(pthread_mutex_trylock)
#if _GTHREAD_USE_MUTEX_TIMEDLOCK
__gthrw(pthread_mutex_timedlock)
#endif
__gthrw(pthread_mutex_unlock)
__gthrw(pthread_mutex_init)
__gthrw(pthread_mutex_destroy)
__gthrw(pthread_cond_init)
__gthrw(pthread_cond_broadcast)
__gthrw(pthread_cond_signal)
__gthrw(pthread_cond_wait)
__gthrw(pthread_cond_timedwait)
__gthrw(pthread_cond_destroy)
__gthrw(pthread_key_create)
__gthrw(pthread_key_delete)
__gthrw(pthread_mutexattr_init)
__gthrw(pthread_mutexattr_settype)
__gthrw(pthread_mutexattr_destroy)
#if defined(_LIBOBJC) || defined(_LIBOBJC_WEAK)
__gthrw(pthread_exit)
#ifdef _POSIX_PRIORITY_SCHEDULING
#ifdef _POSIX_THREAD_PRIORITY_SCHEDULING
__gthrw(sched_get_priority_max)
__gthrw(sched_get_priority_min)
#endif
#endif
__gthrw(pthread_attr_destroy)
__gthrw(pthread_attr_init)
__gthrw(pthread_attr_setdetachstate)
#ifdef _POSIX_THREAD_PRIORITY_SCHEDULING
__gthrw(pthread_getschedparam)
__gthrw(pthread_setschedparam)
#endif
#endif
#if __GXX_WEAK__ && _GLIBCXX_GTHREAD_USE_WEAK
#if defined(__FreeBSD__) || (defined(__sun) && defined(__svr4__))
static volatile int __gthread_active = -1;
static void
__gthread_trigger (void)
{
__gthread_active = 1;
}
static inline int
__gthread_active_p (void)
{
static pthread_mutex_t __gthread_active_mutex = PTHREAD_MUTEX_INITIALIZER;
static pthread_once_t __gthread_active_once = PTHREAD_ONCE_INIT;
int __gthread_active_latest_value = __gthread_active;
if (__builtin_expect (__gthread_active_latest_value < 0, 0))
{
if (__gthrw_(pthread_once))
{
__gthrw_(pthread_mutex_lock) (&__gthread_active_mutex);
__gthrw_(pthread_once) (&__gthread_active_once, __gthread_trigger);
__gthrw_(pthread_mutex_unlock) (&__gthread_active_mutex);
}
if (__gthread_active < 0)
__gthread_active = 0;
__gthread_active_latest_value = __gthread_active;
}
return __gthread_active_latest_value != 0;
}
#else
#ifdef __GLIBC__
__gthrw2(__gthrw_(__pthread_key_create),
__pthread_key_create,
pthread_key_create)
# define GTHR_ACTIVE_PROXY	__gthrw_(__pthread_key_create)
#elif defined (__BIONIC__)
# define GTHR_ACTIVE_PROXY	__gthrw_(pthread_create)
#else
# define GTHR_ACTIVE_PROXY	__gthrw_(pthread_cancel)
#endif
static inline int
__gthread_active_p (void)
{
static void *const __gthread_active_ptr
= __extension__ (void *) &GTHR_ACTIVE_PROXY;
return __gthread_active_ptr != 0;
}
#endif
#else
#if defined(__hppa__) && defined(__hpux__)
static volatile int __gthread_active = -1;
static inline int
__gthread_active_p (void)
{
int __gthread_active_latest_value = __gthread_active;
size_t __s;
if (__builtin_expect (__gthread_active_latest_value < 0, 0))
{
pthread_default_stacksize_np (0, &__s);
__gthread_active = __s ? 1 : 0;
__gthread_active_latest_value = __gthread_active;
}
return __gthread_active_latest_value != 0;
}
#else
static inline int
__gthread_active_p (void)
{
return 1;
}
#endif
#endif
#ifdef _LIBOBJC
#include <config.h>
#ifdef HAVE_SCHED_H
# include <sched.h>
#endif
static pthread_key_t _objc_thread_storage;
static pthread_attr_t _objc_thread_attribs;
static void *thread_local_storage = NULL;
static inline int
__gthread_objc_init_thread_system (void)
{
if (__gthread_active_p ())
{
if (__gthrw_(pthread_key_create) (&_objc_thread_storage, NULL) == 0)
{
if (__gthrw_(pthread_attr_init) (&_objc_thread_attribs) == 0
&& __gthrw_(pthread_attr_setdetachstate) (&_objc_thread_attribs,
PTHREAD_CREATE_DETACHED) == 0)
return 0;
}
}
return -1;
}
static inline int
__gthread_objc_close_thread_system (void)
{
if (__gthread_active_p ()
&& __gthrw_(pthread_key_delete) (_objc_thread_storage) == 0
&& __gthrw_(pthread_attr_destroy) (&_objc_thread_attribs) == 0)
return 0;
return -1;
}
static inline objc_thread_t
__gthread_objc_thread_detach (void (*func)(void *), void *arg)
{
objc_thread_t thread_id;
pthread_t new_thread_handle;
if (!__gthread_active_p ())
return NULL;
if (!(__gthrw_(pthread_create) (&new_thread_handle, &_objc_thread_attribs,
(void *) func, arg)))
thread_id = (objc_thread_t) new_thread_handle;
else
thread_id = NULL;
return thread_id;
}
static inline int
__gthread_objc_thread_set_priority (int priority)
{
if (!__gthread_active_p ())
return -1;
else
{
#ifdef _POSIX_PRIORITY_SCHEDULING
#ifdef _POSIX_THREAD_PRIORITY_SCHEDULING
pthread_t thread_id = __gthrw_(pthread_self) ();
int policy;
struct sched_param params;
int priority_min, priority_max;
if (__gthrw_(pthread_getschedparam) (thread_id, &policy, &params) == 0)
{
if ((priority_max = __gthrw_(sched_get_priority_max) (policy)) == -1)
return -1;
if ((priority_min = __gthrw_(sched_get_priority_min) (policy)) == -1)
return -1;
if (priority > priority_max)
priority = priority_max;
else if (priority < priority_min)
priority = priority_min;
params.sched_priority = priority;
if (__gthrw_(pthread_setschedparam) (thread_id, policy, &params) == 0)
return 0;
}
#endif
#endif
return -1;
}
}
static inline int
__gthread_objc_thread_get_priority (void)
{
#ifdef _POSIX_PRIORITY_SCHEDULING
#ifdef _POSIX_THREAD_PRIORITY_SCHEDULING
if (__gthread_active_p ())
{
int policy;
struct sched_param params;
if (__gthrw_(pthread_getschedparam) (__gthrw_(pthread_self) (), &policy, &params) == 0)
return params.sched_priority;
else
return -1;
}
else
#endif
#endif
return OBJC_THREAD_INTERACTIVE_PRIORITY;
}
static inline void
__gthread_objc_thread_yield (void)
{
if (__gthread_active_p ())
__gthrw_(sched_yield) ();
}
static inline int
__gthread_objc_thread_exit (void)
{
if (__gthread_active_p ())
__gthrw_(pthread_exit) (&__objc_thread_exit_status);
return -1;
}
static inline objc_thread_t
__gthread_objc_thread_id (void)
{
if (__gthread_active_p ())
return (objc_thread_t) __gthrw_(pthread_self) ();
else
return (objc_thread_t) 1;
}
static inline int
__gthread_objc_thread_set_data (void *value)
{
if (__gthread_active_p ())
return __gthrw_(pthread_setspecific) (_objc_thread_storage, value);
else
{
thread_local_storage = value;
return 0;
}
}
static inline void *
__gthread_objc_thread_get_data (void)
{
if (__gthread_active_p ())
return __gthrw_(pthread_getspecific) (_objc_thread_storage);
else
return thread_local_storage;
}
static inline int
__gthread_objc_mutex_allocate (objc_mutex_t mutex)
{
if (__gthread_active_p ())
{
mutex->backend = objc_malloc (sizeof (pthread_mutex_t));
if (__gthrw_(pthread_mutex_init) ((pthread_mutex_t *) mutex->backend, NULL))
{
objc_free (mutex->backend);
mutex->backend = NULL;
return -1;
}
}
return 0;
}
static inline int
__gthread_objc_mutex_deallocate (objc_mutex_t mutex)
{
if (__gthread_active_p ())
{
int count;
do
{
count = __gthrw_(pthread_mutex_unlock) ((pthread_mutex_t *) mutex->backend);
if (count < 0)
return -1;
}
while (count);
if (__gthrw_(pthread_mutex_destroy) ((pthread_mutex_t *) mutex->backend))
return -1;
objc_free (mutex->backend);
mutex->backend = NULL;
}
return 0;
}
static inline int
__gthread_objc_mutex_lock (objc_mutex_t mutex)
{
if (__gthread_active_p ()
&& __gthrw_(pthread_mutex_lock) ((pthread_mutex_t *) mutex->backend) != 0)
{
return -1;
}
return 0;
}
static inline int
__gthread_objc_mutex_trylock (objc_mutex_t mutex)
{
if (__gthread_active_p ()
&& __gthrw_(pthread_mutex_trylock) ((pthread_mutex_t *) mutex->backend) != 0)
{
return -1;
}
return 0;
}
static inline int
__gthread_objc_mutex_unlock (objc_mutex_t mutex)
{
if (__gthread_active_p ()
&& __gthrw_(pthread_mutex_unlock) ((pthread_mutex_t *) mutex->backend) != 0)
{
return -1;
}
return 0;
}
static inline int
__gthread_objc_condition_allocate (objc_condition_t condition)
{
if (__gthread_active_p ())
{
condition->backend = objc_malloc (sizeof (pthread_cond_t));
if (__gthrw_(pthread_cond_init) ((pthread_cond_t *) condition->backend, NULL))
{
objc_free (condition->backend);
condition->backend = NULL;
return -1;
}
}
return 0;
}
static inline int
__gthread_objc_condition_deallocate (objc_condition_t condition)
{
if (__gthread_active_p ())
{
if (__gthrw_(pthread_cond_destroy) ((pthread_cond_t *) condition->backend))
return -1;
objc_free (condition->backend);
condition->backend = NULL;
}
return 0;
}
static inline int
__gthread_objc_condition_wait (objc_condition_t condition, objc_mutex_t mutex)
{
if (__gthread_active_p ())
return __gthrw_(pthread_cond_wait) ((pthread_cond_t *) condition->backend,
(pthread_mutex_t *) mutex->backend);
else
return 0;
}
static inline int
__gthread_objc_condition_broadcast (objc_condition_t condition)
{
if (__gthread_active_p ())
return __gthrw_(pthread_cond_broadcast) ((pthread_cond_t *) condition->backend);
else
return 0;
}
static inline int
__gthread_objc_condition_signal (objc_condition_t condition)
{
if (__gthread_active_p ())
return __gthrw_(pthread_cond_signal) ((pthread_cond_t *) condition->backend);
else
return 0;
}
#else
static inline int
__gthread_create (__gthread_t *__threadid, void *(*__func) (void*),
void *__args)
{
return __gthrw_(pthread_create) (__threadid, NULL, __func, __args);
}
static inline int
__gthread_join (__gthread_t __threadid, void **__value_ptr)
{
return __gthrw_(pthread_join) (__threadid, __value_ptr);
}
static inline int
__gthread_detach (__gthread_t __threadid)
{
return __gthrw_(pthread_detach) (__threadid);
}
static inline int
__gthread_equal (__gthread_t __t1, __gthread_t __t2)
{
return __gthrw_(pthread_equal) (__t1, __t2);
}
static inline __gthread_t
__gthread_self (void)
{
return __gthrw_(pthread_self) ();
}
static inline int
__gthread_yield (void)
{
return __gthrw_(sched_yield) ();
}
static inline int
__gthread_once (__gthread_once_t *__once, void (*__func) (void))
{
if (__gthread_active_p ())
return __gthrw_(pthread_once) (__once, __func);
else
return -1;
}
static inline int
__gthread_key_create (__gthread_key_t *__key, void (*__dtor) (void *))
{
return __gthrw_(pthread_key_create) (__key, __dtor);
}
static inline int
__gthread_key_delete (__gthread_key_t __key)
{
return __gthrw_(pthread_key_delete) (__key);
}
static inline void *
__gthread_getspecific (__gthread_key_t __key)
{
return __gthrw_(pthread_getspecific) (__key);
}
static inline int
__gthread_setspecific (__gthread_key_t __key, const void *__ptr)
{
return __gthrw_(pthread_setspecific) (__key, __ptr);
}
static inline void
__gthread_mutex_init_function (__gthread_mutex_t *__mutex)
{
if (__gthread_active_p ())
__gthrw_(pthread_mutex_init) (__mutex, NULL);
}
static inline int
__gthread_mutex_destroy (__gthread_mutex_t *__mutex)
{
if (__gthread_active_p ())
return __gthrw_(pthread_mutex_destroy) (__mutex);
else
return 0;
}
static inline int
__gthread_mutex_lock (__gthread_mutex_t *__mutex)
{
if (__gthread_active_p ())
return __gthrw_(pthread_mutex_lock) (__mutex);
else
return 0;
}
static inline int
__gthread_mutex_trylock (__gthread_mutex_t *__mutex)
{
if (__gthread_active_p ())
return __gthrw_(pthread_mutex_trylock) (__mutex);
else
return 0;
}
#if _GTHREAD_USE_MUTEX_TIMEDLOCK
static inline int
__gthread_mutex_timedlock (__gthread_mutex_t *__mutex,
const __gthread_time_t *__abs_timeout)
{
if (__gthread_active_p ())
return __gthrw_(pthread_mutex_timedlock) (__mutex, __abs_timeout);
else
return 0;
}
#endif
static inline int
__gthread_mutex_unlock (__gthread_mutex_t *__mutex)
{
if (__gthread_active_p ())
return __gthrw_(pthread_mutex_unlock) (__mutex);
else
return 0;
}
#if !defined( PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP) \
|| defined(_GTHREAD_USE_RECURSIVE_MUTEX_INIT_FUNC)
static inline int
__gthread_recursive_mutex_init_function (__gthread_recursive_mutex_t *__mutex)
{
if (__gthread_active_p ())
{
pthread_mutexattr_t __attr;
int __r;
__r = __gthrw_(pthread_mutexattr_init) (&__attr);
if (!__r)
__r = __gthrw_(pthread_mutexattr_settype) (&__attr,
PTHREAD_MUTEX_RECURSIVE);
if (!__r)
__r = __gthrw_(pthread_mutex_init) (__mutex, &__attr);
if (!__r)
__r = __gthrw_(pthread_mutexattr_destroy) (&__attr);
return __r;
}
return 0;
}
#endif
static inline int
__gthread_recursive_mutex_lock (__gthread_recursive_mutex_t *__mutex)
{
return __gthread_mutex_lock (__mutex);
}
static inline int
__gthread_recursive_mutex_trylock (__gthread_recursive_mutex_t *__mutex)
{
return __gthread_mutex_trylock (__mutex);
}
#if _GTHREAD_USE_MUTEX_TIMEDLOCK
static inline int
__gthread_recursive_mutex_timedlock (__gthread_recursive_mutex_t *__mutex,
const __gthread_time_t *__abs_timeout)
{
return __gthread_mutex_timedlock (__mutex, __abs_timeout);
}
#endif
static inline int
__gthread_recursive_mutex_unlock (__gthread_recursive_mutex_t *__mutex)
{
return __gthread_mutex_unlock (__mutex);
}
static inline int
__gthread_recursive_mutex_destroy (__gthread_recursive_mutex_t *__mutex)
{
return __gthread_mutex_destroy (__mutex);
}
#ifdef _GTHREAD_USE_COND_INIT_FUNC
static inline void
__gthread_cond_init_function (__gthread_cond_t *__cond)
{
if (__gthread_active_p ())
__gthrw_(pthread_cond_init) (__cond, NULL);
}
#endif
static inline int
__gthread_cond_broadcast (__gthread_cond_t *__cond)
{
return __gthrw_(pthread_cond_broadcast) (__cond);
}
static inline int
__gthread_cond_signal (__gthread_cond_t *__cond)
{
return __gthrw_(pthread_cond_signal) (__cond);
}
static inline int
__gthread_cond_wait (__gthread_cond_t *__cond, __gthread_mutex_t *__mutex)
{
return __gthrw_(pthread_cond_wait) (__cond, __mutex);
}
static inline int
__gthread_cond_timedwait (__gthread_cond_t *__cond, __gthread_mutex_t *__mutex,
const __gthread_time_t *__abs_timeout)
{
return __gthrw_(pthread_cond_timedwait) (__cond, __mutex, __abs_timeout);
}
static inline int
__gthread_cond_wait_recursive (__gthread_cond_t *__cond,
__gthread_recursive_mutex_t *__mutex)
{
return __gthread_cond_wait (__cond, __mutex);
}
static inline int
__gthread_cond_destroy (__gthread_cond_t* __cond)
{
return __gthrw_(pthread_cond_destroy) (__cond);
}
#endif
#endif