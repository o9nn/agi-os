#include "std.h"
#include "malloc_.h"
#include <pthread.h>
#include "gserror.h"
#include "gserrors.h"
#include "gpsync.h"
#ifndef PTHREAD_CREATE_DETACHED
# define PTHREAD_CREATE_DETACHED 1
#endif
typedef struct pt_semaphore_t {
int count;
pthread_mutex_t mutex;
pthread_cond_t cond;
} pt_semaphore_t;
uint
gp_semaphore_sizeof(void)
{
return sizeof(pt_semaphore_t);
}
#define SEM_ERROR_CODE(scode)\
(scode != 0 ? gs_note_error(gs_error_ioerror) : 0)
int
gp_semaphore_open(gp_semaphore * sema)
{
pt_semaphore_t * const sem = (pt_semaphore_t *)sema;
int scode;
if (!sema)
return -1;
sem->count = 0;
scode = pthread_mutex_init(&sem->mutex, NULL);
if (scode == 0)
scode = pthread_cond_init(&sem->cond, NULL);
return SEM_ERROR_CODE(scode);
}
int
gp_semaphore_close(gp_semaphore * sema)
{
pt_semaphore_t * const sem = (pt_semaphore_t *)sema;
int scode, scode2;
scode = pthread_cond_destroy(&sem->cond);
scode2 = pthread_mutex_destroy(&sem->mutex);
if (scode == 0)
scode = scode2;
return SEM_ERROR_CODE(scode);
}
int
gp_semaphore_wait(gp_semaphore * sema)
{
pt_semaphore_t * const sem = (pt_semaphore_t *)sema;
int scode, scode2;
scode = pthread_mutex_lock(&sem->mutex);
if (scode != 0)
return SEM_ERROR_CODE(scode);
while (sem->count == 0) {
scode = pthread_cond_wait(&sem->cond, &sem->mutex);
if (scode != 0)
break;
}
if (scode == 0)
--sem->count;
scode2 = pthread_mutex_unlock(&sem->mutex);
if (scode == 0)
scode = scode2;
return SEM_ERROR_CODE(scode);
}
int
gp_semaphore_signal(gp_semaphore * sema)
{
pt_semaphore_t * const sem = (pt_semaphore_t *)sema;
int scode, scode2;
scode = pthread_mutex_lock(&sem->mutex);
if (scode != 0)
return SEM_ERROR_CODE(scode);
if (sem->count++ == 0)
scode = pthread_cond_signal(&sem->cond);
scode2 = pthread_mutex_unlock(&sem->mutex);
if (scode == 0)
scode = scode2;
return SEM_ERROR_CODE(scode);
}
uint
gp_monitor_sizeof(void)
{
return sizeof(pthread_mutex_t);
}
int
gp_monitor_open(gp_monitor * mona)
{
pthread_mutex_t * const mon = (pthread_mutex_t *)mona;
int scode;
if (!mona)
return -1;
scode = pthread_mutex_init(mon, NULL);
return SEM_ERROR_CODE(scode);
}
int
gp_monitor_close(gp_monitor * mona)
{
pthread_mutex_t * const mon = (pthread_mutex_t *)mona;
int scode;
scode = pthread_mutex_destroy(mon);
return SEM_ERROR_CODE(scode);
}
int
gp_monitor_enter(gp_monitor * mona)
{
pthread_mutex_t * const mon = (pthread_mutex_t *)mona;
int scode;
scode = pthread_mutex_lock(mon);
return SEM_ERROR_CODE(scode);
}
int
gp_monitor_leave(gp_monitor * mona)
{
pthread_mutex_t * const mon = (pthread_mutex_t *)mona;
int scode;
scode = pthread_mutex_unlock(mon);
return SEM_ERROR_CODE(scode);
}
typedef struct gp_thread_creation_closure_s {
gp_thread_creation_callback_t proc;
void *proc_data;
} gp_thread_creation_closure_t;
private void *
gp_thread_begin_wrapper(void *thread_data )
{
gp_thread_creation_closure_t closure;
closure = *(gp_thread_creation_closure_t *)thread_data;
free(thread_data);
DISCARD(closure.proc(closure.proc_data));
return NULL;
}
int
gp_create_thread(gp_thread_creation_callback_t proc, void *proc_data)
{
gp_thread_creation_closure_t *closure =
(gp_thread_creation_closure_t *)malloc(sizeof(*closure));
pthread_t ignore_thread;
pthread_attr_t attr;
int code;
if (!closure)
return_error(gs_error_VMerror);
closure->proc = proc;
closure->proc_data = proc_data;
pthread_attr_init(&attr);
pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
code = pthread_create(&ignore_thread, &attr, gp_thread_begin_wrapper,
closure);
if (code) {
free(closure);
return_error(gs_error_ioerror);
}
return 0;
}