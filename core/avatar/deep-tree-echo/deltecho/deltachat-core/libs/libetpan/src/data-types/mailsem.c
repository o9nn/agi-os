#ifdef HAVE_CONFIG_H
#	include <config.h>
#endif
#include "mailsem.h"
#include <stdlib.h>
#include <stdio.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <fcntl.h>
#ifndef _MSC_VER
#include <unistd.h>
#endif
#ifdef LIBETPAN_REENTRANT
#if defined(HAVE_PTHREAD_H) && !defined(IGNORE_PTHREAD_H)
#include <pthread.h>
#include <semaphore.h>
#elif (defined WIN32)
#include <windows.h>
#endif
#endif
struct mailsem_internal {
unsigned int count;
unsigned long waiters_count;
#ifdef LIBETPAN_REENTRANT
#if defined(HAVE_PTHREAD_H) && !defined(IGNORE_PTHREAD_H)
pthread_mutex_t lock;
pthread_cond_t count_nonzero;
#elif (defined WIN32)
HANDLE semaphore;
#endif
#endif
};
#if (defined(LIBETPAN_REENTRANT) && defined(HAVE_PTHREAD_H) && !defined(IGNORE_PTHREAD_H)) || !defined(LIBETPAN_REENTRANT)
static int mailsem_internal_init(struct mailsem_internal * s,
unsigned int initial_count)
{
#ifdef LIBETPAN_REENTRANT
int r;
r = pthread_mutex_init(&s->lock, NULL);
if (r != 0)
goto err;
r = pthread_cond_init(&s->count_nonzero, NULL);
if (r != 0)
goto destroy_mutex;
s->count = initial_count;
s->waiters_count = 0;
return 0;
destroy_mutex:
pthread_mutex_destroy(&s->lock);
err:
return -1;
#else
return -1;
#endif
}
static void mailsem_internal_destroy(struct mailsem_internal * s)
{
#ifdef LIBETPAN_REENTRANT
pthread_cond_destroy(&s->count_nonzero);
pthread_mutex_destroy(&s->lock);
#endif
}
int mailsem_internal_wait(struct mailsem_internal * s)
{
#ifdef LIBETPAN_REENTRANT
int r;
r = pthread_mutex_lock(&s->lock);
if (r != 0)
goto err;
s->waiters_count ++;
while (s->count == 0) {
r = pthread_cond_wait(&s->count_nonzero, &s->lock);
if (r != 0)
goto unlock;
}
s->waiters_count --;
s->count --;
pthread_mutex_unlock(&s->lock);
return 0;
unlock:
pthread_mutex_unlock(&s->lock);
err:
return -1;
#else
return -1;
#endif
}
static int mailsem_internal_post(struct mailsem_internal * s)
{
#ifdef LIBETPAN_REENTRANT
int r;
r = pthread_mutex_lock(&s->lock);
if (r != 0)
goto err;
if (s->waiters_count > 0) {
r = pthread_cond_signal(&s->count_nonzero);
if (r != 0)
goto unlock;
}
s->count ++;
pthread_mutex_unlock(&s->lock);
return 0;
unlock:
pthread_mutex_unlock(&s->lock);
err:
return -1;
#else
return -1;
#endif
}
#elif (defined WIN32)
static int mailsem_internal_init(struct mailsem_internal * s,
unsigned int initial_count)
{
s->semaphore = CreateSemaphore(
NULL,
initial_count,
0x7FFFFFFF,
NULL);
return s->semaphore == NULL ? -1 : 0;
}
static void mailsem_internal_destroy(struct mailsem_internal * s)
{
if (s->semaphore != NULL){
CloseHandle(s->semaphore);
}
}
int mailsem_internal_wait(struct mailsem_internal * s)
{
DWORD dwWaitResult = WAIT_TIMEOUT;
while (dwWaitResult != WAIT_OBJECT_0 && dwWaitResult != WAIT_FAILED){
dwWaitResult = WaitForSingleObject(
s->semaphore,
INFINITE);
}
return dwWaitResult == WAIT_FAILED ? -1 : 0;
}
static int mailsem_internal_post(struct mailsem_internal * s)
{
if (!ReleaseSemaphore(
s->semaphore,
1,
NULL) )
{
return -1;
}
return 0;
}
#endif
LIBETPAN_EXPORT
struct mailsem * mailsem_new(void)
{
struct mailsem * sem;
int r;
sem = malloc(sizeof(* sem));
if (sem == NULL)
goto err;
sem->sem_sem = malloc(sizeof(struct mailsem_internal));
if (sem->sem_sem == NULL)
goto free;
r = mailsem_internal_init(sem->sem_sem, 0);
if (r < 0)
goto free_sem;
return sem;
free_sem:
free(sem->sem_sem);
free:
free(sem);
err:
return NULL;
}
LIBETPAN_EXPORT
void mailsem_free(struct mailsem * sem)
{
mailsem_internal_destroy(sem->sem_sem);
free(sem->sem_sem);
free(sem);
}
LIBETPAN_EXPORT
int mailsem_up(struct mailsem * sem)
{
return mailsem_internal_post(sem->sem_sem);
}
LIBETPAN_EXPORT
int mailsem_down(struct mailsem * sem)
{
return mailsem_internal_wait(sem->sem_sem);
}