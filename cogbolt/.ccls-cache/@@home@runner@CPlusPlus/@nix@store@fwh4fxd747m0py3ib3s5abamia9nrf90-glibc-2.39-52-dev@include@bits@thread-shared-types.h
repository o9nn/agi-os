#ifndef _THREAD_SHARED_TYPES_H
#define _THREAD_SHARED_TYPES_H 1
#include <bits/pthreadtypes-arch.h>
#include <bits/atomic_wide_counter.h>
typedef struct __pthread_internal_list
{
struct __pthread_internal_list *__prev;
struct __pthread_internal_list *__next;
} __pthread_list_t;
typedef struct __pthread_internal_slist
{
struct __pthread_internal_slist *__next;
} __pthread_slist_t;
#include <bits/struct_mutex.h>
#include <bits/struct_rwlock.h>
struct __pthread_cond_s
{
__atomic_wide_counter __wseq;
__atomic_wide_counter __g1_start;
unsigned int __g_refs[2] __LOCK_ALIGNMENT;
unsigned int __g_size[2];
unsigned int __g1_orig_size;
unsigned int __wrefs;
unsigned int __g_signals[2];
};
typedef unsigned int __tss_t;
typedef unsigned long int __thrd_t;
typedef struct
{
int __data __ONCE_ALIGNMENT;
} __once_flag;
#define __ONCE_FLAG_INIT { 0 }
#endif