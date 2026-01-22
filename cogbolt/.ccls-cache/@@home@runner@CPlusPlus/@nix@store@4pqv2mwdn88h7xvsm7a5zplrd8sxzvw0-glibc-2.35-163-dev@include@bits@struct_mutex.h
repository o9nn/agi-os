#ifndef _THREAD_MUTEX_INTERNAL_H
#define _THREAD_MUTEX_INTERNAL_H 1
struct __pthread_mutex_s
{
int __lock;
unsigned int __count;
int __owner;
#ifdef __x86_64__
unsigned int __nusers;
#endif
int __kind;
#ifdef __x86_64__
short __spins;
short __elision;
__pthread_list_t __list;
# define __PTHREAD_MUTEX_HAVE_PREV 1
#else
unsigned int __nusers;
__extension__ union
{
struct
{
short __espins;
short __eelision;
# define __spins __elision_data.__espins
# define __elision __elision_data.__eelision
} __elision_data;
__pthread_slist_t __list;
};
# define __PTHREAD_MUTEX_HAVE_PREV 0
#endif
};
#ifdef __x86_64__
# define __PTHREAD_MUTEX_INITIALIZER(__kind) \
0, 0, 0, 0, __kind, 0, 0, { 0, 0 }
#else
# define __PTHREAD_MUTEX_INITIALIZER(__kind) \
0, 0, 0, __kind, 0, { { 0, 0 } }
#endif
#endif