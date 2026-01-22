#ifndef _KERN_KMUTEX_H_
#define _KERN_KMUTEX_H_   1
#include <kern/lock.h>
#include <mach/kern_return.h>
struct kmutex
{
unsigned int state;
decl_simple_lock_data (, lock)
};
#define KMUTEX_AVAIL       0
#define KMUTEX_LOCKED      1
#define KMUTEX_CONTENDED   2
extern void kmutex_init (struct kmutex *mtxp);
extern kern_return_t kmutex_lock (struct kmutex *mtxp,
boolean_t interruptible);
extern kern_return_t kmutex_trylock (struct kmutex *mtxp);
extern void kmutex_unlock (struct kmutex *mtxp);
#endif