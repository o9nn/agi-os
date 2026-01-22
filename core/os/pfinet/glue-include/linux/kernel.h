#ifndef _HACK_KERNEL_H
#define _HACK_KERNEL_H
#include <stdio.h>
#include <stdlib.h>
#include <assert-backtrace.h>
#define barrier()	((void)0)
#define NORET_TYPE
#define ATTRIB_NORET	__attribute__((noreturn))
#define NORET_AND	noreturn,
#define FASTCALL(x)	x
#define	KERN_EMERG
#define	KERN_ALERT
#define	KERN_CRIT
#define	KERN_ERR
#define	KERN_WARNING
#define	KERN_NOTICE
#define	KERN_INFO
#define	KERN_DEBUG
#define panic(str...)	(printk (str), assert_backtrace (!"panic"))
#define NIPQUAD(addr) \
((unsigned char *)&addr)[0], \
((unsigned char *)&addr)[1], \
((unsigned char *)&addr)[2], \
((unsigned char *)&addr)[3]
#include <linux/sched.h>
#include <linux/bitops.h>
#define printk printf
static inline int
getname (const char *name, char **newp)
{
*newp = malloc (strlen (name) + 1);
strcpy (*newp, name);
return 0;
}
static inline void
putname (char *p)
{
free (p);
}
static inline int
kill_proc (int pid, int signo, int priv)
{
assert_backtrace (signo == SIGURG);
return 0;
}
static inline int
kill_pg (int pgrp, int signo, int priv)
{
assert_backtrace (signo == SIGURG);
return 0;
}
#endif