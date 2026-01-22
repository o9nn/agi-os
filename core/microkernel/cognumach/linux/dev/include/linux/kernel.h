#ifndef _LINUX_KERNEL_H
#define _LINUX_KERNEL_H
#ifdef __KERNEL__
#include <stdarg.h>
#include <linux/linkage.h>
#include <linux/compiler.h>
#define INT_MAX ((int)(~0U>>1))
#define UINT_MAX (~0U)
#define LONG_MAX ((long)(~0UL>>1))
#define ULONG_MAX (~0UL)
#define STACK_MAGIC 0xdeadbeef
#define KERN_EMERG "<0>"
#define KERN_ALERT "<1>"
#define KERN_CRIT "<2>"
#define KERN_ERR "<3>"
#define KERN_WARNING "<4>"
#define KERN_NOTICE "<5>"
#define KERN_INFO "<6>"
#define KERN_DEBUG "<7>"
# define NORET_TYPE
# define ATTRIB_NORET __attribute__((noreturn))
# define NORET_AND noreturn,
extern void math_error(void);
#include <kern/debug.h>
NORET_TYPE void do_exit(long error_code)
ATTRIB_NORET;
extern unsigned long simple_strtoul(const char *,char **,unsigned int);
extern int linux_sprintf(char *buf, const char *fmt, ...);
extern int linux_vsprintf(char *buf, const char *fmt, va_list args);
#ifndef MACH_INCLUDE
#define sprintf linux_sprintf
#define vsprintf linux_vsprintf
#endif
extern int session_of_pgrp(int pgrp);
extern int kill_proc(int pid, int sig, int priv);
extern int kill_pg(int pgrp, int sig, int priv);
extern int kill_sl(int sess, int sig, int priv);
asmlinkage int printk(const char * fmt, ...)
__attribute__ ((format (printf, 1, 2)));
#if DEBUG
#define pr_debug(fmt,arg...) \
printk(KERN_DEBUG fmt,##arg)
#else
#define pr_debug(fmt,arg...) \
do { } while (0)
#endif
#define pr_info(fmt,arg...) \
printk(KERN_INFO fmt,##arg)
#ifdef MACH
#define fsuser() 1
#else
#define fsuser() (current->fsuid == 0)
#endif
#define NIPQUAD(addr) \
(((addr) >> 0) & 0xff), \
(((addr) >> 8) & 0xff), \
(((addr) >> 16) & 0xff), \
(((addr) >> 24) & 0xff)
#endif
#define SI_LOAD_SHIFT 16
struct sysinfo {
long uptime;
unsigned long loads[3];
unsigned long totalram;
unsigned long freeram;
unsigned long sharedram;
unsigned long bufferram;
unsigned long totalswap;
unsigned long freeswap;
unsigned short procs;
char _f[22];
};
#endif