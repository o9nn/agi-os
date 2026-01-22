#ifndef _LINUX_DEVPTS_FS_H
#define _LINUX_DEVPTS_FS_H 1
#include <linux/config.h>
#include <linux/kdev_t.h>
#include <linux/tty.h>
#ifdef CONFIG_DEVPTS_FS
void devpts_pty_new(int, kdev_t);
void devpts_pty_kill(int);
#define unix98_max_ptys NR_PTYS * UNIX98_NR_MAJORS;
#elif defined(CONFIG_DEVPTS_FS_MODULE)
#ifdef BUILDING_PTY_C
void (*devpts_upcall_new)(int,kdev_t) = NULL;
void (*devpts_upcall_kill)(int) = NULL;
unsigned int unix98_max_ptys = NR_PTYS * UNIX98_NR_MAJORS;
EXPORT_SYMBOL(devpts_upcall_new);
EXPORT_SYMBOL(devpts_upcall_kill);
EXPORT_SYMBOL(unix98_max_ptys);
#else
extern void (*devpts_upcall_new)(int,kdev_t);
extern void (*devpts_upcall_kill)(int);
extern unsigned int unix98_max_ptys;
#endif
#ifndef BUILDING_DEVPTS
extern inline void
devpts_pty_new(int line, kdev_t device)
{
if ( devpts_upcall_new )
return devpts_upcall_new(line,device);
}
extern inline void
devpts_pty_kill(int line)
{
if ( devpts_upcall_kill )
return devpts_upcall_kill(line);
}
#endif
#else
extern inline void
devpts_pty_new(int line, kdev_t device) { }
extern inline void
devpts_pty_kill(int line) { }
#endif
#endif