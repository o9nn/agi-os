#ifndef _KERN_XPR_H_
#define _KERN_XPR_H_
#ifndef KERNEL
#include <sys/features.h>
#endif
#include <machine/xpr.h>
#if XPR_DEBUG
#define XPR(flags,xprargs) if(xprflags&flags) xpr xprargs
extern int xprflags;
#define XPR_SYSCALLS 0x00000001
#define XPR_TRAPS 0x00000002
#define XPR_SCHED 0x00000004
#define XPR_NPTCP 0x00000008
#define XPR_NP 0x00000010
#define XPR_TCP 0x00000020
#define XPR_VM_OBJECT (1 << 8)
#define XPR_VM_OBJECT_CACHE (1 << 9)
#define XPR_VM_PAGE (1 << 10)
#define XPR_VM_PAGEOUT (1 << 11)
#define XPR_MEMORY_OBJECT (1 << 12)
#define XPR_VM_FAULT (1 << 13)
#define XPR_INODE_PAGER (1 << 14)
#define XPR_INODE_PAGER_DATA (1 << 15)
#else
#define XPR(flags,xprargs)
#endif
struct xprbuf {
char *msg;
int arg1,arg2,arg3,arg4,arg5;
int timestamp;
int cpuinfo;
};
extern void xpr(char *, int, int, int, int, int);
extern void xpr_dump(struct xprbuf *, int);
extern void xprinit(void);
extern void xprbootstrap(void);
#endif