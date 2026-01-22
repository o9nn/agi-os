#ifndef _SYS_IOCCOM_H_
#define _SYS_IOCCOM_H_
#define IOCPARM_MASK 0x1fff
#define IOCPARM_SHIFT 16
#define IOCGROUP_SHIFT 8
#define IOCPARM_LEN(x) (((x) >> IOCPARM_SHIFT) & IOCPARM_MASK)
#define IOCBASECMD(x) ((x) & ~(IOCPARM_MASK << IOCPARM_SHIFT))
#define IOCGROUP(x) (((x) >> IOCGROUP_SHIFT) & 0xff)
#define IOCPARM_MAX NBPG
#define IOC_VOID (unsigned long)0x20000000
#define IOC_OUT (unsigned long)0x40000000
#define IOC_IN (unsigned long)0x80000000
#define IOC_INOUT (IOC_IN|IOC_OUT)
#define IOC_DIRMASK (unsigned long)0xe0000000
#define _IOC(inout, group, num, len) \
((inout) | (((len) & IOCPARM_MASK) << IOCPARM_SHIFT) | \
((group) << IOCGROUP_SHIFT) | (num))
#define _IO(g,n) _IOC(IOC_VOID, (g), (n), 0)
#define _IOR(g,n,t) _IOC(IOC_OUT, (g), (n), sizeof(t))
#define _IOW(g,n,t) _IOC(IOC_IN, (g), (n), sizeof(t))
#define _IOWR(g,n,t) _IOC(IOC_INOUT, (g), (n), sizeof(t))
#define IOCSNPRINTF(buf, len, cmd) \
snprintf((buf), (len), "_IO%s%s('%c', %hhu)", \
(((cmd) >> 30) & 1) ? "W" : "", \
(((cmd) >> 30) & 2) ? "R" : "", \
(char)IOCGROUP(cmd), (unsigned char)(cmd))
#endif