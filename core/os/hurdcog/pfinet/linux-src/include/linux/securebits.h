#ifndef _LINUX_SECUREBITS_H
#define _LINUX_SECUREBITS_H 1
#define SECUREBITS_DEFAULT 0x00000000
extern unsigned securebits;
#define SECURE_NOROOT 0
#define SECURE_NO_SETUID_FIXUP 2
#define issecure(X) ( (1 << (X+1)) & SECUREBITS_DEFAULT ? \
(1 << (X)) & SECUREBITS_DEFAULT : \
(1 << (X)) & securebits )
#endif