#ifndef _ASMi386_SIGCONTEXT_H
#define _ASMi386_SIGCONTEXT_H
struct _fpreg {
unsigned short significand[4];
unsigned short exponent;
};
struct _fpstate {
unsigned long 	cw,
sw,
tag,
ipoff,
cssel,
dataoff,
datasel;
struct _fpreg	_st[8];
unsigned long	status;
};
struct sigcontext_struct {
unsigned short gs, __gsh;
unsigned short fs, __fsh;
unsigned short es, __esh;
unsigned short ds, __dsh;
unsigned long edi;
unsigned long esi;
unsigned long ebp;
unsigned long esp;
unsigned long ebx;
unsigned long edx;
unsigned long ecx;
unsigned long eax;
unsigned long trapno;
unsigned long err;
unsigned long eip;
unsigned short cs, __csh;
unsigned long eflags;
unsigned long esp_at_signal;
unsigned short ss, __ssh;
struct _fpstate * fpstate;
unsigned long oldmask;
unsigned long cr2;
};
#endif