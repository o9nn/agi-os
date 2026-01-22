#ifndef _I386_GDT_
#define _I386_GDT_
#include "seg.h"
#define	KERNEL_CS	(0x08 | KERNEL_RING)
#define	KERNEL_DS	(0x10 | KERNEL_RING)
#ifndef	MACH_PV_DESCRIPTORS
#define	KERNEL_LDT	0x18
#endif
#ifdef __x86_64__
#define	KERNEL_TSS	0x40
#else
#define	KERNEL_TSS	0x20
#endif
#define	USER_LDT	0x28
#ifdef __x86_64__
#define	USER_TSS	0x58
#else
#define	USER_TSS	0x30
#endif
#ifndef	MACH_PV_DESCRIPTORS
#define	LINEAR_DS	0x38
#endif
#define	USER_GDT	0x48
#define	USER_GDT_SLOTS	2
#define PERCPU_DS	0x68
#define	GDTSZ		sel_idx(0x70)
#ifndef __ASSEMBLER__
extern struct real_descriptor gdt[GDTSZ];
#define _fill_gdt_descriptor(_gdt, segment, base, limit, access, sizebits) \
fill_descriptor(&_gdt[sel_idx(segment)], base, limit, access, sizebits)
#define fill_gdt_descriptor(segment, base, limit, access, sizebits) \
_fill_gdt_descriptor(gdt, segment, base, limit, access, sizebits)
#ifdef __x86_64__
#define _fill_gdt_descriptor64(_gdt, segment, base, limit, access, sizebits) \
fill_descriptor64((struct real_descriptor64 *) &_gdt[sel_idx(segment)], base, limit, access, sizebits)
#define fill_gdt_descriptor64(segment, base, limit, access, sizebits) \
_fill_gdt_descriptor64(gdt, segment, base, limit, access, sizebits)
#endif
#ifdef __x86_64__
#define _fill_gdt_sys_descriptor(_gdt, segment, base, limit, access, sizebits) \
_fill_gdt_descriptor64(_gdt, segment, base, limit, access, sizebits)
#define fill_gdt_sys_descriptor(segment, base, limit, access, sizebits) \
fill_gdt_descriptor64(segment, base, limit, access, sizebits)
#else
#define _fill_gdt_sys_descriptor(_gdt, segment, base, limit, access, sizebits) \
_fill_gdt_descriptor(_gdt, segment, base, limit, access, sizebits)
#define fill_gdt_sys_descriptor(segment, base, limit, access, sizebits) \
fill_gdt_descriptor(segment, base, limit, access, sizebits)
#endif
extern void gdt_init(void);
extern void ap_gdt_init(int cpu);
#endif
#endif