#ifndef _I386AT_IDT_
#define _I386AT_IDT_
#define IDTSZ (0x100)
#define PIC_INT_BASE 0x20
#define IOAPIC_INT_BASE 0x30
#define IOAPIC_SPURIOUS_BASE 0xff
#define CALL_AST_CHECK 0xfa
#define CALL_PMAP_UPDATE 0xfb
#include <i386/idt-gen.h>
#ifndef __ASSEMBLER__
extern void idt_init (void);
extern void ap_idt_init (int cpu);
#endif
#endif