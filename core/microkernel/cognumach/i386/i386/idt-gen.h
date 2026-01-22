#ifndef _I386_IDT_
#define _I386_IDT_
#include <mach/vm_param.h>
#include "seg.h"
#ifndef IDTSZ
#error you need to define IDTSZ
#endif
extern struct real_gate idt[IDTSZ];
#define fill_idt_gate(_idt, int_num, entry, selector, access, dword_count) \
fill_gate(&_idt[int_num], entry, selector, access, dword_count)
#endif