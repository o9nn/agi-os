#ifndef _I386_LDT_
#define _I386_LDT_
#include "seg.h"
#define USER_SCALL 0x07
#if defined(__x86_64__) && ! defined(USER32)
#define USER_CS 0x1f
#define USER_DS 0x17
#else
#define USER_CS 0x17
#define USER_DS 0x1f
#endif
#define LDTSZ 4
#ifndef __ASSEMBLER__
extern struct real_descriptor ldt[LDTSZ];
#define fill_ldt_descriptor(_ldt, selector, base, limit, access, sizebits) \
fill_descriptor(&_ldt[sel_idx(selector)], base, limit, access, sizebits)
#define fill_ldt_gate(_ldt, selector, offset, dest_selector, access, word_count) \
fill_gate((struct real_gate*)&_ldt[sel_idx(selector)], \
offset, dest_selector, access, word_count)
void ldt_init(void);
void ap_ldt_init(int cpu);
#endif
#endif