#ifndef _I386_USER_LDT_H_
#define _I386_USER_LDT_H_
#include <i386/seg.h>
struct user_ldt {
#ifdef MACH_PV_DESCRIPTORS
vm_offset_t alloc;
#endif
struct real_descriptor desc;
struct real_descriptor ldt[1];
};
typedef struct user_ldt * user_ldt_t;
extern void
user_ldt_free(user_ldt_t user_ldt);
#endif