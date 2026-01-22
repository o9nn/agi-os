#ifndef _MACH_MACH_I386_TYPES_H_
#define _MACH_MACH_I386_TYPES_H_
#ifndef __ASSEMBLER__
struct descriptor {
unsigned int low_word;
unsigned int high_word;
};
typedef struct descriptor descriptor_t;
typedef struct descriptor *descriptor_list_t;
typedef const struct descriptor *const_descriptor_list_t;
#endif
#ifndef MACH_KERNEL
typedef unsigned short io_port_t;
typedef mach_port_t io_perm_t;
#endif
#endif