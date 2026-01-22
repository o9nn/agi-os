#ifndef CONS_H
#define CONS_H
#include <mach/machine/vm_types.h>
void cnputc(char c);
static inline int cngetc() { return 0; }
#endif