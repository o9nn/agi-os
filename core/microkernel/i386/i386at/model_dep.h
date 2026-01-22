#ifndef _MODEL_DEP_H_
#define _MODEL_DEP_H_
#include <i386/vm_param.h>
#include <mach/vm_prot.h>
extern vm_offset_t int_stack_top[NCPUS], int_stack_base[NCPUS];
#define ON_INT_STACK(P, CPU)	(((P) & ~(INTSTACK_SIZE-1)) == int_stack_base[CPU])
extern vm_offset_t timemmap(dev_t dev, vm_offset_t off, vm_prot_t prot);
void inittodr(void);
boolean_t init_alloc_aligned(vm_size_t size, vm_offset_t *addrp);
#endif