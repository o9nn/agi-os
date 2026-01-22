#ifndef _MACH_I386_VM_PARAM_H_
#define _MACH_I386_VM_PARAM_H_
#include <mach/machine/vm_types.h>
#define BYTE_SIZE 8
#define I386_PGBYTES 4096
#define I386_PGSHIFT 12
#define PAGE_SHIFT I386_PGSHIFT
#define i386_btop(x) (((phys_addr_t)(x)) >> I386_PGSHIFT)
#define i386_ptob(x) (((phys_addr_t)(x)) << I386_PGSHIFT)
#define i386_round_page(x) ((((phys_addr_t)(x)) + I386_PGBYTES - 1) & \
~(I386_PGBYTES-1))
#define i386_trunc_page(x) (((phys_addr_t)(x)) & ~(I386_PGBYTES-1))
#define VM_MIN_ADDRESS (0ULL)
#ifdef __x86_64__
#if defined(KERNEL) && defined(USER32)
#define VM_MAX_ADDRESS (0xfffff000ULL)
#else
#define VM_MAX_ADDRESS (0x800000000000ULL)
#endif
#else
#define VM_MAX_ADDRESS (0xc0000000UL)
#endif
#endif