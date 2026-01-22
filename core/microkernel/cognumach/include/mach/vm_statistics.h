#ifndef _MACH_VM_STATISTICS_H_
#define _MACH_VM_STATISTICS_H_
#include <mach/machine/vm_types.h>
struct vm_statistics {
integer_t pagesize;
integer_t free_count;
integer_t active_count;
integer_t inactive_count;
integer_t wire_count;
integer_t zero_fill_count;
integer_t reactivations;
integer_t pageins;
integer_t pageouts;
integer_t faults;
integer_t cow_faults;
integer_t lookups;
integer_t hits;
};
typedef struct vm_statistics *vm_statistics_t;
typedef struct vm_statistics vm_statistics_data_t;
#ifdef MACH_KERNEL
extern vm_statistics_data_t vm_stat;
#endif
struct pmap_statistics {
integer_t resident_count;
integer_t wired_count;
};
typedef struct pmap_statistics *pmap_statistics_t;
#endif