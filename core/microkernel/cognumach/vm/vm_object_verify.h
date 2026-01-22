#ifndef _VM_VM_OBJECT_VERIFY_H_
#define _VM_VM_OBJECT_VERIFY_H_
#include <mach/kern_return.h>
#include <mach/boolean.h>
typedef struct vm_object *vm_object_t;
typedef struct vm_object_memory_stats {
unsigned long resident_pages;
unsigned long wired_pages;
unsigned long active_pages;
unsigned long inactive_pages;
unsigned long dirty_pages;
unsigned long referenced_pages;
vm_size_t memory_size;
} vm_object_memory_stats_t;
extern boolean_t vm_object_verify_resident_count(vm_object_t object);
extern void vm_object_increment_resident_count(vm_object_t object);
extern void vm_object_decrement_resident_count(vm_object_t object);
extern kern_return_t vm_object_get_memory_stats(vm_object_t object, vm_object_memory_stats_t *stats);
extern void vm_object_verify_all_counts(void);
#endif