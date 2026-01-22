#ifndef _VM_VM_EXTERNAL_H_
#define _VM_VM_EXTERNAL_H_
typedef struct vm_external {
int existence_size;
char *existence_map;
#if 0
int existence_count;
#endif
} *vm_external_t;
#define VM_EXTERNAL_NULL ((vm_external_t) 0)
#define VM_EXTERNAL_SMALL_SIZE 128
#define VM_EXTERNAL_LARGE_SIZE 8192
typedef int vm_external_state_t;
#define VM_EXTERNAL_STATE_EXISTS 1
#define VM_EXTERNAL_STATE_UNKNOWN 2
#define VM_EXTERNAL_STATE_ABSENT 3
extern void vm_external_module_initialize(void);
extern vm_external_t vm_external_create(vm_offset_t);
extern void vm_external_destroy(vm_external_t);
extern void vm_external_state_set(vm_external_t, vm_offset_t,
vm_external_state_t);
#define vm_external_state_get(e,offset) (((e) != VM_EXTERNAL_NULL) ? \
_vm_external_state_get(e, offset) : \
VM_EXTERNAL_STATE_UNKNOWN)
extern vm_external_state_t _vm_external_state_get(vm_external_t, vm_offset_t);
#endif