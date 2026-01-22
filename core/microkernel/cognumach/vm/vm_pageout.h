#ifndef	_VM_VM_PAGEOUT_H_
#define _VM_VM_PAGEOUT_H_
#include <vm/vm_page.h>
extern vm_page_t vm_pageout_setup(vm_page_t, vm_offset_t, vm_object_t,
vm_offset_t, boolean_t);
extern void vm_pageout_page(vm_page_t, boolean_t, boolean_t);
extern void vm_pageout(void) __attribute__((noreturn));
extern void vm_pageout_start(void);
extern void vm_pageout_resume(void);
#endif