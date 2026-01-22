#ifndef VM_PRINT_H
#define	VM_PRINT_H
#include <vm/vm_map.h>
#include <machine/db_machdep.h>
extern void vm_map_print(db_expr_t addr, boolean_t have_addr,
db_expr_t count, const char *modif);
extern void vm_map_copy_print(const vm_map_copy_t);
#include <vm/vm_object.h>
extern void vm_object_print_part(vm_object_t object, vm_offset_t offset, vm_size_t size);
extern void vm_object_print(vm_object_t);
#include <vm/vm_page.h>
extern void vm_page_print(const vm_page_t);
#endif