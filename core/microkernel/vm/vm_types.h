#ifndef VM_VM_TYPES_H
#define VM_VM_TYPES_H
typedef struct vm_map *vm_map_t;
#define VM_MAP_NULL ((vm_map_t) 0)
typedef struct vm_object *vm_object_t;
#define VM_OBJECT_NULL ((vm_object_t) 0)
typedef struct vm_page *vm_page_t;
#define VM_PAGE_NULL ((vm_page_t) 0)
#endif