#ifndef gsmemory_INCLUDED
#  define gsmemory_INCLUDED
#include "gstypes.h"
#include "gslibctx.h"
typedef struct gs_memory_struct_type_s gs_memory_struct_type_t;
typedef const gs_memory_struct_type_t *gs_memory_type_ptr_t;
#ifndef gs_memory_DEFINED
#  define gs_memory_DEFINED
typedef struct gs_memory_s gs_memory_t;
#endif
typedef struct gs_ptr_procs_s gs_ptr_procs_t;
typedef const gs_ptr_procs_t *gs_ptr_type_t;
typedef struct gs_gc_root_s gs_gc_root_t;
typedef client_name_t struct_name_t;
uint gs_struct_type_size(gs_memory_type_ptr_t);
struct_name_t gs_struct_type_name(gs_memory_type_ptr_t);
#define gs_struct_type_name_string(styp)\
((const char *)gs_struct_type_name(styp))
typedef struct gs_memory_status_s {
ulong allocated;
ulong used;
} gs_memory_status_t;
#define gs_memory_t_proc_alloc_bytes(proc, mem_t)\
byte *proc(mem_t *mem, uint nbytes, client_name_t cname)
#define gs_alloc_bytes_immovable(mem, nbytes, cname)\
((mem)->procs.alloc_bytes_immovable(mem, nbytes, cname))
#define gs_memory_t_proc_resize_object(proc, mem_t)\
void *proc(mem_t *mem, void *obj, uint new_num_elements,\
client_name_t cname)
#define gs_resize_object(mem, obj, newn, cname)\
((mem)->procs.resize_object(mem, obj, newn, cname))
#define gs_memory_t_proc_free_object(proc, mem_t)\
void proc(mem_t *mem, void *data, client_name_t cname)
#define gs_free_object(mem, data, cname)\
((mem)->procs.free_object(mem, data, cname))
#define gs_memory_t_proc_status(proc, mem_t)\
void proc(mem_t *mem, gs_memory_status_t *status)
#define gs_memory_status(mem, pst)\
((mem)->procs.status(mem, pst))
#define gs_memory_t_proc_stable(proc, mem_t)\
mem_t *proc(mem_t *mem)
#define gs_memory_stable(mem)\
((mem)->procs.stable(mem))
#define FREE_ALL_DATA 1
#define FREE_ALL_STRUCTURES 2
#define FREE_ALL_ALLOCATOR 4
#define FREE_ALL_EVERYTHING\
(FREE_ALL_DATA | FREE_ALL_STRUCTURES | FREE_ALL_ALLOCATOR)
#define gs_memory_t_proc_free_all(proc, mem_t)\
void proc(mem_t *mem, uint free_mask, client_name_t cname)
#define gs_memory_free_all(mem, free_mask, cname)\
((mem)->procs.free_all(mem, free_mask, cname))
#define gs_free_all(mem)\
gs_memory_free_all(mem, FREE_ALL_DATA, "(free_all)")
#define gs_memory_t_proc_consolidate_free(proc, mem_t)\
void proc(mem_t *mem)
#define gs_consolidate_free(mem)\
((mem)->procs.consolidate_free(mem))
#define gs_raw_memory_procs(mem_t)\
gs_memory_t_proc_alloc_bytes((*alloc_bytes_immovable), mem_t);\
gs_memory_t_proc_resize_object((*resize_object), mem_t);\
gs_memory_t_proc_free_object((*free_object), mem_t);\
gs_memory_t_proc_stable((*stable), mem_t);\
gs_memory_t_proc_status((*status), mem_t);\
gs_memory_t_proc_free_all((*free_all), mem_t);\
gs_memory_t_proc_consolidate_free((*consolidate_free), mem_t)
typedef struct gs_memory_procs_s {
gs_raw_memory_procs(gs_memory_t);
#define gs_memory_proc_alloc_bytes(proc)\
gs_memory_t_proc_alloc_bytes(proc, gs_memory_t)
#define gs_memory_proc_resize_object(proc)\
gs_memory_t_proc_resize_object(proc, gs_memory_t)
#define gs_memory_proc_free_object(proc)\
gs_memory_t_proc_free_object(proc, gs_memory_t)
#define gs_memory_proc_stable(proc)\
gs_memory_t_proc_stable(proc, gs_memory_t)
#define gs_memory_proc_status(proc)\
gs_memory_t_proc_status(proc, gs_memory_t)
#define gs_memory_proc_free_all(proc)\
gs_memory_t_proc_free_all(proc, gs_memory_t)
#define gs_memory_proc_consolidate_free(proc)\
gs_memory_t_proc_consolidate_free(proc, gs_memory_t)
#define gs_alloc_bytes(mem, nbytes, cname)\
(*(mem)->procs.alloc_bytes)(mem, nbytes, cname)
gs_memory_proc_alloc_bytes((*alloc_bytes));
#define gs_memory_proc_alloc_struct(proc)\
void *proc(gs_memory_t *mem, gs_memory_type_ptr_t pstype,\
client_name_t cname)
#define gs_alloc_struct(mem, typ, pstype, cname)\
(typ *)(*(mem)->procs.alloc_struct)(mem, pstype, cname)
gs_memory_proc_alloc_struct((*alloc_struct));
#define gs_alloc_struct_immovable(mem, typ, pstype, cname)\
(typ *)(*(mem)->procs.alloc_struct_immovable)(mem, pstype, cname)
gs_memory_proc_alloc_struct((*alloc_struct_immovable));
#define gs_memory_proc_alloc_byte_array(proc)\
byte *proc(gs_memory_t *mem, uint num_elements, uint elt_size,\
client_name_t cname)
#define gs_alloc_byte_array(mem, nelts, esize, cname)\
(*(mem)->procs.alloc_byte_array)(mem, nelts, esize, cname)
gs_memory_proc_alloc_byte_array((*alloc_byte_array));
#define gs_alloc_byte_array_immovable(mem, nelts, esize, cname)\
(*(mem)->procs.alloc_byte_array_immovable)(mem, nelts, esize, cname)
gs_memory_proc_alloc_byte_array((*alloc_byte_array_immovable));
#define gs_memory_proc_alloc_struct_array(proc)\
void *proc(gs_memory_t *mem, uint num_elements,\
gs_memory_type_ptr_t pstype, client_name_t cname)
#define gs_alloc_struct_array(mem, nelts, typ, pstype, cname)\
(typ *)(*(mem)->procs.alloc_struct_array)(mem, nelts, pstype, cname)
gs_memory_proc_alloc_struct_array((*alloc_struct_array));
#define gs_alloc_struct_array_immovable(mem, nelts, typ, pstype, cname)\
(typ *)(*(mem)->procs.alloc_struct_array_immovable)(mem, nelts, pstype, cname)
gs_memory_proc_alloc_struct_array((*alloc_struct_array_immovable));
#define gs_memory_proc_object_size(proc)\
uint proc(gs_memory_t *mem, const void *obj)
#define gs_object_size(mem, obj)\
(*(mem)->procs.object_size)(mem, obj)
gs_memory_proc_object_size((*object_size));
#define gs_memory_proc_object_type(proc)\
gs_memory_type_ptr_t proc(gs_memory_t *mem, const void *obj)
#define gs_object_type(mem, obj)\
(*(mem)->procs.object_type)(mem, obj)
gs_memory_proc_object_type((*object_type));
#define gs_memory_proc_alloc_string(proc)\
byte *proc(gs_memory_t *mem, uint nbytes, client_name_t cname)
#define gs_alloc_string(mem, nbytes, cname)\
(*(mem)->procs.alloc_string)(mem, nbytes, cname)
gs_memory_proc_alloc_string((*alloc_string));
#define gs_alloc_string_immovable(mem, nbytes, cname)\
(*(mem)->procs.alloc_string_immovable)(mem, nbytes, cname)
gs_memory_proc_alloc_string((*alloc_string_immovable));
#define gs_memory_proc_resize_string(proc)\
byte *proc(gs_memory_t *mem, byte *data, uint old_num, uint new_num,\
client_name_t cname)
#define gs_resize_string(mem, data, oldn, newn, cname)\
(*(mem)->procs.resize_string)(mem, data, oldn, newn, cname)
gs_memory_proc_resize_string((*resize_string));
#define gs_memory_proc_free_string(proc)\
void proc(gs_memory_t *mem, byte *data, uint nbytes,\
client_name_t cname)
#define gs_free_string(mem, data, nbytes, cname)\
(*(mem)->procs.free_string)(mem, data, nbytes, cname)
gs_memory_proc_free_string((*free_string));
#define gs_memory_proc_register_root(proc)\
int proc(gs_memory_t *mem, gs_gc_root_t *root, gs_ptr_type_t ptype,\
void **pp, client_name_t cname)
#define gs_register_root(mem, root, ptype, pp, cname)\
(*(mem)->procs.register_root)(mem, root, ptype, pp, cname)
gs_memory_proc_register_root((*register_root));
#define gs_memory_proc_unregister_root(proc)\
void proc(gs_memory_t *mem, gs_gc_root_t *root, client_name_t cname)
#define gs_unregister_root(mem, root, cname)\
(*(mem)->procs.unregister_root)(mem, root, cname)
gs_memory_proc_unregister_root((*unregister_root));
#define gs_memory_proc_enable_free(proc)\
void proc(gs_memory_t *mem, bool enable)
#define gs_enable_free(mem, enable)\
(*(mem)->procs.enable_free)(mem, enable)
gs_memory_proc_enable_free((*enable_free));
} gs_memory_procs_t;
void gs_free_const_object(gs_memory_t *mem, const void *data,
client_name_t cname);
void gs_free_const_string(gs_memory_t *mem, const byte *data, uint nbytes,
client_name_t cname);
void gs_free_bytestring(gs_memory_t *mem, gs_bytestring *pbs,
client_name_t cname);
void gs_free_const_bytestring(gs_memory_t *mem, gs_const_bytestring *pbs,
client_name_t cname);
void *gs_resize_struct_array(gs_memory_t *mem, void *obj, uint num_elements,
gs_memory_type_ptr_t pstype,
client_name_t cname);
int gs_register_struct_root(gs_memory_t *mem, gs_gc_root_t *root,
void **pp, client_name_t cname);
gs_memory_proc_free_object(gs_ignore_free_object);
gs_memory_proc_free_string(gs_ignore_free_string);
gs_memory_proc_consolidate_free(gs_ignore_consolidate_free);
void *gs_raw_alloc_struct_immovable(gs_memory_t * rmem,
gs_memory_type_ptr_t pstype,
client_name_t cname);
typedef struct pl_mem_node_s pl_mem_node_t;
#define gs_memory_common\
gs_memory_t *stable_memory;\
gs_memory_procs_t procs;\
gs_lib_ctx_t *gs_lib_ctx;\
pl_mem_node_t *head;\
gs_memory_t *non_gc_memory
struct gs_memory_s {
gs_memory_common;
};
#endif