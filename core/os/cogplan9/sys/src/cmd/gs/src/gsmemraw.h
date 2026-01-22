#ifndef gsmemraw_INCLUDED
#  define gsmemraw_INCLUDED
#if 0
typedef struct gs_memory_status_s {
ulong allocated;
ulong used;
} gs_memory_status_t;
#ifndef gs_raw_memory_t_DEFINED
#define gs_raw_memory_t_DEFINED
typedef struct gs_raw_memory_s gs_raw_memory_t;
#endif
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
typedef struct gs_raw_memory_procs_s {
gs_raw_memory_procs(gs_raw_memory_t);
} gs_raw_memory_procs_t;
struct gs_raw_memory_s {
gs_raw_memory_t *stable_memory;
gs_raw_memory_procs_t procs;
};
#endif
#endif