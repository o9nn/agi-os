#ifndef gsstype_INCLUDED
#  define gsstype_INCLUDED
typedef struct gc_state_s gc_state_t;
typedef struct enum_ptr_s {
const void *ptr;
uint size;
} enum_ptr_t;
#define EV_CONST const
#define struct_proc_clear_marks(proc)\
void proc(const gs_memory_t *cmem, void  *pre, uint size,\
const gs_memory_struct_type_t *pstype)
#define struct_proc_enum_ptrs(proc)\
gs_ptr_type_t proc(const gs_memory_t *mem, EV_CONST void  *ptr, uint size,\
int index, enum_ptr_t *pep, const gs_memory_struct_type_t *pstype,\
gc_state_t *gcst)
#define struct_proc_reloc_ptrs(proc)\
void proc(void  *ptr, uint size,\
const gs_memory_struct_type_t *pstype, gc_state_t *gcst)
#define struct_proc_finalize(proc)\
void proc(void  *ptr)
typedef struct struct_shared_procs_s struct_shared_procs_t;
struct gs_memory_struct_type_s {
uint ssize;
struct_name_t sname;
const struct_shared_procs_t *shared;
struct_proc_clear_marks((*clear_marks));
struct_proc_enum_ptrs((*enum_ptrs));
struct_proc_reloc_ptrs((*reloc_ptrs));
struct_proc_finalize((*finalize));
const void *proc_data;
};
#define extern_st(st) extern const gs_memory_struct_type_t st
#endif