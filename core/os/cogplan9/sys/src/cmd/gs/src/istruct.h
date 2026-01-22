#ifndef istruct_INCLUDED
#  define istruct_INCLUDED
#include "gsstruct.h"
extern const gs_ptr_procs_t ptr_ref_procs;
#define ptr_ref_type (&ptr_ref_procs)
extern_st(st_refs);
#define refs_proc_reloc(proc)\
void proc(ref_packed *from, ref_packed *to, gc_state_t *gcst)
typedef struct gc_procs_with_refs_s {
gc_procs_common;
ptr_proc_reloc((*reloc_ref_ptr), ref_packed);
refs_proc_reloc((*reloc_refs));
} gc_procs_with_refs_t;
#undef gc_proc
#define gc_proc(gcst, proc) ((*(const gc_procs_with_refs_t **)(gcst))->proc)
#define ENUM_RETURN_REF(rptr)\
return (pep->ptr = (const void *)(rptr), ptr_ref_type)
#define ENUM_RETURN_REF_MEMBER(typ, memb)\
ENUM_RETURN_REF(&((typ *)vptr)->memb)
#define RELOC_REF_PTR_VAR(ptrvar)\
ptrvar = (*gc_proc(gcst, reloc_ref_ptr))((const void *)(ptrvar), gcst)
#define RELOC_REF_PTR_MEMBER(typ, memb)\
RELOC_REF_PTR_VAR(((typ *)vptr)->memb)
#define RELOC_REFS(from, upto)\
(*gc_proc(gcst, reloc_refs))((ref_packed *)(from), (ref_packed *)(upto), gcst)
#define RELOC_REF_VAR(refvar)\
RELOC_REFS(&(refvar), &(refvar) + 1)
struct_proc_clear_marks(ref_struct_clear_marks);
struct_proc_enum_ptrs(ref_struct_enum_ptrs);
struct_proc_reloc_ptrs(ref_struct_reloc_ptrs);
#define gs__st_ref_struct(scope_st, stname, stype, sname)\
gs__st_complex_only(scope_st, stname, stype, sname, ref_struct_clear_marks,\
ref_struct_enum_ptrs, ref_struct_reloc_ptrs, 0)
#define gs_public_st_ref_struct(stname, stype, sname)\
gs__st_ref_struct(public_st, stname, stype, sname)
#define gs_private_st_ref_struct(stname, stype, sname)\
gs__st_ref_struct(private_st, stname, stype, sname)
#endif