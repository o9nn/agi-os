#ifndef gsstruct_INCLUDED
#  define gsstruct_INCLUDED
#include "gsstype.h"
#ifndef obj_header_DEFINED
#  define obj_header_DEFINED
typedef struct obj_header_s obj_header_t;
#endif
struct gs_ptr_procs_s {
#define ptr_proc_unmark(proc)\
void proc(enum_ptr_t *, gc_state_t *)
ptr_proc_unmark((*unmark));
#define ptr_proc_mark(proc)\
bool proc(enum_ptr_t *, gc_state_t *)
ptr_proc_mark((*mark));
#define ptr_proc_reloc(proc, typ)\
typ *proc(const typ *, gc_state_t *)
ptr_proc_reloc((*reloc), void);
};
extern const gs_ptr_procs_t ptr_struct_procs;
#define ptr_struct_type (&ptr_struct_procs)
extern const gs_ptr_procs_t ptr_string_procs;
#define ptr_string_type (&ptr_string_procs)
extern const gs_ptr_procs_t ptr_const_string_procs;
#define ptr_const_string_type (&ptr_const_string_procs)
struct gs_gc_root_s {
gs_gc_root_t *next;
gs_ptr_type_t ptype;
void **p;
bool free_on_unregister;
};
#define public_st_gc_root_t()	\
gs_public_st_ptrs1(st_gc_root_t, gs_gc_root_t, "gs_gc_root_t",\
gc_root_enum_ptrs, gc_root_reloc_ptrs, next)
#define if_debug_root(c, msg, rp)\
if_debug4(c, "%s 0x%lx: 0x%lx -> 0x%lx\n",\
msg, (ulong)(rp), (ulong)(rp)->p, (ulong)*(rp)->p)
#define string_proc_reloc(proc)\
void proc(gs_string *, gc_state_t *)
#define const_string_proc_reloc(proc)\
void proc(gs_const_string *, gc_state_t *)
#define param_string_proc_reloc(proc)\
void proc(gs_param_string *, gc_state_t *)
#define gc_procs_common\
\
ptr_proc_reloc((*reloc_struct_ptr), void );\
\
string_proc_reloc((*reloc_string));\
\
const_string_proc_reloc((*reloc_const_string));\
\
param_string_proc_reloc((*reloc_param_string))
typedef struct gc_procs_common_s {
gc_procs_common;
} gc_procs_common_t;
#define gc_proc(gcst, proc) ((*(const gc_procs_common_t **)(gcst))->proc)
#define struct_type_name_string(pstype) ((const char *)((pstype)->sname))
struct_proc_enum_ptrs(gs_no_struct_enum_ptrs);
struct_proc_reloc_ptrs(gs_no_struct_reloc_ptrs);
extern_st(st_free);
extern_st(st_bytes);
extern_st(st_gc_root_t);
#define private_st_const_string()\
BASIC_PTRS(const_string_elts) {\
{ GC_ELT_CONST_STRING, 0 }\
};\
gs__st_basic(private_st, st_const_string, gs_const_string,\
"gs_const_string", const_string_elts, const_string_sdata)
extern_st(st_const_string_element);
#define public_st_const_string_element()\
gs_public_st_element(st_const_string_element, gs_const_string,\
"gs_const_string[]", const_string_elt_enum_ptrs,\
const_string_elt_reloc_ptrs, st_const_string)
#define public_st public const gs_memory_struct_type_t
#define private_st private const gs_memory_struct_type_t
typedef enum {
GC_ELT_OBJ,
GC_ELT_STRING,
GC_ELT_CONST_STRING
} gc_ptr_type_index_t;
typedef struct gc_ptr_element_s {
ushort  type;
ushort offset;
} gc_ptr_element_t;
#define GC_OBJ_ELT(typ, elt)\
{ GC_ELT_OBJ, offset_of(typ, elt) }
#define GC_OBJ_ELT2(typ, e1, e2)\
GC_OBJ_ELT(typ, e1), GC_OBJ_ELT(typ, e2)
#define GC_OBJ_ELT3(typ, e1, e2, e3)\
GC_OBJ_ELT(typ, e1), GC_OBJ_ELT(typ, e2), GC_OBJ_ELT(typ, e3)
#define GC_STRING_ELT(typ, elt)\
{ GC_ELT_STRING, offset_of(typ, elt) }
#define GC_CONST_STRING_ELT(typ, elt)\
{ GC_ELT_CONST_STRING, offset_of(typ, elt) }
typedef struct gc_struct_data_s {
ushort num_ptrs;
ushort super_offset;
const gs_memory_struct_type_t *super_type;
const gc_ptr_element_t *ptrs;
} gc_struct_data_t;
struct_proc_enum_ptrs(basic_enum_ptrs);
struct_proc_reloc_ptrs(basic_reloc_ptrs);
#define BASIC_PTRS(elts)\
private const gc_ptr_element_t elts[] =
#define gs__st_basic_with_super_final(scope_st, stname, stype, sname, nelts, elts, sdata, supst, supoff, pfinal)\
private const gc_struct_data_t sdata = {\
nelts, supoff, supst, elts\
};\
scope_st stname = {\
sizeof(stype), sname, 0, 0, basic_enum_ptrs, basic_reloc_ptrs,\
pfinal, &sdata\
}
#define gs__st_basic_super_final(scope_st, stname, stype, sname, elts, sdata, supst, supoff, pfinal)\
gs__st_basic_with_super_final(scope_st, stname, stype, sname, countof(elts), elts, sdata, supst, supoff, pfinal)
#define gs_public_st_basic_super_final(stname, stype, sname, elts, sdata, supst, supoff, pfinal)\
gs__st_basic_super_final(public_st, stname, stype, sname, elts, sdata, supst, supoff, pfinal)
#define gs_private_st_basic_super_final(stname, stype, sname, elts, sdata, supst, supoff, pfinal)\
gs__st_basic_super_final(private_st, stname, stype, sname, elts, sdata, supst, supoff, pfinal)
#define gs__st_basic_super(scope_st, stname, stype, sname, elts, sdata, supst, supoff)\
gs__st_basic_super_final(scope_st, stname, stype, sname, elts, sdata, supst, supoff, 0)
#define gs_public_st_basic_super(stname, stype, sname, elts, sdata, supst, supoff)\
gs__st_basic_super(public_st, stname, stype, sname, elts, sdata, supst, supoff)
#define gs_private_st_basic_super(stname, stype, sname, elts, sdata, supst, supoff)\
gs__st_basic_super(private_st, stname, stype, sname, elts, sdata, supst, supoff)
#define gs__st_basic(scope_st, stname, stype, sname, elts, sdata)\
gs__st_basic_super(scope_st, stname, stype, sname, elts, sdata, 0, 0)
#define gs_public_st_basic(stname, stype, sname, elts, sdata)\
gs__st_basic(public_st, stname, stype, sname, elts, sdata)
#define gs_private_st_basic(stname, stype, sname, elts, sdata)\
gs__st_basic(private_st, stname, stype, sname, elts, sdata)
#define ENUM_PTRS_BEGIN_PROC(proc)\
gs_ptr_type_t proc(const gs_memory_t *mem, EV_CONST void *vptr, uint size, int index, enum_ptr_t *pep, const gs_memory_struct_type_t *pstype, gc_state_t *gcst)
#define ENUM_PTRS_BEGIN(proc)\
ENUM_PTRS_BEGIN_PROC(proc)\
{ switch ( index ) { default:
#define ENUM_PTRS_WITH(proc, stype_ptr)\
ENUM_PTRS_BEGIN_PROC(proc)\
{ EV_CONST stype_ptr = vptr; switch ( index ) { default:
#define ENUM_OBJ(optr)		\
(pep->ptr = (const void *)(optr), ptr_struct_type)
#define ENUM_STRING2(sdata, ssize) \
(pep->ptr = sdata, pep->size = ssize, ptr_string_type)
#define ENUM_STRING(sptr)	\
ENUM_STRING2((sptr)->data, (sptr)->size)
#define ENUM_CONST_STRING2(sdata, ssize)	\
(pep->ptr = sdata, pep->size = ssize, ptr_const_string_type)
#define ENUM_CONST_STRING(sptr)	\
ENUM_CONST_STRING2((sptr)->data, (sptr)->size)
extern gs_ptr_type_t
enum_bytestring(enum_ptr_t *pep, const gs_bytestring *pbs);
#define ENUM_BYTESTRING(ptr)	\
enum_bytestring(pep, ptr)
extern gs_ptr_type_t
enum_const_bytestring(enum_ptr_t *pep, const gs_const_bytestring *pbs);
#define ENUM_CONST_BYTESTRING(ptr)  \
enum_const_bytestring(pep, ptr)
#define ENUM_OBJ_ELT(typ, elt)\
ENUM_OBJ(((const typ *)vptr)->elt)
#define ENUM_STRING_ELT(typ, elt)\
ENUM_STRING(&((const typ *)vptr)->elt)
#define ENUM_PARAM_STRING_ELT(typ, elt)\
(((const typ *)vptr)->elt.persistent ? 0 : ENUM_STRING(&((const typ *)vptr)->elt))
#define ENUM_CONST_STRING_ELT(typ, elt)\
ENUM_CONST_STRING(&((const typ *)vptr)->elt)
#define ENUM_PTR(i, typ, elt)\
case i: return ENUM_OBJ_ELT(typ, elt)
#define ENUM_PTR2(i, typ, e1, e2) \
ENUM_PTR(i, typ, e1); ENUM_PTR((i)+1, typ, e2)
#define ENUM_PTR3(i, typ, e1, e2, e3) \
ENUM_PTR(i, typ, e1); ENUM_PTR((i)+1, typ, e2); ENUM_PTR((i)+2, typ, e3)
#define ENUM_STRING_PTR(i, typ, elt)\
case i: return ENUM_STRING_ELT(typ, elt)
#define ENUM_PARAM_STRING_PTR(i, typ, elt)\
case i: return ENUM_PARAM_STRING_ELT(typ, elt)
#define ENUM_CONST_STRING_PTR(i, typ, elt)\
case i: return ENUM_CONST_STRING_ELT(typ, elt)
#define ENUM_PTRS_END\
}  ENUM_PTRS_END_PROC }
#define ENUM_PTRS_END_PROC
#define RELOC_PTRS_BEGIN(proc)\
void proc(void *vptr, uint size, const gs_memory_struct_type_t *pstype, gc_state_t *gcst) {
#define RELOC_PTRS_WITH(proc, stype_ptr)\
RELOC_PTRS_BEGIN(proc) stype_ptr = vptr;
#define RELOC_OBJ(ptr)\
(gc_proc(gcst, reloc_struct_ptr)((const void *)(ptr), gcst))
#define RELOC_OBJ_VAR(ptrvar)\
(ptrvar = RELOC_OBJ(ptrvar))
#define RELOC_VAR(ptrvar)	\
RELOC_OBJ_VAR(ptrvar)
#define RELOC_STRING_VAR(ptrvar)\
(gc_proc(gcst, reloc_string)(&(ptrvar), gcst))
#define RELOC_CONST_STRING_VAR(ptrvar)\
(gc_proc(gcst, reloc_const_string)(&(ptrvar), gcst))
#define RELOC_PARAM_STRING_VAR(ptrvar)\
(gc_proc(gcst, reloc_param_string)(&(ptrvar), gcst))
extern void reloc_bytestring(gs_bytestring *pbs, gc_state_t *gcst);
#define RELOC_BYTESTRING_VAR(ptrvar)\
reloc_bytestring(&(ptrvar), gcst)
extern void reloc_const_bytestring(gs_const_bytestring *pbs, gc_state_t *gcst);
#define RELOC_CONST_BYTESTRING_VAR(ptrvar)\
reloc_const_bytestring(&(ptrvar), gcst)
#define RELOC_OBJ_ELT(typ, elt)\
RELOC_VAR(((typ *)vptr)->elt)
#define RELOC_STRING_ELT(typ, elt)\
RELOC_STRING_VAR(((typ *)vptr)->elt)
#define RELOC_CONST_STRING_ELT(typ, elt)\
RELOC_CONST_STRING_VAR(((typ *)vptr)->elt)
#define RELOC_PARAM_STRING_ELT(typ, elt)\
RELOC_PARAM_STRING_VAR(((typ *)vptr)->elt)
#define RELOC_OFFSET_ELT(typ, elt, offset)\
((typ *)vptr)->elt = (void *)\
((char *)RELOC_OBJ((char *)((typ *)vptr)->elt - (offset)) +\
(offset))
#define RELOC_TYPED_OFFSET_ELT(typ, elt, offset)\
(((typ *)vptr)->elt = (void *)RELOC_OBJ(((typ *)vptr)->elt - (offset)),\
((typ *)vptr)->elt += (offset))
#define RELOC_PTR(typ, elt)\
RELOC_OBJ_ELT(typ, elt)
#define RELOC_PTR2(typ, e1, e2) \
RELOC_PTR(typ,e1); RELOC_PTR(typ,e2)
#define RELOC_PTR3(typ, e1, e2, e3) \
RELOC_PTR(typ,e1); RELOC_PTR(typ,e2); RELOC_PTR(typ,e3)
#define RELOC_OFFSET_PTR(typ, elt, offset)\
RELOC_OFFSET_ELT(typ, elt, offset)
#define RELOC_TYPED_OFFSET_PTR(typ, elt, offset)\
RELOC_TYPED_OFFSET_ELT(typ, elt, offset)
#define RELOC_STRING_PTR(typ, elt)\
RELOC_STRING_ELT(typ, elt)
#define RELOC_CONST_STRING_PTR(typ, elt)\
RELOC_CONST_STRING_ELT(typ, elt)
#define RELOC_PARAM_STRING_PTR(typ, elt)\
RELOC_PARAM_STRING_ELT(typ, elt)
#define RELOC_PTRS_END\
}
#define ENUM_USING(supst, ptr, size, index)\
(*(supst).enum_ptrs)(mem, ptr, size, index, pep, &(supst), gcst)
#define RELOC_USING(supst, ptr, size)\
(*(supst).reloc_ptrs)(ptr, size, &(supst), gcst)
#define ENUM_USING_PREFIX(supst, n)\
ENUM_USING(supst, vptr, size, index-(n))
#define ENUM_PREFIX(supst, n)\
return ENUM_USING_PREFIX(supst, n)
#define RELOC_PREFIX(supst)\
RELOC_USING(supst, vptr, size)
#define ENUM_SUPER_ELT(stype, supst, member, n)\
ENUM_USING(supst, &((EV_CONST stype *)vptr)->member, sizeof(((EV_CONST stype *)vptr)->member), index-(n))
#define ENUM_SUPER(stype, supst, member, n)\
return ENUM_SUPER_ELT(stype, supst, member, n)
#define RELOC_SUPER_ELT(stype, supst, member)\
RELOC_USING(supst, &((stype *)vptr)->member, sizeof(((stype *)vptr)->member))
#define RELOC_SUPER(stype, supst, member)\
RELOC_SUPER_ELT(stype, supst, member)
#define ENUM_RETURN(ptr) return ENUM_OBJ(ptr)
#define ENUM_RETURN_PTR(typ, elt) return ENUM_OBJ_ELT(typ, elt)
#define ENUM_RETURN_STRING_PTR(typ, elt) return ENUM_STRING_ELT(typ, elt)
#define ENUM_RETURN_CONST_STRING(ptr) return ENUM_CONST_STRING(ptr)
#define ENUM_RETURN_CONST_STRING_PTR(typ, elt) return ENUM_CONST_STRING_ELT(typ, elt)
#define gs__st_simple(scope_st, stname, stype, sname)\
scope_st stname = { sizeof(stype), sname, 0, 0, gs_no_struct_enum_ptrs, gs_no_struct_reloc_ptrs, 0 }
#define gs_public_st_simple(stname, stype, sname)\
gs__st_simple(public_st, stname, stype, sname)
#define gs_private_st_simple(stname, stype, sname)\
gs__st_simple(private_st, stname, stype, sname)
#define gs__st_simple_final(scope_st, stname, stype, sname, pfinal)\
scope_st stname = { sizeof(stype), sname, 0, 0, gs_no_struct_enum_ptrs, gs_no_struct_reloc_ptrs, pfinal }
#define gs_public_st_simple_final(stname, stype, sname, pfinal)\
gs__st_simple_final(public_st, stname, stype, sname, pfinal)
#define gs_private_st_simple_final(stname, stype, sname, pfinal)\
gs__st_simple_final(private_st, stname, stype, sname, pfinal)
#define CLEAR_MARKS_PROC(proc)\
void proc(const gs_memory_t *cmem, void *vptr, uint size, const gs_memory_struct_type_t *pstype)
#define gs__st_complex_only(scope_st, stname, stype, sname, pclear, penum, preloc, pfinal)\
scope_st stname = { sizeof(stype), sname, 0, pclear, penum, preloc, pfinal }
#define gs_public_st_complex_only(stname, stype, sname, pclear, penum, preloc, pfinal)\
gs__st_complex_only(public_st, stname, stype, sname, pclear, penum, preloc, pfinal)
#define gs_private_st_complex_only(stname, stype, sname, pclear, penum, preloc, pfinal)\
gs__st_complex_only(private_st, stname, stype, sname, pclear, penum, preloc, pfinal)
#define gs__st_complex(scope_st, stname, stype, sname, pclear, penum, preloc, pfinal)\
private struct_proc_clear_marks(pclear);\
private struct_proc_enum_ptrs(penum);\
private struct_proc_reloc_ptrs(preloc);\
private struct_proc_finalize(pfinal);\
gs__st_complex_only(scope_st, stname, stype, sname, pclear, penum, preloc, pfinal)
#define gs_public_st_complex(stname, stype, sname, pclear, penum, preloc, pfinal)\
gs__st_complex(public_st, stname, stype, sname, pclear, penum, preloc, pfinal)
#define gs_private_st_complex(stname, stype, sname, pclear, penum, preloc, pfinal)\
gs__st_complex(private_st, stname, stype, sname, pclear, penum, preloc, pfinal)
#define gs__st_composite(scope_st, stname, stype, sname, penum, preloc)\
private struct_proc_enum_ptrs(penum);\
private struct_proc_reloc_ptrs(preloc);\
gs__st_complex_only(scope_st, stname, stype, sname, 0, penum, preloc, 0)
#define gs_public_st_composite(stname, stype, sname, penum, preloc)\
gs__st_composite(public_st, stname, stype, sname, penum, preloc)
#define gs_private_st_composite(stname, stype, sname, penum, preloc)\
gs__st_composite(private_st, stname, stype, sname, penum, preloc)
#define gs__st_composite_use_final(scope_st, stname, stype, sname, penum, preloc, pfinal)\
private struct_proc_enum_ptrs(penum);\
private struct_proc_reloc_ptrs(preloc);\
gs__st_complex_only(scope_st, stname, stype, sname, 0, penum, preloc, pfinal)
#define gs_public_st_composite_use_final(stname, stype, sname, penum, preloc, pfinal)\
gs__st_composite_use_final(public_st, stname, stype, sname, penum, preloc, pfinal)
#define gs_private_st_composite_use_final(stname, stype, sname, penum, preloc, pfinal)\
gs__st_composite_use_final(private_st, stname, stype, sname, penum, preloc, pfinal)
#define gs__st_composite_final(scope_st, stname, stype, sname, penum, preloc, pfinal)\
private struct_proc_finalize(pfinal);\
gs__st_composite_use_final(scope_st, stname, stype, sname, penum, preloc, pfinal)
#define gs_public_st_composite_final(stname, stype, sname, penum, preloc, pfinal)\
gs__st_composite_final(public_st, stname, stype, sname, penum, preloc, pfinal)
#define gs_private_st_composite_final(stname, stype, sname, penum, preloc, pfinal)\
gs__st_composite_final(private_st, stname, stype, sname, penum, preloc, pfinal)
#define gs__st_composite_only(scope_st, stname, stype, sname, penum, preloc)\
gs__st_complex_only(scope_st, stname, stype, sname, 0, penum, preloc, 0)
#define gs_public_st_composite_only(stname, stype, sname, penum, preloc)\
gs__st_composite_only(public_st, stname, stype, sname, penum, preloc)
#define gs_private_st_composite_only(stname, stype, sname, penum, preloc)\
gs__st_composite_only(private_st, stname, stype, sname, penum, preloc)
#define gs__st_element(scope_st, stname, stype, sname, penum, preloc, basest)\
private ENUM_PTRS_BEGIN_PROC(penum) {\
uint count = size / (uint)sizeof(stype);\
if ( count == 0 ) return 0;\
return ENUM_USING(basest, (EV_CONST char *)vptr + (index % count) * sizeof(stype),\
sizeof(stype), index / count);\
} ENUM_PTRS_END_PROC\
private RELOC_PTRS_BEGIN(preloc) {\
uint count = size / (uint)sizeof(stype);\
for ( ; count; count--, vptr = (char *)vptr + sizeof(stype) )\
RELOC_USING(basest, vptr, sizeof(stype));\
} RELOC_PTRS_END\
gs__st_composite_only(scope_st, stname, stype, sname, penum, preloc)
#define gs_public_st_element(stname, stype, sname, penum, preloc, basest)\
gs__st_element(public_st, stname, stype, sname, penum, preloc, basest)
#define gs_private_st_element(stname, stype, sname, penum, preloc, basest)\
gs__st_element(private_st, stname, stype, sname, penum, preloc, basest)
#define gs__st_ptr(scope_st, stname, stype, sname, penum, preloc)\
private ENUM_PTRS_BEGIN(penum) return 0;\
case 0: return ENUM_OBJ(*(stype const *)vptr);\
ENUM_PTRS_END\
private RELOC_PTRS_BEGIN(preloc) ;\
RELOC_VAR(*(stype *)vptr);\
RELOC_PTRS_END\
gs__st_composite_only(scope_st, stname, stype, sname, penum, preloc)
#define gs_public_st_ptr(stname, stype, sname, penum, preloc)\
gs__st_ptr(public_st, stname, stype, sname, penum, preloc)
#define gs_private_st_ptr(stname, stype, sname, penum, preloc)\
gs__st_ptr(private_st, stname, stype, sname, penum, preloc)
#define gs__st_ptrs1(scope_st, stname, stype, sname, penum, preloc, e1)\
BASIC_PTRS(penum) {\
GC_OBJ_ELT(stype, e1)\
};\
gs__st_basic(scope_st, stname, stype, sname, penum, preloc)
#define gs_public_st_ptrs1(stname, stype, sname, penum, preloc, e1)\
gs__st_ptrs1(public_st, stname, stype, sname, penum, preloc, e1)
#define gs_private_st_ptrs1(stname, stype, sname, penum, preloc, e1)\
gs__st_ptrs1(private_st, stname, stype, sname, penum, preloc, e1)
#define gs__st_strings1(scope_st, stname, stype, sname, penum, preloc, e1)\
BASIC_PTRS(penum) {\
GC_STRING_ELT(stype, e1)\
};\
gs__st_basic(scope_st, stname, stype, sname, penum, preloc)
#define gs_public_st_strings1(stname, stype, sname, penum, preloc, e1)\
gs__st_strings1(public_st, stname, stype, sname, penum, preloc, e1)
#define gs_private_st_strings1(stname, stype, sname, penum, preloc, e1)\
gs__st_strings1(private_st, stname, stype, sname, penum, preloc, e1)
#define gs__st_const_strings1(scope_st, stname, stype, sname, penum, preloc, e1)\
BASIC_PTRS(penum) {\
GC_CONST_STRING_ELT(stype, e1)\
};\
gs__st_basic(scope_st, stname, stype, sname, penum, preloc)
#define gs_public_st_const_strings1(stname, stype, sname, penum, preloc, e1)\
gs__st_const_strings1(public_st, stname, stype, sname, penum, preloc, e1)
#define gs_private_st_const_strings1(stname, stype, sname, penum, preloc, e1)\
gs__st_const_strings1(private_st, stname, stype, sname, penum, preloc, e1)
#define gs__st_const_strings1_ptrs1(scope_st, stname, stype, sname, penum, preloc, e1, e2)\
BASIC_PTRS(penum) {\
GC_CONST_STRING_ELT(stype, e1), GC_OBJ_ELT(stype, e2)\
};\
gs__st_basic(scope_st, stname, stype, sname, penum, preloc)
#define gs_public_st_const_strings1_ptrs1(stname, stype, sname, penum, preloc, e1, e2)\
gs__st_const_strings1_ptrs1(public_st, stname, stype, sname, penum, preloc, e1, e2)
#define gs_private_st_const_strings1_ptrs1(stname, stype, sname, penum, preloc, e1, e2)\
gs__st_const_strings1_ptrs1(private_st, stname, stype, sname, penum, preloc, e1, e2)
#define gs__st_const_strings2(scope_st, stname, stype, sname, penum, preloc, e1, e2)\
BASIC_PTRS(penum) {\
GC_CONST_STRING_ELT(stype, e1), GC_CONST_STRING_ELT(stype, e2)\
};\
gs__st_basic(scope_st, stname, stype, sname, penum, preloc)
#define gs_public_st_const_strings2(stname, stype, sname, penum, preloc, e1, e2)\
gs__st_const_strings2(public_st, stname, stype, sname, penum, preloc, e1, e2)
#define gs_private_st_const_strings2(stname, stype, sname, penum, preloc, e1, e2)\
gs__st_const_strings2(private_st, stname, stype, sname, penum, preloc, e1, e2)
#define gs__st_ptrs2(scope_st, stname, stype, sname, penum, preloc, e1, e2)\
BASIC_PTRS(penum) {\
GC_OBJ_ELT2(stype, e1, e2)\
};\
gs__st_basic(scope_st, stname, stype, sname, penum, preloc)
#define gs_public_st_ptrs2(stname, stype, sname, penum, preloc, e1, e2)\
gs__st_ptrs2(public_st, stname, stype, sname, penum, preloc, e1, e2)
#define gs_private_st_ptrs2(stname, stype, sname, penum, preloc, e1, e2)\
gs__st_ptrs2(private_st, stname, stype, sname, penum, preloc, e1, e2)
#define gs__st_ptrs3(scope_st, stname, stype, sname, penum, preloc, e1, e2, e3)\
BASIC_PTRS(penum) {\
GC_OBJ_ELT3(stype, e1, e2, e3)\
};\
gs__st_basic(scope_st, stname, stype, sname, penum, preloc)
#define gs_public_st_ptrs3(stname, stype, sname, penum, preloc, e1, e2, e3)\
gs__st_ptrs3(public_st, stname, stype, sname, penum, preloc, e1, e2, e3)
#define gs_private_st_ptrs3(stname, stype, sname, penum, preloc, e1, e2, e3)\
gs__st_ptrs3(private_st, stname, stype, sname, penum, preloc, e1, e2, e3)
#define gs__st_ptrs4(scope_st, stname, stype, sname, penum, preloc, e1, e2, e3, e4)\
BASIC_PTRS(penum) {\
GC_OBJ_ELT3(stype, e1, e2, e3), GC_OBJ_ELT(stype, e4)\
};\
gs__st_basic(scope_st, stname, stype, sname, penum, preloc)
#define gs_public_st_ptrs4(stname, stype, sname, penum, preloc, e1, e2, e3, e4)\
gs__st_ptrs4(public_st, stname, stype, sname, penum, preloc, e1, e2, e3, e4)
#define gs_private_st_ptrs4(stname, stype, sname, penum, preloc, e1, e2, e3, e4)\
gs__st_ptrs4(private_st, stname, stype, sname, penum, preloc, e1, e2, e3, e4)
#define gs__st_ptrs5(scope_st, stname, stype, sname, penum, preloc, e1, e2, e3, e4, e5)\
BASIC_PTRS(penum) {\
GC_OBJ_ELT3(stype, e1, e2, e3), GC_OBJ_ELT2(stype, e4, e5)\
};\
gs__st_basic(scope_st, stname, stype, sname, penum, preloc)
#define gs_public_st_ptrs5(stname, stype, sname, penum, preloc, e1, e2, e3, e4, e5)\
gs__st_ptrs5(public_st, stname, stype, sname, penum, preloc, e1, e2, e3, e4, e5)
#define gs_private_st_ptrs5(stname, stype, sname, penum, preloc, e1, e2, e3, e4, e5)\
gs__st_ptrs5(private_st, stname, stype, sname, penum, preloc, e1, e2, e3, e4, e5)
#define gs__st_ptrs6(scope_st, stname, stype, sname, penum, preloc, e1, e2, e3, e4, e5, e6)\
BASIC_PTRS(penum) {\
GC_OBJ_ELT3(stype, e1, e2, e3), GC_OBJ_ELT3(stype, e4, e5, e6)\
};\
gs__st_basic(scope_st, stname, stype, sname, penum, preloc)
#define gs_public_st_ptrs6(stname, stype, sname, penum, preloc, e1, e2, e3, e4, e5, e6)\
gs__st_ptrs6(public_st, stname, stype, sname, penum, preloc, e1, e2, e3, e4, e5, e6)
#define gs_private_st_ptrs6(stname, stype, sname, penum, preloc, e1, e2, e3, e4, e5, e6)\
gs__st_ptrs6(private_st, stname, stype, sname, penum, preloc, e1, e2, e3, e4, e5, e6)
#define gs__st_ptrs7(scope_st, stname, stype, sname, penum, preloc, e1, e2, e3, e4, e5, e6, e7)\
BASIC_PTRS(penum) {\
GC_OBJ_ELT3(stype, e1, e2, e3), GC_OBJ_ELT3(stype, e4, e5, e6), GC_OBJ_ELT(stype, e7)\
};\
gs__st_basic(scope_st, stname, stype, sname, penum, preloc)
#define gs_public_st_ptrs7(stname, stype, sname, penum, preloc, e1, e2, e3, e4, e5, e6, e7)\
gs__st_ptrs7(public_st, stname, stype, sname, penum, preloc, e1, e2, e3, e4, e5, e6, e7)
#define gs_private_st_ptrs7(stname, stype, sname, penum, preloc, e1, e2, e3, e4, e5, e6, e7)\
gs__st_ptrs7(private_st, stname, stype, sname, penum, preloc, e1, e2, e3, e4, e5, e6, e7)
#define gs__st_strings1_ptrs7(scope_st, stname, stype, sname, penum, preloc, e1, e2, e3, e4, e5, e6, e7, e8)\
BASIC_PTRS(penum) {\
GC_CONST_STRING_ELT(stype, e1),\
GC_OBJ_ELT3(stype, e2, e3, e4), GC_OBJ_ELT3(stype, e5, e6, e7), GC_OBJ_ELT(stype, e8)\
};\
gs__st_basic(scope_st, stname, stype, sname, penum, preloc)
#define gs_public_st_strings1_ptrs7(stname, stype, sname, penum, preloc, e1, e2, e3, e4, e5, e6, e7, e8)\
gs__st_strings1_ptrs7(public_st, stname, stype, sname, penum, preloc, e1, e2, e3, e4, e5, e6, e7, e8)
#define gs_private_st_strings1_ptrs7(stname, stype, sname, penum, preloc, e1, e2, e3, e4, e5, e6, e7, e8)\
gs__st_strings1_ptrs7(private_st, stname, stype, sname, penum, preloc, e1, e2, e3, e4, e5, e6, e7, e8)
#define gs__st_suffix_add0(scope_st, stname, stype, sname, penum, preloc, supstname)\
gs__st_basic_with_super_final(scope_st, stname, stype, sname, 0, 0, preloc, &supstname, 0, 0)
#define gs_public_st_suffix_add0(stname, stype, sname, penum, preloc, supstname)\
gs__st_suffix_add0(public_st, stname, stype, sname, penum, preloc, supstname)
#define gs_private_st_suffix_add0(stname, stype, sname, penum, preloc, supstname)\
gs__st_suffix_add0(private_st, stname, stype, sname, penum, preloc, supstname)
#define gs__st_suffix_add0_local(scope_st, stname, stype, sname, supenum, supreloc, supstname)\
scope_st stname = {\
sizeof(stype), sname, 0, 0, basic_enum_ptrs, basic_reloc_ptrs,\
0, &supreloc\
}
#define gs_public_st_suffix_add0_local(stname, stype, sname, supenum, supreloc, supstname)\
gs__st_suffix_add0_local(public_st, stname, stype, sname, supenum, supreloc, supstname)
#define gs_private_st_suffix_add0_local(stname, stype, sname, supenum, supreloc, supstname)\
gs__st_suffix_add0_local(private_st, stname, stype, sname, supenum, supreloc, supstname)
#define gs__st_suffix_add0_final(scope_st, stname, stype, sname, penum, preloc, pfinal, supstname)\
private ENUM_PTRS_BEGIN_PROC(penum) {\
ENUM_PREFIX(supstname, 0);\
} ENUM_PTRS_END_PROC\
private RELOC_PTRS_BEGIN(preloc) {\
RELOC_PREFIX(supstname);\
} RELOC_PTRS_END\
gs__st_complex_only(scope_st, stname, stype, sname, 0, penum, preloc, pfinal)
#define gs_public_st_suffix_add0_final(stname, stype, sname, penum, preloc, pfinal, supstname)\
gs__st_suffix_add0_final(public_st, stname, stype, sname, penum, preloc, pfinal, supstname)
#define gs_private_st_suffix_add0_final(stname, stype, sname, penum, preloc, pfinal, supstname)\
gs__st_suffix_add0_final(private_st, stname, stype, sname, penum, preloc, pfinal, supstname)
#define gs__st_suffix_add1(scope_st, stname, stype, sname, penum, preloc, supstname, e1)\
BASIC_PTRS(penum) {\
GC_OBJ_ELT(stype, e1)\
};\
gs__st_basic_super(scope_st, stname, stype, sname, penum, preloc, &supstname, 0)
#define gs_public_st_suffix_add1(stname, stype, sname, penum, preloc, supstname, e1)\
gs__st_suffix_add1(public_st, stname, stype, sname, penum, preloc, supstname, e1)
#define gs_private_st_suffix_add1(stname, stype, sname, penum, preloc, supstname, e1)\
gs__st_suffix_add1(private_st, stname, stype, sname, penum, preloc, supstname, e1)
#define gs__st_suffix_add1_final(scope_st, stname, stype, sname, penum, preloc, pfinal, supstname, e1)\
BASIC_PTRS(penum) {\
GC_OBJ_ELT(stype, e1)\
};\
gs__st_basic_super_final(scope_st, stname, stype, sname, penum, preloc, &supstname, 0, pfinal)
#define gs_public_st_suffix_add1_final(stname, stype, sname, penum, preloc, pfinal, supstname, e1)\
gs__st_suffix_add1_final(public_st, stname, stype, sname, penum, preloc, pfinal, supstname, e1)
#define gs_private_st_suffix_add1_final(stname, stype, sname, penum, preloc, pfinal, supstname, e1)\
gs__st_suffix_add1_final(private_st, stname, stype, sname, penum, preloc, pfinal, supstname, e1)
#define gs__st_suffix_add_strings1(scope_st, stname, stype, sname, penum, preloc, supstname, e1)\
BASIC_PTRS(penum) {\
GC_STRING_ELT(stype, e1)\
};\
gs__st_basic_super(scope_st, stname, stype, sname, penum, preloc, &supstname, 0)
#define gs_public_st_suffix_add_strings1(stname, stype, sname, penum, preloc, supstname, e1)\
gs__st_suffix_add_strings1(public_st, stname, stype, sname, penum, preloc, supstname, e1)
#define gs_private_st_suffix_add_strings1(stname, stype, sname, penum, preloc, supstname, e1)\
gs__st_suffix_add_strings1(private_st, stname, stype, sname, penum, preloc, supstname, e1)
#define gs__st_suffix_add2(scope_st, stname, stype, sname, penum, preloc, supstname, e1, e2)\
BASIC_PTRS(penum) {\
GC_OBJ_ELT2(stype, e1, e2)\
};\
gs__st_basic_super(scope_st, stname, stype, sname, penum, preloc, &supstname, 0)
#define gs_public_st_suffix_add2(stname, stype, sname, penum, preloc, supstname, e1, e2)\
gs__st_suffix_add2(public_st, stname, stype, sname, penum, preloc, supstname, e1, e2)
#define gs_private_st_suffix_add2(stname, stype, sname, penum, preloc, supstname, e1, e2)\
gs__st_suffix_add2(private_st, stname, stype, sname, penum, preloc, supstname, e1, e2)
#define gs__st_suffix_add2_string1(scope_st, stname, stype, sname, penum, preloc, supstname, e1, e2, e3)\
BASIC_PTRS(penum) {\
GC_OBJ_ELT2(stype, e1, e2),\
GC_STRING_ELT(stype, e3)\
};\
gs__st_basic_super(scope_st, stname, stype, sname, penum, preloc, &supstname, 0)
#define gs_public_st_suffix_add2_string1(stname, stype, sname, penum, preloc, supstname, e1, e2, e3)\
gs__st_suffix_add2_string1(public_st, stname, stype, sname, penum, preloc, supstname, e1, e2, e3)
#define gs_private_st_suffix_add2_string1(stname, stype, sname, penum, preloc, supstname, e1, e2, e3)\
gs__st_suffix_add2_string1(private_st, stname, stype, sname, penum, preloc, supstname, e1, e2, e3)
#define gs__st_suffix_add2_final(scope_st, stname, stype, sname, penum, preloc, pfinal, supstname, e1, e2)\
BASIC_PTRS(penum) {\
GC_OBJ_ELT2(stype, e1, e2)\
};\
gs__st_basic_super_final(scope_st, stname, stype, sname, penum, preloc, &supstname, 0, pfinal)
#define gs_public_st_suffix_add2_final(stname, stype, sname, penum, preloc, pfinal, supstname, e1, e2)\
gs__st_suffix_add2_final(public_st, stname, stype, sname, penum, preloc, pfinal, supstname, e1, e2)
#define gs_private_st_suffix_add2_final(stname, stype, sname, penum, preloc, pfinal, supstname, e1, e2)\
gs__st_suffix_add2_final(private_st, stname, stype, sname, penum, preloc, pfinal, supstname, e1, e2)
#define gs__st_suffix_add3(scope_st, stname, stype, sname, penum, preloc, supstname, e1, e2, e3)\
BASIC_PTRS(penum) {\
GC_OBJ_ELT3(stype, e1, e2, e3)\
};\
gs__st_basic_super(scope_st, stname, stype, sname, penum, preloc, &supstname, 0)
#define gs_public_st_suffix_add3(stname, stype, sname, penum, preloc, supstname, e1, e2, e3)\
gs__st_suffix_add3(public_st, stname, stype, sname, penum, preloc, supstname, e1, e2, e3)
#define gs_private_st_suffix_add3(stname, stype, sname, penum, preloc, supstname, e1, e2, e3)\
gs__st_suffix_add3(private_st, stname, stype, sname, penum, preloc, supstname, e1, e2, e3)
#define gs__st_suffix_add3_string1(scope_st, stname, stype, sname, penum, preloc, supstname, e1, e2, e3, e4)\
BASIC_PTRS(penum) {\
GC_OBJ_ELT3(stype, e1, e2, e3),\
GC_STRING_ELT(stype, e4)\
};\
gs__st_basic_super(scope_st, stname, stype, sname, penum, preloc, &supstname, 0)
#define gs_public_st_suffix_add3_string1(stname, stype, sname, penum, preloc, supstname, e1, e2, e3, e4)\
gs__st_suffix_add3_string1(public_st, stname, stype, sname, penum, preloc, supstname, e1, e2, e3, e4)
#define gs_private_st_suffix_add3_string1(stname, stype, sname, penum, preloc, supstname, e1, e2, e3, e4)\
gs__st_suffix_add3_string1(private_st, stname, stype, sname, penum, preloc, supstname, e1, e2, e3, e4)
#define gs__st_suffix_add3_final(scope_st, stname, stype, sname, penum, preloc, pfinal, supstname, e1, e2, e3)\
BASIC_PTRS(penum) {\
GC_OBJ_ELT3(stype, e1, e2, e3)\
};\
gs__st_basic_super_final(scope_st, stname, stype, sname, penum, preloc, &supstname, 0, pfinal)
#define gs_public_st_suffix_add3_final(stname, stype, sname, penum, preloc, pfinal, supstname, e1, e2, e3)\
gs__st_suffix_add3_final(public_st, stname, stype, sname, penum, preloc, pfinal, supstname, e1, e2, e3)
#define gs_private_st_suffix_add3_final(stname, stype, sname, penum, preloc, pfinal, supstname, e1, e2, e3)\
gs__st_suffix_add3_final(private_st, stname, stype, sname, penum, preloc, pfinal, supstname, e1, e2, e3)
#define gs__st_suffix_add4(scope_st, stname, stype, sname, penum, preloc, supstname, e1, e2, e3, e4)\
BASIC_PTRS(penum) {\
GC_OBJ_ELT3(stype, e1, e2, e3), GC_OBJ_ELT(stype, e4)\
};\
gs__st_basic_super(scope_st, stname, stype, sname, penum, preloc, &supstname, 0)
#define gs_public_st_suffix_add4(stname, stype, sname, penum, preloc, supstname, e1, e2, e3, e4)\
gs__st_suffix_add4(public_st, stname, stype, sname, penum, preloc, supstname, e1, e2, e3, e4)
#define gs_private_st_suffix_add4(stname, stype, sname, penum, preloc, supstname, e1, e2, e3, e4)\
gs__st_suffix_add4(private_st, stname, stype, sname, penum, preloc, supstname, e1, e2, e3, e4)
#define gs__st_suffix_add4_final(scope_st, stname, stype, sname, penum, preloc, pfinal, supstname, e1, e2, e3, e4)\
BASIC_PTRS(penum) {\
GC_OBJ_ELT3(stype, e1, e2, e3), GC_OBJ_ELT(stype, e4)\
};\
gs__st_basic_super_final(scope_st, stname, stype, sname, penum, preloc, &supstname, 0, pfinal)
#define gs_public_st_suffix_add4_final(stname, stype, sname, penum, preloc, pfinal, supstname, e1, e2, e3, e4)\
gs__st_suffix_add4_final(public_st, stname, stype, sname, penum, preloc, pfinal, supstname, e1, e2, e3, e4)
#define gs_private_st_suffix_add4_final(stname, stype, sname, penum, preloc, pfinal, supstname, e1, e2, e3, e4)\
gs__st_suffix_add4_final(private_st, stname, stype, sname, penum, preloc, pfinal, supstname, e1, e2, e3, e4)
#define gs__st_suffix_add5(scope_st, stname, stype, sname, penum, preloc, supstname, e1, e2, e3, e4, e5)\
BASIC_PTRS(penum) {\
GC_OBJ_ELT3(stype, e1, e2, e3), GC_OBJ_ELT2(stype, e4, e5)\
};\
gs__st_basic_super(scope_st, stname, stype, sname, penum, preloc, &supstname, 0)
#define gs_public_st_suffix_add5(stname, stype, sname, penum, preloc, supstname, e1, e2, e3, e4, e5)\
gs__st_suffix_add5(public_st, stname, stype, sname, penum, preloc, supstname, e1, e2, e3, e4, e5)
#define gs_private_st_suffix_add5(stname, stype, sname, penum, preloc, supstname, e1, e2, e3, e4, e5)\
gs__st_suffix_add5(private_st, stname, stype, sname, penum, preloc, supstname, e1, e2, e3, e4, e5)
#define gs__st_suffix_add6(scope_st, stname, stype, sname, penum, preloc, supstname, e1, e2, e3, e4, e5, e6)\
BASIC_PTRS(penum) {\
GC_OBJ_ELT3(stype, e1, e2, e3), GC_OBJ_ELT3(stype, e4, e5, e6)\
};\
gs__st_basic_super(scope_st, stname, stype, sname, penum, preloc, &supstname, 0)
#define gs_public_st_suffix_add6(stname, stype, sname, penum, preloc, supstname, e1, e2, e3, e4, e5, e6)\
gs__st_suffix_add6(public_st, stname, stype, sname, penum, preloc, supstname, e1, e2, e3, e4, e5, e6)
#define gs_private_st_suffix_add6(stname, stype, sname, penum, preloc, supstname, e1, e2, e3, e4, e5, e6)\
gs__st_suffix_add6(private_st, stname, stype, sname, penum, preloc, supstname, e1, e2, e3, e4, e5, e6)
#define gs__st_suffix_add7(scope_st, stname, stype, sname, penum, preloc, supstname, e1, e2, e3, e4, e5, e6, e7)\
BASIC_PTRS(penum) {\
GC_OBJ_ELT3(stype, e1, e2, e3), GC_OBJ_ELT3(stype, e4, e5, e6),\
GC_OBJ_ELT(stype, e7)\
};\
gs__st_basic_super(scope_st, stname, stype, sname, penum, preloc, &supstname, 0)
#define gs_public_st_suffix_add7(stname, stype, sname, penum, preloc, supstname, e1, e2, e3, e4, e5, e6, e7)\
gs__st_suffix_add7(public_st, stname, stype, sname, penum, preloc, supstname, e1, e2, e3, e4, e5, e6, e7)
#define gs_private_st_suffix_add7(stname, stype, sname, penum, preloc, supstname, e1, e2, e3, e4, e5, e6, e7)\
gs__st_suffix_add7(private_st, stname, stype, sname, penum, preloc, supstname, e1, e2, e3, e4, e5, e6, e7)
#define gs__st_suffix_add8(scope_st, stname, stype, sname, penum, preloc, supstname, e1, e2, e3, e4, e5, e6, e7, e8)\
BASIC_PTRS(penum) {\
GC_OBJ_ELT3(stype, e1, e2, e3), GC_OBJ_ELT3(stype, e4, e5, e6),\
GC_OBJ_ELT2(stype, e7, e8)\
};\
gs__st_basic_super(scope_st, stname, stype, sname, penum, preloc, &supstname, 0)
#define gs_public_st_suffix_add8(stname, stype, sname, penum, preloc, supstname, e1, e2, e3, e4, e5, e6, e7, e8)\
gs__st_suffix_add8(public_st, stname, stype, sname, penum, preloc, supstname, e1, e2, e3, e4, e5, e6, e7, e8)
#define gs_private_st_suffix_add8(stname, stype, sname, penum, preloc, supstname, e1, e2, e3, e4, e5, e6, e7, e8)\
gs__st_suffix_add8(private_st, stname, stype, sname, penum, preloc, supstname, e1, e2, e3, e4, e5, e6, e7, e8)
#define gs__st_suffix_add9(scope_st, stname, stype, sname, penum, preloc, supstname, e1, e2, e3, e4, e5, e6, e7, e8, e9)\
BASIC_PTRS(penum) {\
GC_OBJ_ELT3(stype, e1, e2, e3), GC_OBJ_ELT3(stype, e4, e5, e6),\
GC_OBJ_ELT3(stype, e7, e8, e9)\
};\
gs__st_basic_super(scope_st, stname, stype, sname, penum, preloc, &supstname, 0)
#define gs_public_st_suffix_add9(stname, stype, sname, penum, preloc, supstname, e1, e2, e3, e4, e5, e6, e7, e8, e9)\
gs__st_suffix_add9(public_st, stname, stype, sname, penum, preloc, supstname, e1, e2, e3, e4, e5, e6, e7, e8, e9)
#define gs_private_st_suffix_add9(stname, stype, sname, penum, preloc, supstname, e1, e2, e3, e4, e5, e6, e7, e8, e9)\
gs__st_suffix_add9(private_st, stname, stype, sname, penum, preloc, supstname, e1, e2, e3, e4, e5, e6, e7, e8, e9)
#define gs__st_suffix_add10(scope_st, stname, stype, sname, penum, preloc, supstname, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10)\
BASIC_PTRS(penum) {\
GC_OBJ_ELT3(stype, e1, e2, e3), GC_OBJ_ELT3(stype, e4, e5, e6),\
GC_OBJ_ELT3(stype, e7, e8, e9), GC_OBJ_ELT(stype, e10)\
};\
gs__st_basic_super(scope_st, stname, stype, sname, penum, preloc, &supstname, 0)
#define gs_public_st_suffix_add10(stname, stype, sname, penum, preloc, supstname, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10)\
gs__st_suffix_add10(public_st, stname, stype, sname, penum, preloc, supstname, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10)
#define gs_private_st_suffix_add10(stname, stype, sname, penum, preloc, supstname, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10)\
gs__st_suffix_add10(private_st, stname, stype, sname, penum, preloc, supstname, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10)
#define gs__st_suffix_add11(scope_st, stname, stype, sname, penum, preloc, supstname, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11)\
BASIC_PTRS(penum) {\
GC_OBJ_ELT3(stype, e1, e2, e3), GC_OBJ_ELT3(stype, e4, e5, e6),\
GC_OBJ_ELT3(stype, e7, e8, e9), GC_OBJ_ELT2(stype, e10, e11)\
};\
gs__st_basic_super(scope_st, stname, stype, sname, penum, preloc, &supstname, 0)
#define gs_public_st_suffix_add11(stname, stype, sname, penum, preloc, supstname, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11)\
gs__st_suffix_add11(public_st, stname, stype, sname, penum, preloc, supstname, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11)
#define gs_private_st_suffix_add11(stname, stype, sname, penum, preloc, supstname, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11)\
gs__st_suffix_add11(private_st, stname, stype, sname, penum, preloc, supstname, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11)
#define gs__st_ptrs_add0(scope_st, stname, stype, sname, penum, preloc, supstname, member)\
gs__st_basic_with_super_final(scope_st, stname, stype, sname, 0, 0, preloc, &supstname, offset_of(stype, member), 0)
#define gs_public_st_ptrs_add0(stname, stype, sname, penum, preloc, supstname, member)\
gs__st_ptrs_add0(public_st, stname, stype, sname, penum, preloc, supstname, member)
#define gs_private_st_ptrs_add0(stname, stype, sname, penum, preloc, supstname, member)\
gs__st_ptrs_add0(private_st, stname, stype, sname, penum, preloc, supstname, member)
#define gs__st_ptrs_add1(scope_st, stname, stype, sname, penum, preloc, supstname, member, e1)\
BASIC_PTRS(penum) {\
GC_OBJ_ELT(stype, e1)\
};\
gs__st_basic_super(scope_st, stname, stype, sname, penum, preloc, &supstname, offset_of(stype, member))
#define gs_public_st_ptrs_add1(stname, stype, sname, penum, preloc, supstname, member, e1)\
gs__st_ptrs_add1(public_st, stname, stype, sname, penum, preloc, supstname, member, e1)
#define gs_private_st_ptrs_add1(stname, stype, sname, penum, preloc, supstname, member, e1)\
gs__st_ptrs_add1(private_st, stname, stype, sname, penum, preloc, supstname, member, e1)
#define gs__st_ptrs_add2(scope_st, stname, stype, sname, penum, preloc, supstname, member, e1, e2)\
BASIC_PTRS(penum) {\
GC_OBJ_ELT2(stype, e1, e2)\
};\
gs__st_basic_super(scope_st, stname, stype, sname, penum, preloc, &supstname, offset_of(stype, member))
#define gs_public_st_ptrs_add2(stname, stype, sname, penum, preloc, supstname, member, e1, e2)\
gs__st_ptrs_add2(public_st, stname, stype, sname, penum, preloc, supstname, member, e1, e2)
#define gs_private_st_ptrs_add2(stname, stype, sname, penum, preloc, supstname, member, e1, e2)\
gs__st_ptrs_add2(private_st, stname, stype, sname, penum, preloc, supstname, member, e1, e2)
#endif