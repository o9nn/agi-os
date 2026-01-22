#ifndef inamedef_INCLUDED
#  define inamedef_INCLUDED
#include "inameidx.h"
#include "inamestr.h"
#include "inames.h"
#include "gsstruct.h"
#define max_name_extension_bits 6
#if EXTEND_NAMES > max_name_extension_bits
#  undef EXTEND_NAMES
#  define EXTEND_NAMES max_name_extension_bits
#endif
#define max_name_index (uint)((0x10000 << EXTEND_NAMES) - 1)
#define max_name_count max_name_index
struct name_s {
#define pv_no_defn ((ref *)0)
#define pv_other ((ref *)1)
#define pv_valid(pvalue) ((unsigned long)(pvalue) > 1)
ref *pvalue;
};
#define nt_log2_sub_size NT_LOG2_SUB_SIZE
# define nt_sub_size (1 << nt_log2_sub_size)
# define nt_sub_index_mask (nt_sub_size - 1)
typedef struct name_sub_table_s {
name names[NT_SUB_SIZE];
#ifdef EXTEND_NAMES
uint high_index;
#endif
} name_sub_table;
struct name_table_s {
uint free;
uint sub_next;
uint perm_count;
uint sub_count;
uint max_sub_count;
uint name_string_attrs;
gs_memory_t *memory;
uint hash[NT_HASH_SIZE];
struct sub_ {
name_sub_table *names;
name_string_sub_table_t *strings;
} sub[max_name_index / nt_sub_size + 1];
};
#define names_index_string_inline(nt, nidx)\
((nt)->sub[(nidx) >> nt_log2_sub_size].strings->strings +\
((nidx) & nt_sub_index_mask))
#define names_string_inline(nt, pnref)\
names_index_string_inline(nt, names_index_inline(nt, pnref))
#if EXTEND_NAMES
#  define names_index_inline(nt_ignored, pnref)\
( ((const name_sub_table *)\
((pnref)->value.pname - (r_size(pnref) & nt_sub_index_mask)))->high_index + r_size(pnref) )
#else
#  define names_index_inline(nt_ignored, pnref) r_size(pnref)
#endif
#define names_index(nt_ignored, pnref) names_index_inline(nt_ignored, pnref)
#define names_index_ptr_inline(nt, nidx)\
((nt)->sub[(nidx) >> nt_log2_sub_size].names->names +\
((nidx) & nt_sub_index_mask))
#define names_index_ref_inline(nt, nidx, pnref)\
make_name(pnref, nidx, names_index_ptr_inline(nt, nidx));
#define name_index_inline(pnref) names_index_inline(ignored, pnref)
#define name_index_ptr_inline(nt, pnref) names_index_ptr_inline(nt, pnref)
#define name_index_ref_inline(nt, nidx, pnref)\
names_index_ref_inline(nt, nidx, pnref)
#define make_name(pnref, nidx, pnm)\
make_tasv(pnref, t_name, avm_system, (ushort)(nidx), pname, pnm)
void names_unmark_all(name_table * nt);
void names_trace_finish(name_table * nt, gc_state_t * gcst);
#ifndef alloc_save_t_DEFINED
typedef struct alloc_save_s alloc_save_t;
#  define alloc_save_t_DEFINED
#endif
void names_restore(name_table * nt, alloc_save_t * save);
#endif