#ifndef gxcid_INCLUDED
#  define gxcid_INCLUDED
#include "gsstype.h"
#ifndef gs_cid_system_info_DEFINED
#  define gs_cid_system_info_DEFINED
typedef struct gs_cid_system_info_s gs_cid_system_info_t;
#endif
struct gs_cid_system_info_s {
gs_const_string Registry;
gs_const_string Ordering;
int Supplement;
};
extern_st(st_cid_system_info);
extern_st(st_cid_system_info_element);
#define public_st_cid_system_info() \
gs_public_st_const_strings2(st_cid_system_info, gs_cid_system_info_t,\
"gs_cid_system_info_t", cid_si_enum_ptrs, cid_si_reloc_ptrs,\
Registry, Ordering)
#define st_cid_system_info_num_ptrs 2
#define public_st_cid_system_info_element() \
gs_public_st_element(st_cid_system_info_element, gs_cid_system_info_t,\
"gs_cid_system_info_t[]", cid_si_elt_enum_ptrs, cid_si_elt_reloc_ptrs,\
st_cid_system_info)
void cid_system_info_set_null(gs_cid_system_info_t *);
bool cid_system_info_is_null(const gs_cid_system_info_t *);
#endif