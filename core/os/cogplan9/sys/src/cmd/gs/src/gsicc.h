#ifndef gsicc_INCLUDED
#  define gsicc_INCLUDED
#include "gscie.h"
struct _icc;
struct _icmLuBase;
struct gs_cie_icc_s {
gs_cie_common_elements;
uint                num_components;
gs_range4           Range;
unsigned short      file_id;
stream *            instrp;
bool                pcs_is_cielab;
struct _icc *       picc;
struct _icmLuBase * plu;
struct _icmFile   * pfile;
};
#define private_st_cie_icc()                \
gs_private_st_suffix_add1_final( st_cie_icc,              \
gs_cie_icc,              \
"gs_cie_icc",            \
cie_icc_enum_ptrs,       \
cie_icc_reloc_ptrs,      \
cie_icc_finalize,        \
st_cie_common_elements_t,\
instrp )
extern  int     gs_cspace_build_CIEICC( gs_color_space **   ppcspace,
void *              client_data,
gs_memory_t *       pmem );
int
gx_load_icc_profile(gs_cie_icc *picc_info);
void
gx_increment_cspace_count(const gs_color_space * pcs);
#endif