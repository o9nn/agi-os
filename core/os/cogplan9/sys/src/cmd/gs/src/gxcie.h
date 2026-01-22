#ifndef gxcie_INCLUDED
#  define gxcie_INCLUDED
#include "gscie.h"
cs_proc_init_color(gx_init_CIE);
cs_proc_restrict_color(gx_restrict_CIEDEFG);
cs_proc_install_cspace(gx_install_CIEDEFG);
cs_proc_restrict_color(gx_restrict_CIEDEF);
cs_proc_install_cspace(gx_install_CIEDEF);
cs_proc_restrict_color(gx_restrict_CIEABC);
cs_proc_install_cspace(gx_install_CIEABC);
cs_proc_restrict_color(gx_restrict_CIEA);
cs_proc_install_cspace(gx_install_CIEA);
extern	int	gx_cie_to_xyz_alloc(gs_imager_state **,
const gs_color_space *, gs_memory_t *);
extern	void	gx_cie_to_xyz_free(gs_imager_state *);
#define CIE_CHECK_RENDERING(pcs, pconc, pis, do_exit)                   \
BEGIN                                                               \
if (pis->cie_render == 0) {                                     \
\
pconc[0] = pconc[1] = pconc[2] = frac_0;                    \
do_exit;                                                    \
}                                                               \
if (pis->cie_joint_caches->status != CIE_JC_STATUS_COMPLETED) { \
int     code = gs_cie_jc_complete(pis, pcs);                \
\
if (code < 0)                                               \
return code;                                            \
}                                                               \
END
extern  int     gx_cie_remap_finish( cie_cached_vector3,
frac *,
const gs_imager_state *,
const gs_color_space * );
extern GX_CIE_REMAP_FINISH_PROC(gx_cie_remap_finish);
extern GX_CIE_REMAP_FINISH_PROC(gx_cie_real_remap_finish);
extern GX_CIE_REMAP_FINISH_PROC(gx_cie_xyz_remap_finish);
cs_proc_concretize_color(gx_concretize_CIEDEFG);
cs_proc_concretize_color(gx_concretize_CIEDEF);
cs_proc_concretize_color(gx_concretize_CIEABC);
cs_proc_remap_color(gx_remap_CIEABC);
cs_proc_concretize_color(gx_concretize_CIEA);
extern_st(st_cie_common);
extern_st(st_cie_common_elements_t);
extern  void    gx_set_common_cie_defaults( gs_cie_common *,
void *  client_data );
extern  void    gx_cie_load_common_cache(gs_cie_common *, gs_state *);
extern  void    gx_cie_common_complete(gs_cie_common *);
cs_proc_install_cspace(gx_install_CIE);
extern  void *  gx_build_cie_space( gs_color_space **           ppcspace,
const gs_color_space_type * pcstype,
gs_memory_type_ptr_t        stype,
gs_memory_t *               pmem );
cs_proc_concrete_space(gx_concrete_space_CIE);
#endif