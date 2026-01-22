#ifndef igstate_INCLUDED
# define igstate_INCLUDED
#include "gsstate.h"
#include "gxstate.h"
#include "imemory.h"
#include "istruct.h"
#include "gxcindex.h"
typedef struct igstate_obj_s {
ref gstate;
} igstate_obj;
extern_st(st_igstate_obj);
#define public_st_igstate_obj() \
gs_public_st_ref_struct(st_igstate_obj, igstate_obj, "gstatetype")
#define igstate_ptr(rp) r_ptr(&r_ptr(rp, igstate_obj)->gstate, gs_state)
typedef struct ref_device_n_params_s {
ref layer_names, tint_transform;
} ref_device_n_params;
typedef struct ref_cie_procs_s {
union {
ref DEFG;
ref DEF;
} PreDecode;
union {
ref ABC;
ref A;
} Decode;
ref DecodeLMN;
} ref_cie_procs;
typedef struct ref_cie_render_procs_s {
ref TransformPQR, EncodeLMN, EncodeABC, RenderTableT;
} ref_cie_render_procs;
typedef struct ref_separation_params_s {
ref layer_name, tint_transform;
} ref_separation_params;
typedef struct ref_color_procs_s {
ref_cie_procs cie;
union {
ref_device_n_params device_n;
ref_separation_params separation;
ref index_proc;
} special;
} ref_color_procs;
typedef struct ref_colorspace_s {
ref array;
ref_color_procs procs;
} ref_colorspace;
#ifndef int_remap_color_info_DEFINED
# define int_remap_color_info_DEFINED
typedef struct int_remap_color_info_s int_remap_color_info_t;
#endif
typedef struct int_gstate_s {
ref dash_pattern;
struct {
ref red, green, blue, gray;
} screen_procs,
transfer_procs;
ref black_generation;
ref undercolor_removal;
ref_colorspace colorspace;
ref pattern;
struct {
ref dict;
ref_cie_render_procs procs;
} colorrendering;
ref use_cie_color;
ref halftone;
ref pagedevice;
ref remap_color_info;
ref opacity_mask, shape_mask;
} int_gstate;
#define clear_pagedevice(pigs) make_null(&(pigs)->pagedevice)
#define private_st_int_gstate() \
gs_private_st_ref_struct(st_int_gstate, int_gstate, "int_gstate")
#define int_gstate_map_refs(p,m)\
{ register ref *rp_ = (ref *)(p);\
register int i = sizeof(int_gstate) / sizeof(ref);\
do { m(rp_); ++rp_; } while ( --i );\
}
gs_state *int_gstate_alloc(const gs_dual_memory_t * dmem);
#define gs_int_gstate(pgs) ((int_gstate *)gs_state_client_data(pgs))
#define igs (i_ctx_p->pgs)
#define istate gs_int_gstate(igs)
#endif