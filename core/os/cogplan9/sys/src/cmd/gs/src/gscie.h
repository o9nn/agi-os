#ifndef gscie_INCLUDED
#  define gscie_INCLUDED
#include "gconfigv.h"
#include "gsrefct.h"
#include "gsstype.h"
#include "gstypes.h"
#include "gxctable.h"
#ifndef CIE_LOG2_CACHE_SIZE
#  define CIE_LOG2_CACHE_SIZE 9
#endif
#if USE_FPU < 0
#  define CIE_CACHE_USE_FIXED
#endif
#define CIE_FIXED_FRACTION_BITS 12
#define CIE_CACHE_INTERPOLATE
#define CIE_INTERPOLATE_THRESHOLD 0.001
#define CIE_RENDER_TABLE_INTERPOLATE
#define gx_cie_log2_cache_size CIE_LOG2_CACHE_SIZE
#define gx_cie_cache_size (1 << gx_cie_log2_cache_size)
#ifndef CIE_FIXED_FRACTION_BITS
#  define CIE_FIXED_FRACTION_BITS\
((arch_sizeof_long * 8 - gx_cie_log2_cache_size) / 2 - 1)
#endif
#ifdef CIE_RENDER_TABLE_INTERPOLATE
#  define CIE_CACHE_INTERPOLATE
#endif
#define float_lshift(v, nb) ((v) * (1L << (nb)))
#define float_rshift(v, nb) ((v) * (1.0 / (1L << (nb))))
#ifdef CIE_CACHE_INTERPOLATE
#  define _cie_interpolate_bits\
min(arch_sizeof_int * 8 - gx_cie_log2_cache_size - 2, 10)
#  define _cix(i) ((i) >> _cie_interpolate_bits)
#  define _cif(i) ((int)(i) & ((1 << _cie_interpolate_bits) - 1))
#  define cie_interpolate_between(v0, v1, i)\
((v0) + cie_cached_rshift(((v1) - (v0)) * _cif(i) +\
(1 << (_cie_interpolate_bits - 1)),\
_cie_interpolate_bits))
#  define cie_interpolate(p, i)\
cie_interpolate_between((p)[_cix(i)], (p)[_cix(i) + 1], i)
#  define cie_interpolate_fracs(p, i)\
((p)[_cix(i)] +\
(frac)arith_rshift((long)((p)[_cix(i) + 1] - (p)[_cix(i)]) * _cif(i), _cie_interpolate_bits))
#else
#  define _cie_interpolate_bits 0
#  define cie_interpolate_between(v0, v1, i) (v0)
#  define cie_interpolate(p, i) ((p)[i])
#  define cie_interpolate_fracs(p, i) ((p)[i])
#endif
#ifdef CIE_CACHE_USE_FIXED
typedef long cie_cached_value;
#  define _cie_fixed_shift CIE_FIXED_FRACTION_BITS
#  define float2cie_cached(v)\
((cie_cached_value)float_lshift(v, _cie_fixed_shift))
#  define cie_cached2float(v)\
float_rshift(v, _cie_fixed_shift)
#  define cie_cached2int(v, fbits)\
arith_rshift(v, _cie_fixed_shift - (fbits))
#  define _cie_product_excess_bits\
(_cie_fixed_shift * 2 + gx_cie_log2_cache_size - (arch_sizeof_long * 8 - 1))
#  define cie_cached_product2int(v, factor, fbits)\
(_cie_product_excess_bits > 0 ?\
arith_rshift( (v) * arith_rshift(factor, _cie_product_excess_bits) +\
arith_rshift(v, _cie_product_excess_bits) *\
((factor) & ((1 << _cie_product_excess_bits) - 1)),\
_cie_fixed_shift * 2 - _cie_product_excess_bits - (fbits)) :\
arith_rshift((v) * (factor), _cie_fixed_shift * 2 - (fbits)))
#  define cie_cached_rshift(v, n) arith_rshift(v, n)
#else
typedef float cie_cached_value;
#  define float2cie_cached(v) (v)
#  define cie_cached2float(v) (v)
#  define cie_cached2int(v, fbits)\
((int)float_lshift(v, fbits))
#  define cie_cached_product2int(v, factor, fbits)\
((int)float_lshift((v) * (factor), fbits))
#  define cie_cached_rshift(v, n) float_rshift(v, n)
#endif
#ifndef gs_cie_render_DEFINED
#  define gs_cie_render_DEFINED
typedef struct gs_cie_render_s gs_cie_render;
#endif
typedef struct gs_vector3_s {
float u, v, w;
} gs_vector3;
typedef struct gs_matrix3_s {
gs_vector3 cu, cv, cw;
bool is_identity;
} gs_matrix3;
typedef gs_range_t gs_range;
typedef struct gs_range3_s {
gs_range ranges[3];
} gs_range3;
typedef struct gs_range4_s {
gs_range ranges[4];
} gs_range4;
typedef struct gs_cie_common_s gs_cie_common;
typedef struct gs_cie_wbsd_s gs_cie_wbsd;
typedef float (*gs_cie_a_proc) (floatp, const gs_cie_a *);
typedef float (*gs_cie_abc_proc) (floatp, const gs_cie_abc *);
typedef struct gs_cie_abc_proc3_s {
gs_cie_abc_proc procs[3];
} gs_cie_abc_proc3;
typedef float (*gs_cie_def_proc) (floatp, const gs_cie_def *);
typedef struct gs_cie_def_proc3_s {
gs_cie_def_proc procs[3];
} gs_cie_def_proc3;
typedef float (*gs_cie_defg_proc) (floatp, const gs_cie_defg *);
typedef struct gs_cie_defg_proc4_s {
gs_cie_defg_proc procs[4];
} gs_cie_defg_proc4;
typedef float (*gs_cie_common_proc) (floatp, const gs_cie_common *);
typedef struct gs_cie_common_proc3_s {
gs_cie_common_proc procs[3];
} gs_cie_common_proc3;
typedef float (*gs_cie_render_proc) (floatp, const gs_cie_render *);
typedef struct gs_cie_render_proc3_s {
gs_cie_render_proc procs[3];
} gs_cie_render_proc3;
typedef int (*gs_cie_transform_proc)(int, floatp, const gs_cie_wbsd *,
gs_cie_render *, float *);
typedef struct gs_cie_transform_proc3_s {
gs_cie_transform_proc proc;
const char *proc_name;
gs_const_string proc_data;
const char *driver_name;
} gs_cie_transform_proc3;
typedef frac(*gs_cie_render_table_proc) (byte, const gs_cie_render *);
typedef struct gs_cie_render_table_procs_s {
gs_cie_render_table_proc procs[4];
} gs_cie_render_table_procs;
typedef struct gs_cie_wb_s {
gs_vector3 WhitePoint;
gs_vector3 BlackPoint;
} gs_cie_wb;
typedef struct cie_linear_params_s {
bool is_linear;
float scale, origin;
} cie_linear_params_t;
typedef struct cie_cache_params_s {
bool is_identity;
double base, factor;
cie_linear_params_t linear;
} cie_cache_params;
typedef struct cie_cache_floats_s {
cie_cache_params params;
float values[gx_cie_cache_size];
} cie_cache_floats;
typedef struct cie_cache_fracs_s {
cie_cache_params params;
frac values[gx_cie_cache_size];
} cie_cache_fracs;
typedef struct cie_cache_ints_s {
cie_cache_params params;
int values[gx_cie_cache_size];
} cie_cache_ints;
typedef union gx_cie_scalar_cache_s {
cie_cache_floats floats;
cie_cache_fracs fracs;
cie_cache_ints ints;
} gx_cie_scalar_cache;
typedef struct cie_cached_vector3_s {
cie_cached_value u, v, w;
} cie_cached_vector3;
typedef struct cie_interpolation_range_s {
cie_cached_value rmin, rmax;
} cie_interpolation_range_t;
typedef struct cie_vector_cache_params_s {
cie_cached_value base, factor, limit;
cie_interpolation_range_t interpolation_ranges[3];
} cie_vector_cache_params;
typedef struct cie_cache_vectors_s {
cie_vector_cache_params params;
cie_cached_vector3 values[gx_cie_cache_size];
} cie_cache_vectors;
typedef struct gx_cie_vector_cache_s {
cie_cache_floats floats;
cie_cache_vectors vecs;
} gx_cie_vector_cache;
typedef struct gx_cie_vector_cache3_s {
gx_cie_vector_cache caches[3];
cie_interpolation_range_t interpolation_ranges[3];
} gx_cie_vector_cache3_t;
struct gs_cie_common_s {
int (*install_cspace)(const gs_color_space *, gs_state *);
void *client_data;
gs_range3 RangeLMN;
gs_cie_common_proc3 DecodeLMN;
gs_matrix3 MatrixLMN;
gs_cie_wb points;
struct {
gx_cie_scalar_cache DecodeLMN[3];
} caches;
};
#define public_st_cie_common()     \
gs_public_st_ptrs1(st_cie_common, gs_cie_common, "gs_cie_common",\
cie_common_enum_ptrs, cie_common_reloc_ptrs, client_data)
#define gs_cie_common_elements\
gs_cie_common common;		\
rc_header rc
typedef struct gs_cie_common_elements_s {
gs_cie_common_elements;
} gs_cie_common_elements_t;
#define public_st_cie_common_elements()  \
gs_public_st_suffix_add0_local( st_cie_common_elements_t,\
gs_cie_common_elements_t,\
"gs_cie_common_elements_t",\
cie_common_enum_ptrs,\
cie_common_reloc_ptrs,\
st_cie_common)
struct gs_cie_a_s {
gs_cie_common_elements;
gs_range RangeA;
gs_cie_a_proc DecodeA;
gs_vector3 MatrixA;
struct {
gx_cie_vector_cache DecodeA;
} caches;
};
#define private_st_cie_a()	\
gs_private_st_suffix_add0_local(st_cie_a, gs_cie_a, "gs_cie_a",\
cie_common_enum_ptrs,\
cie_common_reloc_ptrs,\
st_cie_common_elements_t)
#define gs_cie_abc_elements\
gs_cie_common_elements;		\
gs_range3 RangeABC;\
gs_cie_abc_proc3 DecodeABC;\
gs_matrix3 MatrixABC;\
\
struct {\
bool skipABC;\
gx_cie_vector_cache3_t DecodeABC;  \
} caches
struct gs_cie_abc_s {
gs_cie_abc_elements;
};
#define private_st_cie_abc()	\
gs_private_st_suffix_add0_local(st_cie_abc, gs_cie_abc, "gs_cie_abc",\
cie_common_enum_ptrs, cie_common_reloc_ptrs,\
st_cie_common_elements_t)
struct gs_cie_def_s {
gs_cie_abc_elements;
gs_range3 RangeDEF;
gs_cie_def_proc3 DecodeDEF;
gs_range3 RangeHIJ;
gx_color_lookup_table Table;
struct {
gx_cie_scalar_cache DecodeDEF[3];
} caches_def;
};
#define private_st_cie_def()	\
gs_private_st_suffix_add1(st_cie_def, gs_cie_def, "gs_cie_def",\
cie_def_enum_ptrs, cie_def_reloc_ptrs,\
st_cie_abc, Table.table)
struct gs_cie_defg_s {
gs_cie_abc_elements;
gs_range4 RangeDEFG;
gs_cie_defg_proc4 DecodeDEFG;
gs_range4 RangeHIJK;
gx_color_lookup_table Table;
struct {
gx_cie_scalar_cache DecodeDEFG[4];
} caches_defg;
};
#define private_st_cie_defg()	\
gs_private_st_suffix_add1(st_cie_defg, gs_cie_defg, "gs_cie_defg",\
cie_defg_enum_ptrs, cie_defg_reloc_ptrs,\
st_cie_abc, Table.table)
extern const gs_range3 Range3_default;
extern const gs_range4 Range4_default;
extern const gs_cie_defg_proc4 DecodeDEFG_default;
extern const gs_cie_defg_proc4 DecodeDEFG_from_cache;
extern const gs_cie_def_proc3 DecodeDEF_default;
extern const gs_cie_def_proc3 DecodeDEF_from_cache;
extern const gs_cie_abc_proc3 DecodeABC_default;
extern const gs_cie_abc_proc3 DecodeABC_from_cache;
extern const gs_cie_common_proc3 DecodeLMN_default;
extern const gs_cie_common_proc3 DecodeLMN_from_cache;
extern const gs_matrix3 Matrix3_default;
extern const gs_range RangeA_default;
extern const gs_cie_a_proc DecodeA_default;
extern const gs_cie_a_proc DecodeA_from_cache;
extern const gs_vector3 MatrixA_default;
extern const gs_vector3 BlackPoint_default;
extern const gs_cie_render_proc3 Encode_default;
extern const gs_cie_render_proc3 EncodeLMN_from_cache;
extern const gs_cie_render_proc3 EncodeABC_from_cache;
extern const gs_cie_transform_proc3 TransformPQR_default;
extern const gs_cie_transform_proc3 TransformPQR_from_cache;
extern const gs_cie_transform_proc TransformPQR_lookup_proc_name;
extern const gs_cie_render_table_procs RenderTableT_default;
extern const gs_cie_render_table_procs RenderTableT_from_cache;
struct gs_cie_wbsd_s {
struct {
gs_vector3 xyz, pqr;
} ws, bs, wd, bd;
};
typedef struct gs_cie_render_table_s {
gx_color_lookup_table lookup;
gs_cie_render_table_procs T;
} gs_cie_render_table_t;
typedef enum {
CIE_RENDER_STATUS_BUILT,
CIE_RENDER_STATUS_INITED,
CIE_RENDER_STATUS_SAMPLED,
CIE_RENDER_STATUS_COMPLETED
} cie_render_status_t;
typedef struct gx_cie_float_fixed_cache_s {
cie_cache_floats floats;
union if_ {
cie_cache_fracs fracs;
cie_cache_ints ints;
} fixeds;
} gx_cie_float_fixed_cache;
struct gs_cie_render_s {
cie_render_status_t status;
rc_header rc;
gs_id id;
void *client_data;
gs_cie_wb points;
gs_matrix3 MatrixPQR;
gs_range3 RangePQR;
gs_cie_transform_proc3 TransformPQR;
gs_matrix3 MatrixLMN;
gs_cie_render_proc3 EncodeLMN;
gs_range3 RangeLMN;
gs_matrix3 MatrixABC;
gs_cie_render_proc3 EncodeABC;
gs_range3 RangeABC;
gs_cie_render_table_t RenderTable;
gs_range3 DomainLMN;
gs_range3 DomainABC;
gs_matrix3 MatrixABCEncode;
cie_cached_value EncodeABC_base[3];
gs_matrix3 MatrixPQR_inverse_LMN;
gs_vector3 wdpqr, bdpqr;
struct {
gx_cie_vector_cache3_t EncodeLMN;
gx_cie_float_fixed_cache EncodeABC[3];
gx_cie_scalar_cache RenderTableT[4];
bool RenderTableT_is_identity;
} caches;
};
extern_st(st_cie_render1);
#define public_st_cie_render1()	\
gs_public_st_composite(st_cie_render1, gs_cie_render, "gs_cie_render",\
cie_render1_enum_ptrs, cie_render1_reloc_ptrs)
typedef enum {
CIE_JC_STATUS_BUILT,
CIE_JC_STATUS_INITED,
CIE_JC_STATUS_COMPLETED
} cie_joint_caches_status_t;
#define GX_CIE_REMAP_FINISH_PROC(proc)\
int proc(cie_cached_vector3 vec3, frac *pconc,\
const gs_imager_state *pis, const gs_color_space *pcs)
typedef struct gx_cie_joint_caches_s {
gs_id cspace_id;
gs_id render_id;
cie_joint_caches_status_t id_status;
cie_joint_caches_status_t status;
rc_header rc;
GX_CIE_REMAP_FINISH_PROC((*remap_finish));
bool skipDecodeABC;
bool skipDecodeLMN;
gx_cie_vector_cache3_t DecodeLMN;
gs_cie_wbsd points_sd;
bool skipPQR;
gx_cie_vector_cache3_t TransformPQR;
bool skipEncodeLMN;
} gx_cie_joint_caches;
#define private_st_joint_caches() \
gs_private_st_simple(st_joint_caches, gx_cie_joint_caches,\
"gx_cie_joint_caches")
typedef struct gs_sample_loop_params_s {
float A, B;
int N;
} gs_sample_loop_params_t;
#define SAMPLE_LOOP_VALUE(i, lp)\
( (((lp).N - (i)) * (lp).A + (i) * (lp).B) / (lp).N )
void gs_cie_cache_init(cie_cache_params *, gs_sample_loop_params_t *,
const gs_range *, client_name_t);
void gs_cie_cache_to_fracs(const cie_cache_floats *, cie_cache_fracs *);
void gs_cie_defg_complete(gs_cie_defg *);
void gs_cie_def_complete(gs_cie_def *);
void gs_cie_abc_complete(gs_cie_abc *);
void gs_cie_a_complete(gs_cie_a *);
gx_cie_joint_caches *gx_currentciecaches(gs_state *);
const gs_cie_common *gs_cie_cs_common(const gs_state *);
int gs_cie_cs_complete(gs_state *, bool);
int gs_cie_jc_complete(const gs_imager_state *, const gs_color_space *pcs);
float gs_cie_cached_value(floatp, const cie_cache_floats *);
#define CIE_CLAMP_INDEX(index)\
index = (index < 0 ? 0 :\
index >= gx_cie_cache_size ? gx_cie_cache_size - 1 : index)
int gs_cie_compute_points_sd(gx_cie_joint_caches *pjc,
const gs_cie_common * pcie,
const gs_cie_render * pcrd);
int gs_cie_render_init(gs_cie_render *);
int gs_cie_render_sample(gs_cie_render *);
int gs_cie_render_complete(gs_cie_render *);
extern int
gs_cspace_build_CIEA(gs_color_space ** ppcspace, void *client_data,
gs_memory_t * pmem),
gs_cspace_build_CIEABC(gs_color_space ** ppcspace, void *client_data,
gs_memory_t * pmem),
gs_cspace_build_CIEDEF(gs_color_space ** ppcspace, void *client_data,
gs_memory_t * pmem),
gs_cspace_build_CIEDEFG(gs_color_space ** ppcspace, void *client_data,
gs_memory_t * pmem);
#define gs_cie_RangeLMN(pcspace)  (&(pcspace)->params.a->common.RangeLMN)
#define gs_cie_DecodeLMN(pcspace) (&(pcspace)->params.a->common.DecodeLMN)
#define gs_cie_MatrixLMN(pcspace) (&(pcspace)->params.a->common.MatrixLMN)
#define gs_cie_WhitePoint(pcspace)\
((pcspace)->params.a->common.points.WhitePoint)
#define gs_cie_BlackPoint(pcspace)\
((pcspace)->params.a->common.points.BlackPoint)
#define gs_cie_a_RangeA(pcspace)      (&(pcspace)->params.a->RangeA)
#define gs_cie_a_DecodeA(pcspace)     (&(pcspace)->params.a->DecodeA)
#define gs_cie_a_MatrixA(pcspace)     (&(pcspace)->params.a->MatrixA)
#define gs_cie_a_RangeA(pcspace)      (&(pcspace)->params.a->RangeA)
#define gs_cie_abc_RangeABC(pcspace)    (&(pcspace)->params.abc->RangeABC)
#define gs_cie_abc_DecodeABC(pcspace)   (&(pcspace)->params.abc->DecodeABC)
#define gs_cie_abc_MatrixABC(pcspace)   (&(pcspace)->params.abc->MatrixABC)
#define gs_cie_def_RangeDEF(pcspace)    (&(pcspace)->params.def->RangeDEF)
#define gs_cie_def_DecodeDEF(pcspace)   (&(pcspace)->params.def->DecodeDEF)
#define gs_cie_def_RangeHIJ(pcspace)    (&(pcspace)->params.def->RangeHIJ)
#define gs_cie_defg_RangeDEFG(pcspace)  (&(pcspace)->params.defg->RangeDEFG)
#define gs_cie_defg_DecodeDEFG(pcspace) (&(pcspace)->params.defg->DecodeDEFG)
#define gs_cie_defg_RangeHIJK(pcspace)  (&(pcspace)->params.defg->RangeHIJK)
extern int
gs_cie_defx_set_lookup_table(gs_color_space * pcspace, int *pdims,
const gs_const_string * ptable);
int gx_serialize_cie_common_elements(const gs_color_space * pcs, stream * s);
#endif