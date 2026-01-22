#ifndef gxtype1_INCLUDED
# define gxtype1_INCLUDED
#include "gscrypt1.h"
#include "gsgdata.h"
#include "gstype1.h"
#include "gxhintn.h"
typedef struct pixel_scale_s {
fixed unit;
fixed half;
int log2_unit;
} pixel_scale;
typedef struct point_scale_s {
pixel_scale x, y;
} point_scale;
#define set_pixel_scale(pps, log2)\
(pps)->unit = ((pps)->half = fixed_half << ((pps)->log2_unit = log2)) << 1
#define scaled_rounded(v, pps)\
(((v) + (pps)->half) & -(pps)->unit)
#define max_total_stem_hints 96
typedef struct {
const byte *ip;
crypt_state dstate;
gs_glyph_data_t cs_data;
} ip_state_t;
#define charstring_this(ch, state, encrypted)\
(encrypted ? decrypt_this(ch, state) : ch)
#define charstring_next(ch, state, chvar, encrypted)\
(encrypted ? (chvar = decrypt_this(ch, state),\
decrypt_skip_next(ch, state)) :\
(chvar = ch))
#define charstring_skip_next(ch, state, encrypted)\
(encrypted ? decrypt_skip_next(ch, state) : 0)
#ifndef gx_path_DEFINED
# define gx_path_DEFINED
typedef struct gx_path_s gx_path;
#endif
#ifndef segment_DEFINED
# define segment_DEFINED
typedef struct segment_s segment;
#endif
#define ostack_size 48
#define ipstack_size 10
struct gs_type1_state_s {
t1_hinter h;
gs_font_type1 *pfont;
gs_imager_state *pis;
gx_path *path;
bool no_grid_fitting;
int paint_type;
void *callback_data;
fixed_coeff fc;
float flatness;
point_scale scale;
gs_log2_scale_point log2_subpixels;
gs_fixed_point origin;
fixed ostack[ostack_size];
int os_count;
ip_state_t ipstack[ipstack_size + 1];
int ips_count;
int init_done;
bool sb_set;
bool width_set;
int num_hints;
gs_fixed_point lsb;
gs_fixed_point width;
int seac_accent;
fixed save_asb;
gs_fixed_point save_lsb;
gs_fixed_point save_adxy;
fixed asb_diff;
gs_fixed_point adxy;
int flex_path_state_flags;
#define flex_max 8
int flex_count;
int ignore_pops;
gs_fixed_point vs_offset;
fixed transient_array[32];
};
extern_st(st_gs_type1_state);
#define public_st_gs_type1_state() \
gs_public_st_composite(st_gs_type1_state, gs_type1_state, "gs_type1_state",\
gs_type1_state_enum_ptrs, gs_type1_state_reloc_ptrs)
typedef fixed *cs_ptr;
#define CLEAR_CSTACK(cstack, csp)\
(csp = (cs_ptr)(cstack) - 1)
#define INIT_CSTACK(cstack, csp, pcis)\
BEGIN\
if ( pcis->os_count == 0 )\
CLEAR_CSTACK(cstack, csp);\
else {\
memcpy(cstack, pcis->ostack, pcis->os_count * sizeof(fixed));\
csp = &cstack[pcis->os_count - 1];\
}\
END
#define CS_CHECK_PUSH(csp, cstack)\
BEGIN\
if (csp >= &cstack[countof(cstack)-1])\
return_error(gs_error_invalidfont);\
END
#define decode_num1(var, c)\
(var = c_value_num1(c))
#define decode_push_num1(csp, cstack, c)\
BEGIN\
CS_CHECK_PUSH(csp, cstack);\
*++csp = int2fixed(c_value_num1(c));\
END
#define decode_num2(var, c, cip, state, encrypted)\
BEGIN\
uint c2 = *cip++;\
int cn = charstring_this(c2, state, encrypted);\
\
var = (c < c_neg2_0 ? c_value_pos2(c, 0) + cn :\
c_value_neg2(c, 0) - cn);\
charstring_skip_next(c2, state, encrypted);\
END
#define decode_push_num2(csp, cstack, c, cip, state, encrypted)\
BEGIN\
uint c2 = *cip++;\
int cn;\
\
CS_CHECK_PUSH(csp, cstack);\
cn = charstring_this(c2, state, encrypted);\
if ( c < c_neg2_0 )\
{ if_debug2('1', "[1] (%d)+%d\n", c_value_pos2(c, 0), cn);\
*++csp = int2fixed(c_value_pos2(c, 0) + (int)cn);\
}\
else\
{ if_debug2('1', "[1] (%d)-%d\n", c_value_neg2(c, 0), cn);\
*++csp = int2fixed(c_value_neg2(c, 0) - (int)cn);\
}\
charstring_skip_next(c2, state, encrypted);\
END
#if arch_sizeof_long > 4
# define sign_extend_num4(lw)\
lw = (lw ^ 0x80000000L) - 0x80000000L
#else
# define sign_extend_num4(lw) DO_NOTHING
#endif
#define decode_num4(lw, cip, state, encrypted)\
BEGIN\
int i;\
uint c4;\
\
lw = 0;\
for ( i = 4; --i >= 0; )\
{ charstring_next(*cip, state, c4, encrypted);\
lw = (lw << 8) + c4;\
cip++;\
}\
sign_extend_num4(lw);\
END
void gs_type1_finish_init(gs_type1_state * pcis);
int gs_type1_sbw(gs_type1_state * pcis, fixed sbx, fixed sby,
fixed wx, fixed wy);
int gs_type1_blend(gs_type1_state *pcis, fixed *csp, int num_results);
int gs_type1_seac(gs_type1_state * pcis, const fixed * cstack,
fixed asb_diff, ip_state_t * ipsp);
int gs_type1_endchar(gs_type1_state * pcis);
void type1_cis_get_metrics(const gs_type1_state * pcis, double psbw[4]);
#endif