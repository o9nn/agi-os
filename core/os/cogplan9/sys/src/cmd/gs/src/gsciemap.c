#include "math_.h"
#include "gx.h"
#include "gserrors.h"
#include "gxcspace.h"
#include "gxarith.h"
#include "gxcie.h"
#include "gxdevice.h"
#include "gxcmap.h"
#include "gxistate.h"
#define LOOKUP_INDEX_(vin, pcache, fbits)\
(cie_cached_value)\
((vin) <= (pcache)->vecs.params.base ? 0 :\
(vin) >= (pcache)->vecs.params.limit ? (gx_cie_cache_size - 1) << (fbits) :\
cie_cached_product2int( ((vin) - (pcache)->vecs.params.base),\
(pcache)->vecs.params.factor, fbits ))
#define LOOKUP_ENTRY_(vin, pcache)\
(&(pcache)->vecs.values[(int)LOOKUP_INDEX(vin, pcache, 0)])
#ifdef DEBUG
private cie_cached_value
LOOKUP_INDEX(cie_cached_value vin, const gx_cie_vector_cache *pcache,
int fbits)
{
return LOOKUP_INDEX_(vin, pcache, fbits);
}
private const cie_cached_vector3 *
LOOKUP_ENTRY(cie_cached_value vin, const gx_cie_vector_cache *pcache)
{
return LOOKUP_ENTRY_(vin, pcache);
}
#else
# define LOOKUP_INDEX(vin, pcache, fbits) LOOKUP_INDEX_(vin, pcache, fbits)
# define LOOKUP_ENTRY(vin, pcache) LOOKUP_ENTRY_(vin, pcache)
#endif
#ifdef DEBUG
# define GX_CIE_REMAP_FINISH(vec3, pconc, pis, pcs)\
gx_cie_remap_finish(vec3, pconc, pis, pcs)
#else
# define GX_CIE_REMAP_FINISH(vec3, pconc, pis, pcs)\
((pis)->cie_joint_caches->remap_finish(vec3, pconc, pis, pcs))
#endif
private void cie_lookup_mult3(cie_cached_vector3 *,
const gx_cie_vector_cache3_t *);
#ifdef DEBUG
private void
cie_lookup_map3(cie_cached_vector3 * pvec,
const gx_cie_vector_cache3_t * pc, const char *cname)
{
if_debug5('c', "[c]lookup %s 0x%lx [%g %g %g]\n",
(const char *)cname, (ulong) pc,
cie_cached2float(pvec->u), cie_cached2float(pvec->v),
cie_cached2float(pvec->w));
cie_lookup_mult3(pvec, pc);
if_debug3('c', "        =[%g %g %g]\n",
cie_cached2float(pvec->u), cie_cached2float(pvec->v),
cie_cached2float(pvec->w));
}
#else
# define cie_lookup_map3(pvec, pc, cname) cie_lookup_mult3(pvec, pc)
#endif
int
gx_concretize_CIEDEFG(const gs_client_color * pc, const gs_color_space * pcs,
frac * pconc, const gs_imager_state * pis)
{
const gs_cie_defg *pcie = pcs->params.defg;
int i;
fixed hijk[4];
frac abc[3];
cie_cached_vector3 vec3;
if_debug4('c', "[c]concretize DEFG [%g %g %g %g]\n",
pc->paint.values[0], pc->paint.values[1],
pc->paint.values[2], pc->paint.values[3]);
CIE_CHECK_RENDERING(pcs, pconc, pis, return 0);
for (i = 0; i < 4; ++i) {
int tdim = pcie->Table.dims[i] - 1;
double factor = pcie->caches_defg.DecodeDEFG[i].floats.params.factor;
double v0 = pc->paint.values[i];
const gs_range *const rangeDEFG = &pcie->RangeDEFG.ranges[i];
double value =
(v0 < rangeDEFG->rmin ? 0.0 :
v0 > rangeDEFG->rmax ? factor :
(v0 - rangeDEFG->rmin) * factor /
(rangeDEFG->rmax - rangeDEFG->rmin));
int vi = (int)value;
double vf = value - vi;
double v = pcie->caches_defg.DecodeDEFG[i].floats.values[vi];
if (vf != 0 && vi < factor)
v += vf *
(pcie->caches_defg.DecodeDEFG[i].floats.values[vi + 1] - v);
v = (v < 0 ? 0 : v > tdim ? tdim : v);
hijk[i] = float2fixed(v);
}
gx_color_interpolate_linear(hijk, &pcie->Table, abc);
#define SCALE_TO_RANGE(range, frac) ( \
float2cie_cached(((range).rmax - (range).rmin) * frac2float(frac) + \
(range).rmin) \
)
vec3.u = SCALE_TO_RANGE(pcie->RangeABC.ranges[0], abc[0]);
vec3.v = SCALE_TO_RANGE(pcie->RangeABC.ranges[1], abc[1]);
vec3.w = SCALE_TO_RANGE(pcie->RangeABC.ranges[2], abc[2]);
if (!pis->cie_joint_caches->skipDecodeABC)
cie_lookup_map3(&vec3 , &pcie->caches.DecodeABC,
"Decode/MatrixABC");
GX_CIE_REMAP_FINISH(vec3, pconc, pis, pcs);
return 0;
}
int
gx_concretize_CIEDEF(const gs_client_color * pc, const gs_color_space * pcs,
frac * pconc, const gs_imager_state * pis)
{
const gs_cie_def *pcie = pcs->params.def;
int i;
fixed hij[3];
frac abc[3];
cie_cached_vector3 vec3;
if_debug3('c', "[c]concretize DEF [%g %g %g]\n",
pc->paint.values[0], pc->paint.values[1],
pc->paint.values[2]);
CIE_CHECK_RENDERING(pcs, pconc, pis, return 0);
for (i = 0; i < 3; ++i) {
int tdim = pcie->Table.dims[i] - 1;
double factor = pcie->caches_def.DecodeDEF[i].floats.params.factor;
double v0 = pc->paint.values[i];
const gs_range *const rangeDEF = &pcie->RangeDEF.ranges[i];
double value =
(v0 < rangeDEF->rmin ? 0.0 :
v0 > rangeDEF->rmax ? factor :
(v0 - rangeDEF->rmin) * factor /
(rangeDEF->rmax - rangeDEF->rmin));
int vi = (int)value;
double vf = value - vi;
double v = pcie->caches_def.DecodeDEF[i].floats.values[vi];
if (vf != 0 && vi < factor)
v += vf *
(pcie->caches_def.DecodeDEF[i].floats.values[vi + 1] - v);
v = (v < 0 ? 0 : v > tdim ? tdim : v);
hij[i] = float2fixed(v);
}
gx_color_interpolate_linear(hij, &pcie->Table, abc);
vec3.u = SCALE_TO_RANGE(pcie->RangeABC.ranges[0], abc[0]);
vec3.v = SCALE_TO_RANGE(pcie->RangeABC.ranges[1], abc[1]);
vec3.w = SCALE_TO_RANGE(pcie->RangeABC.ranges[2], abc[2]);
if (!pis->cie_joint_caches->skipDecodeABC)
cie_lookup_map3(&vec3 , &pcie->caches.DecodeABC,
"Decode/MatrixABC");
GX_CIE_REMAP_FINISH(vec3, pconc, pis, pcs);
return 0;
}
#undef SCALE_TO_RANGE
int
gx_remap_CIEABC(const gs_client_color * pc, const gs_color_space * pcs,
gx_device_color * pdc, const gs_imager_state * pis, gx_device * dev,
gs_color_select_t select)
{
frac conc[4];
cie_cached_vector3 vec3;
if_debug3('c', "[c]remap CIEABC [%g %g %g]\n",
pc->paint.values[0], pc->paint.values[1],
pc->paint.values[2]);
CIE_CHECK_RENDERING(pcs, conc, pis, goto map3);
vec3.u = float2cie_cached(pc->paint.values[0]);
vec3.v = float2cie_cached(pc->paint.values[1]);
vec3.w = float2cie_cached(pc->paint.values[2]);
if (!pis->cie_joint_caches->skipDecodeABC) {
const gs_cie_abc *pcie = pcs->params.abc;
cie_lookup_map3(&vec3 , &pcie->caches.DecodeABC,
"Decode/MatrixABC");
}
switch (GX_CIE_REMAP_FINISH(vec3 , conc, pis, pcs)) {
case 4:
if_debug4('c', "[c]=CMYK [%g %g %g %g]\n",
frac2float(conc[0]), frac2float(conc[1]),
frac2float(conc[2]), frac2float(conc[3]));
gx_remap_concrete_cmyk(conc[0], conc[1], conc[2], conc[3],
pdc, pis, dev, select);
goto done;
default:
return_error(gs_error_unknownerror);
case 3:
;
}
map3:
if_debug3('c', "[c]=RGB [%g %g %g]\n",
frac2float(conc[0]), frac2float(conc[1]),
frac2float(conc[2]));
gx_remap_concrete_rgb(conc[0], conc[1], conc[2], pdc, pis,
dev, select);
done:
pdc->ccolor.paint.values[0] = pc->paint.values[0];
pdc->ccolor.paint.values[1] = pc->paint.values[1];
pdc->ccolor.paint.values[2] = pc->paint.values[2];
pdc->ccolor_valid = true;
return 0;
}
int
gx_concretize_CIEABC(const gs_client_color * pc, const gs_color_space * pcs,
frac * pconc, const gs_imager_state * pis)
{
const gs_cie_abc *pcie = pcs->params.abc;
cie_cached_vector3 vec3;
if_debug3('c', "[c]concretize CIEABC [%g %g %g]\n",
pc->paint.values[0], pc->paint.values[1],
pc->paint.values[2]);
CIE_CHECK_RENDERING(pcs, pconc, pis, return 0);
vec3.u = float2cie_cached(pc->paint.values[0]);
vec3.v = float2cie_cached(pc->paint.values[1]);
vec3.w = float2cie_cached(pc->paint.values[2]);
if (!pis->cie_joint_caches->skipDecodeABC)
cie_lookup_map3(&vec3 , &pcie->caches.DecodeABC,
"Decode/MatrixABC");
GX_CIE_REMAP_FINISH(vec3, pconc, pis, pcs);
return 0;
}
int
gx_concretize_CIEA(const gs_client_color * pc, const gs_color_space * pcs,
frac * pconc, const gs_imager_state * pis)
{
const gs_cie_a *pcie = pcs->params.a;
cie_cached_value a = float2cie_cached(pc->paint.values[0]);
cie_cached_vector3 vlmn;
if_debug1('c', "[c]concretize CIEA %g\n", pc->paint.values[0]);
CIE_CHECK_RENDERING(pcs, pconc, pis, return 0);
if (!pis->cie_joint_caches->skipDecodeABC)
vlmn = *LOOKUP_ENTRY(a, &pcie->caches.DecodeA);
else
vlmn.u = vlmn.v = vlmn.w = a;
GX_CIE_REMAP_FINISH(vlmn, pconc, pis, pcs);
return 0;
}
int
gx_cie_remap_finish(cie_cached_vector3 vec3, frac * pconc,
const gs_imager_state * pis,
const gs_color_space *pcs)
{
return pis->cie_joint_caches->remap_finish(vec3, pconc, pis, pcs);
}
int
gx_cie_real_remap_finish(cie_cached_vector3 vec3, frac * pconc,
const gs_imager_state * pis,
const gs_color_space *pcs)
{
const gs_cie_render *pcrd = pis->cie_render;
const gx_cie_joint_caches *pjc = pis->cie_joint_caches;
const gs_const_string *table = pcrd->RenderTable.lookup.table;
int tabc[3];
if (!pjc->skipDecodeLMN)
cie_lookup_map3(&vec3 , &pjc->DecodeLMN,
"Decode/MatrixLMN+MatrixPQR");
if (!pjc->skipPQR)
cie_lookup_map3(&vec3 , &pjc->TransformPQR,
"Transform/Matrix'PQR+MatrixLMN");
if (!pjc->skipEncodeLMN)
cie_lookup_map3(&vec3 , &pcrd->caches.EncodeLMN,
"EncodeLMN+MatrixABC");
#define SET_TABC(i, t)\
BEGIN\
tabc[i] = cie_cached2int(vec3 .t - pcrd->EncodeABC_base[i],\
_cie_interpolate_bits);\
if ((uint)tabc[i] > (gx_cie_cache_size - 1) << _cie_interpolate_bits)\
tabc[i] = (tabc[i] < 0 ? 0 :\
(gx_cie_cache_size - 1) << _cie_interpolate_bits);\
END
SET_TABC(0, u);
SET_TABC(1, v);
SET_TABC(2, w);
#undef SET_TABC
if (table == 0) {
#define EABC(i)\
cie_interpolate_fracs(pcrd->caches.EncodeABC[i].fixeds.fracs.values, tabc[i])
pconc[0] = EABC(0);
pconc[1] = EABC(1);
pconc[2] = EABC(2);
#undef EABC
return 3;
} else {
int m = pcrd->RenderTable.lookup.m;
#define RT_LOOKUP(j, i) pcrd->caches.RenderTableT[j].fracs.values[i]
#ifdef CIE_RENDER_TABLE_INTERPOLATE
fixed rfix[3];
const int s = _fixed_shift - _cie_interpolate_bits;
#define EABC(i)\
cie_interpolate_fracs(pcrd->caches.EncodeABC[i].fixeds.ints.values, tabc[i])
#define FABC(i, s)\
((s) > 0) ? (EABC(i) << (s)) : (EABC(i) >> -(s))
rfix[0] = FABC(0, s);
rfix[1] = FABC(1, s);
rfix[2] = FABC(2, s);
#undef FABC
#undef EABC
if_debug6('c', "[c]ABC=%g,%g,%g => iabc=%g,%g,%g\n",
cie_cached2float(vec3.u), cie_cached2float(vec3.v),
cie_cached2float(vec3.w), fixed2float(rfix[0]),
fixed2float(rfix[1]), fixed2float(rfix[2]));
gx_color_interpolate_linear(rfix, &pcrd->RenderTable.lookup,
pconc);
if_debug3('c', "[c]  interpolated => %g,%g,%g\n",
frac2float(pconc[0]), frac2float(pconc[1]),
frac2float(pconc[2]));
if (!pcrd->caches.RenderTableT_is_identity) {
#define frac2cache_index(v) frac2bits(v, gx_cie_log2_cache_size)
pconc[0] = RT_LOOKUP(0, frac2cache_index(pconc[0]));
pconc[1] = RT_LOOKUP(1, frac2cache_index(pconc[1]));
pconc[2] = RT_LOOKUP(2, frac2cache_index(pconc[2]));
if (m > 3)
pconc[3] = RT_LOOKUP(3, frac2cache_index(pconc[3]));
#undef frac2cache_index
}
#else
#define RI(i)\
pcrd->caches.EncodeABC[i].ints.values[tabc[i] >> _cie_interpolate_bits]
int ia = RI(0);
int ib = RI(1);
int ic = RI(2);
const byte *prtc = table[ia].data + ib + ic;
if_debug6('c', "[c]ABC=%g,%g,%g => iabc=%d,%d,%d\n",
cie_cached2float(vec3.u), cie_cached2float(vec3.v),
cie_cached2float(vec3.w), ia, ib, ic);
if (pcrd->caches.RenderTableT_is_identity) {
pconc[0] = byte2frac(prtc[0]);
pconc[1] = byte2frac(prtc[1]);
pconc[2] = byte2frac(prtc[2]);
if (m > 3)
pconc[3] = byte2frac(prtc[3]);
} else {
#if gx_cie_log2_cache_size == 8
# define byte2cache_index(b) (b)
#else
# if gx_cie_log2_cache_size > 8
# define byte2cache_index(b)\
( ((b) << (gx_cie_log2_cache_size - 8)) +\
((b) >> (16 - gx_cie_log2_cache_size)) )
# else
# define byte2cache_index(b) ((b) >> (8 - gx_cie_log2_cache_size))
# endif
#endif
pconc[0] = RT_LOOKUP(0, byte2cache_index(prtc[0]));
pconc[1] = RT_LOOKUP(1, byte2cache_index(prtc[1]));
pconc[2] = RT_LOOKUP(2, byte2cache_index(prtc[2]));
if (m > 3)
pconc[3] = RT_LOOKUP(3, byte2cache_index(prtc[3]));
#undef byte2cache_index
}
#endif
#undef RI
#undef RT_LOOKUP
return m;
}
}
private frac
float2frac_clamp(floatp x)
{
return float2frac((x <= 0 ? 0 : x >= 1 ? 1 : x));
}
int
gx_cie_xyz_remap_finish(cie_cached_vector3 vec3, frac * pconc,
const gs_imager_state * pis,
const gs_color_space *pcs)
{
const gx_cie_joint_caches *pjc = pis->cie_joint_caches;
if (!pjc->skipDecodeLMN)
cie_lookup_map3(&vec3 , &pjc->DecodeLMN,
"Decode/MatrixLMN");
pconc[0] = float2frac_clamp(cie_cached2float(vec3.u));
pconc[1] = float2frac_clamp(cie_cached2float(vec3.v));
pconc[2] = float2frac_clamp(cie_cached2float(vec3.w));
return 3;
}
private void
cie_lookup_mult3(cie_cached_vector3 * pvec,
const gx_cie_vector_cache3_t * pc)
{
#ifdef CIE_CACHE_INTERPOLATE
cie_cached_value u, v, w;
#ifdef CIE_CACHE_USE_FIXED
# define LOOKUP_INTERPOLATE_BETWEEN(v0, v1, i, ftemp)\
cie_interpolate_between(v0, v1, i)
#else
float ftemp;
# define LOOKUP_INTERPOLATE_BETWEEN(v0, v1, i)\
((v0) + ((v1) - (v0)) *\
((ftemp = float_rshift(i, _cie_interpolate_bits)), ftemp - (int)ftemp))
#endif
#define I_IN_RANGE(j, n)\
(pvec->n >= pc->interpolation_ranges[j].rmin &&\
pvec->n < pc->interpolation_ranges[j].rmax)
#define I_INDEX(j, n)\
LOOKUP_INDEX(pvec->n, &pc->caches[j], _cie_interpolate_bits)
#define I_ENTRY(i, j)\
&pc->caches[j].vecs.values[(int)cie_cached_rshift(i, _cie_interpolate_bits)]
#define I_ENTRY1(i, p)\
(i >= (gx_cie_cache_size - 1) << _cie_interpolate_bits ? p : p + 1)
if (I_IN_RANGE(0, u)) {
cie_cached_value i = I_INDEX(0, u);
const cie_cached_vector3 *p = I_ENTRY(i, 0);
const cie_cached_vector3 *p1 = I_ENTRY1(i, p);
if_debug0('C', "[c]Interpolating u.\n");
u = LOOKUP_INTERPOLATE_BETWEEN(p->u, p1->u, i);
v = LOOKUP_INTERPOLATE_BETWEEN(p->v, p1->v, i);
w = LOOKUP_INTERPOLATE_BETWEEN(p->w, p1->w, i);
} else {
const cie_cached_vector3 *p = LOOKUP_ENTRY(pvec->u, &pc->caches[0]);
if_debug0('C', "[c]Not interpolating u.\n");
u = p->u, v = p->v, w = p->w;
}
if (I_IN_RANGE(1, v)) {
cie_cached_value i = I_INDEX(1, v);
const cie_cached_vector3 *p = I_ENTRY(i, 1);
const cie_cached_vector3 *p1 = I_ENTRY1(i, p);
if_debug0('C', "[c]Interpolating v.\n");
u += LOOKUP_INTERPOLATE_BETWEEN(p->u, p1->u, i);
v += LOOKUP_INTERPOLATE_BETWEEN(p->v, p1->v, i);
w += LOOKUP_INTERPOLATE_BETWEEN(p->w, p1->w, i);
} else {
const cie_cached_vector3 *p = LOOKUP_ENTRY(pvec->v, &pc->caches[1]);
if_debug0('C', "[c]Not interpolating v.\n");
u += p->u, v += p->v, w += p->w;
}
if (I_IN_RANGE(2, w)) {
cie_cached_value i = I_INDEX(2, w);
const cie_cached_vector3 *p = I_ENTRY(i, 2);
const cie_cached_vector3 *p1 = I_ENTRY1(i, p);
if_debug0('C', "[c]Interpolating w.\n");
u += LOOKUP_INTERPOLATE_BETWEEN(p->u, p1->u, i);
v += LOOKUP_INTERPOLATE_BETWEEN(p->v, p1->v, i);
w += LOOKUP_INTERPOLATE_BETWEEN(p->w, p1->w, i);
} else {
const cie_cached_vector3 *p = LOOKUP_ENTRY(pvec->w, &pc->caches[2]);
if_debug0('C', "[c]Not interpolating w.\n");
u += p->u, v += p->v, w += p->w;
}
#undef I_IN_RANGE
#undef I_INDEX
#undef I_ENTRY
#undef I_ENTRY1
pvec->u = u;
pvec->v = v;
pvec->w = w;
#else
const cie_cached_vector3 *pu = LOOKUP_ENTRY(pvec->u, &pc->caches[0]);
const cie_cached_vector3 *pv = LOOKUP_ENTRY(pvec->v, &pc->caches[1]);
const cie_cached_vector3 *pw = LOOKUP_ENTRY(pvec->w, &pc->caches[2]);
if_debug0('C', "[c]Not interpolating.\n");
pvec->u = pu->u + pv->u + pw->u;
pvec->v = pu->v + pv->v + pw->v;
pvec->w = pu->w + pv->w + pw->w;
#endif
}