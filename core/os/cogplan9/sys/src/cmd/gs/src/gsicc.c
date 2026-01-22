#include "math_.h"
#include "memory_.h"
#include "gx.h"
#include "gserrors.h"
#include "gsstruct.h"
#include "stream.h"
#include "gxcspace.h"
#include "gxarith.h"
#include "gxcie.h"
#include "gzstate.h"
#include "stream.h"
#include "icc.h"
#include "gsicc.h"
typedef struct _icmFileGs icmFileGs;
struct _icmFileGs {
ICM_FILE_BASE
stream *strp;
};
private void
cie_icc_finalize(void * pvicc_info)
{
gs_cie_icc * picc_info = (gs_cie_icc *)pvicc_info;
if (picc_info->plu != NULL) {
picc_info->plu->del(picc_info->plu);
picc_info->plu = NULL;
}
if (picc_info->picc != NULL) {
picc_info->picc->del(picc_info->picc);
picc_info->picc = NULL;
}
if (picc_info->pfile != NULL) {
picc_info->pfile->del(picc_info->pfile);
picc_info->pfile = NULL;
}
}
private_st_cie_icc();
gs_private_st_composite( st_color_space_CIEICC,
gs_paint_color_space,
"gs_color_space_CIEICC",
cs_CIEICC_enum_ptrs,
cs_CIEICC_reloc_ptrs );
private
ENUM_PTRS_WITH(cs_CIEICC_enum_ptrs, gs_color_space * pcs)
return ENUM_USING( *pcs->params.icc.alt_space.type->stype,
&pcs->params.icc.alt_space,
sizeof(pcs->params.separation.alt_space),
index - 1 );
ENUM_PTR(0, gs_color_space, params.icc.picc_info);
ENUM_PTRS_END
private
RELOC_PTRS_WITH(cs_CIEICC_reloc_ptrs, gs_color_space * pcs)
RELOC_PTR(gs_color_space, params.icc.picc_info);
RELOC_USING( *pcs->params.icc.alt_space.type->stype,
&pcs->params.icc.alt_space,
sizeof(pcs->params.separation.alt_space) );
RELOC_PTRS_END
private cs_proc_num_components(gx_num_components_CIEICC);
private cs_proc_base_space(gx_alt_space_CIEICC);
private cs_proc_init_color(gx_init_CIEICC);
private cs_proc_restrict_color(gx_restrict_CIEICC);
private cs_proc_concrete_space(gx_concrete_space_CIEICC);
private cs_proc_concretize_color(gx_concretize_CIEICC);
private cs_proc_adjust_cspace_count(gx_adjust_cspace_CIEICC);
private cs_proc_serialize(gx_serialize_CIEICC);
private const gs_color_space_type gs_color_space_type_CIEICC = {
gs_color_space_index_CIEICC,
true,
true,
&st_color_space_CIEICC,
gx_num_components_CIEICC,
gx_alt_space_CIEICC,
gx_init_CIEICC,
gx_restrict_CIEICC,
gx_concrete_space_CIEICC,
gx_concretize_CIEICC,
NULL,
gx_default_remap_color,
gx_install_CIE,
gx_spot_colors_set_overprint,
gx_adjust_cspace_CIEICC,
gx_no_adjust_color_count,
gx_serialize_CIEICC,
gx_cspace_is_linear_default
};
private int
gx_num_components_CIEICC(const gs_color_space * pcs)
{
return pcs->params.icc.picc_info->num_components;
}
private const gs_color_space *
gx_alt_space_CIEICC(const gs_color_space * pcs)
{
return (pcs->params.icc.picc_info->picc == NULL)
? (const gs_color_space *)&pcs->params.icc.alt_space
: NULL;
}
private void
gx_init_CIEICC(gs_client_color * pcc, const gs_color_space * pcs)
{
int i, ncomps = pcs->params.icc.picc_info->num_components;
for (i = 0; i < ncomps; ++i)
pcc->paint.values[i] = 0.0;
gx_restrict_CIEICC(pcc, pcs);
}
private void
gx_restrict_CIEICC(gs_client_color * pcc, const gs_color_space * pcs)
{
int i, ncomps = pcs->params.icc.picc_info->num_components;
const gs_range * ranges = pcs->params.icc.picc_info->Range.ranges;
for (i = 0; i < ncomps; ++i) {
floatp v = pcc->paint.values[i];
floatp rmin = ranges[i].rmin, rmax = ranges[i].rmax;
if (v < rmin)
pcc->paint.values[i] = rmin;
else if (v > rmax)
pcc->paint.values[i] = rmax;
}
}
private const gs_color_space *
gx_concrete_space_CIEICC(const gs_color_space * pcs, const gs_imager_state * pis)
{
if (pcs->params.icc.picc_info->picc == NULL) {
const gs_color_space * pacs = (const gs_color_space *)
&pcs->params.icc.alt_space;
return cs_concrete_space(pacs, pis);
} else
return gx_concrete_space_CIE(NULL, pis);
}
private int
gx_concretize_CIEICC(
const gs_client_color * pcc,
const gs_color_space * pcs,
frac * pconc,
const gs_imager_state * pis )
{
const gs_icc_params * picc_params = &pcs->params.icc;
const gs_cie_icc * picc_info = picc_params->picc_info;
stream * instrp = picc_info->instrp;
icc * picc = picc_info->picc;
double inv[4], outv[3];
cie_cached_vector3 vlmn;
gs_client_color lcc = *pcc;
int i, ncomps = picc_info->num_components;
if (picc == NULL)
return picc_params->alt_space.type->concretize_color(
pcc,
(const gs_color_space *)&picc_params->alt_space,
pconc,
pis );
CIE_CHECK_RENDERING(pcs, pconc, pis, return 0);
if (picc_info->file_id != (instrp->read_id | instrp->write_id))
return_error(gs_error_ioerror);
((icmFileGs *)picc->fp)->strp = instrp;
gx_restrict_CIEICC(&lcc, pcs);
for (i = 0; i < ncomps; i++)
inv[i] = lcc.paint.values[i];
if (picc_info->plu->e_inSpace == icSigLabData) {
inv[0] *= 100;
inv[1] = inv[1]*255 - 128;
inv[2] = inv[2]*255 - 128;
}
if (picc_info->plu->lookup(picc_info->plu, outv, inv) > 1)
return_error(gs_error_unregistered);
if (picc_info->pcs_is_cielab) {
floatp f[3];
const gs_vector3 * pwhtpt = &picc_info->common.points.WhitePoint;
f[1] = (outv[0] + 16.0) / 116.0;
f[0] = f[1] + outv[1] / 500.0;
f[2] = f[1] - outv[2] / 200;
for (i = 0; i < 3; i++) {
if (f[i] >= 6.0 / 29.0)
outv[i] = f[i] * f[i] * f[i];
else
outv[i] = 108.0 * (f[i] - 4.0 / 29.0) / 841.0;
}
outv[0] *= pwhtpt->u;
outv[1] *= pwhtpt->v;
outv[2] *= pwhtpt->w;
}
vlmn.u = float2cie_cached(outv[0]);
vlmn.v = float2cie_cached(outv[1]);
vlmn.w = float2cie_cached(outv[2]);
gx_cie_remap_finish(vlmn, pconc, pis, pcs);
return 0;
}
private void
gx_adjust_cspace_CIEICC(const gs_color_space * pcs, int delta)
{
const gs_icc_params * picc_params = &pcs->params.icc;
rc_adjust_const(picc_params->picc_info, delta, "gx_adjust_cspace_CIEICC");
picc_params->alt_space.type->adjust_cspace_count(
(const gs_color_space *)&picc_params->alt_space, delta );
}
void
gx_increment_cspace_count(const gs_color_space * pcs)
{
pcs->type->adjust_cspace_count(pcs, 1);
}
private int
icmFileGs_seek(icmFile *pp, long int offset)
{
icmFileGs *p = (icmFileGs *)pp;
return spseek(p->strp, offset);
}
private size_t
icmFileGs_read(icmFile *pp, void *buffer, size_t size, size_t count)
{
icmFileGs *p = (icmFileGs *)pp;
uint tot;
int status = sgets(p->strp, buffer, size * count, &tot);
return (status < 0) ? status : tot;
}
private size_t
icmFileGs_write(icmFile *pp, void *buffer, size_t size, size_t count)
{
icmFileGs *p = (icmFileGs *)pp;
uint tot;
int status = sputs(p->strp, buffer, size * count, &tot);
return (status < 0) ? status : tot;
}
private int
icmFileGs_flush(icmFile *pp)
{
icmFileGs *p = (icmFileGs *)pp;
return s_std_write_flush(p->strp);
}
private int
icmFileGs_delete(icmFile *pp)
{
free(pp);
return 0;
}
private icmFile *
gx_wrap_icc_stream(stream *strp)
{
icmFileGs *p;
if ((p = (icmFileGs *) calloc(1,sizeof(icmFileGs))) == NULL)
return NULL;
p->seek = icmFileGs_seek;
p->read = icmFileGs_read;
p->write = icmFileGs_write;
p->flush = icmFileGs_flush;
p->del = icmFileGs_delete;
p->strp = strp;
return (icmFile *)p;
}
int
gx_load_icc_profile(gs_cie_icc *picc_info)
{
stream * instrp = picc_info->instrp;
icc * picc;
icmLuBase * plu = NULL;
icmFile *pfile = NULL;
if (picc_info->file_id != (instrp->read_id | instrp->write_id))
return_error(gs_error_ioerror);
if ((picc = new_icc()) == NULL)
return_error(gs_error_limitcheck);
{
icProfileClassSignature profile_class;
icColorSpaceSignature cspace_type;
gs_vector3 * ppt;
pfile = gx_wrap_icc_stream (instrp);
if ((picc->read(picc, pfile, 0)) != 0)
goto return_rangecheck;
profile_class = picc->header->deviceClass;
if ( profile_class != icSigInputClass &&
profile_class != icSigDisplayClass &&
profile_class != icSigOutputClass &&
profile_class != icSigColorSpaceClass )
goto return_rangecheck;
cspace_type = picc->header->pcs;
if (cspace_type == icSigLabData)
picc_info->pcs_is_cielab = true;
else if (cspace_type == icSigXYZData)
picc_info->pcs_is_cielab = false;
else
goto return_rangecheck;
cspace_type = picc->header->colorSpace;
if (cspace_type == icSigCmykData) {
if (picc_info->num_components != 4)
goto return_rangecheck;
} else if ( cspace_type == icSigRgbData ||
cspace_type == icSigLabData ) {
if (picc_info->num_components != 3)
goto return_rangecheck;
} else if (cspace_type == icSigGrayData) {
if (picc_info->num_components != 1)
goto return_rangecheck;
}
plu = picc->get_luobj( picc,
icmFwd,
icmDefaultIntent,
0,
icmLuOrdNorm );
if (plu == NULL)
goto return_rangecheck;
ppt = &picc_info->common.points.WhitePoint;
ppt->u = picc->header->illuminant.X;
ppt->v = picc->header->illuminant.Y;
ppt->w = picc->header->illuminant.Z;
picc_info->picc = picc;
picc_info->plu = plu;
picc_info->pfile = pfile;
}
return 0;
return_rangecheck:
if (plu != NULL)
plu->del(plu);
if (picc != NULL)
picc->del(picc);
if (pfile != NULL)
pfile->del(pfile);
return_error(gs_error_rangecheck);
}
private int
gx_install_CIEICC(const gs_color_space * pcs, gs_state * pgs)
{
const gs_icc_params * picc_params = (const gs_icc_params *)&pcs->params.icc;
gs_cie_icc * picc_info = picc_params->picc_info;
gx_cie_load_common_cache(&picc_info->common, pgs);
gx_cie_common_complete(&picc_info->common);
return gs_cie_cs_complete(pgs, true);
}
int
gs_cspace_build_CIEICC(
gs_color_space ** ppcspace,
void * client_data,
gs_memory_t * pmem )
{
gs_cie_icc * picc_info;
gs_color_space * pcs;
picc_info = gx_build_cie_space( ppcspace,
&gs_color_space_type_CIEICC,
&st_cie_icc,
pmem );
if (picc_info == NULL)
return_error(gs_error_VMerror);
gx_set_common_cie_defaults(&picc_info->common, client_data);
picc_info->common.points.WhitePoint.u = (float)0.9642;
picc_info->common.points.WhitePoint.v = 1.0000;
picc_info->common.points.WhitePoint.w = (float)0.8249;
picc_info->common.install_cspace = gx_install_CIEICC;
picc_info->num_components = 0;
picc_info->Range = Range4_default;
picc_info->instrp = NULL;
picc_info->pcs_is_cielab = false;
picc_info->picc = NULL;
picc_info->plu = NULL;
picc_info->pfile = NULL;
pcs = *ppcspace;
pcs->params.icc.picc_info = picc_info;
return 0;
}
private int
gx_serialize_CIEICC(const gs_color_space * pcs, stream * s)
{
const gs_icc_params * p = &pcs->params.icc;
gs_cie_icc *picc = p->picc_info;
uint n;
int code = gx_serialize_cspace_type(pcs, s);
long avail, pos, count;
byte buf[100];
if (code < 0)
return code;
code = gx_serialize_cie_common_elements(pcs, s);
if (code < 0)
return code;
code = sputs(s, (byte *)&picc->num_components, sizeof(picc->num_components), &n);
if (code < 0)
return code;
code = sputs(s, (byte *)&picc->Range, sizeof(picc->Range), &n);
if (code < 0)
return code;
if (sseek(picc->instrp, 0) < 0)
return_error(gs_error_unregistered);
if (savailable(picc->instrp, &avail) != 0)
return_error(gs_error_unregistered);
code = sputs(s, (byte *)&avail, sizeof(avail), &n);
if (code < 0)
return code;
for (pos = 0; pos < avail; pos += count) {
count = min(sizeof(buf), avail - pos);
code = sgets(picc->instrp, buf, count, &n);
if (code < 0)
return code;
code = sputs(s, buf, count, &n);
if (code < 0)
return code;
}
return sputs(s, (byte *)&picc->pcs_is_cielab, sizeof(picc->pcs_is_cielab), &n);
}