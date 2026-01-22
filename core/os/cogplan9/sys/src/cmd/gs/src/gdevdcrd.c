#include "math_.h"
#include "memory_.h"
#include "string_.h"
#include "gx.h"
#include "gserrors.h"
#include "gsparam.h"
#include "gscspace.h"
#include "gscrd.h"
#include "gscrdp.h"
#include "gxdevcli.h"
#include "gdevdcrd.h"
#define DENT(v, f)\
(v <= 0.5 ? v * f : (v - 0.5) * (1 - (0.5 * f)) / 0.5 + 0.5 * f)
private const gs_vector3 bit_WhitePoint = {(float)0.9505, 1, (float)1.0890};
private const gs_range3 bit_RangePQR = {
{{0, (float)0.9505}, {0, 1}, {0, (float)1.0890}}
};
private const float dent_PQR = 1.0;
private int
bit_TransformPQR_proc(int index, floatp in, const gs_cie_wbsd * pwbsd,
gs_cie_render * pcrd, float *out)
{
*out = DENT(in, dent_PQR);
return 0;
}
private const gs_cie_transform_proc3 bit_TransformPQR = {
bit_TransformPQR_proc, "bitTPQRDefault", {0, 0}, 0
};
private const float dent_LMN = 1.0;
private float
bit_EncodeLMN_proc(floatp in, const gs_cie_render * pcrd)
{
return DENT(in, dent_LMN);
}
private const gs_cie_render_proc3 bit_EncodeLMN = {
{bit_EncodeLMN_proc, bit_EncodeLMN_proc, bit_EncodeLMN_proc}
};
private const gs_range3 bit_RangeLMN = {
{{0, (float)0.9505}, {0, 1}, {0, (float)1.0890}}
};
private const gs_matrix3 bit_MatrixABC = {
{(float) 3.24063, (float)-0.96893, (float) 0.05571},
{(float)-1.53721, (float) 1.87576, (float)-0.20402},
{(float)-0.49863, (float) 0.04152, (float) 1.05700}
};
private float
bit_EncodeABC_proc(floatp in, const gs_cie_render * pcrd)
{
return pow(max(in, 0.0), 0.45);
}
private const gs_cie_render_proc3 bit_EncodeABC = {
{bit_EncodeABC_proc, bit_EncodeABC_proc, bit_EncodeABC_proc}
};
private const byte bit_rtt0[2*2*3] = {
0,0,0,
0,0,255,
0,255,0,
0,255,255
};
private const byte bit_rtt1[2*2*3] = {
255,0,0,
255,0,255,
255,255,0,
255,255,255
};
private const gs_const_string bit_rt_data[2] = {
{bit_rtt0, 2*2*3}, {bit_rtt1, 2*2*3}
};
private frac
bit_rt_proc(byte in, const gs_cie_render *pcrd)
{
return frac_1 * in / 255;
}
private const gs_cie_render_table_t bit_RenderTable = {
{3, {2, 2, 2}, 3, bit_rt_data},
{{bit_rt_proc, bit_rt_proc, bit_rt_proc}}
};
int
sample_device_crd_get_params(gx_device *pdev, gs_param_list *plist,
const char *crd_param_name)
{
int ecode = 0;
if (param_requested(plist, "CRDName") > 0) {
gs_param_string cns;
int code;
cns.data = (const byte *)crd_param_name;
cns.size = strlen(crd_param_name);
cns.persistent = true;
code = param_write_string(plist, "CRDName", &cns);
if (code < 0)
ecode = code;
}
if (param_requested(plist, crd_param_name) > 0) {
gs_cie_render *pcrd;
int code = gs_cie_render1_build(&pcrd, pdev->memory,
"sample_device_crd_get_params");
if (code >= 0) {
gs_cie_transform_proc3 tpqr;
tpqr = bit_TransformPQR;
tpqr.driver_name = pdev->dname;
code = gs_cie_render1_initialize(pdev->memory, pcrd, NULL,
&bit_WhitePoint, NULL ,
NULL , &bit_RangePQR, &tpqr,
NULL , &bit_EncodeLMN, &bit_RangeLMN,
&bit_MatrixABC, &bit_EncodeABC, NULL ,
&bit_RenderTable);
if (code >= 0) {
code = param_write_cie_render1(plist, crd_param_name, pcrd,
pdev->memory);
}
rc_decrement(pcrd, "sample_device_crd_get_params");
}
if (code < 0)
ecode = code;
}
if (param_requested(plist, bit_TransformPQR.proc_name) > 0) {
gs_cie_transform_proc my_proc = bit_TransformPQR_proc;
byte *my_addr = gs_alloc_string(pdev->memory, sizeof(my_proc),
"sd_crd_get_params(proc)");
int code;
if (my_addr == 0)
code = gs_note_error(gs_error_VMerror);
else {
gs_param_string as;
memcpy(my_addr, &my_proc, sizeof(my_proc));
as.data = my_addr;
as.size = sizeof(my_proc);
as.persistent = true;
code = param_write_string(plist, bit_TransformPQR.proc_name, &as);
}
if (code < 0)
ecode = code;
}
return ecode;
}