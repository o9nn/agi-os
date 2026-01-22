#include "math_.h"
#include "memory_.h"
#include "string_.h"
#include "gx.h"
#include "gscdefs.h"
#include "gsdevice.h"
#include "gserrors.h"
#include "gsmatrix.h"
#include "gsparam.h"
#include "gsstruct.h"
#include "gsutil.h"
#include "gxcspace.h"
#include "gscolor2.h"
#include "gscrd.h"
extern_gs_lib_device_list();
public_st_cie_render1();
private
ENUM_PTRS_WITH(cie_render1_enum_ptrs, gs_cie_render *pcrd) return 0;
case 0: return ENUM_OBJ(pcrd->client_data);
case 1: return ENUM_OBJ(pcrd->RenderTable.lookup.table);
case 2: return (pcrd->RenderTable.lookup.table ?
ENUM_CONST_STRING(&pcrd->TransformPQR.proc_data) :
0);
ENUM_PTRS_END
private RELOC_PTRS_WITH(cie_render1_reloc_ptrs, gs_cie_render *pcrd);
RELOC_OBJ_VAR(pcrd->client_data);
if (pcrd->RenderTable.lookup.table)
{
RELOC_OBJ_VAR(pcrd->RenderTable.lookup.table);
RELOC_CONST_STRING_VAR(pcrd->TransformPQR.proc_data);
}
RELOC_PTRS_END
private int
tpqr_identity(int index, floatp in, const gs_cie_wbsd * pwbsd,
gs_cie_render * pcrd, float *out)
{
*out = in;
return 0;
}
private int
tpqr_from_cache(int index, floatp in, const gs_cie_wbsd * pwbsd,
gs_cie_render * pcrd, float *out)
{
*out = in;
return 0;
}
private float
render_identity(floatp in, const gs_cie_render * pcrd)
{
return in;
}
private frac
render_table_identity(byte in, const gs_cie_render * pcrd)
{
return byte2frac(in);
}
private float
EncodeABC_cached_A(floatp in, const gs_cie_render * pcrd)
{
return gs_cie_cached_value(in, &pcrd->caches.EncodeABC[0].floats);
}
private float
EncodeABC_cached_B(floatp in, const gs_cie_render * pcrd)
{
return gs_cie_cached_value(in, &pcrd->caches.EncodeABC[1].floats);
}
private float
EncodeABC_cached_C(floatp in, const gs_cie_render * pcrd)
{
return gs_cie_cached_value(in, &pcrd->caches.EncodeABC[2].floats);
}
private float
EncodeLMN_cached_L(floatp in, const gs_cie_render * pcrd)
{
return gs_cie_cached_value(in, &pcrd->caches.EncodeLMN.caches[0].floats);
}
private float
EncodeLMN_cached_M(floatp in, const gs_cie_render * pcrd)
{
return gs_cie_cached_value(in, &pcrd->caches.EncodeLMN.caches[1].floats);
}
private float
EncodeLMN_cached_N(floatp in, const gs_cie_render * pcrd)
{
return gs_cie_cached_value(in, &pcrd->caches.EncodeLMN.caches[2].floats);
}
private frac
RTT_cached(byte in, const gs_cie_render * pcrd, int i)
{
return pcrd->caches.RenderTableT[i].fracs.values[
in * (gx_cie_cache_size - 1) / 255
];
}
private frac
RTT_cached_0(byte in, const gs_cie_render * pcrd)
{
return RTT_cached(in, pcrd, 0);
}
private frac
RTT_cached_1(byte in, const gs_cie_render * pcrd)
{
return RTT_cached(in, pcrd, 1);
}
private frac
RTT_cached_2(byte in, const gs_cie_render * pcrd)
{
return RTT_cached(in, pcrd, 2);
}
private frac
RTT_cached_3(byte in, const gs_cie_render * pcrd)
{
return RTT_cached(in, pcrd, 3);
}
private int
tpqr_do_lookup(gs_cie_render *pcrd, const gx_device *dev_proto)
{
gx_device *dev;
gs_memory_t *mem = pcrd->rc.memory;
gs_c_param_list list;
gs_param_string proc_addr;
int code;
code = gs_copydevice(&dev, dev_proto, mem);
if (code < 0)
return code;
gs_c_param_list_write(&list, mem);
code = param_request((gs_param_list *)&list,
pcrd->TransformPQR.proc_name);
if (code >= 0) {
code = gs_getdeviceparams(dev, (gs_param_list *)&list);
if (code >= 0) {
gs_c_param_list_read(&list);
code = param_read_string((gs_param_list *)&list,
pcrd->TransformPQR.proc_name,
&proc_addr);
if (code == 0 && proc_addr.size == sizeof(gs_cie_transform_proc)) {
memcpy(&pcrd->TransformPQR.proc, proc_addr.data,
sizeof(gs_cie_transform_proc));
} else
code = gs_note_error(gs_error_rangecheck);
}
}
gs_c_param_list_release(&list);
gs_free_object(mem, dev, "tpqr_do_lookup(device)");
return code;
}
private int
tpqr_lookup(int index, floatp in, const gs_cie_wbsd * pwbsd,
gs_cie_render * pcrd, float *out)
{
const gx_device *const *dev_list;
int count = gs_lib_device_list(&dev_list, NULL);
int i;
int code;
for (i = 0; i < count; ++i)
if (!strcmp(gs_devicename(dev_list[i]),
pcrd->TransformPQR.driver_name))
break;
if (i < count)
code = tpqr_do_lookup(pcrd, dev_list[i]);
else
code = gs_note_error(gs_error_undefined);
if (code < 0)
return code;
return pcrd->TransformPQR.proc(index, in, pwbsd, pcrd, out);
}
const gs_cie_transform_proc3 TransformPQR_default = {
tpqr_identity,
0,
{0, 0},
0
};
const gs_cie_transform_proc3 TransformPQR_from_cache = {
tpqr_from_cache,
0,
{0, 0},
0
};
const gs_cie_transform_proc TransformPQR_lookup_proc_name = tpqr_lookup;
const gs_cie_render_proc3 Encode_default = {
{render_identity, render_identity, render_identity}
};
const gs_cie_render_proc3 EncodeLMN_from_cache = {
{EncodeLMN_cached_L, EncodeLMN_cached_M, EncodeLMN_cached_N}
};
const gs_cie_render_proc3 EncodeABC_from_cache = {
{EncodeABC_cached_A, EncodeABC_cached_B, EncodeABC_cached_C}
};
const gs_cie_render_table_procs RenderTableT_default = {
{render_table_identity, render_table_identity, render_table_identity,
render_table_identity
}
};
const gs_cie_render_table_procs RenderTableT_from_cache = {
{RTT_cached_0, RTT_cached_1, RTT_cached_2, RTT_cached_3}
};
int
gs_cie_render1_build(gs_cie_render ** ppcrd, gs_memory_t * mem,
client_name_t cname)
{
gs_cie_render *pcrd;
rc_alloc_struct_1(pcrd, gs_cie_render, &st_cie_render1, mem,
return_error(gs_error_VMerror), cname);
pcrd->id = gs_next_ids(mem, 1);
pcrd->client_data = 0;
pcrd->RenderTable.lookup.table = 0;
pcrd->status = CIE_RENDER_STATUS_BUILT;
*ppcrd = pcrd;
return 0;
}
int
gs_cie_render1_init_from(const gs_memory_t *mem,
gs_cie_render * pcrd,
void *client_data,
const gs_cie_render * pfrom_crd,
const gs_vector3 * WhitePoint,
const gs_vector3 * BlackPoint,
const gs_matrix3 * MatrixPQR,
const gs_range3 * RangePQR,
const gs_cie_transform_proc3 * TransformPQR,
const gs_matrix3 * MatrixLMN,
const gs_cie_render_proc3 * EncodeLMN,
const gs_range3 * RangeLMN,
const gs_matrix3 * MatrixABC,
const gs_cie_render_proc3 * EncodeABC,
const gs_range3 * RangeABC,
const gs_cie_render_table_t * RenderTable)
{
pcrd->id = gs_next_ids(mem, 1);
pcrd->client_data = client_data;
pcrd->points.WhitePoint = *WhitePoint;
pcrd->points.BlackPoint =
*(BlackPoint ? BlackPoint : &BlackPoint_default);
pcrd->MatrixPQR = *(MatrixPQR ? MatrixPQR : &Matrix3_default);
pcrd->RangePQR = *(RangePQR ? RangePQR : &Range3_default);
pcrd->TransformPQR =
*(TransformPQR ? TransformPQR : &TransformPQR_default);
pcrd->MatrixLMN = *(MatrixLMN ? MatrixLMN : &Matrix3_default);
pcrd->EncodeLMN = *(EncodeLMN ? EncodeLMN : &Encode_default);
if (pfrom_crd &&
!memcmp(&pcrd->EncodeLMN, &EncodeLMN_from_cache,
sizeof(EncodeLMN_from_cache))
)
memcpy(&pcrd->caches.EncodeLMN, &pfrom_crd->caches.EncodeLMN,
sizeof(pcrd->caches.EncodeLMN));
pcrd->RangeLMN = *(RangeLMN ? RangeLMN : &Range3_default);
pcrd->MatrixABC = *(MatrixABC ? MatrixABC : &Matrix3_default);
pcrd->EncodeABC = *(EncodeABC ? EncodeABC : &Encode_default);
if (pfrom_crd &&
!memcmp(&pcrd->EncodeABC, &EncodeABC_from_cache,
sizeof(EncodeABC_from_cache))
)
memcpy(pcrd->caches.EncodeABC, pfrom_crd->caches.EncodeABC,
sizeof(pcrd->caches.EncodeABC));
pcrd->RangeABC = *(RangeABC ? RangeABC : &Range3_default);
if (RenderTable) {
pcrd->RenderTable = *RenderTable;
if (pfrom_crd &&
!memcmp(&pcrd->RenderTable.T, &RenderTableT_from_cache,
sizeof(RenderTableT_from_cache))
) {
memcpy(pcrd->caches.RenderTableT, pfrom_crd->caches.RenderTableT,
sizeof(pcrd->caches.RenderTableT));
pcrd->caches.RenderTableT_is_identity =
pfrom_crd->caches.RenderTableT_is_identity;
}
} else {
pcrd->RenderTable.lookup.table = 0;
pcrd->RenderTable.T = RenderTableT_default;
}
pcrd->status = CIE_RENDER_STATUS_BUILT;
return 0;
}
int
gs_cie_render1_initialize(const gs_memory_t *mem,
gs_cie_render * pcrd, void *client_data,
const gs_vector3 * WhitePoint,
const gs_vector3 * BlackPoint,
const gs_matrix3 * MatrixPQR,
const gs_range3 * RangePQR,
const gs_cie_transform_proc3 * TransformPQR,
const gs_matrix3 * MatrixLMN,
const gs_cie_render_proc3 * EncodeLMN,
const gs_range3 * RangeLMN,
const gs_matrix3 * MatrixABC,
const gs_cie_render_proc3 * EncodeABC,
const gs_range3 * RangeABC,
const gs_cie_render_table_t * RenderTable)
{
return gs_cie_render1_init_from(mem, pcrd, client_data, NULL,
WhitePoint, BlackPoint,
MatrixPQR, RangePQR, TransformPQR,
MatrixLMN, EncodeLMN, RangeLMN,
MatrixABC, EncodeABC, RangeABC,
RenderTable);
}