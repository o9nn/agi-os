#include "math_.h"
#include "memory_.h"
#include "ghost.h"
#include "oper.h"
#include "gscolor.h"
#include "gscspace.h"
#include "gscolor2.h"
#include "gsmatrix.h"
#include "gsimage.h"
#include "gxfixed.h"
#include "gsstruct.h"
#include "gxiparam.h"
#include "idict.h"
#include "idparam.h"
#include "estack.h"
#include "ialloc.h"
#include "igstate.h"
#include "ilevel.h"
#include "store.h"
#include "stream.h"
#include "ifilter.h"
#include "iimage.h"
private int zimage_data_setup(i_ctx_t *i_ctx_p, const gs_pixel_image_t * pim,
gx_image_enum_common_t * pie,
const ref * sources, int npop);
private int image_proc_process(i_ctx_t *);
private int image_file_continue(i_ctx_t *);
private int image_string_continue(i_ctx_t *);
private int image_cleanup(i_ctx_t *);
int
data_image_params(const gs_memory_t *mem,
const ref *op, gs_data_image_t *pim,
image_params *pip, bool require_DataSource,
int num_components, int max_bits_per_component,
bool has_alpha)
{
int code;
int decode_size;
ref *pds;
check_type(*op, t_dictionary);
check_dict_read(*op);
if ((code = dict_int_param(op, "Width", 0, max_int_in_fixed / 2,
-1, &pim->Width)) < 0 ||
(code = dict_int_param(op, "Height", 0, max_int_in_fixed / 2,
-1, &pim->Height)) < 0 ||
(code = dict_matrix_param(mem, op, "ImageMatrix",
&pim->ImageMatrix)) < 0 ||
(code = dict_bool_param(op, "MultipleDataSources", false,
&pip->MultipleDataSources)) < 0 ||
(code = dict_int_param(op, "BitsPerComponent", 1,
max_bits_per_component, -1,
&pim->BitsPerComponent)) < 0 ||
(code = decode_size = dict_floats_param(mem, op, "Decode",
num_components * 2,
&pim->Decode[0], NULL)) < 0 ||
(code = dict_bool_param(op, "Interpolate", false,
&pim->Interpolate)) < 0
)
return code;
pip->pDecode = &pim->Decode[0];
if ((code = dict_find_string(op, "DataSource", &pds)) <= 0) {
if (require_DataSource)
return (code < 0 ? code : gs_note_error(e_rangecheck));
return 1;
}
if (pip->MultipleDataSources) {
long i, n = num_components + (has_alpha ? 1 : 0);
if (!r_is_array(pds))
return_error(e_typecheck);
if (r_size(pds) != n)
return_error(e_rangecheck);
for (i = 0; i < n; ++i)
array_get(mem, pds, i, &pip->DataSource[i]);
} else
pip->DataSource[0] = *pds;
return 0;
}
int
pixel_image_params(i_ctx_t *i_ctx_p, const ref *op, gs_pixel_image_t *pim,
image_params *pip, int max_bits_per_component,
bool has_alpha)
{
int num_components =
gs_color_space_num_components(gs_currentcolorspace(igs));
int code;
if (num_components < 1)
return_error(e_rangecheck);
pim->ColorSpace = gs_currentcolorspace(igs);
code = data_image_params(imemory, op, (gs_data_image_t *) pim, pip, true,
num_components, max_bits_per_component,
has_alpha);
if (code < 0)
return code;
pim->format =
(pip->MultipleDataSources ? gs_image_format_component_planar :
gs_image_format_chunky);
return dict_bool_param(op, "CombineWithColor", false,
&pim->CombineWithColor);
}
int
zimage_setup(i_ctx_t *i_ctx_p, const gs_pixel_image_t * pim,
const ref * sources, bool uses_color, int npop)
{
gx_image_enum_common_t *pie;
int code =
gs_image_begin_typed((const gs_image_common_t *)pim, igs,
uses_color, &pie);
if (code < 0)
return code;
return zimage_data_setup(i_ctx_p, (const gs_pixel_image_t *)pim, pie,
sources, npop);
}
int
image1_setup(i_ctx_t * i_ctx_p, bool has_alpha)
{
os_ptr          op = osp;
gs_image_t      image;
image_params    ip;
int             code;
gs_image_t_init(&image, gs_currentcolorspace(igs));
code = pixel_image_params( i_ctx_p,
op,
(gs_pixel_image_t *)&image,
&ip,
(level2_enabled ? 16 : 8),
has_alpha );
if (code < 0)
return code;
image.Alpha = (has_alpha ? gs_image_alpha_last : gs_image_alpha_none);
return zimage_setup( i_ctx_p,
(gs_pixel_image_t *)&image,
&ip.DataSource[0],
image.CombineWithColor,
1 );
}
private int
zimage1(i_ctx_t *i_ctx_p)
{
return image1_setup(i_ctx_p, false);
}
private int
zimagemask1(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
gs_image_t image;
image_params ip;
int code;
gs_image_t_init_mask_adjust(&image, false,
gs_incachedevice(igs) != CACHE_DEVICE_NONE);
code = data_image_params(imemory, op, (gs_data_image_t *) & image,
&ip, true, 1, 1, false);
if (code < 0)
return code;
if (ip.MultipleDataSources)
return_error(e_rangecheck);
return zimage_setup(i_ctx_p, (gs_pixel_image_t *)&image, &ip.DataSource[0],
true, 1);
}
#define NUM_PUSH(nsource) ((nsource) * 2 + 5)
#define EBOT_NUM_SOURCES(ep) ((ep) + 2)
#define EBOT_SOURCE(ep, i)\
((ep) + 3 + (EBOT_NUM_SOURCES(ep)->value.intval - 1 - (i)) * 2)
#define ETOP_SOURCE(ep, i)\
((ep) - 4 - (i) * 2)
#define ETOP_PLANE_INDEX(ep) ((ep) - 2)
#define ETOP_NUM_SOURCES(ep) ((ep) - 1)
private int
zimage_data_setup(i_ctx_t *i_ctx_p, const gs_pixel_image_t * pim,
gx_image_enum_common_t * pie, const ref * sources, int npop)
{
int num_sources = pie->num_planes;
int inumpush = NUM_PUSH(num_sources);
int code;
gs_image_enum *penum;
int px;
const ref *pp;
check_estack(inumpush + 2);
make_int(EBOT_NUM_SOURCES(esp), num_sources);
for (px = 0, pp = sources; px < num_sources; px++, pp++) {
es_ptr ep = EBOT_SOURCE(esp, px);
make_int(ep + 1, 1);
switch (r_type(pp)) {
case t_file:
if (!level2_enabled)
return_error(e_typecheck);
{
int pi;
for (pi = 0; pi < px; ++pi)
if (sources[pi].value.pfile == pp->value.pfile) {
make_int(ep + 1, -pi);
EBOT_SOURCE(esp, pi)[1].value.intval++;
break;
}
}
case t_string:
if (r_type(pp) != r_type(sources)) {
if (pie != NULL)
gx_image_end(pie, false);
return_error(e_typecheck);
}
check_read(*pp);
break;
default:
if (!r_is_proc(sources)) {
if (pie != NULL)
gx_image_end(pie, false);
return_error(e_typecheck);
}
check_proc(*pp);
}
*ep = *pp;
}
if ((penum = gs_image_enum_alloc(imemory_local, "image_setup")) == 0)
return_error(e_VMerror);
code = gs_image_enum_init(penum, pie, (const gs_data_image_t *)pim, igs);
if (code != 0) {
int code1 = gs_image_cleanup_and_free_enum(penum);
if (code >= 0)
pop(npop);
if (code >= 0 && code1 < 0)
code = code1;
return code;
}
push_mark_estack(es_other, image_cleanup);
esp += inumpush - 1;
make_int(ETOP_PLANE_INDEX(esp), 0);
make_int(ETOP_NUM_SOURCES(esp), num_sources);
make_struct(esp, avm_local, penum);
switch (r_type(sources)) {
case t_file:
push_op_estack(image_file_continue);
break;
case t_string:
push_op_estack(image_string_continue);
break;
default:
push_op_estack(image_proc_process);
break;
}
pop(npop);
return o_push_estack;
}
private es_ptr
zimage_pop_estack(es_ptr tep)
{
return tep - NUM_PUSH(ETOP_NUM_SOURCES(tep)->value.intval);
}
private int
image_proc_continue(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
gs_image_enum *penum = r_ptr(esp, gs_image_enum);
int px = ETOP_PLANE_INDEX(esp)->value.intval;
int num_sources = ETOP_NUM_SOURCES(esp)->value.intval;
uint size, used[gs_image_max_planes];
gs_const_string plane_data[gs_image_max_planes];
const byte *wanted;
int i, code;
if (!r_has_type_attrs(op, t_string, a_read)) {
check_op(1);
esp = zimage_pop_estack(esp);
image_cleanup(i_ctx_p);
return_error(!r_has_type(op, t_string) ? e_typecheck : e_invalidaccess);
}
size = r_size(op);
if (size == 0 && ETOP_SOURCE(esp, 0)[1].value.intval == 0)
code = 1;
else {
for (i = 0; i < num_sources; i++)
plane_data[i].size = 0;
plane_data[px].data = op->value.bytes;
plane_data[px].size = size;
code = gs_image_next_planes(penum, plane_data, used);
if (code == e_RemapColor) {
op->value.bytes += used[px];
r_dec_size(op, used[px]);
ETOP_SOURCE(esp, 0)[1].value.intval = 0;
return code;
}
}
if (code) {
esp = zimage_pop_estack(esp);
pop(1);
image_cleanup(i_ctx_p);
return (code < 0 ? code : o_pop_estack);
}
pop(1);
wanted = gs_image_planes_wanted(penum);
do {
if (++px == num_sources)
px = 0;
} while (!wanted[px]);
ETOP_PLANE_INDEX(esp)->value.intval = px;
return image_proc_process(i_ctx_p);
}
private int
image_proc_process(i_ctx_t *i_ctx_p)
{
int px = ETOP_PLANE_INDEX(esp)->value.intval;
gs_image_enum *penum = r_ptr(esp, gs_image_enum);
const byte *wanted = gs_image_planes_wanted(penum);
int num_sources = ETOP_NUM_SOURCES(esp)->value.intval;
const ref *pp;
ETOP_SOURCE(esp, 0)[1].value.intval = 0;
while (!wanted[px]) {
if (++px == num_sources)
px = 0;
ETOP_PLANE_INDEX(esp)->value.intval = px;
}
pp = ETOP_SOURCE(esp, px);
push_op_estack(image_proc_continue);
*++esp = *pp;
return o_push_estack;
}
private int
image_file_continue(i_ctx_t *i_ctx_p)
{
gs_image_enum *penum = r_ptr(esp, gs_image_enum);
int num_sources = ETOP_NUM_SOURCES(esp)->value.intval;
for (;;) {
uint min_avail = max_int;
gs_const_string plane_data[gs_image_max_planes];
int code;
int px;
const ref *pp;
bool at_eof = false;
for (px = 0, pp = ETOP_SOURCE(esp, 0); px < num_sources;
++px, pp -= 2
) {
int num_aliases = pp[1].value.intval;
stream *s = pp->value.pfile;
int min_left;
uint avail;
if (num_aliases <= 0)
num_aliases = ETOP_SOURCE(esp, -num_aliases)[1].value.intval;
while ((avail = sbufavailable(s)) <=
(min_left = sbuf_min_left(s)) + num_aliases - 1) {
int next = s->end_status;
switch (next) {
case 0:
s_process_read_buf(s);
continue;
case EOFC:
at_eof = true;
break;
case INTC:
case CALLC:
return
s_handle_read_exception(i_ctx_p, next, pp,
NULL, 0, image_file_continue);
default:
return_error(e_ioerror);
}
break;
}
if (avail >= min_left)
avail = (avail - min_left) / num_aliases;
if (avail < min_avail)
min_avail = avail;
plane_data[px].data = sbufptr(s);
plane_data[px].size = avail;
}
{
int pi;
uint used[gs_image_max_planes];
code = gs_image_next_planes(penum, plane_data, used);
for (pi = 0, pp = ETOP_SOURCE(esp, 0); pi < num_sources;
++pi, pp -= 2
)
sbufskip(pp->value.pfile, used[pi]);
if (code == e_RemapColor)
return code;
}
if (at_eof)
code = 1;
if (code) {
int code1;
esp = zimage_pop_estack(esp);
code1 = image_cleanup(i_ctx_p);
return (code < 0 ? code : code1 < 0 ? code1 : o_pop_estack);
}
}
}
private int
image_string_continue(i_ctx_t *i_ctx_p)
{
gs_image_enum *penum = r_ptr(esp, gs_image_enum);
int num_sources = ETOP_NUM_SOURCES(esp)->value.intval;
gs_const_string sources[gs_image_max_planes];
uint used[gs_image_max_planes];
memset(sources, 0, sizeof(sources[0]) * num_sources);
for (;;) {
int px;
int code = gs_image_next_planes(penum, sources, used);
if (code == e_RemapColor)
return code;
stop_now:
if (code) {
esp -= NUM_PUSH(num_sources);
image_cleanup(i_ctx_p);
return (code < 0 ? code : o_pop_estack);
}
for (px = 0; px < num_sources; ++px)
if (sources[px].size == 0) {
const ref *psrc = ETOP_SOURCE(esp, px);
uint size = r_size(psrc);
if (size == 0) {
code = 1;
goto stop_now;
}
sources[px].data = psrc->value.bytes;
sources[px].size = size;
}
}
}
private int
image_cleanup(i_ctx_t *i_ctx_p)
{
es_ptr ep_top = esp + NUM_PUSH(EBOT_NUM_SOURCES(esp)->value.intval);
gs_image_enum *penum = r_ptr(ep_top, gs_image_enum);
return gs_image_cleanup_and_free_enum(penum);
}
const op_def zimage_op_defs[] =
{
{"1.image1", zimage1},
{"1.imagemask1", zimagemask1},
{"1%image_proc_continue", image_proc_continue},
{"0%image_file_continue", image_file_continue},
{"0%image_string_continue", image_string_continue},
op_def_end(0)
};