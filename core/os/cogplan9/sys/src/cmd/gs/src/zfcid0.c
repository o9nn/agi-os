#include "memory_.h"
#include "ghost.h"
#include "oper.h"
#include "gsmatrix.h"
#include "gsccode.h"
#include "gsstruct.h"
#include "gxfcid.h"
#include "gxfont1.h"
#include "gxalloc.h"
#include "stream.h"
#include "bfont.h"
#include "files.h"
#include "ichar.h"
#include "ichar1.h"
#include "icid.h"
#include "idict.h"
#include "idparam.h"
#include "ifcid.h"
#include "ifont1.h"
#include "ifont2.h"
#include "ifont42.h"
#include "store.h"
font_proc_glyph_outline(zcharstring_glyph_outline);
private int
get_index(gs_glyph_data_t *pgd, int count, ulong *pval)
{
int i;
if (pgd->bits.size < count)
return_error(e_rangecheck);
*pval = 0;
for (i = 0; i < count; ++i)
*pval = (*pval << 8) + pgd->bits.data[i];
pgd->bits.data += count;
pgd->bits.size -= count;
return 0;
}
private int
cid0_read_bytes(gs_font_cid0 *pfont, ulong base, uint count, byte *buf,
gs_glyph_data_t *pgd)
{
const font_data *pfdata = pfont_data(pfont);
byte *data = buf;
gs_font *gdfont = 0;
int code = 0;
if (base != (long)base || base > base + count)
return_error(e_rangecheck);
if (r_has_type(&pfdata->u.cid0.DataSource, t_null)) {
const ref *pgdata = &pfdata->u.cid0.GlyphData;
if (r_has_type(pgdata, t_string)) {
uint size = r_size(pgdata);
if (base >= size || count > size - base)
return_error(e_rangecheck);
data = pgdata->value.bytes + base;
} else {
ulong skip = base;
uint copied = 0;
uint index = 0;
ref rstr;
uint size;
for (;; skip -= size, ++index) {
int code = array_get(pfont->memory, pgdata, index, &rstr);
if (code < 0)
return code;
if (!r_has_type(&rstr, t_string))
return_error(e_typecheck);
size = r_size(&rstr);
if (skip < size)
break;
}
size -= skip;
if (count <= size) {
data = rstr.value.bytes + skip;
} else {
if (data == 0) {
data = gs_alloc_string(pfont->memory, count,
"cid0_read_bytes");
if (data == 0)
return_error(e_VMerror);
gdfont = (gs_font *)pfont;
}
memcpy(data, rstr.value.bytes + skip, size);
copied = size;
while (copied < count) {
int code = array_get(pfont->memory, pgdata, ++index, &rstr);
if (code < 0)
goto err;
if (!r_has_type(&rstr, t_string)) {
code = gs_note_error(e_typecheck);
goto err;
}
size = r_size(&rstr);
if (size > count - copied)
size = count - copied;
memcpy(data + copied, rstr.value.bytes, size);
copied += size;
}
}
}
} else {
stream *s;
uint nread;
check_read_known_file(s, &pfdata->u.cid0.DataSource, return_error);
if (sseek(s, base) < 0)
return_error(e_ioerror);
if (data == 0) {
data = gs_alloc_string(pfont->memory, count, "cid0_read_bytes");
if (data == 0)
return_error(e_VMerror);
gdfont = (gs_font *)pfont;
}
if (sgets(s, data, count, &nread) < 0 || nread != count) {
code = gs_note_error(e_ioerror);
goto err;
}
}
gs_glyph_data_from_string(pgd, data, count, gdfont);
return code;
err:
if (data != buf)
gs_free_string(pfont->memory, data, count, "cid0_read_bytes");
return code;
}
private int
z9_glyph_data(gs_font_base *pbfont, gs_glyph glyph, gs_glyph_data_t *pgd,
int *pfidx)
{
gs_font_cid0 *pfont = (gs_font_cid0 *)pbfont;
const font_data *pfdata = pfont_data(pfont);
long glyph_index = (long)(glyph - gs_min_cid_glyph);
gs_glyph_data_t gdata;
ulong fidx;
int code;
gdata.memory = pfont->memory;
if (!r_has_type(&pfdata->u.cid0.GlyphDirectory, t_null)) {
code = font_gdir_get_outline(pfont->memory,
&pfdata->u.cid0.GlyphDirectory,
glyph_index, &gdata);
if (code < 0)
return code;
if (!gdata.bits.data)
return_error(e_rangecheck);
code = get_index(&gdata, pfont->cidata.FDBytes, &fidx);
if (code < 0)
return code;
if (fidx >= pfont->cidata.FDArray_size)
return_error(e_rangecheck);
if (pgd)
*pgd = gdata;
*pfidx = (int)fidx;
return code;
}
if (glyph_index < 0 || glyph_index >= pfont->cidata.common.CIDCount) {
*pfidx = 0;
if (pgd)
gs_glyph_data_from_null(pgd);
return_error(e_rangecheck);
}
{
byte fd_gd[(MAX_FDBytes + MAX_GDBytes) * 2];
uint num_bytes = pfont->cidata.FDBytes + pfont->cidata.common.GDBytes;
ulong base = pfont->cidata.CIDMapOffset + glyph_index * num_bytes;
ulong gidx, fidx_next, gidx_next;
int rcode = cid0_read_bytes(pfont, base, (ulong)(num_bytes * 2), fd_gd,
&gdata);
gs_glyph_data_t orig_data;
if (rcode < 0)
return rcode;
orig_data = gdata;
if ((code = get_index(&gdata, pfont->cidata.FDBytes, &fidx)) < 0 ||
(code = get_index(&gdata, pfont->cidata.common.GDBytes, &gidx)) < 0 ||
(code = get_index(&gdata, pfont->cidata.FDBytes, &fidx_next)) < 0 ||
(code = get_index(&gdata, pfont->cidata.common.GDBytes, &gidx_next)) < 0
)
DO_NOTHING;
gs_glyph_data_free(&orig_data, "z9_glyph_data");
if (code < 0)
return code;
if (gidx_next <= gidx) {
*pfidx = 0;
if (pgd)
gs_glyph_data_from_null(pgd);
return_error(e_undefined);
}
if (fidx >= pfont->cidata.FDArray_size)
return_error(e_rangecheck);
*pfidx = (int)fidx;
if (pgd == 0)
return 0;
return cid0_read_bytes(pfont, gidx, gidx_next - gidx, NULL, pgd);
}
}
private int
z9_glyph_outline(gs_font *font, int WMode, gs_glyph glyph, const gs_matrix *pmat,
gx_path *ppath, double sbw[4])
{
gs_font_cid0 *const pfcid = (gs_font_cid0 *)font;
ref gref;
gs_glyph_data_t gdata;
int code, fidx, ocode;
gdata.memory = font->memory;
code = pfcid->cidata.glyph_data((gs_font_base *)pfcid, glyph, &gdata,
&fidx);
if (code < 0)
return code;
glyph_ref(font->memory, glyph, &gref);
ocode = zcharstring_outline(pfcid->cidata.FDArray[fidx], WMode, &gref, &gdata,
pmat, ppath, sbw);
gs_glyph_data_free(&gdata, "z9_glyph_outline");
return ocode;
}
private int
z9_glyph_info(gs_font *font, gs_glyph glyph, const gs_matrix *pmat,
int members, gs_glyph_info_t *info)
{
int wmode = (members & GLYPH_INFO_WIDTH0 ? 0 : 1);
return z1_glyph_info_generic(font, glyph, pmat, members, info,
&gs_default_glyph_info, wmode);
}
private int
z9_FDArray_glyph_data(gs_font_type1 * pfont, gs_glyph glyph,
gs_glyph_data_t *pgd)
{
return_error(e_invalidfont);
}
private int
z9_FDArray_seac_data(gs_font_type1 *pfont, int ccode, gs_glyph *pglyph,
gs_const_string *gstr, gs_glyph_data_t *pgd)
{
return_error(e_invalidfont);
}
private int
fd_array_element(i_ctx_t *i_ctx_p, gs_font_type1 **ppfont, ref *prfd)
{
charstring_font_refs_t refs;
gs_type1_data data1;
build_proc_refs build;
gs_font_base *pbfont;
gs_font_type1 *pfont;
int fonttype = 1;
int code = charstring_font_get_refs(prfd, &refs);
if (code < 0 ||
(code = dict_int_param(prfd, "FontType", 1, 2, 1, &fonttype)) < 0
)
return code;
switch (fonttype) {
case 1:
data1.interpret = gs_type1_interpret;
data1.subroutineNumberBias = 0;
data1.lenIV = DEFAULT_LENIV_1;
code = charstring_font_params(imemory, prfd, &refs, &data1);
if (code < 0)
return code;
code = build_proc_name_refs(imemory, &build,
"%Type1BuildChar", "%Type1BuildGlyph");
break;
case 2:
code = type2_font_params(prfd, &refs, &data1);
if (code < 0)
return code;
code = charstring_font_params(imemory, prfd, &refs, &data1);
if (code < 0)
return code;
code = build_proc_name_refs(imemory, &build,
"%Type2BuildChar", "%Type2BuildGlyph");
break;
default:
return_error(e_Fatal);
}
if (code < 0)
return code;
code = build_gs_FDArray_font(i_ctx_p, prfd, &pbfont, fonttype,
&st_gs_font_type1, &build);
if (code < 0)
return code;
pfont = (gs_font_type1 *)pbfont;
pbfont->FAPI = NULL;
pbfont->FAPI_font_data = NULL;
charstring_font_init(pfont, &refs, &data1);
pfont->data.procs.glyph_data = z9_FDArray_glyph_data;
pfont->data.procs.seac_data = z9_FDArray_seac_data;
*ppfont = pfont;
return 0;
}
private int
notify_remove_font_type9(void *proc_data, void *event_data)
{
if (event_data == NULL) {
gs_font_cid0 *pfcid = proc_data;
int i;
for (i = 0; i < pfcid->cidata.FDArray_size; ++i) {
if (pfcid->cidata.FDArray[i]->data.parent == (gs_font_base *)pfcid)
pfcid->cidata.FDArray[i]->data.parent = NULL;
}
}
return 0;
}
private int
zbuildfont9(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
build_proc_refs build;
int code = build_proc_name_refs(imemory, &build, NULL, "%Type9BuildGlyph");
gs_font_cid_data common;
ref GlyphDirectory, GlyphData, DataSource;
ref *prfda, cfnstr;
ref *pCIDFontName, CIDFontName;
gs_font_type1 **FDArray;
uint FDArray_size;
int FDBytes;
uint CIDMapOffset;
gs_font_base *pfont;
gs_font_cid0 *pfcid;
uint i;
if (code < 0 ||
(code = cid_font_data_param(op, &common, &GlyphDirectory)) < 0 ||
(code = dict_find_string(op, "FDArray", &prfda)) < 0 ||
(code = dict_find_string(op, "CIDFontName", &pCIDFontName)) <= 0 ||
(code = dict_int_param(op, "FDBytes", 0, MAX_FDBytes, -1, &FDBytes)) < 0
)
return code;
CIDFontName = *pCIDFontName;
if (r_has_type(&GlyphDirectory, t_null)) {
ref *pGlyphData;
if ((code = dict_find_string(op, "GlyphData", &pGlyphData)) < 0 ||
(code = dict_uint_param(op, "CIDMapOffset", 0, max_uint - 1,
max_uint, &CIDMapOffset)) < 0)
return code;
GlyphData = *pGlyphData;
if (r_has_type(&GlyphData, t_integer)) {
ref *pds;
stream *ignore_s;
if ((code = dict_find_string(op, "DataSource", &pds)) < 0)
return code;
check_read_file(ignore_s, pds);
DataSource = *pds;
} else {
if (!r_has_type(&GlyphData, t_string) && !r_is_array(&GlyphData))
return_error(e_typecheck);
make_null(&DataSource);
}
} else {
make_null(&GlyphData);
make_null(&DataSource);
CIDMapOffset = 0;
}
if (!r_is_array(prfda))
return_error(e_invalidfont);
FDArray_size = r_size(prfda);
if (FDArray_size == 0)
return_error(e_invalidfont);
FDArray = ialloc_struct_array(FDArray_size, gs_font_type1 *,
&st_gs_font_type1_ptr_element,
"buildfont9(FDarray)");
if (FDArray == 0)
return_error(e_VMerror);
memset(FDArray, 0, sizeof(gs_font_type1 *) * FDArray_size);
for (i = 0; i < FDArray_size; ++i) {
ref rfd;
array_get(imemory, prfda, (long)i, &rfd);
code = fd_array_element(i_ctx_p, &FDArray[i], &rfd);
if (code < 0)
goto fail;
}
code = build_gs_simple_font(i_ctx_p, op, &pfont, ft_CID_encrypted,
&st_gs_font_cid0, &build,
bf_Encoding_optional |
bf_UniqueID_ignored);
if (code < 0)
goto fail;
pfont->procs.enumerate_glyph = gs_font_cid0_enumerate_glyph;
pfont->procs.glyph_outline = z9_glyph_outline;
pfont->procs.glyph_info = z9_glyph_info;
pfcid = (gs_font_cid0 *)pfont;
pfcid->cidata.common = common;
pfcid->cidata.CIDMapOffset = CIDMapOffset;
pfcid->cidata.FDArray = FDArray;
pfcid->cidata.FDArray_size = FDArray_size;
pfcid->cidata.FDBytes = FDBytes;
pfcid->cidata.glyph_data = z9_glyph_data;
pfcid->cidata.proc_data = 0;
get_font_name(imemory, &cfnstr, &CIDFontName);
copy_font_name(&pfcid->font_name, &cfnstr);
ref_assign(&pfont_data(pfont)->u.cid0.GlyphDirectory, &GlyphDirectory);
ref_assign(&pfont_data(pfont)->u.cid0.GlyphData, &GlyphData);
ref_assign(&pfont_data(pfont)->u.cid0.DataSource, &DataSource);
code = define_gs_font((gs_font *)pfont);
if (code >= 0)
code = gs_notify_register(&pfont->notify_list, notify_remove_font_type9, pfont);
if (code >= 0) {
for (i = 0; i < FDArray_size; ++i) {
FDArray[i]->dir = pfont->dir;
FDArray[i]->data.parent = pfont;
}
return code;
}
fail:
ifree_object(FDArray, "buildfont9(FDarray)");
return code;
}
int
ztype9mapcid(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
gs_font *pfont;
gs_font_cid0 *pfcid;
int code = font_param(op - 1, &pfont);
gs_glyph_data_t gdata;
int fidx;
if (code < 0)
return code;
if (pfont->FontType != ft_CID_encrypted)
return_error(e_invalidfont);
check_type(*op, t_integer);
pfcid = (gs_font_cid0 *)pfont;
gdata.memory = pfont->memory;
code = pfcid->cidata.glyph_data((gs_font_base *)pfcid,
(gs_glyph)(gs_min_cid_glyph + op->value.intval),
&gdata, &fidx);
if (code < 0) {
int default_fallback_CID = 0 ;
if_debug2('J', "[J]ztype9cidmap() use CID %d instead of glyph-missing CID %d\n", default_fallback_CID, op->value.intval);
op->value.intval = default_fallback_CID;
code = pfcid->cidata.glyph_data((gs_font_base *)pfcid,
(gs_glyph)(gs_min_cid_glyph + default_fallback_CID),
&gdata, &fidx);
if (code < 0) {
if_debug1('J', "[J]ztype9cidmap() could not load default glyph (CID %d)\n", op->value.intval);
return_error(e_invalidfont);
}
}
make_const_string(op - 1,
a_readonly | imemory_space((gs_ref_memory_t *)pfont->memory),
gdata.bits.size,
gdata.bits.data);
make_int(op, fidx);
return code;
}
const op_def zfcid0_op_defs[] =
{
{"2.buildfont9", zbuildfont9},
{"2.type9mapcid", ztype9mapcid},
op_def_end(0)
};