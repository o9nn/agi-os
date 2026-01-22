#include "memory_.h"
#include "ctype_.h"
#include "string_.h"
#include "gx.h"
#include "gserrors.h"
#include "gsutil.h"
#include "gxfcid.h"
#include "gxfcopy.h"
#include "gxfont.h"
#include "gxfont42.h"
#include "gdevpsf.h"
#include "gdevpdfx.h"
#include "gdevpdfo.h"
#include "gdevpdtb.h"
#include "gdevpdtf.h"
#define MAX_NO_SUBSET_GLYPHS 500
struct pdf_base_font_s {
gs_font_base *copied;
gs_font_base *complete;
enum {
DO_SUBSET_UNKNOWN = 0,
DO_SUBSET_NO,
DO_SUBSET_YES
} do_subset;
bool is_standard;
int num_glyphs;
byte *CIDSet;
gs_string font_name;
bool written;
cos_dict_t *FontFile;
};
BASIC_PTRS(pdf_base_font_ptrs) {
GC_OBJ_ELT(pdf_base_font_t, copied),
GC_OBJ_ELT(pdf_base_font_t, complete),
GC_OBJ_ELT(pdf_base_font_t, CIDSet),
GC_OBJ_ELT(pdf_base_font_t, FontFile),
GC_STRING_ELT(pdf_base_font_t, font_name)
};
gs_private_st_basic(st_pdf_base_font, pdf_base_font_t, "pdf_base_font_t",
pdf_base_font_ptrs, pdf_base_font_data);
#define SUBSET_PREFIX_SIZE 7
bool
pdf_has_subset_prefix(const byte *str, uint size)
{
int i;
if (size < SUBSET_PREFIX_SIZE || str[SUBSET_PREFIX_SIZE - 1] != '+')
return false;
for (i = 0; i < SUBSET_PREFIX_SIZE - 1; ++i)
if ((uint)(str[i] - 'A') >= 26)
return false;
return true;
}
private ulong
hash(ulong v, int index, ushort w)
{
return v * 3141592653u + w;
}
int
pdf_add_subset_prefix(const gx_device_pdf *pdev, gs_string *pstr, byte *used, int count)
{
uint size = pstr->size;
byte *data = gs_resize_string(pdev->pdf_memory, pstr->data, size,
size + SUBSET_PREFIX_SIZE,
"pdf_add_subset_prefix");
int len = (count + 7) / 8;
ulong v = 0;
ushort t = 0;
int i;
if (data == 0)
return_error(gs_error_VMerror);
for (i = 0; i < len; i += sizeof(ushort))
v = hash(v, i, *(ushort *)(used + i));
if (i < len) {
i -= sizeof(ushort);
memmove(&t, used + i, len - i);
v = hash(v, i, *(ushort *)(used + i));
}
memmove(data + SUBSET_PREFIX_SIZE, data, size);
for (i = 0; i < SUBSET_PREFIX_SIZE - 1; ++i, v /= 26)
data[i] = 'A' + (v % 26);
data[SUBSET_PREFIX_SIZE - 1] = '+';
pstr->data = data;
pstr->size = size + SUBSET_PREFIX_SIZE;
return 0;
}
private int
pdf_end_fontfile(gx_device_pdf *pdev, pdf_data_writer_t *pdw)
{
return pdf_close_aside(pdw->pdev);
}
int
pdf_base_font_alloc(gx_device_pdf *pdev, pdf_base_font_t **ppbfont,
gs_font_base *font, const gs_matrix *orig_matrix,
bool is_standard, bool orig_name)
{
gs_memory_t *mem = pdev->pdf_memory;
gs_font *copied;
gs_font *complete;
pdf_base_font_t *pbfont =
gs_alloc_struct(mem, pdf_base_font_t,
&st_pdf_base_font, "pdf_base_font_alloc");
const gs_font_name *pfname = pdf_choose_font_name((gs_font *)font, orig_name);
gs_const_string font_name;
char fnbuf[3 + sizeof(long) / 3 + 1];
int code;
if (pbfont == 0)
return_error(gs_error_VMerror);
code = gs_copy_font((gs_font *)font, orig_matrix, mem, &copied);
if (code < 0)
goto fail;
memset(pbfont, 0, sizeof(*pbfont));
{
copied->FontMatrix.tx = copied->FontMatrix.ty = 0;
}
switch (font->FontType) {
case ft_encrypted:
case ft_encrypted2:
pbfont->do_subset = (is_standard ? DO_SUBSET_NO : DO_SUBSET_UNKNOWN);
pbfont->num_glyphs = -1;
break;
case ft_TrueType:
pbfont->num_glyphs = ((gs_font_type42 *)font)->data.trueNumGlyphs;
pbfont->do_subset =
(pbfont->num_glyphs <= MAX_NO_SUBSET_GLYPHS ?
DO_SUBSET_UNKNOWN : DO_SUBSET_YES);
break;
case ft_CID_encrypted:
pbfont->num_glyphs = ((gs_font_cid0 *)font)->cidata.common.CIDCount;
goto cid;
case ft_CID_TrueType:
pbfont->num_glyphs = ((gs_font_cid2 *)font)->cidata.common.CIDCount;
cid:
pbfont->do_subset = DO_SUBSET_YES;
pbfont->CIDSet =
gs_alloc_bytes(mem, (pbfont->num_glyphs + 7) / 8,
"pdf_base_font_alloc(CIDSet)");
if (pbfont->CIDSet == 0) {
code = gs_note_error(gs_error_VMerror);
goto fail;
}
memset(pbfont->CIDSet, 0, (pbfont->num_glyphs + 7) / 8);
break;
default:
code = gs_note_error(gs_error_rangecheck);
goto fail;
}
if (pbfont->do_subset != DO_SUBSET_YES) {
if (is_standard)
complete = copied, code = 0;
else
code = gs_copy_font((gs_font *)font, &font->FontMatrix, mem, &complete);
if (code >= 0)
code = gs_copy_font_complete((gs_font *)font, complete);
if (pbfont->num_glyphs < 0) {
int index, count;
gs_glyph glyph;
for (index = 0, count = 0;
(font->procs.enumerate_glyph((gs_font *)font, &index,
GLYPH_SPACE_NAME, &glyph),
index != 0);
)
++count;
pbfont->num_glyphs = count;
}
} else
complete = copied;
pbfont->copied = (gs_font_base *)copied;
pbfont->complete = (gs_font_base *)complete;
pbfont->is_standard = is_standard;
if (pfname->size > 0) {
font_name.data = pfname->chars;
font_name.size = pfname->size;
while (pdf_has_subset_prefix(font_name.data, font_name.size)) {
font_name.data += SUBSET_PREFIX_SIZE;
font_name.size -= SUBSET_PREFIX_SIZE;
}
} else {
sprintf(fnbuf, ".F%lx", (ulong)copied);
font_name.data = (byte *)fnbuf;
font_name.size = strlen(fnbuf);
}
pbfont->font_name.data =
gs_alloc_string(mem, font_name.size, "pdf_base_font_alloc(font_name)");
if (pbfont->font_name.data == 0)
goto fail;
memcpy(pbfont->font_name.data, font_name.data, font_name.size);
pbfont->font_name.size = font_name.size;
*ppbfont = pbfont;
return 0;
fail:
gs_free_object(mem, pbfont, "pdf_base_font_alloc");
return code;
}
gs_string *
pdf_base_font_name(pdf_base_font_t *pbfont)
{
return &pbfont->font_name;
}
gs_font_base *
pdf_base_font_font(const pdf_base_font_t *pbfont, bool complete)
{
return (complete ? pbfont->complete : pbfont->copied);
}
bool
pdf_base_font_is_subset(const pdf_base_font_t *pbfont)
{
return pbfont->do_subset == DO_SUBSET_YES;
}
void
pdf_base_font_drop_complete(pdf_base_font_t *pbfont)
{
pbfont->complete = NULL;
}
int
pdf_base_font_copy_glyph(pdf_base_font_t *pbfont, gs_glyph glyph,
gs_font_base *font)
{
int code =
gs_copy_glyph_options((gs_font *)font, glyph,
(gs_font *)pbfont->copied,
(pbfont->is_standard ? COPY_GLYPH_NO_NEW : 0));
if (code < 0)
return code;
if (pbfont->CIDSet != 0 &&
(uint)(glyph - GS_MIN_CID_GLYPH) < pbfont->num_glyphs
) {
uint cid = glyph - GS_MIN_CID_GLYPH;
pbfont->CIDSet[cid >> 3] |= 0x80 >> (cid & 7);
}
return 0;
}
bool
pdf_do_subset_font(gx_device_pdf *pdev, pdf_base_font_t *pbfont, gs_id rid)
{
gs_font_base *copied = pbfont->copied;
if (pbfont->do_subset == DO_SUBSET_UNKNOWN) {
int max_pct = pdev->params.MaxSubsetPct;
bool do_subset = pdev->params.SubsetFonts && max_pct > 0;
if (do_subset && max_pct < 100) {
do_subset = false;
if (max_pct > 0) {
int max_subset_used = pbfont->num_glyphs * max_pct / 100;
int used, index;
gs_glyph ignore_glyph;
do_subset = true;
for (index = 0, used = 0;
(copied->procs.enumerate_glyph((gs_font *)copied,
&index, GLYPH_SPACE_INDEX,
&ignore_glyph), index != 0);
)
if (++used > max_subset_used) {
do_subset = false;
break;
}
}
}
pbfont->do_subset = (do_subset ? DO_SUBSET_YES : DO_SUBSET_NO);
}
return (pbfont->do_subset == DO_SUBSET_YES);
}
int
pdf_write_FontFile_entry(gx_device_pdf *pdev, pdf_base_font_t *pbfont)
{
stream *s = pdev->strm;
const char *FontFile_key;
switch (pbfont->copied->FontType) {
case ft_TrueType:
case ft_CID_TrueType:
FontFile_key = "/FontFile2";
break;
default:
if (pdev->ResourcesBeforeUsage)
FontFile_key = "/FontFile";
else
FontFile_key = "/FontFile3";
}
stream_puts(s, FontFile_key);
pprintld1(s, " %ld 0 R", pbfont->FontFile->id);
return 0;
}
private int
pdf_adjust_font_name(gx_device_pdf *pdev, long id, pdf_base_font_t *pbfont)
{
int i;
byte *chars = (byte *)pbfont->font_name.data;
byte *data;
uint size = pbfont->font_name.size;
char suffix[sizeof(long) * 2 + 2];
uint suffix_size;
#define SUFFIX_CHAR '~'
for (i = size;
i > 0 && isxdigit(chars[i - 1]);
--i)
DO_NOTHING;
if (i < size && i > 0 && chars[i - 1] == SUFFIX_CHAR) {
do {
--i;
} while (i > 0 && chars[i - 1] == SUFFIX_CHAR);
size = i + 1;
}
sprintf(suffix, "%c%lx", SUFFIX_CHAR, id);
suffix_size = strlen(suffix);
data = gs_resize_string(pdev->pdf_memory, chars, size,
size + suffix_size,
"pdf_adjust_font_name");
if (data == 0)
return_error(gs_error_VMerror);
memcpy(data + size, (const byte *)suffix, suffix_size);
pbfont->font_name.data = data;
pbfont->font_name.size = size + suffix_size;
#undef SUFFIX_CHAR
return 0;
}
int
pdf_write_embedded_font(gx_device_pdf *pdev, pdf_base_font_t *pbfont,
gs_int_rect *FontBBox, gs_id rid, cos_dict_t **ppcd)
{
bool do_subset = pdf_do_subset_font(pdev, pbfont, rid);
gs_font_base *out_font =
(do_subset || pbfont->complete == NULL ? pbfont->copied : pbfont->complete);
gs_const_string fnstr;
pdf_data_writer_t writer;
int code;
if (pbfont->written)
return 0;
code = pdf_begin_data_stream(pdev, &writer, DATA_STREAM_BINARY |
(pdev->CompressFonts ?
DATA_STREAM_COMPRESS : 0), 0);
if (code < 0)
return code;
if (pdev->CompatibilityLevel == 1.2 &&
!do_subset && !pbfont->is_standard ) {
int code = pdf_adjust_font_name(pdev, writer.pres->object->id, pbfont);
if (code < 0)
return code;
}
fnstr.data = pbfont->font_name.data;
fnstr.size = pbfont->font_name.size;
switch (out_font->FontType) {
case ft_composite:
code = 0;
break;
case ft_encrypted2:
if (!pdev->HaveCFF) {
return_error(gs_error_unregistered);
}
case ft_encrypted:
if (pdev->HavePDFWidths) {
code = copied_drop_extension_glyphs((gs_font *)out_font);
if (code < 0)
return code;
}
if (!pdev->HaveCFF) {
int lengths[3];
code = psf_write_type1_font(writer.binary.strm,
(gs_font_type1 *)out_font,
WRITE_TYPE1_WITH_LENIV |
WRITE_TYPE1_EEXEC | WRITE_TYPE1_EEXEC_PAD,
NULL, 0, &fnstr, lengths);
if (lengths[0] > 0) {
if (code < 0)
return code;
code = cos_dict_put_c_key_int((cos_dict_t *)writer.pres->object,
"/Length1", lengths[0]);
}
if (lengths[1] > 0) {
if (code < 0)
return code;
code = cos_dict_put_c_key_int((cos_dict_t *)writer.pres->object,
"/Length2", lengths[1]);
if (code < 0)
return code;
code = cos_dict_put_c_key_int((cos_dict_t *)writer.pres->object,
"/Length3", lengths[2]);
}
} else {
#define TYPE2_OPTIONS (WRITE_TYPE2_NO_LENIV | WRITE_TYPE2_CHARSTRINGS)
code = cos_dict_put_string_copy((cos_dict_t *)writer.pres->object, "/Subtype", "/Type1C");
if (code < 0)
return code;
code = psf_write_type2_font(writer.binary.strm,
(gs_font_type1 *)out_font,
TYPE2_OPTIONS |
(pdev->CompatibilityLevel < 1.3 ? WRITE_TYPE2_AR3 : 0),
NULL, 0, &fnstr, FontBBox);
}
goto finish;
case ft_TrueType: {
gs_font_type42 *const pfont = (gs_font_type42 *)out_font;
#define TRUETYPE_OPTIONS (WRITE_TRUETYPE_NAME | WRITE_TRUETYPE_HVMTX)
const int options = TRUETYPE_OPTIONS |
(pdev->CompatibilityLevel <= 1.2 ?
WRITE_TRUETYPE_NO_TRIMMED_TABLE : 0) |
(pfont->data.numGlyphs != pfont->data.trueNumGlyphs ||
pbfont->do_subset == DO_SUBSET_YES ?
WRITE_TRUETYPE_CMAP : 0);
stream poss;
if (pdev->HavePDFWidths) {
code = copied_drop_extension_glyphs((gs_font *)out_font);
if (code < 0)
return code;
}
s_init(&poss, pdev->memory);
swrite_position_only(&poss);
code = psf_write_truetype_font(&poss, pfont, options, NULL, 0, &fnstr);
if (code < 0)
return code;
code = cos_dict_put_c_key_int((cos_dict_t *)writer.pres->object, "/Length1", stell(&poss));
if (code < 0)
return code;
if (code < 0)
return code;
code = psf_write_truetype_font(writer.binary.strm, pfont,
options, NULL, 0, &fnstr);
goto finish;
}
case ft_CID_encrypted:
code = cos_dict_put_string_copy((cos_dict_t *)writer.pres->object, "/Subtype", "/CIDFontType0C");
if (code < 0)
return code;
code = psf_write_cid0_font(writer.binary.strm,
(gs_font_cid0 *)out_font, TYPE2_OPTIONS,
NULL, 0, &fnstr);
goto finish;
case ft_CID_TrueType:
#define CID2_OPTIONS WRITE_TRUETYPE_HVMTX
code = psf_write_cid2_font(writer.binary.strm,
(gs_font_cid2 *)out_font,
CID2_OPTIONS, NULL, 0, &fnstr);
finish:
*ppcd = (cos_dict_t *)writer.pres->object;
if (code < 0) {
pdf_end_fontfile(pdev, &writer);
return code;
}
code = pdf_end_fontfile(pdev, &writer);
break;
default:
code = gs_note_error(gs_error_rangecheck);
}
pbfont->written = true;
return code;
}
int
pdf_write_CharSet(gx_device_pdf *pdev, pdf_base_font_t *pbfont)
{
stream *s = pdev->strm;
gs_font_base *font = pbfont->copied;
int index;
gs_glyph glyph;
stream_puts(s, "(");
for (index = 0;
(font->procs.enumerate_glyph((gs_font *)font, &index,
GLYPH_SPACE_NAME, &glyph),
index != 0);
) {
gs_const_string gstr;
int code = font->procs.glyph_name((gs_font *)font, glyph, &gstr);
if (code >= 0 &&
bytes_compare(gstr.data, gstr.size, (const byte *)".notdef", 7)
)
pdf_put_name(pdev, gstr.data, gstr.size);
}
stream_puts(s, ")");
return 0;
}
int
pdf_write_CIDSet(gx_device_pdf *pdev, pdf_base_font_t *pbfont,
long *pcidset_id)
{
pdf_data_writer_t writer;
int code;
code = pdf_begin_data_stream(pdev, &writer,
DATA_STREAM_BINARY |
(pdev->CompressFonts ? DATA_STREAM_COMPRESS : 0),
gs_no_id);
if (code < 0)
return code;
stream_write(writer.binary.strm, pbfont->CIDSet,
(pbfont->num_glyphs + 7) / 8);
code = pdf_end_data(&writer);
if (code < 0)
return code;
*pcidset_id = pdf_resource_id(writer.pres);
return 0;
}
bool
pdf_is_standard_font(pdf_base_font_t *bfont)
{ return bfont->is_standard;
}
void
pdf_set_FontFile_object(pdf_base_font_t *bfont, cos_dict_t *pcd)
{
bfont->FontFile = pcd;
}
const cos_dict_t *
pdf_get_FontFile_object(pdf_base_font_t *bfont)
{
return bfont->FontFile;
}